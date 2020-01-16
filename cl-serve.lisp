(defpackage :cl-serve
  (:nicknames :websrv)
  (:use :cl :port :cl-serve.utils :cl-fad :lisp-binary :cl+ssl :flexi-streams)
  (:export :*socket-stream*
	   :*socket*
	   :*socket-80*
	   :*host*
	   :*dispatch-table*
	   :*error-dispatch-table*
	   :*status-code-database*
	   :*project-dir*
	   :*static-dir*
	   :defhandler
	   :set-dispatch-table!
	   :start
	   :stop))

(in-package :cl-serve)

;;;; Primitives available in package `:port'
;;; (port:open-socket-server &optional port) ==> socket
;;; (port:open-socket host port &optional bin) ==> socket-stream
;;;; CL Primitives for stream IO
;;; (read stream)
;;; (print stream)

;;;; Global parameters
(defvar *socket* nil)
(defvar *socket-80* nil)
(defvar *socket-stream* nil)
(defvar *listener-lock* (make-lock :name "listener-lock"))
(defvar *active-listeners* nil)
(defvar *host* "localhost")
(defvar *socket-process* nil)
(defvar *socket-process-80* nil)
(defvar *dispatch-table* nil)
(defvar *status-code-database*
  '(("success" . "200 OK")
    ("moved-perm" . "301 Moved Permanently")
    ("see-other" . "303 See Other")
    ("not-found" . "404 Not Found")
    ("server-error" . "500 Server Error")))
(defvar *error-dispatch-table* nil)
(defvar *project-dir* nil)
(defvar *static-dir* nil)

(defun get-statuscode-string
    (code &optional (code-db *status-code-database*))
  (get-assoc-value code code-db))

;;;; Main web server functions/macros
(defmacro with-open-socket ((var socket) &body body)
  `(let ((,var ,socket))
     (unwind-protect
	  ,@body
       (stop ,var))))

(define-condition not-decodable-char ()
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "A character is not decodable in the stream."))))

(defun parse-uri-params (params)
  "Given `params' of the form `pkey_1=pval_1&pkey_2=pval_2...', return a list of the form `((pkey_1 . pval_1) (pkey_2 . pval_2) ...)', where the `pkey's are interned as Lisp symbols. By the way, the `pval's are also parsed.
e.g: (parse-uri-params \"name=arnold%20ngoran&age=28\") => ((NAME . \"arnold ngoran\") (AGE . \"28\"))"
  ;; Split `params' where pattern is the character #\&
  (if (zerop (length params))  ; No parameters
      nil
      (let ((param-lst (string-split params "&")))
	(labels ((rec (param-lst res)
		   (if (null param-lst)
		       res
		       ;; Split `params' where pattern
		       ;; is the character #\=
		       (let* ((lst (string-split
				    (car param-lst)
				    "="
				    :recursive-p nil))
			      (pair (if (= (length lst) 1)
					(list (car lst) "no-value")
					lst))
			      (dotted-pair  (make-sym-val-pair
				     pair #'parse-uri-pvalue)))
			 (rec (cdr param-lst)
			      (append res (list pair)))))))
	  (rec param-lst nil)))))

(defun parse-uri-pvalue (pval)
  "Given URI parameter value `pval' of the form ((%[0-9 0-9])?[0-9a-zA-Z+])*, return `pval' where HTTP symbols are transformed into normal UTF-8 characters.
e.g: (parse-uri-pvalue \"arnold%20le+messie\") => \"arnold le messie\""
  (let ((pval-lst (coerce pval 'list)))
    (labels ((rec (lst)
	       (if (null lst)
		   nil
		   (case (car lst)
		     (#\% (cons (http-char (cadr lst) (caddr lst))
				(rec (cdddr lst))))
		     (#\+ (cons #\Space (rec (cdr lst))))
		     (t (cons (car lst) (rec (cdr lst))))))))
      (coerce (rec pval-lst) 'string))))

(defmacro helper-set! (path fn table)
  `(typecase ,table
     ((or null list)  ; It is an assoc list
      (push (cons ,path ,fn) ,table))  ; update alist
     (hash-table (setf (gethash ,path ,table) ,fn))
     (t (error "~A type not yet supported!" ,table))))

(defmacro set-dispatch-table! (path fn status)
  (if (equal status "success")
      `(helper-set! ,path ,fn *dispatch-table*)
      `(helper-set! ,path ,fn *error-dispatch-table*)))  
	 
(defmacro defhandler ((path &key (doctypep t)
			    (content-type "text/html")
			    (status "success"))
		      &body body)
  `(let ((path (if (and (not (zerop (length ,path)))
			(equal (subseq ,path 0 1) "/"))
		   (subseq ,path 1)
		   ,path)))     
       (set-dispatch-table!
	path
	(lambda (headers params)
	  (let ((*standard-output* *socket-stream*))
	    (declare (ignorable headers params))
	    (print-resp-header
	     ,status :http-ver (get-assoc-value "HTTP-VER" headers)
	     :content-type ,content-type)
	    (terpri *socket-stream*)
	    (when (equal ,content-type "text/html")
	      (progn
		(when ,doctypep
		  (progn
		    (format *socket-stream* "~A~%"
			    "<!DOCTYPE html>"))))) ; HTML 5
	    ,@body))
	,status)))

(defun parse-req-init-line ()
  "Given the request initial line of the form \"REQ_TYPE URL HTTP_VERSION\", return 3 elements in the following order: URL REQ_TYPE HTTP_VERSION. Where URL is parsed and return of the form (DOMAIN_NAME . PARAMS). `PARAMS' might be nil if URL does not contain the character #\? .
`REQ_TYPE' has one of the values `GET', `POST' and `HEAD'.
e.g: (parse-req-init-line \"GET /home.html?name=arnold+ngoran&age=28 HTTP/1.1\") => (\"home.html\" (NAME . \"arnold ngoran\") (AGE . \"28\")) 
\"GET\" 
\"HTTP/1.1\""
  (let* (
	 ;;(req-line (safe-read-line *socket-stream*))
	 (req-line (read-line *socket-stream* nil nil))
	 (lst (string-split req-line #\Space)))
	(print req-line)
	(if (< (length lst) 2)
	    (values 'error nil nil nil)
	    (let* ((url (subseq (cadr lst) 1)) ; Skip the initial #\ character
		   (url-broken (string-split url #\?))
		   (url-brkn-parsed (cons (car url-broken)
					  (parse-uri-params (cadr url-broken)))))
	      (values
	       'success
	       url-brkn-parsed
	       (car lst)
	       (caddr lst))))))

;;; Request headers are og the following form:
;;; header_1: value_1
;;; header_2: value_2
;;; .................
;;; header_N: value_N
;;; Two consecutive header lines are separated by a new line
(defun parse-req-headers ()
  "Return list of dotted-pairs where each car is the header's key (as Lisp symbol) and each cdr is the header's value, in the order that they appear in the stream.
e.g: (parse-req-headers (concatenate 'string \"name:arnold\" (coerce '(#\Newline) 'string) \"age:28\")) => ((AGE . \"28\") (NAME . \"arnold\"))."
  (print "PARSE-REQ-HEADERS")
  (labels ((parse-one-line (hdr)
	     (make-sym-val-pair
	      hdr #'(lambda (x) (string-trim '(#\Space #\Return) x))))
	   (rec (res)
	     (let ((c (peek-char nil *socket-stream*)))
	       (cond ((find c '(#\Newline #\Return #\Linefeed) :test #'char=)
		      (read-char *socket-stream* nil nil)
		      (values 'success res))
		     ((not (alpha-char-p c))
		      (signal 'not-decodable-char)
		      (values 'error nil))
		     (t
		      (let* ((hline (read-line *socket-stream* nil ""))
			     (hdr-lst (string-split hline #\: :recursive-p nil)))
			;;(print hline)
			;;(print hdr-lst)
			(if (not (pairp hdr-lst))
			    (values 'success res)
			    (rec (cons (parse-one-line hdr-lst)
				       res)))))))))
    (rec nil)))

;;; Request body contains user-entered data
;;; and eventually uploaded files.
;;; The body is of the same form as the headers,
;;; that is lines of key:values separated by a new line.
(defun parse-req-body (headers)
  ;; If there is a request body, there should be a `CONTENT-LENGTH'
  ;; header line.
  (let ((content-line (assoc 'content-length headers)))
    (when content-line
      ;; TODO: Content is not necessarily a string
      (let ((content (make-string
		      (parse-integer (cdr content-line)))))
	(read-sequence content *socket-stream*)
	;; When of type `string', content might have the same form
	;; as URI params, so let's parse it to get a nice assoc list.
	(parse-uri-params content)))))

(defun print-resp-header (status &key http-ver
				   (content-type "text/html"))
  "Print the response header, which comes before the response body. It is required for the browser to interpret the body and render it properly."
  (let ((status (get-statuscode-string status))
	(http-ver (if http-ver
		      http-ver
		      "HTTP/1.1"))
	(content-type (if (equal content-type "text/html")
			  (concatenate 'string
				       content-type
				       "; charset=utf-8")
			  content-type)))
    (format *socket-stream* "~A ~A~%" http-ver status)
    (format *socket-stream* "Content-Type: ~A~%" content-type)))

(defun static-p (ftype)
  (not (or (equal ftype "text/html")
	   (equal ftype "*/*"))))

(defun print-error-resp (headers params)
  (print "Invalid Request | Returned 404.html page")
  (funcall (get-assoc-value "not-found" *error-dispatch-table*)
	   headers params))

(defun print-resp-body-file (path accept-type)
  ;;(declare (ignore content-type))  ; for now
  (if (equal accept-type "text")
      (with-lock (*listener-lock*) (copy-text-file path *socket-stream*))
      (with-lock (*listener-lock*) (copy-binary-file path *socket-stream*))))

(defun print-static-resp (accept path headers)
  (let* ((local-path (merge-pathnames path *static-dir*))
	 (file (probe-file local-path))
	 (http-ver (get-assoc-value "HTTP-VER" headers))
	 (status-code (if file "success" "not-found"))
	 (accept-type (car (string-split accept "/" :recursive-p nil))))
    (if (and (cl-fad:file-exists-p local-path)
	     (not (cl-fad:directory-pathname-p local-path)))
	(progn
	  (print-resp-header status-code :http-ver http-ver :content-type accept)
	  (terpri *socket-stream*)
	  (print-resp-body-file local-path accept-type)
	  t)
	nil)))

;;; FIND-REQ-HANDLER takes a path as argument and returns a lambda
;;; function of two parameters.
;;; It might be a pattern matcher, which matches a path to a handler.
(defun find-req-handler (path)
  "Return the request handler that matches the given `path'."
  ;; Look up first into `*dispatch-table'. If handler is not found
  ;; (value is NIL), then look up into `*error-dispatch-table'.
  ;; When handler is still not found, return the `not-found' handler.
  (let ((handler (get-assoc-value path *dispatch-table*)))
    (when (not handler)
      (setf handler (get-assoc-value path *error-dispatch-table*)))
    handler))

(defun get-resource (path headers params)
  (let* ((content-type (get-assoc-value "CONTENT-TYPE" headers))
	 (accept (car (string-split (get-assoc-value "ACCEPT" headers)
				    "," :recursive-p nil)))
	 (handler (find-req-handler path)))
    (cond (handler (funcall handler headers params))
	  ((and (or (static-p accept) (static-p content-type))
		(print-static-resp accept path headers)))
	  (t (print-error-resp headers params)))))

(defun parse-request ()
  ;; The first line is the request initial line
  (print "Parsing REQ LINE")
  (multiple-value-bind (error-code url req-type http-ver)
      (parse-req-init-line)
    ;;(declare (ignore req-type http-ver)) ; To consider later
    ;;(print "Parsing REQ HEADERS")
    (if (eq error-code 'error)
	(values 'error nil nil nil)
	(let* ((path (car url))
	       (uri-params (cdr url)))
	  (multiple-value-bind (error-code headers)	       
	       ;; The following lines are headers
	      (parse-req-headers)
	    (if (eq error-code 'error)
		(values 'error nil nil nil)
		(let* ((headers (cons (cons "REQ-TYPE" req-type)
				      headers))
		       (headers (cons (cons "HTTP-VER" http-ver)
				      headers))
		       ;; The following lines are body content
		       (body (parse-req-body headers))
		       ;; Build all params
		       (params (append uri-params body)))
		  (values 'success path headers params))))))))

(defun process-request (stream)
  (print "Process request...")
  (setf *socket-stream* stream)
  ;;(print *socket-stream*)
  (multiple-value-bind (error-code path headers params)
      (parse-request)
    ;;(let ((*standard-output* stream))
    (if (eq error-code 'error)
	(print "Bad request")
	(let ((req-type (get-assoc-value "REQ-TYPE" headers)))
	  (print headers)
	  (cond ((equal req-type "GET")
		 (get-resource path headers params))
		(t (print "/!\\ Request type not yet supported.")))))))

(defun run-listener (&optional (socket *socket*) &key cert privkey)
    (with-open-socket (sock socket)      
	  (loop
	     (block main-loop
	       (restart-case
		   (handler-bind ((not-decodable-char
				   #'(lambda (c)
				       (format t "~&/!\\ ~A" c)
				       (return-from main-loop)))
				  (sb-int:simple-stream-error
				   #'(lambda (ex)
				       (invoke-restart 'print-error-and-continue ex)
				       (return-from main-loop)))
				  (sb-int:stream-decoding-error
				   #'(lambda (ex)
				       (invoke-restart 'print-error-and-continue ex)
				       (return-from main-loop)))
				  (cl+ssl::ssl-error-syscall
				   #'(lambda (ex)
				       (invoke-restart 'print-error-and-continue ex)
				       (return-from main-loop)))
				  (cl+ssl::ssl-error-ssl
				   #'(lambda (ex)
				       (invoke-restart 'print-error-and-continue ex)
				       (return-from main-loop)))
				  (cl+ssl::ssl-error
				   #'(lambda (ex)
				       (invoke-restart 'print-error-and-continue ex)
				       (return-from main-loop)))
				  (flexi-streams:external-format-encoding-error
				   #'(lambda (ex)
				       (invoke-restart 'print-error-and-continue ex)
				       (return-from main-loop))))
		     (with-open-stream
			 (stream (ignore-errors (socket-accept sock)))
		       (terpri)
		       (print "#################################")
		       (print "Running Listener...")
		       ;;(print cert)
		       ;;(print privkey)
		       (when (and cert privkey)
			 (setf stream (cl+ssl:make-ssl-server-stream
				       stream
				       :external-format '(:utf-8 :eol-style :crlf)
				       :certificate (namestring cert)
				       :key (namestring privkey))))
		       ;;(print (read-char stream))
		       ;;(print stream)
		       (process-request stream)
		       ;;(print (eof-p stream))
		       (finish-output stream)
		       ;(clear-input stream)
		       ;;(print (eof-p stream))
		       ))
		 (print-error-and-continue (ex)
		   (format t "~&/!\\ Error~%: ~A" ex)
		   (print "Execution continues...")))))))

(defun run-listener-80 (&optional (socket *socket*) &key cert privkey)
    (with-open-socket (sock socket)      
      (loop
	 (block main-loop
	       (restart-case
		   (handler-bind ((sb-int:simple-stream-error
				   #'(lambda (ex)
				       (invoke-restart 'print-error-and-continue ex)
				       (return-from main-loop)))
				  (sb-int:stream-decoding-error
				   #'(lambda (ex)
				       (invoke-restart 'print-error-and-continue ex)
				       (return-from main-loop))))
		     (with-open-stream
			 (stream (socket-accept sock))
		       (terpri)
		       (print "#################################")
		       (print "Running Listener HTTP:80...")		       
		       (let ((*socket-stream* stream))
			 (multiple-value-bind (error-code url req-type http-ver)
			     (parse-req-init-line)
			   ;;(clear-input *socket-stream*)
			   (let ((path (car url))
				 (http-ver (string-right-trim '(#\Return) http-ver)))
			     (when (equal req-type "GET")
			       (print "Redirect to HTTPS:443...")
			       (format *socket-stream* "~A ~A~%" http-ver "301 Moved Permanently")
			       (format *socket-stream* "Location: https://netersys.com/~A~%" path)
			       (terpri *socket-stream*)))))))
		 (print-error-and-continue (ex)
		   (format t "~&/!\\ Error~%: ~A" ex)
		   (print "Execution continues...")))))))
		 

(defun start (&key (port 8080) (host "localhost") cert privkey)
  (setf *socket* (open-socket-server port)
	*socket-80* (open-socket-server 80)
	*host* host)
  ;; Create a parallel thread (or process)
  ;; where to listen to request and response.
  (setf *socket-process*
	(make-process "listener" #'run-listener *socket* :cert cert :privkey privkey))
  (setf *socket-process-80*
	(make-process "listener-80" #'run-listener-80 *socket-80* :cert cert :privkey privkey))
  *socket*)

(defun stop (&optional (socket *socket*))
  "Stop the given `socket', otherwise stop the current one, defined by the global variable *socket*."
  (socket-server-close socket)
  (when (process-active-p *socket-process*)
    (kill-process *socket-process*))
  (when (process-active-p *socket-process-80*)
    (kill-process *socket-process-80*)))


