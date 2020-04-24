(defpackage :cl-serve
  (:nicknames :websrv)
  (:use :cl :port :cl-serve.utils :cl-fad :lisp-binary :cl+ssl :flexi-streams)
  (:export :*domain*
	   :*domain-ip*
	   :*subdomains*
	   :*notfound-handler*
	   :*socket-stream*
	   :*socket*
	   :*socket-80*
	   :*host*
	   :*all-dispatch-table*
	   :*error-dispatch-table*
	   :*status-code-database*
	   :*project-dir*
	   :*static-dir*
	   :defhandler
	   :set-dispatch-table!
	   :set-subdomains
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
(defvar *proc-80* nil)
(defvar *proc* nil)
(defvar *socket-stream* nil)
(defvar *listener-lock* (make-lock :name "listener-lock"))
(defvar *active-listeners* nil)
(defvar *domain* nil)
(defvar *domain-ip* nil)
(defvar *subdomains* nil)
(defvar *socket-process* nil)
(defvar *socket-process-80* nil)
(defvar *dispatch-table* nil)
;(defvar *dispatch-table* (make-hash-table :test #'equal))
;(defvar *error-dispatch-table* (make-hash-table :test #'equal))
(defvar *dispatch-table* `(("" . ,(make-hash-table :test #'equal))))
(defvar *error-dispatch-table* `(("" . ,(make-hash-table :test #'equal))))
(defvar *notfound-handler* "not-found")

(defvar *status-code-database*
  '(
    ;; 1xx: Informational - Request received, continuing process
    ("continue" . "100 Continue")
    ("switch-proto" . "101 Switching Protocols")
    ;; 2xx: Success - The action was successfully received, understood, and accepted
    ("success" . "200 OK")
    ("created" . "201 Created")
    ("accepted" . "202 Accepted")
    ("nonauth-info" . "203 Non-Authoritative Information")
    ("no-content" . "204 No Content")
    ("reset-content" . "205 Reset Content")
    ("partial-content" . "206 Partial Content")
    ;; 3xx: Redirection - Further action must be taken in order to complete the request
    ("mult-choice" . "300 Multiple Choices")
    ("moved-perm" . "301 Moved Permanently")
    ("found" . "302 Found")
    ("see-other" . "303 See Other")
    ("not-modif" . "304 Not Modified")
    ("use-proxy" . "305 Use Proxy")
    ("temp-redirect" . "307 Temporary Redirect")
    ;; 4xx: Client Error - The request contains bad syntax or cannot be fulfilled
    ("bad-req" . "400 Bad Request")
    ("unauth" . "401 Unauthorized")
    ("pay-required" . "402 Payment Required")
    ("forbidden" . "403 Forbidden")
    ("not-found" . "404 Not Found")
    ("not-allowed" . "405 Method Not Allowed")
    ("not-accept" . "406 Not Acceptable")
    ("proxy-auth" . "407 Proxy Authentication Required")
    ("req-timeout" . "408 Request Time-out")
    ("conflict" . "409 Conflict")
    ("gone" . "410 Gone")
    ("len-required" . "411 Length Required")
    ("precond-failed" . "412 Precondition Failed")
    ("reqent-too-large" . "413 Request Entity Too Large")
    ("uri-too-large" . "414 Request-URI Too Large")
    ("unsupp-media-type" . "415 Unsupported Media Type")
    ("range-not-satisf" . "416 Requested range not satisfiable")
    ("expect-failed" . "417 Expectation Failed")
    ;; 5xx: Server Error - The server failed to fulfill an apparently valid request
    ("server-error" . "500 Internal Server Error")
    ("not-implemented" . "501 Not Implemented")
    ("bad-gateway" . "502 Bad Gateway")
    ("unavailable" . "503 Service Unavailable")
    ("gateway-timeout" . "504 Gateway Time-out")
    ("httpver-bad" . "505 HTTP Version not supported")))
(defvar *error-dispatch-table* nil)
(defvar *project-dir* nil)
(defvar *static-dir* nil)

(defun get-statuscode-string
    (code &optional (code-db *status-code-database*))
  (get-assoc-value code code-db))

;;;; Main web server functions/macros
(defun build-subdomain (&optional (subdom ""))  
  (if (equal subdom "")
      *domain*
      (concatenate 'string subdom "." *domain*)))

(defun map-host->name (host)
  (if (or (equal (concatenate 'string "www." *domain*) host)
	  (equal (concatenate 'string *domain-ip* ":80") host)
	  (equal (concatenate 'string *domain-ip* ":443") host)
	  (equal *domain-ip* host))
      *domain*
      host))

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
			      (dotted-pair
			       (make-sym-val-pair pair
						  #'parse-uri-pvalue)))
			 (rec (cdr param-lst)
			      (append res (list dotted-pair)))))))
	  (rec param-lst nil)))))

(defun parse-uri-pvalue (pval)
  "Given URI parameter value `pval' of the form ((%[0-9 0-9])?[0-9a-zA-Z+])*, return `pval' where HTTP symbols are transformed into normal UTF-8 characters.
e.g: (parse-uri-pvalue \"arnold%20le+messie\") => \"arnold le messie\""
  (let ((pval-lst (coerce pval 'list)))
    (labels ((rec (lst result)
	       (if (null lst)
		   (nreverse result)
		   (case (car lst)
		     (#\% (rec (cdddr lst)
			       (cons (http-char (cadr lst) (caddr lst))
				     result)))
		     (#\+ (rec (cdr lst) (cons #\Space result)))
		     (t (rec (cdr lst) (cons (car lst) result)))))))
      (coerce (rec pval-lst nil) 'string))))

(defmacro helper-set! (host path fn table)
  `(setf (gethash ,path
		  (cdr (assoc ,host ,table :test #'equal)))
	 ,fn))

(defun set-dispatch-table! (host path fn status)
  (if (equal status "success")
      (helper-set! host path fn *dispatch-table*)
      (helper-set! host path fn *error-dispatch-table*)))

(defmacro defhandler ((path &key (host "") (doctypep t)
			    (content-type "text/html")
			    (status "success"))
		      &body body)
  `(let ((path (if (and (not (zerop (length ,path)))
			(equal (subseq ,path 0 1) "/"))
		   (subseq ,path 1)
		   ,path)))     
     (set-dispatch-table!
      ,(build-subdomain host)
	path
	(lambda (stream headers params)
	    (declare (ignorable headers params))
	    (print-resp-header stream
	     ,status :http-ver (get-assoc-value "HTTP-VER" headers)
	     :content-type ,content-type)
	    (terpri stream)
	    (cond ((equal ,content-type "text/html")
		   (progn
		     (when ,doctypep
		       (progn
			 (format stream "~A~%"
				 "<!DOCTYPE html>"))) ; HTML 5
		     (format stream "~A" ,@body)))
		  ((equal ,content-type "lambda")
		   ,@body)))
	,status)))

(defun parse-req-init-line (stream)
  "Given the request initial line of the form \"REQ_TYPE URL HTTP_VERSION\", return 3 elements in the following order: URL REQ_TYPE HTTP_VERSION. Where URL is parsed and return of the form (DOMAIN_NAME . PARAMS). `PARAMS' might be nil if URL does not contain the character #\? .
`REQ_TYPE' has one of the values `GET', `POST' and `HEAD'.
e.g: (parse-req-init-line \"GET /home.html?name=arnold+ngoran&age=28 HTTP/1.1\") => (\"home.html\" (NAME . \"arnold ngoran\") (AGE . \"28\")) 
\"GET\" 
\"HTTP/1.1\""
  (let* (
	 ;;(req-line (safe-read-line *socket-stream*))	 
	 (req-line (read-line stream nil nil))  ; ##### FIXME hanging
	 (lst (string-split req-line #\Space)))
	(print req-line)
	(if (< (length lst) 3)
	    (values 'error nil nil nil)
	    (let* ((abs-url (cadr lst))
		   ;; Skip the initial #\/ character
		   (url (if (string-equal (subseq abs-url 0 1) "/")
			    (subseq abs-url 1)
			    abs-url))
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
(defun parse-req-headers (stream)
  "Return list of dotted-pairs where each car is the header's key (as Lisp symbol) and each cdr is the header's value, in the order that they appear in the stream.
e.g: (parse-req-headers (concatenate 'string \"name:arnold\" (coerce '(#\Newline) 'string) \"age:28\")) => ((AGE . \"28\") (NAME . \"arnold\"))."
  (print "PARSE-REQ-HEADERS")
  (labels ((parse-one-line (hdr)
	     (make-sym-val-pair
	      hdr #'(lambda (x) (string-trim '(#\Space #\Return) x))))
	   (rec (res)
	     (let ((c (peek-char nil stream)))
	       (cond ((find c '(#\Newline #\Return #\Linefeed)
			    :test #'char=)
		      (read-line stream nil nil)
		      (values 'success res))
		     ((not (alpha-char-p c))
		      (signal 'not-decodable-char)
		      (values 'error nil))
		     (t
		      (let* ((hline (read-line stream nil ""))
			     (hdr-lst (string-split hline #\:
						    :recursive-p nil)))
			(if (not (pairp hdr-lst))
			    (values 'success res)
			    (rec (cons (parse-one-line hdr-lst)
				       res)))))))))
    (rec nil)))

;;; Request body contains user-entered data
;;; and eventually uploaded files.
;;; The body is of the same form as the headers,
;;; that is lines of key:values separated by a new line.
(defun parse-req-body (stream headers)
  ;; If there is a request body, there should be a `CONTENT-LENGTH'
  ;; header line.
  (let ((content-line (assoc "CONTENT-LENGTH" headers
			     :test #'equal)))
    (when content-line
      ;; TODO: Content is not necessarily a string
      (let ((content (make-string
		      (parse-integer (cdr content-line)))))
	(read-sequence content stream)
	;; When of type `string', content might have the same form
	;; as URI params, so let's parse it to get a nice assoc list.
	(parse-uri-params content)))))

(defun print-resp-header (stream status &key http-ver
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
    (format stream "~A ~A~%" http-ver status)
    (format stream "Content-Type: ~A~%" content-type)))

(defun static-p (ftype)
  (not (or (equal ftype "text/html")
	   (equal ftype "*/*"))))

(defun print-error-resp (stream headers params)
  (print "Invalid Request | Returned 404.html page")
  (let* ((lst-handler
	     (get-assoc-value *domain*
			      *error-dispatch-table*))
	 (handler (gethash *notfound-handler* lst-handler)))
    (funcall handler stream headers params)))

(defun print-resp-body-file (stream path accept-type)
  ;;(declare (ignore content-type))  ; for now
  (if (equal accept-type "text")
      ;;(with-lock (*listener-lock*) (copy-text-file path stream))
      ;;(with-lock (*listener-lock*) (copy-binary-file path stream))
      (copy-text-file path stream)
      (copy-binary-file path stream))
  )

(defun print-static-resp (stream accept path headers)
  (let* ((local-path (merge-pathnames path *static-dir*))
	 (file (probe-file local-path))
	 (http-ver (get-assoc-value "HTTP-VER" headers))
	 (status-code (if file "success" "not-found"))
	 (accept-type (car (string-split accept "/"
					 :recursive-p nil))))
    (if (and (cl-fad:file-exists-p local-path)
	     (not (cl-fad:directory-pathname-p local-path)))
	(progn
	  (print-resp-header stream status-code
			     :http-ver http-ver :content-type accept)
	  (terpri stream)
	  (print-resp-body-file stream local-path accept-type)
	  t)
	nil)))

;;; FIND-REQ-HANDLER takes a path as argument and returns a lambda
;;; function of two parameters.
;;; It might be a pattern matcher, which matches a path to a handler.
(defun find-req-handler (path host)
  "Return the request handler that matches the given `path'."
  ;; Look up first into `*dispatch-table'. If handler is not found
  ;; (value is NIL), then look up into `*error-dispatch-table'.
  ;; When handler is still not found, return the `not-found' handler.
  (let ((lst-handler (get-assoc-value host *dispatch-table*))
	(flag nil)
	(handler nil))
    (when lst-handler
      (setf handler (gethash path lst-handler)))
    handler))

(defun get-resource (stream path host headers params)
  (let* ((content-type (get-assoc-value "CONTENT-TYPE" headers))
	 (accept (car (string-split (get-assoc-value "ACCEPT" headers)
				    "," :recursive-p nil)))
	 (handler (find-req-handler path host)))
    (cond (handler (funcall handler stream headers params))
	  ((and (or (static-p accept) (static-p content-type))
		(print-static-resp stream accept path headers)))
	  (t (print-error-resp stream headers params)))))

(defun parse-request (stream)
  ;; The first line is the request initial line
  (print "Parsing REQ LINE")
  (multiple-value-bind (error-code url req-type http-ver)
      (parse-req-init-line stream)
    (if (eq error-code 'error)
	(values 'error nil nil nil)
	(let* ((path (car url))
	       (uri-params (cdr url)))
	  (multiple-value-bind (error-code headers)	       
	       ;; The following lines are headers
	      (parse-req-headers stream)
	    (if (eq error-code 'error)
		(values 'error nil nil nil)
		(let* ((headers (cons (cons "REQ-TYPE" req-type)
				      headers))
		       (headers (cons (cons "HTTP-VER" http-ver)
				      headers))
		       ;; The following lines are body content
		       (body (parse-req-body stream headers))
		       ;; Build all params
		       (params (append uri-params body)))
		  (values 'success path headers params))))))))

(defun process-request (stream)
  (print "Process request...")  
  (block main-loop
    (restart-case
	(handler-bind
	    ((not-decodable-char
	      #'(lambda (c)
		  (format t "~&/!\\ Error:~%~A" c)))
	     (sb-int:simple-stream-error
	      #'(lambda (ex)
		  (invoke-restart 'print-error-and-continue ex)))
	     (sb-sys:io-timeout
	      #'(lambda (ex)
		  (invoke-restart 'print-error-and-continue ex)))
	     (sb-int:stream-decoding-error
	      #'(lambda (ex)
		  (invoke-restart 'print-error-and-continue ex)))
	     (cl+ssl::ssl-error
	      #'(lambda (ex)
		  (invoke-restart 'print-error-and-continue ex))))
	  (unwind-protect
	       (multiple-value-bind
		     (error-code path headers params)
		   ;; READ REQUEST FROM STREAM
		   (parse-request stream)
		 ;; WRITE RESPONSE TO STREAM
		 (if (eq error-code 'error)
		     (print "Bad request")
		     (let* ((req-type
			     (get-assoc-value "REQ-TYPE"
					      headers))
			    (host (get-assoc-value "HOST"
						   headers))
			    (host (map-host->name host)))
		       (print headers)
		       (cond ((or (equal req-type "GET")
				  (equal req-type "POST"))
			      (get-resource stream path host
					    headers params))
			     (t
			      (print "/!\\ Request type not yet supported.")))))
		 (force-output stream))
	    (close stream)))
      (print-error-and-continue (ex)
	(format t "~&/!\\ Error:~%~A" ex)
	(print "Execution continues...")))))

(defun run-listener (&key cert privkey)
  (setf *socket-serv* (open-socket-server 443))
  (unwind-protect
    (loop
       (let* ((socket-stream (socket-accept *socket-serv*))
	      (socket
	       (ignore-errors
		 (cl+ssl:make-ssl-server-stream
		  socket-stream
		  :external-format '(:utf-8 :eol-style :crlf)
		  :certificate (namestring cert)
		  :key (namestring privkey)))))
	 (when socket
	   (terpri)
	   (print "#################################")
	   (print "Running Listener...")
	   (push
	    (make-process "process-request"
			  #'process-request socket)
	    *proc*)
	   (when (= 100 (length *proc*))
	     (setf *proc* nil)))))
    (socket-server-close *socket-serv*)))

(defun run-listener-80 (domain)
  (setf *socket-serv-80* (open-socket-server 80))
  (unwind-protect
    (loop
       (let ((socket (socket-accept *socket-serv-80*)))
	 (when socket
	   (terpri)
	   (print "#################################")
	   (print "Running Listener HTTP:80...")
	   (format t "~&> Received request from host ~a~%"
		   (socket-host/port socket))
	   (push
	    (make-process "redirect-to-443"
			  #'redirect-to-443 socket domain)
	    *proc-80*)
	   (when (= 100 (length *proc-80*))
	     (setf *proc-80* nil)))))
    (socket-server-close *socket-serv-80*)))

(defun redirect-to-443 (socket domain)
  (block main-loop
    (restart-case
	(handler-bind
	    ((sb-int:simple-stream-error
	      #'(lambda (ex)
		  (invoke-restart
		   'print-error-and-continue ex)))
	     (sb-int:stream-decoding-error
	      #'(lambda (ex)
		  (invoke-restart
		   'print-error-and-continue ex)))
	     (sb-sys:io-timeout
	      #'(lambda (ex)
		  (invoke-restart
		   'print-error-and-continue ex))))
	  (unwind-protect
	       (multiple-value-bind (error-code url req-type http-ver)
		   (parse-req-init-line socket)
		 (let ((path (car url))
		       (http-ver (string-right-trim
				  '(#\Return) http-ver)))
		   (when (equal req-type "GET")
		     (print "Redirect to HTTPS:443...")
		     (format socket "~A ~A~%"
			     http-ver "301 Moved Permanently")
		     (format socket
			     "Location: https://~A/~A~%"
			     domain path)		     
		     (force-output socket))))
	    (close socket)))
      (print-error-and-continue (ex)
	(format t "~&/!\\ Error~%: ~A" ex)
	(print "Execution continues...")))))

(defun format-subdomain ()  
  (let ((sd (mapcar #'(lambda (s)
			(concatenate 'string s "." *domain*))
		    *subdomains*)))
    (setf *subdomains* sd)))

(defun set-subdomains (lst-sub)
  (setf *subdomains* lst-sub)
  (format-subdomain)
  (push *domain* *subdomains*)
  (setf *dispatch-table*
	(mapcar #'(lambda (s)
		    (cons s (make-hash-table :test #'equal)))
		*subdomains*))
  (setf *error-dispatch-table*
	(mapcar #'(lambda (s)
		    (cons s (make-hash-table :test #'equal)))
		*subdomains*)))

(defun start (domain &key cert privkey)
  ;; Create a parallel thread (or process)
  ;; where to listen to request and response.
  (make-process "listener" #'run-listener :cert cert :privkey privkey)
  (make-process "listener-80" #'run-listener-80 domain))

(defun stop ()
  "Stop the given `socket', otherwise stop the current one, defined by the global variable *socket*."
  (print *socket-serv*)
  (print *socket-serv-80*)
  (socket-server-close *socket-serv*)
  (socket-server-close *socket-serv-80*))
