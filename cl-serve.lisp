(defpackage :cl-serve
  (:nicknames :websrv)
  (:use :cl :port :cl-serve.utils)
  (:export :*socket*
	   :*dispatch-table*
	   :*error-dispatch-table*
	   :*status-code-database*
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
(defvar *socket-process* nil)
(defvar *dispatch-table* nil)
(defvar *status-code-database*
  '((success . "200 OK")
    (see-other . "303 See Other")
    (not-found . "404 Not Found")
    (server-error . "500 Server Error")))
(defvar *error-dispatch-table* nil)

(defun get-statuscode-string
    (code &optional (code-db *status-code-database*))
  (get-assoc-value code code-db))

;;;; Main web server functions/macros
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
			      ;; Pair is a dotted-list
			      (pair (make-sym-val-pair
				     lst #'parse-uri-pvalue)))
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

(defun parse-req-init-line (req-line)
  "Given the request initial line of the form \"REQ_TYPE URL HTTP_VERSION\", return 3 elements in the following order: URL REQ_TYPE HTTP_VERSION. Where URL is parsed and return of the form (DOMAIN_NAME . PARAMS). `PARAMS' might be nil if URL does not contain the character #\? .
`REQ_TYPE' has one of the values `GET', `POST' and `HEAD'.
e.g: (parse-req-init-line \"GET /home.html?name=arnold+ngoran&age=28 HTTP/1.1\") => (\"home.html\" (NAME . \"arnold ngoran\") (AGE . \"28\")) 
\"GET\" 
\"HTTP/1.1\""
  (let* ((lst (string-split req-line #\Space))
	 (url (subseq (cadr lst) 1)) ; Skip the initial #\ character
	 (url-broken (string-split url #\?))
	 (url-brkn-parsed (cons (car url-broken)
				(parse-uri-params (cadr url-broken)))))
    (values
     url-brkn-parsed
     (car lst)
     (caddr lst))))

;;; Request headers are og the following form:
;;; header_1: value_1
;;; header_2: value_2
;;; .................
;;; header_N: value_N
;;; Two consecutive header lines are separated by a new line
(defun parse-req-headers (stream)
  "Return list of dotted-pairs where each car is the header's key (as Lisp symbol) and each cdr is the header's value, in the order that they appear in the stream.
e.g: (parse-req-headers (concatenate 'string \"name:arnold\" (coerce '(#\Newline) 'string) \"age:28\")) => ((AGE . \"28\") (NAME . \"arnold\"))."
    (labels ((parse-one-line (hdr)
	       (make-sym-val-pair
		(string-split hdr #\: :recursive-p nil) #'identity))
	     (rec (stream res)
	       (let ((hline (read-line stream)))
		 ;; blank line (of length 1) marks
		 ;; that there is no more header line
		 (if (= 1 (length hline))
		     res
		     (rec stream
			  (cons (parse-one-line hline)
				res))))))
      (rec stream nil)))

;;; Request body contains user-entered data
;;; and eventually uploaded files.
;;; The body is of the same form as the headers,
;;; that is lines of key:values separated by a new line.
(defun parse-req-body (headers stream)
  ;; If there is a request body, there should be a `CONTENT-LENGTH'
  ;; header line.
  (let ((content-line (assoc 'content-length headers)))
    (when content-line
      ;; TODO: Content is not necessarily a string
      (let ((content (make-string
		      (parse-integer (cdr content-line)))))
	(read-sequence content stream)
	;; When of type `string', content might have the same form
	;; as URI params, so let's parse it to get a nice assoc list.
	(parse-uri-params content)))))

(defun parse-request (stream)
  ;; The first line is the request initial line
  (multiple-value-bind (url req-type http-ver)
      (parse-req-init-line (read-line stream))
    ;(declare (ignore req-type http-ver)) ; To consider later
    (let* ((path (car url))
	   (uri-params (cdr url))
	   ;; The following lines are headers
	   (headers (parse-req-headers stream))
	   (headers (cons (cons 'req-type req-type)
			  headers))
	   (headers (cons (cons 'http-ver http-ver)
			  headers))
	   ;; The following lines are body content
	   (body (parse-req-body headers stream))
	   ;; Build all params
	   (params (append uri-params body)))
      (values path headers params))))

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
    (format *standard-output* "~A ~A~%" http-ver status)
    (format *standard-output* "Content-Type: ~A~%" content-type)))

(defmacro helper-set! (path fn table)
  `(typecase ,table
     ((or null list)  ; It is an assoc list
      (push (cons ,path ,fn) ,table))  ; update alist
     (hash-table (setf (gethash ,path ,table) ,fn))
     (t (error "~A type not yet supported!" ,table))))

(defmacro set-dispatch-table! (path fn status)
  (if (eq status 'success)
      `(helper-set! ,path ,fn *dispatch-table*)
      `(helper-set! ,path ,fn *error-dispatch-table*)))  
	 
;;; FIND-REQ-HANDLER takes a path as argument and returns a lambda
;;; function of two parameters.
;;; It might be a pattern matcher, which matches a path to a handler.
;(defun find-req-handler (path)
;  (labels ((test 
(defun find-req-handler (path)
  "Return the request handler that matches the given `path'."
  ;; Look up first into `*dispatch-table'. If handler is not found
  ;; (value is NIL), then look up into `*error-dispatch-table'.
  ;; When handler is still not found, return the `not-found' handler.
  (let ((handler (get-assoc-value path *dispatch-table*)))
    (when (not handler)
      (setf handler (get-assoc-value path *error-dispatch-table*)))
    (when (not handler)
      (setf handler
	    (get-assoc-value "not-found" *error-dispatch-table*)))
    handler))

(defmacro with-open-socket ((var socket) &body body)
  `(let ((,var ,socket))
     (unwind-protect
	  ,@body
       (stop ,var))))

(defmacro defhandler ((path &key (doctypep t)
			    (content-type "text/html")
			    (status 'success))
		      &body body)
  `(let ((path (if (and (not (zerop (length ,path)))
			(equal (subseq ,path 0 1) "/"))
		   (subseq ,path 1)
		   ,path)))
     (set-dispatch-table!
      path
      (lambda (headers params)
	(declare (ignorable headers params))
	(print-resp-header
	 ',status :http-ver (get-assoc-value 'http-ver headers)
	 :content-type ,content-type)
	(terpri)
	(when (equal ,content-type "text/html")
	  (progn
	    ;(print "OK")
	    (when ,doctypep
	      (progn
		;(format *standard-output* "~&OK 2~%")
		(format *standard-output* "~A~%"
			"<!DOCTYPE html>"))))) ; HTML 5
	,@body)
      ,status)))

;;;; Start/Stop server
(defun run-listener (&optional (socket *socket*) host port)
  "Loop of listening to request, handling it and sending a response."
  (with-open-socket (sock socket)
    (loop (with-open-stream
	      (stream
	       (if (and host port)
		   (open-socket host port)
		   (socket-accept sock)))
	    (multiple-value-bind (path headers params)
		(parse-request stream)
	      (let ((*standard-output* stream))
		(funcall (find-req-handler path)
			 headers params)))))))

(defun start (&key (port 8080) host)
  (setf *socket* (open-socket-server port))
  ;; Create a parallel thread (or process)
  ;; where to listen to request and response.
  (setf *socket-process*
	(make-process "listener" #'run-listener *socket* host port))
  *socket*)

(defun stop (&optional (socket *socket*))
  "Stop the given `socket', otherwise stop the current one, defined by the global variable *socket*."
  (socket-server-close socket)
  (kill-process *socket-process*))
