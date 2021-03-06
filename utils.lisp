(defpackage :cl-serve.utils
  (:use :cl :babel :cl-smtp)
  (:import-from :alexandria :switch)
  (:export :get-assoc-value
	   :binary-file-p
	   :notnull
	   :string-split
	   :parse-key-val-string
	   :make-sym-val-pair
	   :list->dotted
	   :http-char
	   :pairp
	   :copy-binary-file
	   :copy-text-file
	   :dotted-pair-p
	   :safe-read-line
	   :read-line-crlf
	   :get-email))

(in-package :cl-serve.utils)

;;;; Helper functions
(defun notnull (x)
  (not (null x)))

(defun get-assoc-value (key alist)
  "Given `alist', return the value corresponding to the given `key', if it exists, or return NIL otherwise. Each element of the alist must be a dotted-pair.
e.g: (get-assoc-value 'name '((age . 28) (name . arnold)))
     => ARNOLD"
  (cdr (assoc key alist :test #'equal)))

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code
	 (parse-integer (coerce (list c1 c2) 'string)
				   :radix 16
				   :junk-allowed t)))
    (if code
	(code-char code)
	default)))	

(defun string-split (str pattern &key (recursive-p t))
  "Remove all occurrences of `pattern' from `str' and return the list of resulted substrings. If `recursive-p' is T then `pattern' is search over all the characters of `str', otherwise we stop at its first occurrence.
e.g: (string-split \"arnold est non seulement beau mais innofensif\" \"no\") => "
  (let* ((pattern (if (typep pattern 'character)
		      (coerce (list pattern) 'string)
		      pattern))
	 (len (length pattern)))
    (labels ((rec (str cur-str res)
	       (cond ((zerop (length str))
		      (append res (list cur-str)))
		     ((< (length str) len)
		      (rec ""
			   (concatenate 'string cur-str str)
			   res))		     
		     ((string= (subseq str 0 len)
			       pattern)
		      (if recursive-p
			  (rec (subseq str len) ""
			       (append res (list cur-str)))
			  (append res (list cur-str (subseq str 1)))))
		     (t (rec (subseq str 1)
			     (concatenate 'string
					  cur-str (subseq str 0 1))
			     res)))))
      (rec str "" nil))))

(defun make-sym-val-pair (lst fn)
  "Given a list `lst', return a dotted-pair where the CAR is the symbol corresponding to the string located at `(car lst)' and the CDR is the result of the application of the function `fn' (of one argument) to `(cdr lst)'."
  (let ((pair (list->dotted lst)))
    (cons (string-upcase (car pair))
	  (funcall fn (cdr pair)))))

(defun dotted-pair-p (x)
  (and (consp x) (not (consp (cdr x)))))

(defun pairp (x)
  (and (consp x) (= (length x) 2)))

(defun list->dotted (lst)
  "Given the 2 element list `lst', return a dotted list.
e.g: (list->dotted '(1 2)) => (1 . 2)
     (list->dotted '(1 ())) => (1)"
  (if (not (= (length lst) 2))
      (error "The given list must contain exactly 2 elements.")
      (cons (car lst) (cadr lst))))

(defun parse-key-val-string (str fn)
  "Return list of dotted-pairs where each car is the string's key (as Lisp symbol) and each cdr is the header's value. The given `string' is supposed to be of the form key:values separated by a new line.
N.B: The results does not preserve the order of the lines in the given `string'.
e.g: (parse-key-val-string (concatenate 'string \"name:arnold\" (coerce '(#\Newline) 'string) \"age:28\") #'identity) => ((AGE . \"28\") (NAME . \"arnold\"))."
  (let ((str (string-split str #\Newline)))
    (labels ((parse-one-line (hdr)
	       (make-sym-val-pair
		(string-split hdr #\:) fn))
	     (rec (str res)
	       (if (null str)
		   res
		   (rec (cdr str)
			(cons (parse-one-line (car str))
			      res)))))
      (rec str nil))))

(defun copy-binary-file (from-path to-stream)
  (with-open-file (from-stream from-path :direction :input :element-type '(unsigned-byte 8)
			       :if-does-not-exist nil)
    (when from-stream
      (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
	
	(loop for bytes-read = (read-sequence buffer from-stream)
	   while (plusp bytes-read)
	   do (write-sequence buffer to-stream :end bytes-read))))))

(defun copy-text-file (from-path to-stream
		       &key (content-encoding "gzip"))
  (with-open-file (stream from-path :if-does-not-exist nil)
    (if (or (equal content-encoding "gzip")
	    (equal content-encoding "deflate"))
	(let ((file-in-octets (flexi-streams:string-to-octets
			       (with-output-to-string (s)
				 (when stream
				   (loop for line = (read-line
						     stream nil)
				      while line
				      do (format s "~a~%" line))))
			       :external-format 'utf-8)))
	  (if (equal content-encoding "gzip")
	      (write-sequence (salza2:compress-data
			       file-in-octets 'salza2:gzip-compressor)
			      to-stream)
	      (write-sequence (salza2:compress-data
			       file-in-octets
			       'salza2:deflate-compressor)
			      to-stream)))
	(cl-fad:copy-stream stream to-stream))))

(defun safe-read-line (stream)
  (coerce
   (loop :for c = (read-byte stream nil nil)
      :until (= c 10)
      :collect (code-char c)
      :do (print (code-char c)))
   'string))

(defun read-line-crlf (stream &optional eof-error-p)
  (let ((s (make-string-output-stream)))
    (loop
       for empty = t then nil
       for c = (read-char stream eof-error-p nil)
       while (and c (not (eql c #\return)))
       do
	 (unless (eql c #\newline)
	   (write-char c s))
       finally
	 (return
	   (if empty nil (get-output-stream-string s))))))

(defun binary-file-p (content-type)
  (or (equal content-type "image/webp")
      (equal content-type "image/webm")
      (equal content-type "image/bmp")
      (equal content-type "image/jpeg")
      (equal content-type "image/gif")
      (equal content-type "image/png")
      (equal content-type "image/tiff")
      (equal content-type "image/svg+xml")
      (equal content-type "audio/aac")
      (equal content-type "audio/midi")
      (equal content-type "audio/ogg")
      (equal content-type "audio/webm")
      (equal content-type "audio/3gpp")
      (equal content-type "audio/3gpp2")
      (equal content-type "audio/x-wav")
      (equal content-type "video/mpeg")
      (equal content-type "video/ogg")
      (equal content-type "video/webm")
      (equal content-type "video/3gpp2")
      (equal content-type "video/x-msvideo")))

;;; Email
(defun get-email (text from recipients &key metadata)
  "Generic send SMTP mail with some `text' `from' a domain email to RECIPIENTS"
  (let ((text (format nil "~A~%~%-------- Métadonnées:~%~A"
		      text metadata)))
    (cl-smtp:send-email "localhost" from recipients "Contact" text)))
