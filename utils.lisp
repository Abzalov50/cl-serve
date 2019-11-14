(defpackage :cl-serve.utils
  (:use :cl))

(in-package :cl-serve.utils)

;;;; Helper functions
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
			  (append res (list cur-str str))))
		     (t (rec (subseq str 1)
			     (concatenate 'string
					  cur-str (subseq str 0 1))
			     res)))))
      (rec str "" nil))))

(defun make-sym-val-pair (lst fn)
  "Given a list `lst', return a dotted-pair where the CAR is the symbol corresponding to the string located at `(car lst)' and the CDR is the result of the application of the function `fn' (of one argument) to `(cdr lst)'."
  (let ((pair (list->dotted lst)))
    (cons (intern (string-upcase (car pair)))
	  (funcall fn (cdr pair)))))

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
