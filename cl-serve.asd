;;; -*- mode: lisp -*-

(defpackage :cl-serve-system
  (:use :cl :asdf))
(in-package :cl-serve-system)

(defsystem cl-serve
  :author "Arnold N'GORAN"
  :licence "LLGPL"
  :components ((:file "utils")
	       (:file "cl-serve"
		      :depends-on ("utils")))
  :depends-on (port cl-who cl-fad lisp-binary babel cl+ssl flexi-streams))
