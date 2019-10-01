;;; -*- mode: lisp -*-

(defpackage :cl-serve-system
  (:use :cl :asdf))
(in-package :cl-serve-system)

(defsystem cl-serve
  :author "Arnold N'GORAN"
  :licence "LLGPL"
  :components ((:file "cl-serve"))
  :depends-on (port cl-who))
