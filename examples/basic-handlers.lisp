(defpackage :cl-serve.basic-handler
  (:use :cl :cl-serve :cl-who))

(in-package :cl-serve.basic-handler)

;;; Start a socket server
(start)

;;;; Examples of handlers      
(defhandler ("")
  (with-html-output (*standard-output*)
    (:html
     (:head
      (:title "Home page | Blog"))
     (:body
      (:h1 "Welcome to my blog!")))))

(defhandler ("/home")
  (with-html-output (*standard-output*)
    (:html
     (:head
      (:title "Home page | Blog"))
     (:body
      (:h1 "Welcome to my blog!")))))

(defhandler ("not-found" :status 'not-found)
  (with-html-output (*standard-output*)
    (:html
     (:head
      (:title "Page not found | Blog"))
     (:body
      (:p "The page you are requesting does not exist yet!")
      (:hr)
      (:i "cl-serve 1.0")))))
