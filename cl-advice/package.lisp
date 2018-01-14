;; -*- coding: utf-8; -*-
;; This file must be loaded by load-cl-advice.lisp

(cl:defpackage :cl-advice 
  (:export 
   #:install-advice
   #:define-advice
   #:uninstall-advice
   #:portably-without-package-locks
   #:|С-декорированной-функцией|
   #:make-symbol-for-expression
   ;#:*undecorated-function-source-locations*
   ;#:get-original-function-source-location
   )
  (:use :cl))

(cl:defpackage :function-advice-generated-names
               (:use))