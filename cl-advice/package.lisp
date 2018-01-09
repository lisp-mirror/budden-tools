;;; -*- coding: utf-8; System :cl-advice;  -*-

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
  (:shadow
   #:advice-definition-location-indicator)
  (:use :cl))
