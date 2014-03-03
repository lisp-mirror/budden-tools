;;; -*- Encoding: utf-8; -*-
(asdf:of-system :buddens-readtable)
(defpackage :buddens-readtable 
  (:use :cl :named-readtables)
  (:export #:*read-eval-stream*)
  )
