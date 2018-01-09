;;; -*- Encoding: utf-8; system :buddens-readtable ; -*-
(defpackage :buddens-readtable 
  (:use :cl :named-readtables :budden-tools)
  (:export budden-tools:*read-eval-stream*
           #:enable-triple-quote-reader)
  )
