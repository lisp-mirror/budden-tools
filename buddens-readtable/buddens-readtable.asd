;;; -*- Encoding: utf-8; -*-
;; -*- Mode: Lisp -*-

(defsystem :buddens-readtable
        :serial t
        :depends-on 
        (:named-readtables :buddens-reader)
	:components 
        (
         (:file "buddens-readtable-package")
         (:file "buddens-readmacros")
         (:file "buddens-readmacros-test")
         (:file "buddens-readtable" :description "Здесь определены таблицы чтения")
;	 (:file "buddens-sharpdot")
         (:file "buddens-readtable-test")
))
; asdf
