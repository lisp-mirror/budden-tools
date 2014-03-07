;;; -*- Encoding: utf-8; -*-
;; -*- Mode: Lisp -*-

(defsystem :buddens-readtable
        :serial t
        :depends-on 
        (:named-readtables :see-packages)
	:components 
        (
         (:file "buddens-readtable-package")
         (:file "buddens-readmacros")
         (:file "buddens-readtable") ; here readtables are defined
;	 (:file "buddens-sharpdot")
))	
; asdf
