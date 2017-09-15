;;; -*- Encoding: utf-8;  Mode: Lisp  -*-

(defsystem :decorate-function
  :serial t
  :description "Convenient API for hooking on functions, unhooking and calling original version from hook. See also lw:defadvice"
  :components (
               (:file "decorate-function" 
                      :description "An unfinished portable defadvice")
               (:file "decorate-function-tests")))
