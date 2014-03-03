;;; -*- Encoding: utf-8;  Mode: Lisp  -*-

(defsystem :decorate-function
  :serial t
  :description "Convenient API for hooking on functions, unhooking and calling original version from hook. See also lw:defadvice"
  :components ( 
   (:file "decorate-function" 
    ;:doc "Uniform API for substituting previously defined functions by their new versions"
    ))) 
