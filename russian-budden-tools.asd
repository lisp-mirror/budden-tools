;;; -*- Encoding: utf-8;  Mode: Lisp -*-

(in-package #:asdf)

#.(defparameter russian-file
  #-(and sbcl russian) :file 
  #+(and sbcl russian) :windows-1251-file)

; needs asdf2-tools

(defsystem russian-budden-tools
  :serial t
  ;:documentation "Some utilities by Budden, public domain"
  :depends-on 
  (:budden-tools)
  :components ( 
               (:package-file "pkg-russian-budden-tools")
               (#.russian-file "russian-budden-tools")
               ) 
  )
