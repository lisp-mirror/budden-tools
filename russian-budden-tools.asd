;;; -*- Encoding: utf-8;  Mode: Lisp -*-

(in-package #:asdf)

; needs asdf2-tools.lisp 

(defsystem russian-budden-tools
  :serial t
  ;:documentation "Some utilities by Budden, public domain"
  #+new-projects-structure :defsystem-depends-on #+new-projects-structure (:asdf-main-extensions)
  :depends-on 
  (:budden-tools)
  :components ( 
               (asdf::package-file "pkg-russian-budden-tools")
               (:file "russian-budden-tools")
               (:file "ru-bu-to-file-convertors")
               ) 
  )
