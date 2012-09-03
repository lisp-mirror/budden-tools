;; -*- Mode: Lisp -*-

(in-package #:asdf)

#.(defparameter russian-file
  #-(and sbcl russian) :file 
  #+(and sbcl russian) :windows-1251-file)

(pushnew :building-see-packages *features*)

(defsystem budden-tools
  :serial t
  :depends-on 
  (:alexandria :split-sequence :cl-utilities :named-readtables
   :buddens-readtable :cl-ppcre		
   :iterate-keywords :swank)
  :components ( 
    (:file "hierarchical-packages"
     :doc "port of Allergo's hierarchical packages to some more implementations by Tim Bradshaw with some changes by Budden"
     )
   (:file "def-merge-packages"
    :doc "defpackage with some new clauses which are useful in conjunction with this library. See def-merge-packages::!4"
    )
   #+budden (:file "let1")
   
   (:package-file "package") ; might need 
   (:package-file "package-proga-implementation")
   #-budden (:file "let1")
   (#.russian-file "def-trivial-test")
   (#.russian-file "budden-tools")
   (:file "with-conc-name")
   (:file "defstruct-meta")
   (#.russian-file "proga")
   (:file "proga-test")
   (:file "iterate-extensions")
   (#.russian-file "defun-to-file")
   (:file "decorate-function" :doc "Smart API for substituting previously defined functions by their new versions")
   (#.russian-file "variable-type" :doc "Some environment related stuff")
   (:file "pass-by-ref" :doc "Pass place to a function by reference")
   )
  ) 
