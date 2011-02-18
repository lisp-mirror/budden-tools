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
   (:file "hierarchical-packages")
   (:file "merge-packages-simple")
   #+budden (:file "let1")
   
   (:package-file "package") ; might need 
 #-budden (:file "let1")
   (#.russian-file "trivial-deftest")
   (#.russian-file "budden-tools")
   (:file "with-conc-name")
   (:file "defstruct-meta")
   (#.russian-file "proga")
   (:file "iterate-extensions")
   (#.russian-file "defun-to-file")
   (:file "decorate-function")
   (:file "variable-type")
   )
  ) 
