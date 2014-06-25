;;; -*- Encoding: utf-8; Mode: Lisp -*-

(in-package #:asdf)

(pushnew :building-see-packages *features*)

(defsystem :budden-tools  
  :serial t
  :description "Some utilities by Budden, public domain"
  :depends-on 
  (:alexandria :cl-fad :split-sequence :cl-utilities :named-readtables
   :cl-ppcre		
   :iterate-keywords :swank :decorate-function)
  :components ( 
    ;(:file "hierarchical-packages"
    ; :doc "port of Allergo's hierarchical packages to some more implementations by Tim Bradshaw with some changes by Budden"
    ; )
   (:file "def-merge-packages"
    :description "defpackage with some new clauses which are useful in conjunction with this library. See def-merge-packages::!"
    )
   (:file "package") ; might need
   (:file "package-proga-implementation")
   (:file "def-toplevel-progn")
   (:file "let1")
   (:file "def-trivial-test"
    :description "yet another test suite")
   (:file "budden-tools"
    :description "General purpose utilities") 
   (:file "with-conc-name" :description "DEPRECATED")
   #+lispworks6.1 (:file "lw-macro-friendly-stepper" :description "put breakpoints in macros without macroexpansion (for stepper)")
   #+lispworks6.1 (:file "lw-macro-friendly-dbg-vars-and-macros" :description "find frame source correction for debugger")
   #+lispworks6.1 (:file "lw-macro-friendly-dbg" :description "find frame source correction for debugger")
   #+lispworks6.1 (:file "res-fun-with-keyargs-in-a-debugger" :description ":restart-with-keywords-risky debugger command - restart frames with &key args")
   (:file "proga" :description "DEPRECATED")
   (:file "proga-test" :description "DEPRECATED") 

   ; inlining perga system to simplify build
   (:file "perga-implementation-package")
   (:file "perga-vars-and-macros")
   (:file "perga-aux-funs")
   (:file "perga")

   (:file "iterate-extensions" :description "some iterate drivers")
   (:file "defun-to-file" :description "When named function is created, it can be saved to a file in order to find-source to work")
   #+lispworks
   (:file "variable-type" :description "Compilation environment related stuff"
   )
   (:file "pass-by-ref" :description "Pass place to a function by reference"
   )
   (:file "print-hash-table" :description "Print hashtable readably and other hash table utils"
    )
   )) 
