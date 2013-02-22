;;; -*- Encoding: utf-8; Mode: Lisp -*-

(in-package #:asdf)

(pushnew :building-see-packages *features*)

(defsystem :budden-tools
  :serial t
  ;:documentation "Some utilities by Budden, public domain"
  :depends-on 
  (:alexandria :split-sequence :cl-utilities :named-readtables
   :buddens-readtable :cl-ppcre		
   :iterate-keywords :swank :decorate-function)
  :components ( 
    ;(:file "hierarchical-packages"
    ; :doc "port of Allergo's hierarchical packages to some more implementations by Tim Bradshaw with some changes by Budden"
    ; )
   (:file "def-merge-packages"
    ;:documentation "defpackage with some new clauses which are useful in conjunction with this library. See def-merge-packages::!"
    )
   (:package-file "package") ; might need 
   (:package-file "package-proga-implementation")
   (:file "let1")
   (:file "def-trivial-test") ; yet another test suite
   (:file "budden-tools")   ; some useful functions
   (:file "with-conc-name") ; obsolete
   (:file "proga")          ; portable, semi-obsolete
   (:file "proga-test") 
   (:file "iterate-extensions") ; some iterate drivers
   (:file "defun-to-file")      ; not very useful, deprecating is considered
   (:file "variable-type" ;:documentation "Compilation environment related stuff (implementation-dependent)"
   )
   (:file "pass-by-ref" ;:documentation "Pass place to a function by reference (portable)"
   )
   (:file "print-hash-table" ;:documentation "Print hashtable readably "
)
   #+lispworks6.1
   (:file "lw-macro-friendly-stepper")
   )) 
