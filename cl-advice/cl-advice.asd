;;; -*- coding: utf-8;  Mode: Lisp  -*-

(defsystem :cl-advice
  :serial t
  :depends-on (:alexandria)
  :description "Unfinished but useful implementation of portable advise facility"
  :components ((:file "package")
               (:file "portably-without-package-locks")
               #+ccl (:file "ccl--advise-global-def--redefinition")
               (:file "cl-advice" 
                      :description "An unfinished portable advice facility")
               #+sbcl
               (:file "sbcl--find-definition-sources-by-name--patch"
                      :description "Enable findind advice definitions along with original function definition")
               (:file "cl-advice-tests" :description "Tests are always executed when building. We don't recommend to change this due to high complexity of debugging missing or incorrect advices")
               )
  :author "Денис Будяк <budden73@gmail.com>"
  :license "MIT")
