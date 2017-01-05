;;; -*- Encoding: utf-8; system :buddens-readtable -*-
;; -*- Mode: Lisp -*-

(named-readtables:in-readtable :buddens-readtable-a)
(in-package :budden-tools)

(def-trivial-test::! test--let-with-conc-type.1
                     '(T "ASDF" T)
  (let-with-conc-type x string "asdf"
                      (list (X.EQUAL "asdf") (X.UPCASE) (X.EQUAL X.UPCASE))
                      ))

(defun STRING-Some-test-function (x y) (string= x y))

(def-trivial-test::! test--let-with-conc-type.smart-case
                     'T
  (let-with-conc-type Some-test-variable string "asdf"
                      (Some-test-variable.Some-test-function "asdf")))

(def-trivial-test::! test--with-conc-name.1
                     'T
  (perga-implementation:perga
   (let Some-test-variable "asdf")
   (:@ with-conc-name Some-test-variable STRING)
   (Some-test-variable.Some-test-function "asdf")))
