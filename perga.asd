(in-package #:asdf)

(defsystem :perga
  :serial t
  :depends-on (:budden-tools :alexandria)
  :components
  (
   (:file "perga-implementation-package")
   (:file "perga-vars-and-macros")
   (:file "perga-aux-funs")
   (:file "perga")
   ))


(defsystem :perga-test
  :serial t
  :depends-on (:perga)
  :components
  ((:file "perga-test-1")
   (:file "perga-test-2")))
