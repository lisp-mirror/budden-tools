;;; -*- Encoding: utf-8;  Mode: Lisp  -*-
(in-package #:asdf)

(defsystem :perga
  :serial t
  :depends-on (:budden-tools)
  :description "perga system currenty contains no components. When :budden-tools is loaded, perga is working"
  )


(defsystem :perga-test
  :serial t
  :depends-on (:budden-tools)
  :components
  ((:file "perga-test-1")
   (:file "perga-test-2" :description "outdated. Does not work")))
