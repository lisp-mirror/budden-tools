;; Copyright (C) Денис Будяк 2016
;; Лицензия MIT
(def-merge-packages::! :test-budden-tools
                       (:always t)
  (:use :cl :budden-tools))

(in-package :test-budden-tools)

(def-trivial-test::! test-collect-duplicates
  (budden-tools:collect-duplicates '(1 1 2 3 3 3) :test 'eql)
  '(1 3))