;; Copyright (C) Денис Будяк 2016
;; Лицензия MIT
(def-merge-packages::! :test-budden-tools
                       (:always t)
  (:use :cl :budden-tools))

(in-package :test-budden-tools)

(def-trivial-test::! test-collect-duplicates
  (budden-tools:collect-duplicates '(1 1 2 3 3 3) :test 'eql)
  '(1 3))

(def-trivial-test::! collect-into-tail-of-list-1
  '(1 2 3)
  (let (Рез Тр)
    (collect-into-tail-of-list 1 Рез Тр)
    (collect-into-tail-of-list 2 Рез Тр)
    (collect-into-tail-of-list 3 Рез Тр)
    Рез))

(def-trivial-test::! collect-into-tail-of-list-как-в-очередь
  '(1 2 3)
  (let (Рез Тр)
    (collect-into-tail-of-list 1 Рез Тр)
    (pop Рез)
    (pop Рез) ; это уже ничего не изменит
    (collect-into-tail-of-list 1 Рез Тр)
    (collect-into-tail-of-list 2 Рез Тр)
    (pop Рез)
    (pop Рез)
    (collect-into-tail-of-list 0 Рез Тр)
    (collect-into-tail-of-list 1 Рез Тр)
    (collect-into-tail-of-list 2 Рез Тр)
    (pop Рез)
    (collect-into-tail-of-list 3 Рез Тр)
    Рез))
  

