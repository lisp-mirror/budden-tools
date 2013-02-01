;;; -*- Encoding: utf-8; -*-
(in-package :budden-tools)

;; For me (budden), let1 is in cl package. For the rest, it is in 
;; budden-tools package

(defmacro let1 (variable + &body progn) 
  "Shortcut for (let ((a b)) . c) or (destructuring-bind a b . c)"
  (if (atom variable)
      `(let ((,variable ,+)) ,@progn)
    `(destructuring-bind ,variable ,+ ,@progn)))

