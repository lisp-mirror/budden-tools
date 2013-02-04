;;; -*- Encoding: utf-8; -*-
(in-package :budden-tools)

(defmacro let1 (variable + &body progn) 
  "Shortcut for (let ((a b)) . c) or (destructuring-bind a b . c)"
  (if (atom variable)
      `(let ((,variable ,+)) ,@progn)
    `(destructuring-bind ,variable ,+ ,@progn)))

