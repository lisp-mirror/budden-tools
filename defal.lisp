(defpackage :defal (:use :cl)
  (:export "!")
  )

(in-package :defal)

(defmacro ! (&rest attributes)
  (declare (ignore attributes)))