

(in-package :test-let-around-compile-file-or-load)

(eval-when (:compile-toplevel)
  (setf *my-super-var* 1)
  (setf *my-another-var* *my-super-var*)
  ; (budden-tools::show-exprt *my-super-var*)
  )
