;;; -*- Encoding: utf-8; -*-
(in-package :perga-implementation)


(defun perga-syntax-error (format args)
  (apply 'error format args))

#+lispworks (cl-user::defadvice (lispworks-tools::stepize fix-parents-for-perga-advice :after :documentation "Makes perga clauses breakpointable")
    (context form)
  (declare (ignore form))
  (lw-macro-friendly-stepper::fix-parents-for-macro-in-context context 'perga))

