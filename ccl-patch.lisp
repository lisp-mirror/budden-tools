;;; -*- coding: utf-8; system :budden-tools ; -*-

(in-package :ccl) 


(defun decorated--ccl--apropos-aux (original-fn theString symtuple indent)
  (declare (ignore theString original-fn))
  (let ((sym (aref symtuple 0))
        val)
    (format t "~a" (apropos-string-indented symtuple indent))
    (when (setq val (fboundp sym))
      (cond ((functionp val)
             (princ ", Def: ")
             (prin1 (type-of val)))
            ((setq val (macro-function sym))
             (princ ", Def: MACRO ")
             (prin1 (type-of val)))
            (t (princ ", Special form"))))
    (when (boundp sym)
      ;; THAT's the difference
      (princ ",  Value: ")
      (typecase (symbol-value sym)
        ((or symbol number package)
         (prin1 (symbol-value sym)))
        (t
         (princ "..."))))
    (when (find-class sym nil)
      (princ ", Class: ")
      (prin1 (find-class sym)))
    (terpri)))

(cl-advice:define-advice apropos-aux 'decorated--ccl--apropos-aux :advice-name dont-print-value-of-variables)
