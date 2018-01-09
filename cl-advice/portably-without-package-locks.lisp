;;; -*- coding: utf-8; System :cl-advice;  -*-

(in-package :cl-advice)

;; FIXME is it a proper place for this?
;; FIXME add &key package
(defun make-symbol-for-expression (expression &key add-asterisks-around)
  (let ((symbol-name
         (with-standard-io-syntax
           (with-output-to-string (ou)
             (when add-asterisks-around
               (format ou "*"))
             (format ou "<~S>" expression)
             (when add-asterisks-around
               (format ou "*"))))))
    (make-symbol symbol-name)))


(defmacro portably-without-package-locks (&body body)
  "An attempt to override package locks in a cross-implementation manner. Misplaced and maybe erroneous"
`(#+sbcl sb-ext:without-package-locks
#+allegro excl::without-package-locks
#+cmu ext:without-package-locks
#+lispworks let 
#+lispworks 
((lw:*handle-warn-on-redefinition* :warn)
 ; (dspec:*redefinition-action* :warn)
 (hcl:*packages-for-warn-on-redefinition* nil))
#+clisp ext:without-package-lock #+clisp ()
#+ccl let
#+ccl ((ccl:*warn-if-redefine-kernel* nil)) 
#-(or allegro lispworks sbcl clisp cmu ccl) 
progn
,@body))
