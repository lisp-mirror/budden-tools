;;; -*- coding: utf-8;  Mode: Lisp  -*-

#|

Unfinished but useful implementation of portable advise facility.

As an extra, it contains an extension to SBCL's find definition facility to enable finding the exact definition of asdf system.

To make everything work, do the following:

i) load this file as a source (no compilation) just after loading and upgrading asdf. 
ii) Immediately after that, compile and load ../asdf-3.x.x-tools.lisp 

Do that as early as possible. Preferrably, do that before finding your asdf systems.

|#


(eval-when (:compile-toplevel)
  (error "Don't compile this file - load it as a source"))

(defun load-cl-advice ()
  (dolist (source-file
           '("package.lisp"
             "portably-without-package-locks.lisp"
             #+ccl "ccl--advise-global-def--redefinition.lisp"
             "cl-advice.lisp"
             #+sbcl "sbcl--find-definition-sources-by-name--patch.lisp"
             "cl-advice-tests.lisp"))
    (let ((actual-source (merge-pathnames source-file *load-truename*)))
      (multiple-value-bind (compilation-result warnings-p failure-p)
                           (compile-file actual-source)
        (cond
         (failure-p (cerror "Try to continue" "Compilation of ~S failed" actual-source))
         (warnings-p (cerror "Try to continue" "Compilation of ~S warned" actual-source))
         )
        (let ((load-success (load compilation-result)))
          (unless load-success (cerror "Try to continue" "Loading of ~S unsuccessful" compilation-result)))))))

(load-cl-advice)

;; (C) "Денис Будяк <budden73@gmail.com>"
;; MIT License
