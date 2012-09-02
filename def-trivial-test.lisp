;; Written by Denis Budyak. 
;; Code is in public domain

(in-package :trivial-deftest)

;;; Trivial test suite. Define tests in the
;;; source file, run them on loading. Tests 
;;; can be viewed as examples
(defvar *run-tests* t)
(defvar *break-on-test-failure* t)

#|(defmacro ! (name expr1 expr2 &rest keyargs &key (test ''equalp)) "defines a test. Synonym for deftest"
    (let ((function-name (intern (concatenate 'string (string '#:test-fun-) (princ-to-string name)))))
      (unintern function-name)
      (when *run-tests*
        `(eval-when (:load-toplevel :execute) 
           (eval ; macros would expand at compile time otherwise
            `(progn
               (defun ,',function-name ()
                 (unless (funcall ,',test ,',expr1 ,',expr2)
                   (warn "deftest failed: ~S" '(,',function-name ,',expr1 ,',expr2 ,@',keyargs))))
               (,',function-name)))))))|#


(defmacro ! (name expr1 expr2 &rest keyargs &key (test ''equalp)) 
  "Defines a test"
    (let ((function-name (intern (concatenate 'string (string '#:test-fun-) (princ-to-string name)))))
      (unintern function-name)
      (when *run-tests*
        `(eval-when (:load-toplevel :execute) 
           ;(eval ; macros would expand at compile time otherwise
           ; `
           (progn
             (defun ,function-name ()
               (unless (funcall ,test ,expr1 ,expr2)
                 (if *break-on-test-failure* 
                     (cerror "continue" "deftest failed: ~S" '(,function-name ,expr1 ,expr2 ,@keyargs))
                   (warn "deftest failed: ~S" '(,function-name ,expr1 ,expr2 ,@keyargs))
                   )))
             (,function-name))
            ;)
           ))))


