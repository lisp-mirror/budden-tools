(in-package :trivial-deftest)

;;; Trivial test suite. Define tests in the
;;; source file, run them on loading. Tests 
;;; can be viewed as examples
(defvar *run-tests* t)
(defmacro deftest (name expr1 expr2 &rest keyargs &key (test 'equalp)) "defines a test"
    (let ((function-name (intern (concatenate 'string (string '#:test-fun-) (princ-to-string name)))))
      (unintern function-name)
      (when *run-tests*
        `(eval-when (:load-toplevel :execute) 
           (eval ; иначе макросы начнут раскрыватьс€ во врем€ компил€ции
            `(progn
               (defun ,',function-name ()
                 (unless (funcall ',',test ,',expr1 ,',expr2)
                   (warn "deftest failed: ~S" '(,',function-name ,',expr1 ,',expr2 ,@',keyargs))))
               (,',function-name)))))))


