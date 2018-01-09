;; -*- coding: utf-8; -*-
;; This file must be loaded by load-cl-advice.lisp

(in-package :ccl)

(cl-advice:portably-without-package-locks
 (defun advise-global-def (def when stuff &optional method-p dynamic-extent-arglist)
   (flet ((get-gensym-counter ()
            ;; FIXME What about race conditions in getting gensym counter? 
            (prog1 *gensym-counter* (incf *gensym-counter*))))
     (let* ((saved-method-var
             (cl-advice::make-symbol-for-expression
              `(saved-method-var ,def ,(get-gensym-counter))))
            (doit-with-args-args
             (cl-advice::make-symbol-for-expression
              `(doit-with-args-args ,def ,(get-gensym-counter)))))
    `(lambda (,@(if (and method-p (neq when :after))
                  `(&method ,saved-method-var))
              &rest arglist)
       ,@(and dynamic-extent-arglist '((declare (dynamic-extent arglist))))
       (declare (ftype function ,def))
       (declare (ignorable arglist))
       (let ()
         ,(ecase
            when
            (:before
             `(block nil
                ,stuff                  
                (return ,(if method-p
                           `(apply-with-method-context ,saved-method-var (symbol-function ',def) arglist)
                           `(apply ',def arglist)))))
            (:after         
             `(block nil
                (let ((values (multiple-value-list (apply (function ,def) arglist))))
                  ;(declare (dynamic-extent values))
                  ,stuff
                  (return (values-list values)))))
            (:around
             ;; stuff is e.g. (+ 5 (:do-it))
             (if method-p 
                 `(macrolet ((:do-it ()
                               `(apply-with-method-context ,',saved-method-var 
                                                           (symbol-function ',',def)
                                                           arglist)))
                    (flet ((call-next-advice (&rest ,doit-with-args-args)
                             (apply-with-method-context ,saved-method-var
                                                        (symbol-function ',def)
                                                        ,doit-with-args-args)))
                      (block nil
                        (return  ,stuff))))
                 `(macrolet ((:do-it ()
                               `(apply (function ,',def) arglist)))
                    (flet ((call-next-advice (&rest ,doit-with-args-args)
                             (apply (function ,def) ,doit-with-args-args)))
                      (block nil
                        (return  ,stuff)))))))))))))

