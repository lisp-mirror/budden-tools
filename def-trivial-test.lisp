;;; -*- Encoding: utf-8; system :budden-tools; -*-
;; Written by Denis Budyak. 
;; Code is in public domain

(in-package :def-trivial-test)

;;; Trivial test suite. Define tests in the
;;; source file, run them on loading. Tests 
;;; can be viewed as examples
(defvar *run-tests* t)
(defvar *break-on-test-failure* t)

#-lispworks
(defmacro ! (name expr1 expr2 &rest keyargs &key (test ''equalp))
  "Defines a test. Names are not uninterned, beware!"
  (when *run-tests*
    (let* ((current-fn-symbol
             (cond
               ((stringp name) (make-symbol name))
               ((symbolp name) name)))
           (x (gensym "X"))
           (y (gensym "Y"))
           (message (gensym "MESSAGE"))
           (the-form 
            `(progn
               (defun ,current-fn-symbol ()
                 "function for def-trivial-test::!"
                 (let ((,x ,expr1)
                       (,y ,expr2))
                   (unless (funcall ,test ,x ,y)
                     (let ((,message
                            (format
                             nil "deftest failed: ~S~%x=~S~%y=~S"
                             '(,current-fn-symbol ,expr1 ,expr2 ,@keyargs)
                             ,x ,y)))
                       (if *break-on-test-failure* 
                           (cerror "continue" "~A" ,message)
                           (warn "~A" ,message))
                       ))))
               (,current-fn-symbol)
               )))
      the-form)))

#+lispworks
(defmacro ! (name expr1 expr2 &rest keyargs &key (test ''equalp))
  "Defines a test"
  (when *run-tests*
    (let* ((name (string name))
           (current-fn-symbol (make-symbol name))
           (the-form 
            `(dspec:def (! ,name)
               (dspec:record-definition `(! ,',name) (dspec:location))
               (defun ,current-fn-symbol ()
                 "function for def-trivial-test::!"
                 (unless (funcall ,test ,expr1 ,expr2)
                 (if *break-on-test-failure* 
                     (cerror "continue" "deftest failed: ~S" '(,current-fn-symbol ,expr1 ,expr2 ,@keyargs))
                   (warn "deftest failed: ~S" '(,current-fn-symbol ,expr1 ,expr2 ,@keyargs))
                   )))
               (,current-fn-symbol)
               )))
      the-form)))

#+lispworks
(defun canonicalize-!-dspec (dspec)
  (and
   (consp dspec)
   (eq (first dspec) '!)
   (typep (second dspec) '(or symbol string))
   `(! ,(string (second dspec)))
   ))

#+lispworks
(dspec:define-dspec-class ! nil 
  "def-trivial-test::! locatable"
  :canonicalize #'canonicalize-!-dspec
  )


