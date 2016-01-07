;;; -*- Encoding: utf-8; -*-
;; Этот файл - не часть системы, его надо запускать руками, желательно, при наличии
;; :buddens-readtable-a

(declaim (optimize debug))

(def-merge-packages::! :defpackage-l2-simple-test
                       (:always t)
                       (:use :cl)
                       (:shadow #:??))

(in-package :defpackage-l2-simple-test)

(defun make-three-packages ()
  (dolist (name '(:p1+p2 :p1 :p2))
    (when (find-package name)
      (delete-package name)))

  (eval '(defpackage :p1 (:use :cl) (:export :sym :s1 :nth)))
  (eval '(defpackage :p2 (:use :cl) (:export :sym :s2 :nth)))
  (eval '(defpackage-l2::! :p1+p2 (:always t) (:use :p1 :p2) (:export :p1+p2s-own)))
  )


(defun ?? (name package list-in-string)
  "errs if set of symbols from package is not the same as list. list must be represented as a string to read"
  (format t "~%Running ~A~%" name)
  (let* ((symbols
          (iterk:iter
           (:for s :in-package package)
           (:collect s)))
         (list (read-from-string list-in-string)))
    (assert (null (set-exclusive-or symbols list)))))

(make-three-packages)

(?? 't1 :p1+p2 "(p1:s1 p2:s2 cl:nth p1+p2::sym p1+p2::p1+p2s-own)")

(let ((md (defpackage-l2:ensure-package-metadata :p1+p2)))
  (assert (eq (defpackage-l2:package-metadata-l2-package-p md) t))
  (assert (equalp (defpackage-l2:package-metadata-body-of-last-definition md)
                  '((:ALWAYS T) (:USE :P1 :P2)
                    (:export :p1+p2s-own)))))
                  

(defun got-buddens-readtable-a ()
  (and (find-package :budden-tools)
       (find-package :named-readtables)
       (eval
        (read-from-string
         "(and
            (named-readtables:find-readtable :buddens-readtable-a)
            (budden-tools::packages-seen-p :buddens-readtable-a))"))))

(when (got-buddens-readtable-a)
  (def-trivial-test::! local-nicknames.1
    (progn
      (defpackage-l2::! :pckg-with-loc-nick
                        (:always t)
        (:local-nicknames :but :budden-tools)
        )
      (let ((*readtable* (budden-tools:find-readtable :buddens-readtable-a))
            (*package* (find-package :pckg-with-loc-nick)))
        (read-from-string "but:byref")))
    'budden-tools:byref))


(when (got-buddens-readtable-a)
  (def-trivial-test::! forbidden-symbols-work.1
    (perga-implementation:perga
      (make-three-packages)
      (:@ multiple-value-bind (result error)
          (ignore-errors
           (let ((*readtable* (budden-tools:find-readtable :buddens-readtable-a))
                 (*package* (find-package :p1+p2)))
             (read-from-string "SYM"))))
      (budden-tools::ignored result)
      (typep error 'error))
    t))

(when (got-buddens-readtable-a)
  (def-trivial-test::! unexport-can-cause-unforbid.1
    (perga-implementation:perga
      (make-three-packages)
      (unexport
       (budden-tools::the* (not null) (find-symbol "SYM" :p1)) :p1)
      (let ((*readtable* (budden-tools:find-readtable :buddens-readtable-a))
            (*package* (find-package :p1+p2)))
        (read-from-string "SYM"))
      t)
    t))


(def-trivial-test::! can-t-export-inherited.1
  (let* ((second-value
          (nth-value 1
                     (ignore-errors
                      (eval '(defpackage-l2::! :trash-package (:use :cl) (:export :cons)))))))
    (typep second-value 'error))
  t)


(def-trivial-test::! can-t-export-inherited.2
                     (let (second-value)
                       (eval '(defpackage-l2::! :trash-package (:use :cl)))
                       (eval '(defpackage-l2::! :trash-package (:export :cons)))
                       (setf second-value 
                             (nth-value 1
                                        (ignore-errors
                                         (eval
                                          '(defpackage-l2::! :trash-package
                                                             (:use :cl)
                                             (:export :cons))))))
                       (typep second-value 'error))
  t)

(defmacro yields-error-p (form-to-eval)
  (let ((second-value (gensym)))
    `(let ((,second-value
            (nth-value 1
                       (ignore-errors (eval ,form-to-eval)))))
       (typep ,second-value 'error))))

(def-trivial-test::! can-t-export-forbidden.1
                     (progn
                       (make-three-packages)
                       (yields-error-p
                        '(defpackage-l2::! :p1+p2 (:always t) (:use :p1 :p2) (:export :sym))))
  t)

(def-trivial-test::! can-export-after-it-become-not-inherited.1
                     (progn
                       (eval '(defpackage-l2::! :trash-package (:use :cl)))
                       (eval '(defpackage-l2::! :trash-package (:export :cons)))
                       t
                       )
  t)


(def-trivial-test::! can-export-after-it-become-not-inherited.2
                     (progn
                       (eval '(defpackage-l2::! :trash-package (:export :cons)))
                       (eval '(defpackage-l2::! :trash-package (:use :cl)))
                       t
                       )
  t)

