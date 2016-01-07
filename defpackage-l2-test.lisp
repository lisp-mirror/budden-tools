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
    (ignore-errors
     (def-merge-packages:delete-package-metadata name)
     (delete-package name)))

  (defpackage-l2::! :p1 (:always t) (:use :cl) (:export :sym :s1 :nth) (:print-defpackage-form t))
  (defpackage-l2::! :p2 (:always t) (:use :cl) (:export :sym :s2 :nth))
  (defpackage-l2::! :p1+p2 (:always t) (:print-defpackage-form t) (:use :p1 :p2) (:export :s1 :s2 nth))
  )


(defun ?? (name package list)
  "errs if set of external symbols from package is not the same as list"
  (format t "~%Running ~A~%" name)
  (let ((externals 
         (iterk:iter
           (:for s :in-package package :external-only t)
           (:collect s))))
    (assert (null (set-exclusive-or externals list)))))

(make-three-packages)

;;; Should warn here as p1:sym and p2:sym clash
(?? 't1 :p1+p2 '(p1:s1 p2:s2 cl:nth))

;;; There is a way to suppress a warning: 
(defpackage-l2::! :p1+p2 (:always t) (:use :p1 :p2) (:forbid :sym) (:export :s1 :s2 nth))
(?? 't2 :p1+p2 '(p1:s1 p2:s2 cl:nth))

;;;

;;; Now an example of greater flexibility
;;; We want package to use cl package. Also we want p1:sym there 
;;; Also we want to export p2:sym. So, 

; define intermediate package.
(defpackage-l2::! :intermediate
                       (:always t)
                       (:use :p1 :p2)
                       (:forbid :sym)
                       (:export :s1 :s2))

(?? 't4 :intermediate '(p1:s1 p2:s2))

(let ((md (defpackage-l2:ensure-package-metadata :intermediate)))
  (assert (eq (defpackage-l2:package-metadata-l2-package-p md) t))
  (assert (equalp (defpackage-l2:package-metadata-body-of-last-definition md)
                  '((:ALWAYS T) (:USE :P1 :P2) (:FORBID :SYM)
                    (:EXPORT :S1 :S2)))))
                  

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
