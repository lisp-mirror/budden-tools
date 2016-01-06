;;; -*- Encoding: utf-8; -*-
;; Этот файл - не часть системы, его надо запускать руками

(declaim (optimize debug))

(def-merge-packages::! :defpackage-l2-simple-test
                       (:always t)
                       (:use :cl)
                       (:shadow #:??))

(in-package :defpackage-l2-simple-test)

(defpackage-l2::! :p1 (:always t) (:use :cl) (:export :sym :s1 :nth))
(defpackage-l2::! :p2 (:always t) (:use :cl) (:export :sym :s2 :nth))



(defun ?? (name package list)
  "errs if set of external symbols from package is not the same as list"
  (format t "~%Running ~A~%" name)
  (let ((externals 
         (iterk:iter
           (:for s :in-package package :external-only t)
           (:collect s))))
    (assert (null (set-exclusive-or externals list)))))

;;; Should warn here as p1:sym and p2:sym clash
(defpackage-l2::! :p1+p2 (:always t) (:use :p1 :p2) (:export :s1 :s2 nth))
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


