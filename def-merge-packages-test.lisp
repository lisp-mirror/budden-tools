;;; -*- Encoding: utf-8; -*-

(declaim (optimize debug))

(def-merge-packages::! :def-merge-packages-test
                       (:always t)
                       (:use :cl)
                       (:shadow #:??))

(in-package :def-merge-packages-test)

(def-merge-packages::! :p1 (:always t) (:use :cl) (:export :sym :s1 :nth))
(def-merge-packages::! :p2 (:always t) (:use :cl) (:export :sym :s2 :nth))



(defun ?? (name package list)
  "errs if set of external symbols from package is not the same as list"
  (format t "~%Running ~A~%" name)
  (let ((externals 
         (iterk:iter
           (:for s :in-package package :external-only t)
           (:collect s))))
    (assert (null (set-exclusive-or externals list)))))

;;; Should warn here as p1:sym and p2:sym clash
(def-merge-packages::! :p1+p2 (:always t) (:use :p1 :p2) (:export :s1 :s2 nth))
(?? 't1 :p1+p2 '(p1:s1 p2:s2 cl:nth))

;;; There is a way to suppress a warning: 
(def-merge-packages::! :p1+p2 (:always t) (:use :p1 :p2) (:forbid :sym) (:export :s1 :s2 nth))
(?? 't2 :p1+p2 '(p1:s1 p2:s2 cl:nth))

;;;
(def-merge-packages::! :p1+p2
                       (:always t)
                       (:use :p1 :p2)
                       (:shadowing-import-from :p1 p1:sym)
                       (:export :s1 :s2 :nth :sym))
(?? 't3 :p1+p2 '(p1:s1 p2:s2 cl:nth p1:sym))

;;; Now an example of greater flexibility
;;; We want package to use cl package. Also we want p1:sym there 
;;; Also we want to export p2:sym. So, 

; define intermediate package.
(def-merge-packages::! :intermediate
                       (:always t)
                       (:use :p1 :p2)
                       (:forbid :sym)
                       (:export :s1 :s2))

(?? 't4 :intermediate '(p1:s1 p2:s2))

(defpackage :p1+p2-with-sym-from-p2 ; final package we want
  (:use :intermediate)
  (:shadowing-import-from :p2 :sym) ; custom import
  (:export :sym) ; custom export
  )

(?? 't5 :p1+p2-with-sym-from-p2 '(p2:sym))



