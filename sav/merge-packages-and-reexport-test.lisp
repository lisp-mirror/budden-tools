;;; -*- Encoding: utf-8; -*-
(defpackage :p1 (:use :cl) (:export :sym :s1 :nth))
(defpackage :p2 (:use :cl) (:export :sym :s2 :nth))

(defun p1::?? (package list)
  "errs if set of external symbols from package is not the same as list"
  (let ((externals 
         (iter:iter (:for s :in-package package :external-only t)
                    (:collect s))))
    (assert (null (set-exclusive-or externals list)))))

;;; Should warn here as p1:sym and p2:sym clash
(merge-packages-and-reexport:! :p1+p2 (:p1 :p2))
(p1::?? :p1+p2 '(p1:s1 p2:s2 cl:nth))

;;; There is a way to suppress a warning: 
(merge-packages-and-reexport:! :p1+p2 (:p1 :p2) :dont-warn-on (:sym))
(p1::?? :p1+p2 '(p1:s1 p2:s2 cl:nth))

;;;
(merge-packages-and-reexport:! :p1+p2 (:p1 :p2) :use-first-duplicate t)
(p1::?? :p1+p2 '(p1:s1 p2:s2 cl:nth p1:sym))

;;; Now an example of greater flexibility
;;; We want package to use cl package. Also we want p1:sym there 
;;; Also we want to export p2:sym. So, 

; define intermediate package.
(merge-packages-and-reexport:! :intermediate (:p1 :p2) :dont-warn-on (:sym))

(p1::?? :intermediate '(p1:s1 p2:s2))

(defpackage :p1+p2-with-sym-from-p1 ; final package we want
  (:use :intermediate)
  (:shadowing-import-from :p2 :sym) ; custom import
  (:export :sym) ; custom export
  )

(p1::?? :p1+p2-with-sym-from-p1 '(p2:sym))

; now we may reepxport all symbols which were imported 
; from intermediate package
(merge-packages-and-reexport:reexport :intermediate :p1+p2-with-sym-from-p1)

(p1::?? :p1+p2-with-sym-from-p1 '(p1:s1 p2:s2 cl:nth p2:sym))


