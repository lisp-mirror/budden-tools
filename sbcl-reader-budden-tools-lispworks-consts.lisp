;;;; -*- Encoding: utf-8; -*-
;;;; READ and friends

;;;; This software is ported from SBCL to Lispworks by Budden. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; ;b is commented out by budden
;;; ;brt - buddens-readtable

(def-merge-packages::!
 :sbcl-reader-budden-tools-lispworks
 (:always t)
 (:use :cl 
  :budden-tools 
  )
 (:export "
   sbcl-reader-budden-tools-lispworks:*return-package-and-symbol-name-from-read*
   sbcl-reader-budden-tools-lispworks:read-token
   sbcl-reader-budden-tools-lispworks:potential-symbol
   sbcl-reader-budden-tools-lispworks:make-potential-symbol
   sbcl-reader-budden-tools-lispworks:potential-symbol-casified-name
   sbcl-reader-budden-tools-lispworks:potential-symbol-package
   sbcl-reader-budden-tools-lispworks:potential-symbol-qualified
   sbcl-reader-budden-tools-lispworks:potential-symbol-p
 "
 ))

(in-package :sbcl-reader-budden-tools-lispworks)

(defparameter +char-attr-whitespace+ 0)
(defparameter +char-attr-terminating-macro+ 1)
(defparameter +char-attr-single-escape+ 2)
(defparameter +char-attr-multiple-escape+ 3)
(defparameter +char-attr-constituent+ 4)
(defparameter +char-attr-constituent-dot+ 10 #|5|#)
(defparameter +char-attr-constituent-expt+ 5 #|6|#)
(defparameter +char-attr-constituent-slash+ 6 #|7|#)
(defparameter +char-attr-constituent-digit+ 7 #|8|#)
(defparameter +char-attr-constituent-sign+ 8 #|9|#)
;;; the following two are not static but depend on *READ-BASE*.
;;; DECIMAL-DIGIT is for characters being digits in base 10 but not in
;;; base *READ-BASE* (which is therefore perforce smaller than 10);
;;; DIGIT-OR-EXPT is for characters being both exponent markers and
;;; digits in base *READ-BASE* (which is therefore perforce larger
;;; than 10).  -- CSR, 2004-03-16
(defparameter +char-attr-constituent-decimal-digit+ 15 #|10|#) ; потому что 10 занят
(defparameter +char-attr-constituent-digit-or-expt+ 12 #|11|#) ; потому что 11 стало 12

(defparameter +char-attr-package-delimiter+ 11 #|12|#)
(defparameter +char-attr-invalid+ 13)
(defparameter +char-attr-delimiter+ 14) ; (a fake for READ-UNQUALIFIED-TOKEN)


