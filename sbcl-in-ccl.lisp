;;; -*- Encoding: utf-8; system :see-packages ; -*-

;;; Функции из sbcl, нужные для ccl

(named-readtables:in-readtable nil)

(def-merge-packages::! :sbcl-in-ccl
                       (:always t)
                       (:use :ccl :cl :cl-advice :defpackage-budden)
                       (:export "
  SBCL-IN-CCL::TRULY-THE
  DEFPACKAGE-BUDDEN:find-package-or-lose-a-la-sbcl
  DEFPACKAGE-BUDDEN:find-undeleted-package-or-lose-a-la-sbcl
"))

(in-package :sbcl-in-ccl)

(defmacro truly-the (type-specifier form) `(the ,type-specifier ,form))
