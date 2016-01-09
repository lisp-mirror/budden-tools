;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;

(in-package :asdf-load-source-cl-file)

;;;# ASDF component: load-source-cl-file

(defclass load-source-cl-file (asdf:cl-source-file)
  ()
  (:documentation
   "File which loads as a source. To satisfy asdf object model limitations, it creates fake stub for fasl file when it is 'compiled'. When the fake file is 'loaded', source file is loaded insted"))

;; Allow for naked :load-source-cl-file in asdf definitions.
(setf (find-class (intern "LOAD-SOURCE-CL-FILE" :asdf)) (find-class 'load-source-cl-file))
