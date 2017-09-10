;; -*- coding: utf-8; -*-
;; This file is not a part of system. Please load it before you start loading systems.
;; It works only in combination with asdf-3

(in-package :sb-introspect)

(defun sb-introspect--find-definition-sources-by-name-sb--decorated (original-fn name type)
  "Decorator for sb-introspect::find-definition-sources to support :asdf-system"
  (case type
    (:asdf-system
     (typecase name
       ((or symbol string)
        (let* ((system (asdf:find-system name))
               ;; we reuse the slot which seem to be unused
               (location (and system (asdf:system-source-control system)))
               (result (and location (list (translate-source-location location)))))
          result))))
    (t
     (funcall original-fn name type))))

(decorate-function:decorate-function 'sb-introspect::find-definition-sources-by-name #'sb-introspect--find-definition-sources-by-name-sb--decorated)

(unless (getf swank/sbcl::*definition-types* :asdf-system)
  (setf swank/sbcl::*definition-types*
        (list* :asdf-system 'asdf:defsystem swank/sbcl::*definition-types*)))





