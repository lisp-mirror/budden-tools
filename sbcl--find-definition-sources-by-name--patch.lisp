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
        (let* ((system (asdf:find-system name nil))
               ;; we reuse the slot which seem to be unused
               (location (and system (asdf:system-source-control system)))
               (result (and location (list (translate-source-location location)))))
          result))))
    (:original-of-decorated-function
     (typecase name
       (symbol
        (let* ((location (decorate-function:get-original-function-source-location name)))
          (and location (list location #|(translate-source-location location)|#))))))
    (:decorator-of-function
     (typecase name
       (symbol
        (let* ((decorator (decorate-function:get-function-decorator name))
               (decorator-sources
                (typecase decorator
                  (symbol (find-definition-sources-by-name decorator :function))
                  (function
                   (let* ((src (find-definition-source decorator)))
                     (and src (list src)))))))
          decorator-sources
          ))))
    (t
     (funcall original-fn name type))))

(decorate-function:decorate-function 'sb-introspect::find-definition-sources-by-name #'sb-introspect--find-definition-sources-by-name-sb--decorated)

(defun ensure-definition-type (type head)
  (unless (getf swank/sbcl::*definition-types* type)
    (setf swank/sbcl::*definition-types*
          (list* type head swank/sbcl::*definition-types*))))

(ensure-definition-type :asdf-system 'asdf:defsystem)
(ensure-definition-type :original-of-decorated-function 'decorate-function:get-undecorated)
(ensure-definition-type :decorator-of-function 'decorate-function:get-function-decorator)

;; вероятно, нужно ещё продекорировать find-definition-source




