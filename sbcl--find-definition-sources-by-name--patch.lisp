;; -*- coding: utf-8; -*-
;; This file is not a part of system. Please load it before you start loading systems.
;; It works only in combination with asdf-3

(in-package :sb-introspect)

(defun get-next-encap-info (encap-info)
  (and encap-info
       (sb-impl::encapsulation-info
        (sb-impl::encapsulation-info-definition encap-info))))

(defun encapsulation-definition-sources (name)
  "Copy of sb-impl::encapsulated-p"
  (let ((fdefn (sb-impl::find-fdefn name))
        result)
    (flet ((get-sources-from-this-fn (fn is-it-decorator decorator-type)
             ;; Patching of SWANK backend is required to make use of decorator-type
             (declare (ignore decorator-type))
             (let ((decorator-or-function-itself
                    (cond
                     (is-it-decorator
                      (and (sb-impl::closurep fn) 
                           (second
                            (sb-impl::%closure-values fn))))
                     (t fn))))
               ;(budden-tools:show-exprt
               ; `(,decorator-or-function-itself ,is-it-decorator ,decorator-type))
               (typecase decorator-or-function-itself
                 (null)
                 (symbol
                  (setf result
                        (append result
                                (find-definition-sources-by-name
                                 decorator-or-function-itself
                                 :function))))
                 (function
                  ;(budden-tools:show-exprt decorator-or-function-itself)
                  (setf result
                        (append
                         result
                         (list (find-definition-source decorator-or-function-itself)))))
                 (t
                  (warn "SB-INTROSPECT::ENCAPSULATION-DEFINITION-SOURCES: What is ~S?"
                        decorator-or-function-itself))))))
      (when (and fdefn (typep (sb-impl::fdefn-fun fdefn) 'generic-function))
        (warn "Can't get encapsulation definition sources for generic functions (yet)")
        (return-from encapsulation-definition-sources nil))
      (unless fdefn
        (return-from encapsulation-definition-sources nil))
      (do* ((encap-info (sb-impl::encapsulation-info (sb-impl::fdefn-fun fdefn))
                        (get-next-encap-info encap-info))
            (first-time-p t nil)
            (this-encap-info-type
             (and encap-info
                  (sb-impl::encapsulation-info-type encap-info)))
            (next-encap-info (get-next-encap-info encap-info)))
           ((null encap-info) nil)
        (typecase encap-info
          (sb-impl::encapsulation-info
           (when first-time-p
             (get-sources-from-this-fn (symbol-function name) t this-encap-info-type))
           (let* ((fn ; decorator lambda generated by encapsulate or original function itself
                   (sb-impl::encapsulation-info-definition encap-info)
                   ;(sb-impl::fdefn-fun fdefn)
                   )
                  (is-it-decorator (not (null next-encap-info)))
                  (encapsulation-type
                   (and is-it-decorator
                        (sb-impl::encapsulation-info-type next-encap-info))))
             (get-sources-from-this-fn fn is-it-decorator encapsulation-type)))))
      result)))
         
(defun ensure-definition-type (type head)
  (unless (getf swank/sbcl::*definition-types* type)
    (setf swank/sbcl::*definition-types*
          (list* type head swank/sbcl::*definition-types*))))

;;; One can add more decorations in other places by decorating the function further. 
#| There are 3 things to do:
i) ensure-definition-type
ii) when defining an object, associate its definition location to the object somehow (make sure it survives fasl dumping and loading
iii) decorate sb-introspect::find-definition-sources-by-name (and maybe sb-introspect::find-definition-source ?)
|#

(ensure-definition-type :decoration-definition 'decorate-function:decorate-function)
(ensure-definition-type :asdf-system 'asdf:defsystem)
(ensure-definition-type :decorator-of-function 'sb-int:encapsulate)

(defun sb-introspect--find-definition-sources-by-name-sb--decorated (original-fn name type)
  "Decorator for sb-introspect::find-definition-sources-by-name to support :asdf-system"
  (case type
    (:asdf-system
     (typecase name
       ((or symbol string)
        (let* ((system (ignore-errors (asdf:find-system name nil)))
               ;; we reuse the slot which seem to be unused. Location is recorded to this slot
               ;; in the ASDF/INTERFACE::DECORATE-DEFSYSTEM, see
               ;; c:/yar/lp/budden-tools/asdf-3.1.4-tools.lisp
               (location (and system (asdf:system-source-control system)))
               (result (and location (list (translate-source-location location)))))
          result))))
    (:decorator-of-function
     (encapsulation-definition-sources name))
    (:decoration-definition
     (when (symbolp name)
       (let (result
             (flag nil))
         (dolist (v (get name 'decorate-function::decoration-definition-location-indicator))
           (when flag (push v result))
           (setf flag (not flag)))
         (mapcar 'translate-source-location (nreverse result)))))
    (t
     (funcall original-fn name type))))

(decorate-function:decorate-function 'sb-introspect::find-definition-sources-by-name #'sb-introspect--find-definition-sources-by-name-sb--decorated)
;; what about sb-introspect::find-definition-source ?




