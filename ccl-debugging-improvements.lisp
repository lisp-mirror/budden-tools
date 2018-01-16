;;; -*- coding: utf-8; system :budden-tools; -*-

(named-readtables:in-readtable nil)
(in-package :swank/ccl)

;;; COPY of emacs-inspect from CCL
(defmethod emacs-inspect :around ((f function)) 
  (append
   (label-value-line "Name" (function-name f))
   `("Its argument list is: "
     ,(princ-to-string (arglist f)) (:newline))
   (label-value-line "Documentation" (documentation  f t))
   (when (function-lambda-expression f)
     (label-value-line "Lambda Expression"
                       (function-lambda-expression f)))
   (when (ccl:function-source-note f)
     (label-value-line "Source note"
                       (ccl:function-source-note f)))
   (when (typep f 'ccl:compiled-lexical-closure)
     (append
      (label-value-line "Inner function" (ccl::closure-function f))
      '("Closed over values:" (:newline))
      (loop for (name value) in (ccl::closure-closed-over-values f)
            append (label-value-line (format nil " ~a" name)
                                     value))))
   (when (ccl::function-encapsulated-p f)
     (label-value-line "Encapsulated"
                       (ccl::function-encapsulated-p f)))
   (when (ccl::function-symbol-map f)
     (label-value-line "Symbol map"
                       (ccl::function-symbol-map f)))
   (when (ignore-errors (ccl::%function-to-function-vector f))
     (label-value-line "Function vector"
                       (ccl::%function-to-function-vector f)))
   ))
  