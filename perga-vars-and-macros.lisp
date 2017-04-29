;;; -*- Encoding: utf-8; system :budden-tools; -*-
(in-package :perga-implementation)

(defvar -source-level-form-table- nil) ; не нужно - в степпере не используется. 
  ; вместо неё, исходник хранится в локальной переменной
  ; ф-ии stepize и поэтому недоступен

#+lispworks (defmacro print-source-level-form-table ()
  (print COMPILER::*SOURCE-LEVEL-FORM-TABLE*))
(defmacro compiler-break ()
  (break "compiler-break ~A" #+lispworks COMPILER:*function-name* #-sbcl nil ))

#+lispworks (defmacro capture-source-level-form-table ()
  (setf -source-level-form-table- COMPILER::*SOURCE-LEVEL-FORM-TABLE*) nil)


#|(defmacro perga (&whole form &body body)
  (declare (ignore body))
  (DBG17::BEGIN-SOURCE-LOCATION-SUBSTITUTIONS-FN)
  (MULTIPLE-VALUE-PROG1
      (perga-expander form)
    (DBG17::END-SOURCE-LOCATION-SUBSTITUTIONS-FN)))|#

#+lispworks 
(defmacro inner-perga (&whole form &body body)
  (declare (ignore body))
  (PERGA-EXPANDER form))
  

#+lispworks 
(defmacro perga (&whole form &body body)
  (declare (ignorable body form))
  `(lw-macro-friendly-dbg:with-source-location-substitutions
    (inner-perga ,@body)))

#-lispworks
(defmacro perga (&whole form &body body)
  (declare (ignore body))
  (PERGA-EXPANDER form))

  
;  (PERGA-EXPANDER form))
          



(defun remove-perga-clause (name)
  (setf (get name 'perga-transformer) nil))

#+lispworks 
(dspec:define-form-parser def-perga-clause (name &rest args)
  (declare (ignore def-perga-clause args))
  name)

(defun def-perga-clause-fun (symbol transformer &optional location)
  #-lispworks (declare (ignore location))
  (setf (get symbol 'perga-transformer) transformer)
  #+lispworks (when location (lispworks:record-definition symbol location))
  )

(defmacro def-perga-clause (symbol transformer)
  "symbol is not evaluated. Transformer is evaluated, should be a function of
  (body     - tail of entire body, starting from clause. body == (clause . forms-after-clause)
   clause   - clause to open, clause == (head . tail) == (first body)
   head     - head (car) of clause, symbol.
     Clause is identifyed by head, so head === symbol when transformer
     is called via some def-perga-clause definition. 
   tail     - tail (cdr) of clause
   forms-after-clause == (cdr body).
   Transformer should return two values:
   If second value is t, this means that form is processed by the clause, 
         first value should be a processed form, no further processing of body occurs.
   if second value is nil, clause is kept intact forms-after-clause are processed. 
   Consider some standard transformers: open-up-if-3, open-up-if-4, wind-up-tail-if-3,
   wind-up-tail-if-second-is-atom, open-up.

   Most of the time you don't need def-perga-clause in your code. Use just
   (perga ... (:@ your-macro ...rest-of-macro-call-header...) ...forms-of-body...)
   "
  #+lispworks
   `(dspec:def ,symbol
      (def-perga-clause-fun ',symbol ,transformer (dspec:location))
      )
   #-lispworks
   `(def-perga-clause-fun ',symbol ,transformer)
   )

(defmacro with-unwind (cleanup &body body)
  "Use (:@ with-unwind cleanup-form) perga clause to push cleanup form to dynamic stack"
  `(unwind-protect
       (perga ,@body)
     ,cleanup))


