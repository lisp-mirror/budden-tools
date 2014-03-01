;; Lispworks IDE is unable to find source of toplevel forms, e.g. when errors happen at load-time
;; Let's fix it.
;; With this small utility, now instead of just (/ (read-from-string "0")) one can write 
;; (def-toplevel-progn "divide-by-zero" () (/ (read-from-string "0")))
;; And instead of (eval-when (:load-toplevel) (/ (read-from-string "0")))
;; one can write 
;; (def-toplevel-progn "divide-by-zero-2" (:load-toplevel) (/ (read-from-string "0")))
;; name of a progn must be unique inside a lisp image. Symbols seem to be not appropriate
;; as they should be used for more useful things than making anchors for locating errors
;; Code is in public domain, Denis Budyak, 2014

(in-package :budden-tools)

(defmacro def-toplevel-progn (name (&rest eval-when-situations) &body body)
  "Define toplevel form so that it would be found by a debugger"
  (let* ((current-fn-symbol (make-symbol name))
         (internal-form 
          `(dspec:def (def-toplevel-progn ,name)
             (dspec:record-definition `(def-toplevel-progn ,',name) (dspec:location))
             (defun ,current-fn-symbol () "function for def-toplevel-progn" ,@body)
             (,current-fn-symbol)
             ))
         (outer-form
          (cond
           (eval-when-situations
            `(eval-when ,eval-when-situations
               ,internal-form))
           (t internal-form)
           )))
    outer-form))

(defun canonicalize-def-toplevel-progn-dspec (dspec)
  (and
   (consp dspec)
   (eq (first dspec) 'def-toplevel-progn)
   (typep (second dspec) 'string)
   `(def-toplevel-progn ,(second dspec))
   ))

(dspec:define-dspec-class def-toplevel-progn nil 
  "Toplevel progn locatable in a debugger session"
  :canonicalize #'canonicalize-def-toplevel-progn-dspec
  )

; turned out to be unnecessary
;(dspec:define-form-parser
;    (def-toplevel-form
;     (:alias defpackage)))


