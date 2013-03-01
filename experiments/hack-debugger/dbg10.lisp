(in-package :lw-macro-friendly-stepper)
(import 'iterk:iter)

(defmacro iter2 (&rest body)
      (let* ( ; (body (cdr form))
             (form-table (my-wombat body))
             (result (macroexpand-1 `(iterk:iter ,.body)))
             )
        (let ((*print-circle* t))
          (budden-tools::show-expr `(,form-table ,result)))
        result
        )
      )    
  
(defun f ()
  (iterk:iter (:for i from 3 to 6)
         (:adjoining (:finding (* i i) maximizing #'integer-length
                      :into foo) :test #'=))
  )


(defun f() 
  (let ((a 1))
    (+ a 2)))


(defun f () 
  (iterk:iter
   (:for i from 1 to 2)
   (:collect (- i)))
  )



