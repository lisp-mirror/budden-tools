(in-package :budden-tools)

(defstruct reference-box getter setter)

(defmacro byref (symbol &environment environment)
  "ћожно обобщить на любое место, но недосуг"
  (with-gensyms (new-value)
    (assert (symbolp symbol))
    (assert (not (constantp symbol environment)))
  `(make-reference-box :getter (lambda () ,symbol)
                       :setter (lambda (,new-value) (setf ,symbol ,new-value)))
  ))


(defmacro with-byref-params (symbols &body body)
  (iter 
    (:for symbol in symbols)
    (:for setter-new-name = (make-symbol (str+ symbol "-setter")))
    (assert (symbolp symbol))
    (:collect `(assert (reference-box-p ,symbol)) :into checks)
    (:appending
     `((,setter-new-name (reference-box-setter ,symbol))
       (,symbol (funcall (reference-box-getter ,symbol))))
     :into inits)
    (:collecting `(funcall ,setter-new-name ,symbol) :into cleanup)
    (:finally
     (return 
      `(progn
         ,@checks
         (let ,inits
           (unwind-protect 
               (proga ,@body)
             ,@cleanup)))))))


(def-trivial-test::! pass-by-ref.1 
                     (proga
                       (flet incf-arg (arg)
                         (with-byref-params (arg)
                           (incf arg)))
                       (let x 4)
                       (incf-arg (byref x))
                       x)
                     5)
