;;; -*- Encoding: utf-8; -*-
(in-package :budden-tools)

(defstruct reference-box "Box to pass place to a function by reference"
  v ; for debugging only 
  getter
  setter)

(defmacro byref (place &environment environment)
  "Pass place to a function as a parameter. E.g. (my-modifier (byref var)). Callee must use with-byref-params"
  (with-gensyms (box new-value)
    (assert (not (constantp place environment)))
    `(let ((,box (make-reference-box :v ,place
                                     :getter (lambda () ,place))))
       (setf (reference-box-setter ,box)
             (lambda (,new-value) (setf ,place ,new-value
                                        (reference-box-v ,box) ,new-value)))
       ,box)
    ))

(defmacro with-byref-params (symbols &body body)
  "Handles parameter passed by reference. See test"
  (assert (listp symbols) () "with-byref-params: ~S должно было быть символом" symbols)
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


(defmacro assert-byvalue (x)
  "If you afraid someone would pass unexpected byref param, you can add the assertion"
  `(assert (not (typep ,x 'reference-box)) ()
     "Param ~S was passed byref unexpectedly" ',x
     ))


(def-trivial-test::! pass-by-ref.1 
                     (proga
                       (flet incf-arg (arg)
                         (with-byref-params (arg)
                           (incf arg)))
                       (let x 4)
                       (incf-arg (byref x))
                       x)
                     5)
