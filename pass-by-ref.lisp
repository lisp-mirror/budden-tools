;;; -*- Encoding: utf-8; -*-
(in-package :budden-tools)

(defstruct reference-box "Box to pass place to a function by reference"
  v ; for debugging only 
  getter
  setter)

(defmacro byref (place &environment environment)
  "Imitates 'var' parameter in Pascal. Use with-byref-params to declare parameter as var, byref to pass that parameter. See test.
   Note that returning var parameters is arranged to occur in cleanup forms of unwind-protect. So if your routine exits abnormally due to a condition, var parameter might change anyway." 
  (with-gensyms (box new-value)
    (assert (not (constantp place environment)))
    `(let ((,box (make-reference-box :v ,place
                                     :getter (lambda () ,place))))
       (setf (reference-box-setter ,box)
             (lambda (,new-value) (setf ,place ,new-value
                                        (reference-box-v ,box) ,new-value)))
       ,box)
    ))


(defmacro with-byref-params (symbols &body perga-body)
  "Imitates 'var' parameter in Pascal. Use with-byref-params to declare parameter as var, byref to pass that parameter. See test.
   Note that returning var parameters is arranged to occur in cleanup forms of unwind-protect. So if your routine exits abnormally due to a condition, var parameter might change anyway." 
  (assert (typep symbols '(or (cons symbol) null)) () "with-byref-params: ~S должно было быть списком символов или nil-ом"
    symbols)
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
               (perga ,@perga-body)
             ,@cleanup)))))))

(defmacro with-byref-params-proga (symbols &body proga-body)
  "Deprecated version of with-byref-params. Body is wrapped inside proga. Use with-byref-params instead" 
  (assert (typep symbols '(or (cons symbol) null)) () "with-byref-params: ~S должно было быть списком символов или nil-ом"
    symbols)
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
               (proga ,@proga-body)
             ,@cleanup)))))))


(defmacro assert-byvalue (x)
  "If you afraid someone would pass unexpected byref param, you can add the assertion"
  `(assert (not (typep ,x 'reference-box)) ()
     "Param ~S was passed byref unexpectedly" ',x
     ))


(def-trivial-test::! pass-by-ref.1 
                     (PERGA-IMPLEMENTATION:perga
                       (flet incf-arg (arg)
                         (with-byref-params (arg)
                           (incf arg)))
                       (let x 4)
                       (incf-arg (byref x))
                       x)
                     5)
