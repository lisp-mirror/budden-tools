;;; -*- Encoding: utf-8; -*-
;;; printing hashtables readably. Tested on lispworks 6.0

(in-package :budden-tools)

(defvar *print-hashtable-readably* t
  "when *print-hashtable-readably* is t, hashtables are printed readably with their contents.")

(cl-user::portably-without-package-locks
; non-toplevel
(defmethod print-object ((o hash-table) stream)
  (cond (*print-hashtable-readably*  
         (format stream "#.~S" `(mkhash '(:test ,(hash-table-test o))
                                                        ',(iter 
                                                            (:for (k v) :in-hashtable o)
                                                            (:appending (list k v)))))
         )
        (t (call-next-method))
        ))

)


(defun mkhash (options keys-and-values)
  (iterate-keywords:iter 
    (:with result = (apply #'make-hash-table options))
    (:for key = (pop keys-and-values))
    (:while key)
    (:for value = (pop keys-and-values))
    (setf (gethash key result) value)
    (:finally (return result))
    ))
         
         


#+example
(proga ; http://code.google.com/p/def-symbol-readmacro
  (let *print-readably* t *print-circle* t) (let s (gensym))
  (let h (make-hash-table))
  (setf (gethash s h) 4)
  (print `(,s ,h)))



