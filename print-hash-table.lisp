;;; -*- Encoding: utf-8; -*-
;;; printing hashtables readably. Tested on lispworks 6.0

(in-package :budden-tools)

(defvar *print-hashtable-readably* t
  "when *print-hashtable-readably* is t, hashtables are printed readably with their contents.")

(decorate-function:portably-without-package-locks
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




(defun hash-table-difference (h1 h2 &key (test 'eql) (key 'identity))
  "Subtracts h2 from h1. h1 and h2 are two hash tables with the same test function. Returns new hash table with keys
   and values from h1 which are either absent from h2, or contain different (with respect to test and key) values in h1 and h2"
  (let* ((key1 (hash-table-test h1))
         (key2 (hash-table-test h2))
         (result (make-hash-table :test key1)))
    (assert (eq key1 key2))
    (maphash
     (lambda (k1 v1)
       (multiple-value-bind (v2 present-p) (gethash k1 h2)
         (cond
          ((and present-p
                (funcall test (funcall key v1) (funcall key v2)))
           ; do nothing - values coincide
           )
          (t
           (setf (gethash k1 result) v1))
          )))
     h1)
    result))


(def-trivial-test::! hash-table-difference.1
                     (hash-table-difference
                      (mkhash nil '(1 2 3 4 5 6))
                      (mkhash nil '(1 3 4 2 5 6)))
                     (mkhash nil '(1 2 3 4)))
