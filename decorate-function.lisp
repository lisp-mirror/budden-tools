(in-package :budden-tools)

(defvar *undecorated-functions* (make-hash-table :test 'eq))

(defun decorate-function (symbol decorator-fn)
  "See example"
  (proga
    #+lispworks (let lispworks:*handle-warn-on-redefinition* nil)
    (symbol-macrolet old (gethash symbol *undecorated-functions*))
    (let old-fn (or old (setf old (symbol-function symbol))))
    (setf (symbol-function symbol) 
          (lambda (&rest args) (apply decorator-fn old-fn args)))))

(defun undecorate-function (symbol)
  (let1 old-function
      (gethash symbol *undecorated-functions*)
    (assert old-function)
    (setf (symbol-function symbol) old-function)))


(defun apply-undecorated (symbol args)
  (apply (gethash symbol *undecorated-functions* (symbol-function symbol)) args))


#+example
(progn 
  (remhash 'foo *undecorated-functions*)
  (defun foo (x) x)
  (defun decorate-foo (fn &rest args) (let1 (y) args (+ y (apply fn args))))
  (decorate-function 'foo #'decorate-foo)
  (assert (= (foo 1) 2))
  (decorate-function 'foo #'decorate-foo)
  (assert (= (foo 1) 2))
  (defun foo (x) (- x)) 
  ; this is a flaw. Old #'foo is taken from *undecorated-functions*. 
  ; but code is intended for decorating system functions.
  (decorate-function 'foo #'decorate-foo)
  (assert (= (foo 1) 2))
  )
  
