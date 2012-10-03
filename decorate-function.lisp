;;; Централизованное API для переопределения функций. См. примеры в конце файла.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :decorate-function 
    (:export 
     #:decorate-function
     #:undecorate-function
     #:apply-undecorated
     #:get-undecorated)
    (:use :cl)))

(in-package :decorate-function)

;;; Здесь нормально (с проверкой переопределения) сделаны макросы. Ф-ии надо переделать. 

(defvar *undecorated-functions* (make-hash-table :test 'eq))

; maps symbol to (symbol . original-macro-function)
; symbol is an uninterned symbol to which original macro definition is copied while
; macro is being decorated
(defvar *undecorated-macros* (make-hash-table :test 'eq))

(defun decorate-function (symbol decorator-fn)
  "See example"
  (let (#+lispworks (lispworks:*handle-warn-on-redefinition* nil))
    (symbol-macrolet ((old (gethash symbol *undecorated-functions*)))
      (let ((old-fn (or old (setf old (symbol-function symbol)))))
        (setf (symbol-function symbol) 
              (lambda (&rest args) (apply decorator-fn old-fn args)))))))


(defun |decorateMacro-getUndecoratedInvoker| (symbol)
  (car (gethash symbol *undecorated-macros*)))

(defun |decorateMacro-checkRedefinition| (symbol)
  "Checks if macro was redefined and errs if it was. Returns decoration entry"
  (let ((decoration-entry (gethash symbol *undecorated-macros*)))
    (cond
     ((null decoration-entry) decoration-entry)
     ((not (string= symbol (car decoration-entry)))
      (error "name mismatch between '~A with saved macro symbol '~A" symbol (car decoration-entry)))
     ((not (eq (macro-function symbol) (cdr decoration-entry)))
      (cerror "make macro undecorated. Current definition will be kept"
              "macro '~A was decorated and then redefined" symbol)
      (remhash symbol *undecorated-macros*)
      nil)
     (t decoration-entry)
     ))
  )

(defun |decorateMacro| (symbol decorator-macro)
  "See example"
  (let (#+lispworks (lispworks:*handle-warn-on-redefinition* nil)
                    (entry (|decorateMacro-checkRedefinition| symbol)))
    (macrolet ((mf (s) `(macro-function ,s)))
      (cond
       (entry 
        (setf (mf symbol) (mf decorator-macro)
              (cdr entry) (mf decorator-macro)))
       (t 
        (let ((old-def-symbol (make-symbol (symbol-name symbol))))
          (setf (mf old-def-symbol) (mf symbol))
          (setf (gethash symbol *undecorated-macros*)
                (cons old-def-symbol (mf decorator-macro)))
          (setf (mf symbol) (mf decorator-macro))))
       ))))

(defun |undecorateMacro| (symbol)
  (let ((decoration-entry (|decorateMacro-checkRedefinition| symbol)))
    (when decoration-entry
      (setf (macro-function symbol) (macro-function (car decoration-entry)))
      (remhash symbol *undecorated-macros*))))

(defun undecorate-function (symbol)
  (let ((old-function (gethash symbol *undecorated-functions*)))
    (assert old-function)
    (setf (symbol-function symbol) old-function)))


(defun apply-undecorated (symbol args)
  "If function of a symbol is decorated, calls original function. If it is not decorated, call just the #'symbol"
  (apply (or (gethash symbol *undecorated-functions* (symbol-function symbol))
             symbol) args))

(defun get-undecorated (symbol)
  (gethash symbol *undecorated-functions* (symbol-function symbol)))


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
  
#+example
(progn ; evaluate it, not compile
  (defmacro original (symbol) `',symbol)
  (defmacro decorate-original (symbol) `(list :decorated (,(|decorateMacro-getUndecoratedInvoker| 'original) ,symbol)))
  (|decorateMacro| 'original 'decorate-original)
  (print (original 'asdf))
  (|undecorateMacro| 'original)
  )
