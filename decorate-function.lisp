;;; -*- Encoding: utf-8; System :decorate-function -*-
;;; decorate-function:
;;; Что делает? Позволяет подменить symbol-function, удовлетворяя следующим условиям:
;;; 1. Имеется возможность вызвать исходную функцию из подменённой
;;; 2. Можно вернуть symbol-function её исходное значение
;;; 3. При этом не нужно заводить переменную для хранения исходной функции.
;;; 4. При повторной подмене предыдущая подмена аннулируется. Таким образом, возможен обычный цикл
;;; итеративной разработки с той же логикой как и для defun/defparameter. 
;;; Будет работать неправильно, если после декорирования функция была переопределена с помощью defun 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :decorate-function 
    (:export 
     #:decorate-function
     #:undecorate-function
     #:apply-undecorated
     #:portably-without-package-locks
     #:get-undecorated
     #:get-function-decorator
     #:|С-декорированной-функцией|
     #:*undecorated-function-source-locations*
     #:get-original-function-source-location
     )
    (:use :cl)))

;(asdf:of-system :decorate-function) ; we are too early
(in-package :decorate-function)

; disable stepping
(declaim (optimize (debug 3) (compilation-speed 3) (safety 3)))


;;; Здесь нормально (с проверкой переопределения) сделаны макросы. Ф-ии надо переделать. 

(defstruct function-decoration
  "All we know about this decoration"
  (name (error "mandatory slot name missing") :type symbol)
  (old-function nil :type (or null function))
  old-function-source-location 
  ;; We generate lambda when decorating
  ;; new-function and new-function-source-location store decorator, not the lambda
  ;; which is actually written to (symbol-function name)
  (new-function nil :type (or symbol function))
  ;; also we store new-lambda which is actually assigned to (symbol-function name)
  ;; thus we are able to track the situation when the name was redefined and we are trying
  ;; to apply decoration again
  new-lambda 
  )

(defvar *function-decorations*
  (swank/backend:make-weak-key-hash-table :test 'eq)
  "Name of original function -> function-decoration")

; maps symbol to (symbol . original-macro-function)
; symbol is an uninterned symbol to which original macro definition is copied while
; macro is being decorated
(defvar *undecorated-macros* (swank/backend:make-weak-key-hash-table :test 'eq))

(defmacro portably-without-package-locks (&body body)
  "An attempt to override package locks in a cross-implementation manner. Misplaced and maybe erroneous"
`(#+sbcl sb-ext:without-package-locks
#+allegro excl::without-package-locks
#+cmu ext:without-package-locks
#+lispworks let 
#+lispworks 
((lw:*handle-warn-on-redefinition* :warn)
 ; (dspec:*redefinition-action* :warn)
 (hcl:*packages-for-warn-on-redefinition* nil))
#+clisp ext:without-package-lock #+clisp ()
#+ccl let
#+ccl ((ccl:*warn-if-redefine-kernel* nil)) 
#-(or allegro lispworks sbcl clisp cmu ccl) 
progn
,@body))

(defun decorate-function (symbol decorator-fn)
  "See example"
  (check-type symbol symbol)
  (assert (fboundp symbol) () "It hardly makes sense to decorate non-functions")
  (check-type decorator-fn (and (not null) (or symbol function)))
  (let* (#+lispworks
         (lispworks:*handle-warn-on-redefinition* nil)
         (old-entry (gethash symbol *function-decorations*))
         (old-ok (and old-entry (eq (function-decoration-new-lambda old-entry)
                                    (symbol-function symbol))))
         (old-fn (if old-ok
                     (function-decoration-old-function old-entry)
                     (symbol-function symbol)))
         (new-entry
          (make-function-decoration
           :name symbol
           :old-function old-fn
           #+sbcl :old-function-source-location
           #+sbcl (sb-introspect::find-definition-source old-fn)
           :new-function decorator-fn)))
    (setf (symbol-function symbol)
          (lambda (&rest args) (apply decorator-fn old-fn args)))
    (setf (gethash symbol *function-decorations*)
          new-entry)
    (symbol-function symbol)))

(defun get-function-decorator (name)
  "Returns decorator for function name. If it is undecorated, returns nil"
  (check-type name (and symbol (not null)))
  (let* ((entry (gethash name *function-decorations*)))
    (and entry (function-decoration-new-function entry))))

(defun decorate-macro-get-undecorated-invoker (symbol)
  (car (gethash symbol *undecorated-macros*)))

(defun decorate-macro-check-redefinition (symbol)
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

(defun decorate-macro (symbol decorator-macro)
  "See example"
  (let (#+lispworks (lispworks:*handle-warn-on-redefinition* nil)
                    (entry (decorate-macro-check-redefinition symbol)))
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

(defun undecorate-macro (symbol)
  (let ((decoration-entry (decorate-macro-check-redefinition symbol)))
    (when decoration-entry
      (setf (macro-function symbol) (macro-function (car decoration-entry)))
      (remhash symbol *undecorated-macros*))))

(defun undecorate-function (symbol)
  (let ((decoration (gethash symbol *function-decorations*)))
    (assert decoration () "Function ~S is not decorated" symbol)
    (remhash symbol *function-decorations*)
    (setf (symbol-function symbol) (function-decoration-old-function decoration))
    ))

(defun get-original-function-source-location (symbol)
  (let ((decoration (gethash symbol *function-decorations*)))
    (and decoration
         (function-decoration-old-function-source-location decoration))))

(defun apply-undecorated (symbol args)
  "If function of a symbol is decorated, calls original function. If it is not decorated, call just the #'symbol"
  (apply (get-undecorated symbol) args))

(defun get-undecorated (symbol)
  "If function is not decorated, returns symbol-function of symbol"
  (let ((decoration (gethash symbol *function-decorations*)))
    (if decoration
        (function-decoration-old-function decoration)
        (symbol-function symbol))))

(defmacro |С-декорированной-функцией| (|Функция| |Декоратор| &body |Тело|)
  "Функция и декоратор - это символы, хотя декоратор может быть и лямбдой. Вычисляются.
ПРАВЬМЯ - этому место в пакете decorate-function. Не стоит питать иллюзий на тему безопасности этйо конструкции - её действие, хоть и временное, но распространяется на все треды. Возвращает nil"
  (let ((|Функция-однократно| (gensym "Функция-однократно")))
    `(let ((,|Функция-однократно| ,|Функция|))
       (unwind-protect
           (progn
             (decorate-function:decorate-function ,|Функция-однократно| ,|Декоратор|)
             ,@|Тело|)
         (decorate-function:undecorate-function ,|Функция-однократно|)))))


#+example
(progn 
  (remhash 'foo *function-decorations*)
  (defun foo (x) x)
  (defun decorated-foo (fn &rest args) (let1 (y) args (+ y (apply fn args))))
  (decorate-function 'foo #'decorated-foo)
  (assert (= (foo 1) 2))
  (decorate-function 'foo #'decorated-foo)
  (assert (= (foo 1) 2))
  (defun foo (x) (- x)) 
  ; Тhis is a flaw. Old #'foo is taken from *function-decorations*. 
  ; but code is intended for decorating system functions.
  (decorate-function 'foo #'decorated-foo)
  (assert (= (foo 1) 2))
  )
  
#+example
(progn ; evaluate it, not compile
  (defmacro original (symbol) `',symbol)
  (defmacro decorate-original (symbol) `(list :decorated (,(decorate-macro-get-undecorated-invoker 'original) ,symbol)))
  (decorate-macro 'original 'decorate-original)
  (print (original 'asdf))
  (undecorate-macro 'original)
  )



