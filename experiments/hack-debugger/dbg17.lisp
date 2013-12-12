;; Лучшее из этого кода находится в lw-macro-friendly-dbg.lisp 

;; Копия hack-debugger 5. 
;; Текущий результат: умеем собирать инфу, но она 
;; обрабатывается в неопределённом будущем.
;; Надо обрабатывать своевременно. 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :dbg17
    (:use :cl :lispworks)
    (:export #:set-source-location-substitution
     #:with-source-location-substitutions
     #:*interesting-function-name*
     #:*w-table*
     #:*w-form*
     #:*address-substitution-table*
     #:END-SOURCE-LOCATION-SUBSTITUTIONS-FN
     #:BEGIN-SOURCE-LOCATION-SUBSTITUTIONS-FN
     #:*with-source-location-substitutions-level*
     #:*COMPILE-TIME-SUBSTITUTION-TABLE*
     )))

(in-package :dbg17)

(defmacro compiler-break ()
  (break "compiler-break ~A" COMPILER:*function-name*))

(defvar *frame* nil)

(defadvice (system::dbg-edit-frame-internal hap-frame :around)
    (frame pane)
  (setf *frame* frame)
  (call-next-advice frame pane))


;; итак, мы умеем подменять путь для нормального кода (не в степпере, для степпера надо вернуться к dbg5). 
;; А как мы найдём его? 
    

#|
выяснили, что 
связывание COMPILER::*source-level-form-table* происходит в
COMPILER::in-process-forms-in-file. 

Тогда compiler::process-form (следующий вызов) - уже со связанной таблицей. 
Значит, для него и сделаем around. Логично? 

Не логично, т.к. он много раз вызывается вложенно и таблица достраивается. 
А нам нужно отсчитать от корня. 

Лучше повиснем на compiler::wombat-2 
и будем работать с его результатом. 

Составим таблицу, какой конс мы на какой заменяем. 
В результате wombat-2 найдём исходный и заменяемый конс и 

|#

;(defadvice (compiler::process-form hap-source-level-form-table :around)
;    (i-form)
;  (let ((result (call-next-advice i-form)))
;    (print COMPILER::*source-level-form-table* *trace-output*)
;    (capture-source-level-form-table)
;    result))

#| Заготовка кода прохода по таблице с преобразовательной записью
    (when (and first-cons-value second-cons-value)
      (maphash 
       (lambda (key value)
         (typecase value
           (number
            (when (eq key *second-cons*)
              (break "ura")
              (setf (gethash key hash) first-cons-value))
            (when (eq key *first-cons*) (setf (gethash key hash) second-cons-value))
            )
           (COMPILER::multiple-transforms-record
           ; пропускаем его
            )
           (t
           ; что-то невиданное, потом сделать warning
            )
          ))
       hash))))
|#

(defparameter *w-form* nil "Сворованная у wombat-2 форма")
(defparameter *w-table* nil "Сворованная у wombat-2 таблица")

;(defparameter *address-substitution-table* nil 
;  "Ключ - адрес реального кода, значение - адрес места, где надо показать. Пока работает только для одной
; функции *interesting-function-name*"
;  )

; (defparameter *interesting-function-name* 'y)

#|(defadvice (dbg::call-frame-edit-path hack-path
                                      :around
                                      :documentation
                                      "Пример подмены пути редактирования для конкретной функции")
    (c)
  (let (
        (original-path (call-next-advice c))
        maybe-new-path)
    (when (eq *interesting-function-name*
              (ignore-errors
                (slot-value *frame* 'dbg::function-name)))
      (setf maybe-new-path (gethash original-path *address-substitution-table*)))
    (or maybe-new-path 
        original-path)))


(REMOVE-ADVICE 'dbg::call-frame-edit-path 'hack-path) |#

(defadvice (compiler::wombat-2 hack-two-conses :around)
    (form &optional (table COMPILER::*source-level-form-table*))
  (setf *w-table* table
        *w-form* form)
  (call-next-advice form table))


(defun find-source-address-in-a-hash (source)
  (gethash source *w-table*)) 




; сделать трассировку compiler::wombat-2 и скомпилировать f (через compile defun)
