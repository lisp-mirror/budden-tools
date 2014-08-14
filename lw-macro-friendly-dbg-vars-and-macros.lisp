;;; -*- Encoding: utf-8; -*-
;;; infrastructure for making debugger more macro friendly
#|
Written by Denis Budyak, 
Email: (map 'string 'code-char '(98
 117 100 100 101 110 55 51 64 103 109 97 105 108 46 99 111 109))

The code is in public domain.

We interfere process of mapping sources to "addresses" so that 
we could set the source form to show as a substitute for computer-generated
macroexpanded form. 

See also lw-macro-friendly-stepper.lisp - the same for stepper using conses smash.

State of the code is alpha. Feedback is greatly appreciated.
|#
(def-merge-packages::! :lw-macro-friendly-dbg
                       (:use :cl :lispworks)
                       (:always t)
                       (:export "
     lw-macro-friendly-dbg:set-source-location-substitution
     lw-macro-friendly-dbg:with-source-location-substitutions
     lw-macro-friendly-dbg:*interesting-function-name*
     lw-macro-friendly-dbg:*w-table*
     lw-macro-friendly-dbg:*w-form*
     lw-macro-friendly-dbg:*address-substitution-table*
     lw-macro-friendly-dbg:END-SOURCE-LOCATION-SUBSTITUTIONS-FN
     lw-macro-friendly-dbg:BEGIN-SOURCE-LOCATION-SUBSTITUTIONS-FN
     lw-macro-friendly-dbg:*with-source-location-substitutions-level*
     lw-macro-friendly-dbg:*COMPILE-TIME-SUBSTITUTION-TABLE*
     lw-macro-friendly-dbg:*disable-set-source-location-substitution-warning*"
     ))

(in-package :lw-macro-friendly-dbg)

(defmacro compiler-break ()
  "debugging tool"
  (break "compiler-break ~A" COMPILER:*function-name*))

#| for debugging 
(defvar *frame* nil)

(defadvice (system::dbg-edit-frame-internal hap-frame :around)
    (frame pane)
  (setf *frame* frame)
  (call-next-advice frame pane))  |#


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



; сделать трассировку compiler::wombat-2 и скомпилировать f (через compile defun)


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
        original-path))) |#

#|  (let ((first-cons-address (find-source-address-in-a-hash *first-cons*))
        (second-cons-address (find-source-address-in-a-hash *second-cons*)))
    (assert (and first-cons-address second-cons-address))
    (setf (gethash first-cons-address *address-substitution-table*) second-cons-address)
    (setf (gethash second-cons-address *address-substitution-table*) first-cons-address)
    )) |#


#| Где упоминается COMPILER::MULTIPLE-TRANSFORMS-RECORD

(COMPILER::TRANSFORM-FORM COMPILER::GET-FORM-PATH COMPILER::FIND-NODE-SOURCE-PATH 
COMPILER::FIND-NODE-FORM DBG::GENERATE-SCL-INFO DBG17:END-SOURCE-LOCATION-SUBSTITUTIONS-FN 
COMPILER::COMPILER-SOURCE-LEVEL-TRANSFORMATION-A COMPILER::NODE-REPLACING-NODE)

|#

#|
(defadvice (COMPILER::FIND-NODE-SOURCE-PATH take-code-from-compile-time-substitution-table :around)
    (node)
  (when
      (and (SLOT-EXISTS-P node 'compiler::source)
           (SLOT-BOUNDP node 'compiler::source)
           )
    (HACK-SOURCE-LEVEL-FORM-TABLE (slot-value node 'compiler::source)))
  (CALL-NEXT-ADVICE node))

(remove-advice 'COMPILER::FIND-NODE-SOURCE-PATH 'take-code-from-compile-time-substitution-table)
|#
;(remove-advice 'compiler::get-form-path 'take-code-from-compile-time-substitution-table)
; 'DBG::GENERATE-SCL-INFO



(defparameter *w-form* nil "Сворованная у wombat-2 форма")
(defparameter *w-table* nil "Сворованная у wombat-2 таблица")

(defvar *compile-time-substitution-table* nil
  "Ключ таблицы - форма из сгенерированного кода, 
   значение - место в исходнике из файла, на котором форма будет показываться")

(defvar *with-source-location-substitutions-level* nil "Если уровень не 0, то подстановки действуют")

     
(defmacro with-bound-compile-time-substitution-table (&body body)
  `(let ((*compile-time-substitution-table* *compile-time-substitution-table*)
         (*with-source-location-substitutions-level* (or *with-source-location-substitutions-level* 0)))
     ,@body))  


(defmacro begin-source-location-substitutions ()
  (begin-source-location-substitutions-fn)
  )

(defmacro end-source-location-substitutions ()
  (end-source-location-substitutions-fn)
  )

(defmacro with-source-location-substitutions (form)
  `(progn 
     (begin-source-location-substitutions)
     (multiple-value-prog1
         ,form
       (end-source-location-substitutions))))
