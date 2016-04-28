;; Copyright (C) Денис Будяк 2016
;; Лицензия MIT
(def-merge-packages::! :test-let-around-compile-file-or-load
                       (:always t)
  (:use :cl :budden-tools))

(in-package :test-let-around-compile-file-or-load)

(eval-when (:compile-toplevel :load-toplevel)
  (error "Не компилируй этот файл. Загружай его"))

(pushnew '*my-super-var*
         СВЯЗАТЬ-СПЕЦИАЛЬНЫЕ-ПЕРЕМЕННЫЕ-ВОКРУГ-LOAD-ИЛИ-COMPILE-FILE::*СВЯЗАТЬ-ВОКРУГ-COMPILE-FILE*
         )

(defparameter *my-super-var* nil)
(defparameter *my-another-var* nil)

(compile-file
 (merge-pathnames "data/test-let-around-compile-file-or-load.data.lisp"
                  (budden-tools:path-to-a-file *load-truename*)))

(assert (null *my-super-var*))
(assert (eql 1 *my-another-var*))

#|(defun-to-file read-macro-arglist (double-tilda-formatter)
             "Нашли идентификатор макроса. Запускаем чтение рекурсивно. Читаем открывающую скобку, параметры
через запятую. Возвращаем список аргументов макроса, где каждый аргумент - список лексем,
который мы квотим. Неоднозначность получается в случае 
macro(arg1,/*comment*/,arg2). В этом случае, /*comment*/ является аргументом. 
В случае macro(a,,b) второй аргумент - это пустая строка. Но на это забиваем." 
             ; (error "not implemented")
             ; нужно в цикле читать fbody. Знать, как произошёл возврат (через special перем или через 
             ; multiple values
             ; собирать возвраты каждого fbody
             ; вернуть их как список
             ;(break "вошли в read-macro-arglist")
             (matchit-stream
              [
               #\( 
               ! (iter
                  (let ((mpf::*terminator-char-found* nil) 
                        (mpf::*aux-term-char-found* nil))
                    (:collecting
                     (generate-code-from-firebird-lexems
                      (apply 'firebird-lexer-only %source%
                             :require-balanced-parens t 
                             :terminator-char #\)
                             :aux-term-char #\,
                             (dispatch-keyargs-simple double-tilda-formatter)
                             ) %source%)
                     :into res)
                    (when mpf::*terminator-char-found* 
                      (return-from read-macro-arglist res))
                    (unless mpf::*aux-term-char-found*
                      (error "Unbalanced parens in macro arglist. Last read lexems are ~S" res))
                    ))
               ]
              )
             )

|#