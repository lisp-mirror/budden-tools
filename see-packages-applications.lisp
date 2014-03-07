;;; -*- Encoding: utf-8; -*-
(in-package :budden-tools)
(in-readtable nil) 

; почему-то не работает, видимо из-за начинки нашего readtable
(def-symbol-readmacro |/WITH-PACKAGE/|
                      (lambda (stream symbol)
                        (declare (ignore symbol))
                        (let* ((*package* (find-package (read stream)))
                               (res (read stream)))
                          (print res)
                          (it-is-a-car-symbol-readmacro res))))



(defpackage :package-for-read-symbol-name (:use))
(defparameter +package-for-read-symbol-name+ 
  (find-package :package-for-read-symbol-name))

(defun read-symbol-name (stream)
  "Читает символ, но возвращает только его имя. Если символ в потоке не имеет квалификатора, то такой символ вообще не будет создан"
  (let1 symbol-or-string (let1 *package* +package-for-read-symbol-name+
                 (read stream t))
    (etypecase symbol-or-string
      (string symbol-or-string)
      (symbol
       (assert (eq (symbol-package symbol-or-string) +package-for-read-symbol-name+))
       (unintern symbol-or-string +package-for-read-symbol-name+)
       (symbol-name symbol-or-string)))))



#+nil (defun ^-reader-internal (stream read-object object)
  "Если read-object=nil, то мы уже считали объект и читаем только то, что идёт после него"
  (let* ((object (if read-object (read stream t) object))
         (field-name (read-symbol-name stream))
         (args (read-delimited-list #\) stream t))) ; дерьмо вот здесь, и никак не решить. 
    (unread-char #\) stream) ; очень сомнительно.
    `(|^| ,object ,field-name ,@args)))


#|ф-я была только для примера (defun guess-where-i-am (object)
  "Делает так, что при закрытии скобки пишется сообщение, является ли этот объект каром списка.
  Если объект читается вне скобок, то ничего не происходит"
  (when *reading-parens* 
    (budden-tools::push-function-to-call-when-paren-is-closing
     (lambda (result stream)
       (assert (consp result) () 
         "Something wrong with symbol readmacro: list reader on ~S returned atom ~S" stream result)
       (cond ((eq object (car result)) 
              (format *trace-output* "~S: I am a car" object)
              result)
             (t 
              (format *trace-output* "~S: i am not a car" object)
              result))
       )))
  object)|#

(defun splice-later-if-a-car (object)
  "При закрытии скобки, если этот объект является каром списка, то ожидает, что он сам является списком. 
Сплайсит в его хвост хвост прочитанного списка и поднимает на один уровень. Например 
 (a^b c) ---/custom-token-parser/---> ((^ a b) c) ---/splice-if-a-car/---> (^ a b c)"
  (when *reading-parens* 
    (budden-tools::push-function-to-call-when-paren-is-closing
     (lambda (result stream)
       (assert (consp result) () 
         "Something wrong with symbol readmacro: list reader on ~S returned atom ~S" stream result)
       (cond ((eq object (car result)) 
              ;(format *trace-output* "~S: I am a car" object)
              (assert (listp object) () "splice-later-if-a-car: object ~S was expected to be a list" object)
              (append object (cdr result)) ; вернём при закрытии скобки
              )
             (t 
              ;(format *trace-output* "~S: i am not a car" object)
              result ; вернём при закрытии скобки
              )))))
  object)


(defun ^-reader-internal-2 (stream read-object object read-field-name field-name)
  "Если read-object=nil, то мы уже считали объект и читаем только то, что идёт после него"
  (let* ((object (if read-object (read stream t) object))
         (field-name (if read-field-name (read-symbol-name stream) field-name))
         (ret `(|^| ,object ,field-name))
         )
    (splice-later-if-a-car ret)
    ret))


(defun ^-reader-internal-3 (stream read-object object read-field-name field-name)
  "Если read-object=nil, то мы уже считали объект и читаем только то, что идёт после него.FIXME - здесь делается let пакету, 
в результате чего может многое испортиться. поправить read-symbol-name"
  (let* ((object (if read-object (read stream t) object))
         (field-name (if read-field-name (read-symbol-name stream) field-name))
         
         )
    (list '|^| object field-name)))


(defun closing-paren-splice-cdr-into-car (readmacro-returned)
  "Для symbol-readmacro, допускающего дополнительные данные после себя до закрытия скобки. 
symbol-readmacro должен вернуть список. cdr считанного списка вставляется в хвост того
списка, который вернул symbol-readmacro."
  ;(assert (null *functions-to-call-when-paren-is-closing*)
  ;    () "Wrong call to closing-paren-splice-cdr-into-car")
  (push-function-to-call-when-paren-is-closing 
        (lambda (list stream)
          (declare (ignore stream))
          (let1 car (car list)
            (assert (eq car readmacro-returned) () "Symbol-readmacro is not in the first position in a list ~S" list)
            (append car (cdr list))
            )))
  readmacro-returned)
  

;; FIXME при печати-чтении ^ выполняется повторно. Переименовать ^ во что-то, если это опасно 
;; (а опасно это может быть, если читать '(a b c --> d e f), хотя опасность не столь велика - ошибка чтения
(defun ^-reader (stream symbol)
  "См. также второе определение и найдёшь определение макроса ^"
  (declare (ignore symbol))
;  (it-is-a-car-symbol-readmacro (^-reader-internal-2 stream t nil t nil))
  (closing-paren-splice-cdr-into-car (^-reader-internal-3 stream t nil t nil))
  )



(def-symbol-readmacro |^| '^-reader)
; функция ^ определена в variable-type.lisp

;(def-symbol-readmacro def-merge-packages:|EXPORT2| 'def-merge-packages::export2-reader)                            

(defun convert-carat-to-^ (stream symbol-name package)
  "превращает инфиксный ^ в (^ a b). Для этого анализирует структуру
прочитанного списка после того, как он прочитан.
Это позволяет писать как (о^fun arg), так и o^field (а не (o^field)). 
Здесь есть ещё не вполне понятная проблема: если результат выполнения в виде 
символа является данными (например, заключён внутрь eval), и всё это написано
в файле, подлежащем компиляции, в файл _иногда_ попадает только сам символ, но не его
определения макросов. При последующем чтении определение
функции и макроса теряется и возникает ошибка типа 'неопределённая переманная #:(\^ ... ...)'.
Непонятно не столько то, что эта ошибка возникает, сколько то, что она возинкает
не всегда. Это большая проблема, её надо решать.
Были попытки использовать load-time-value, но без особого успеха пока что.
Можно ещё попробовать:
make-load-form
анализ, в какой части списка мы находимся. 
"
  (let* ((p (position #\^ symbol-name :from-end t)))
    (cond 
     ((null p) (values nil nil))
     ((= (+ p 1) (length symbol-name)) (values nil nil))
     (t ; (break "~A" symbol-name)
        (let ((beg (subseq symbol-name 0 p))
              (end (subseq symbol-name (+ p 1))))
          (values (let ((*package* package))
                    (funcall '^-reader-internal-2 
                             (make-concatenated-stream (make-string-input-stream (str+ beg " ")) stream)
                             t nil 
                             nil end))
                  t))))))

; (setf (get-custom-token-parsers-for-package :budden) nil)
; (pushnew 'convert-carat-to-^ (get-custom-token-parsers-for-package :budden))


(defparameter *essential-binding-checkers* 
  '(boundp fboundp) ;  ap5:rboundp - куда-то в другое мсто запихать
  "List of function names. Either function recieves one parameter, a symbol. If symbol is essential
and should be uninterned with caution, some of functions return true. E.g., if symbol denotes ap5 relation,
it is essential from ap5 viewpoint"
  )
                                     

; FIXME - теперь мы не можем добавлять новые свойства и записывать
; существующие
(defmacro with-proplist-carat (var &body body)
  "в var содержится property-list (:key value :key2 value2). Тогда внутри body var^key и var^KEY раскроется в 
код с getf* для поиска value. Ключи сравниваются с помощью string-equal"
  (unless body (warn "with-proplist-carat: empty body"))
  `(with-custom-carat-implementation 
    (,var (o f &rest mo) 
          `(prog1
               (getf* ,o ,f :test 'string-equal)
             (assert (assoc-getf* ,o ,f :test 'string-equal))))
    ,@body))

(setf (get 'with-proplist-carat 'proga-implementation::proga-transformer) 
      'proga-implementation::open-up)



