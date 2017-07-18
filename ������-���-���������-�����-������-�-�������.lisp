;;; -*- coding: utf-8; -*-
;;; Код взят из руководства SBCL и портирован на :trivial-gray-streams. Запускался только под SBCL, тестов нет.

(named-readtables:in-readtable nil)

(def-merge-packages::! :ПОТОКИ-ЗПТ-СЧИТАЮЩИЕ-БУКВЫ-СТРОКИ-И-КОЛОНКИ
  (:always t)
  (:use :trivial-gray-streams :cl)
  (:export "
   ПОТОКИ-ЗПТ-СЧИТАЮЩИЕ-БУКВЫ-СТРОКИ-И-КОЛОНКИ:Считающий-входной-поток-литер
   ПОТОКИ-ЗПТ-СЧИТАЮЩИЕ-БУКВЫ-СТРОКИ-И-КОЛОНКИ:Счётчик-литер-из
   ПОТОКИ-ЗПТ-СЧИТАЮЩИЕ-БУКВЫ-СТРОКИ-И-КОЛОНКИ:Счётчик-строк-из
   ПОТОКИ-ЗПТ-СЧИТАЮЩИЕ-БУКВЫ-СТРОКИ-И-КОЛОНКИ:Счётчик-колонок-из
            
  "
   ))

(in-package :ПОТОКИ-ЗПТ-СЧИТАЮЩИЕ-БУКВЫ-СТРОКИ-И-КОЛОНКИ)

(defclass |Обёрнутый-поток| (fundamental-stream)
  ((|Поток| :initarg :|Поток| :reader |Поток-из|))
  (:documentation "Обёрнутый поток. См. потомков (и насладитесь глубиной иерархии!"))
  
(defmethod stream-element-type ((|Поток| |Обёрнутый-поток|))
  (stream-element-type (|Поток-из| |Поток|)))
     
(defmethod close ((|Поток| |Обёрнутый-поток|) &key abort)
  (close (|Поток-из| |Поток|) :abort abort))
     
(defclass |Обёрнутый-входной-поток-литер|
          (|Обёрнутый-поток| fundamental-character-input-stream)
  ()
  (:documentation "Серые сумерки между ночью и днём. См. предков и потомков"))
     
(defmethod stream-read-char ((|Поток| |Обёрнутый-входной-поток-литер|))
  (read-char (|Поток-из| |Поток|) nil :eof))

(defmethod stream-unread-char ((|Поток| |Обёрнутый-входной-поток-литер|)
                               char)
  (unread-char char (|Поток-из| |Поток|)))
     
(defclass |Считающий-входной-поток-литер|
          (|Обёрнутый-входной-поток-литер|)
  ((|Счётчик-литер| :initform 1 :accessor |Счётчик-литер-из|)
   (|Счётчик-строк| :initform 1 :accessor |Счётчик-строк-из|)
   (|Счётчик-колонок| :initform 1 :accessor |Счётчик-колонок-из|)
   (|Стар-счётчик-колонок| :initform 1 :accessor |Стар-счётчик-колонок-из|))
  (:documentation "См. потомков. Функции |Номер-буквы|, |Номер-строки| и |Номер-колонки| дают нужные значения. Пример использования:

\(with-input-from-string (input \"1 2
      3 :foo  \")
       (let ((counted-stream (make-instance '|Считающий-входной-поток-литер|
                              :|Поток| input)))
         (loop for thing = (read counted-stream) while thing
            unless (numberp thing) do
              (error \"Non-number ~S (line ~D, column ~D)\" thing
                     (|Счётчик-строк-из| counted-stream)
                     (- (|Счётчик-колонок-из| counted-stream)
                        (length (format nil \"~S\" thing))))
            end
            do (print thing))))
                   "))
     
(defmethod stream-read-char ((|Поток| |Считающий-входной-поток-литер|))
  (with-accessors ((|Внутр-поток| |Поток-из|) (|Литеры| |Счётчик-литер-из|)
                   (|Строки| |Счётчик-строк-из|) (|Колонки| |Счётчик-колонок-из|)
                   (|Стар| |Стар-счётчик-колонок-из|)) |Поток|
    (let ((char (call-next-method)))
      (cond ((eql char :eof)
             :eof)
            ((char= char #\Newline)
             (incf |Строки|)
             (incf |Литеры|)
             (setf |Стар| |Колонки|)
             (setf |Колонки| 1)
             char)
            (t
             (incf |Литеры|)
             (incf |Колонки|)
             char)))))

(defmethod stream-unread-char ((|Поток| |Считающий-входной-поток-литер|)
                               char)
  (with-accessors ((|Внутр-поток| |Поток-из|) (chars |Счётчик-литер-из|)
                   (lines |Счётчик-строк-из|) (cols |Счётчик-колонок-из|)
                   (prev |Стар-счётчик-колонок-из|)) |Поток|
    (cond ((char= char #\Newline)
           (decf lines)
           (decf chars)
           (setf cols prev))
          (t
           (decf chars)
           (decf cols)
           char))
    (call-next-method)))
