;; -*- encoding : utf-8; coding : utf-8; system :see-packages; -*- 

(named-readtables::in-readtable nil)

#|
Что у нас плохого?
1. Есть ещё как минимум одна структура - BUDDEN-TOOLS:ROW-COL-OFFSET , к-рую надо бы привести к этому виду. КРоме того, SBCL как-то должен уметь считать строки.
2. Есть несколько способов посчитать смещение в файле, и, в зависимости от контекста, некоторые из них недоступны. 
- row,col - требует считать в потоке символы #\newline - это просто не всегда возможно. Например, это невозможно, если поток не наш. Можно было бы отдельно открывать файл, но для этого нужно иметь ссылку на поток. Кроме того, если это поток чтения с терминала, то данная информация в принципе будет недоступна.
- file-position - может запутаться из-за кодировки и из-за расхождения в символах конца строки на разных ОС
- buffer-position - здесь буквы считаются за 1, включая букву конца строки. Это неплохой способ, не считая того, что он не всегда может быть переведён в два остальных. 

Ф-ии, которые могут преобразовать одно в другое:
budden-tools::input-stream-position-in-chars - считает буквы
budden-tools::fix-offset-2

3. Иногда нам неинтересен номер строки, а интересен "объект" строка - это в случае, если мы читаем не файл, а буфер редактора. 

4. Иногда важна топология (считать смещения) - тогда либо нужны числа, либо алгебра
операций над парами. 

|#


(def-merge-packages::! :LEXEM-POS
  (:always t)
  (:documentation "Положение лексемы")
  (:use :cl  
        )
  (:export "
   LEXEM-POS:CHAR-FRC
   LEXEM-POS:MAKE-CHAR-FRC
   LEXEM-POS:CHAR-FRC-P
   LEXEM-POS:CHAR-FRC-FILE
   LEXEM-POS:CHAR-FRC-ROW
   LEXEM-POS:CHAR-FRC-COL

   LEXEM-POS:FILE-AND-FILE-POSITION
   LEXEM-POS:MAKE-FILE-AND-FILE-POSITION
   LEXEM-POS:FILE-AND-FILE-POSITION-P
   LEXEM-POS:FILE-AND-FILE-POSITION-FILE
   LEXEM-POS:FILE-AND-FILE-POSITION-POSITION

   LEXEM-POS:FILE-OFFSET-CHARS-NEWLINE-IS-1
   LEXEM-POS:MAKE-FILE-OFFSET-CHARS-NEWLINE-IS-1
   LEXEM-POS:FILE-OFFSET-CHARS-NEWLINE-IS-1-P
   LEXEM-POS:FILE-OFFSET-CHARS-NEWLINE-IS-1-FILE
   LEXEM-POS:FILE-OFFSET-CHARS-NEWLINE-IS-1-OFFSET

   ; обобщает различные числовые смещения в файле
   LEXEM-POS:NUMERIC-FILE-POSITION 
            
   LEXEM-POS:LEXEM-POS
   LEXEM-POS:MAKE-LEXEM-POS
   LEXEM-POS:LEXEM-POS-P
   LEXEM-POS:LEXEM-POS-FILE
   LEXEM-POS:LEXEM-POS-START
   LEXEM-POS:LEXEM-POS-END
   "))

(in-package :lexem-pos)

(defstruct CHAR-FRC "char-file-row-col"
  file ; это можеть быть имя файла или буфер редактора (oi::buffer)
  row  ; это - либо номер строки (от 0), либо это может быть строка редактора (oi::line)
  (col 0 :type integer) 
  )

(defmethod cl:make-load-form ((self CHAR-FRC) &optional env)
  (make-load-form-saving-slots self :environment env))

(defstruct FILE-AND-FILE-POSITION
  file ; это можеть быть имя файла или буфер редактора (oi::buffer), может отсутствовать, если понятно из контекста
  (position -1 :type integer) ; смещение в файле, получаемое из потока с помощью file-position
  )

(defmethod cl:make-load-form ((self FILE-AND-FILE-POSITION) &optional env)
  (make-load-form-saving-slots self :environment env))


(defstruct file-offset-chars-newline-is-1
  file
  (offset -1 :type integer) ; смещение в буквах, #\newline считается за одну букву
  )

(defmethod cl:make-load-form ((self FILE-OFFSET-CHARS-NEWLINE-IS-1) &optional env)
  (make-load-form-saving-slots self :environment env))


(deftype numeric-file-position () '(or FILE-AND-FILE-POSITION file-offset-chars-newline-is-1))

;; неудачно - lexem-pos получилась мутабельной (см. lexer-yar::prepare-empty-lexem) .
;; Если переделать в лексеме так, чтобы было два поля "начало" и "конец", 
;; можно будет избавиться от этой записи вовсе. 
(defstruct lexem-pos
  file ; это можеть быть имя файла или буфер редактора (oi::buffer)
  (start nil :type (or null CHAR-FRC numeric-file-position))
  (end nil :type (or null CHAR-FRC numeric-file-position))
  ) 

(defmethod cl:make-load-form ((self lexem-pos) &optional env)
  (make-load-form-saving-slots self :environment env))
