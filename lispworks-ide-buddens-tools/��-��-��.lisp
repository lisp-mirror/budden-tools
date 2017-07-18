; -*- coding: utf-8; system :EDITOR-BUDDEN-TOOLS ;  -*- 

(named-readtables:in-readtable :buddens-readtable-a)
(in-package :editor-budden-tools)

(defgeneric goto-xy (pathname row col)
  (:documentation "Открыть в редакторе файл и перейти на строку и колонку"))

(defgeneric goto-offset (PATHNAME OFFSET &KEY KILL-BUFFER set-foreground-window subtract-no-of-newlines)
  (:documentation "Открыть в редакторе файл и перейти на строку и колонку"))


(defgeneric real-point-offset (point)
  (:documentation "Сколько букв от начала файла в этой точке"))

