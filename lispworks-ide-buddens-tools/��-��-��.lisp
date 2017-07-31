; -*- coding: utf-8; system :EDITOR-BUDDEN-TOOLS ;  -*- 

(named-readtables:in-readtable :buddens-readtable-a)
(in-package :editor-budden-tools)

(defgeneric goto-xy (pathname row col)
  (:documentation "Открыть в редакторе файл и перейти на строку (считая с 1) и колонку (считая с 1) - нужно иметь в виду, что в text widget в tcl колонки считаются с 0, а end - это конец строки. См. также goto-offset,
                   КАРТЫ-ИСХОДНИКОВ-ТЕЛО::NUMERIC-FILE-POSITION-TO-BUFFER-OFFSET
                   ODUVANCHIK-INTERFACE:COUNT-LINES
                   ODUVANCHIK-INTERNALS::MARK-ROW-AND-COL
                   GOTO-OFFSET
                   "))

(defgeneric goto-offset (PATHNAME OFFSET &KEY KILL-BUFFER set-foreground-window subtract-no-of-newlines)
  (:documentation "Открыть в редакторе файл и перейти на позицию, считая с 1. См. также GOTO-XY"))


(defgeneric real-point-offset-0-based (point)
  (:documentation "Сколько букв от начала файла в этой точке, считая от 0 (sic!)"))

