;;; -*- Encoding: utf-8; Mode: Lisp -*-
;;; Поставьте в первую строку вашего файла
;;; (defal:! "-*- mode : lisp; другие ваши переменные -*-")
;;; вместо обычного ;; -*- mode : lisp ...  -*-
;;; Тогда информация из attrubite line будет доступна не только редактору, 
;;; но и компилятору лиспа. 
;;; Сам макрос пока ничего не делает. 
;;; Данный файл не входит ни в какую систему, поскольку может возникнуть желание
;;; загрузить его раньше системы :budden-tools, при запуске IDE.
;;; FIXME взять из редактора код разбора attribute line
(defpackage :defal (:use :cl)
  (:export "!")
  )

(in-package :defal)

(defmacro ! (&rest attributes)
  (declare (ignore attributes)))