; -*- coding: utf-8;   -*- 
;;; Правильнее было бы назвать ide-budden-tools
;;; Историческое название надо поменять. Работа с редактором и сокращения для ppcre

(asdf:defsystem :editor-budden-tools
  :depends-on (:cl-utilities :budden-tools :buddens-readtable)
  :serial t
  :components (
               (:file "editor-budden-tools-package")
               (:file "ма-пе-ти" :description "содержит родовые ф-ии")
               (:file "editor-budden-tools-sbcl")
               (:file "ppcre-shortcuts") ; сокращения, удобные для работы с ppcre
               ))
