; -*- coding: utf-8;   -*- 
;;; Правильнее было бы назвать ide-budden-tools
;;; Работа со средой лиспворкса и немножко - с SBCL

(asdf:defsystem :editor-budden-tools
  :depends-on (:cl-utilities :budden-tools :buddens-readtable)
  :serial t
  :components (
               (:file "editor-budden-tools-package")
               (:file "ма-пе-ти" :description "содержит родовые ф-ии")
               #+lispworks (:file "editor-budden-tools") ; навигация по коду
               #+sbcl (:file "editor-budden-tools-sbcl")
               (:file "ppcre-shortcuts") ; сокращения, удобные для работы с ppcre
               #+lispworks (:file "syntax-coloring-mpf") ; shift-f12 - раскрасить fbody
               #+lispworks (:file "message-area") ; подмена строки состояния в листенере и подсказки интерпретатора
               #+lispworks (:file "error-browser-for-paren-reader") ; помощник для поиска ошибок чтения. Пример простого расширения отладчика
               #+lispworks (:file "listener-confirm-destroy-function")
               #+lispworks (:file "layout-debugger")
               #+lispworks (:file "fasl-files-in-temp-dir")
               #+lispworks (:file "find-readtable-for-point")
               )
  ;:perform (load-op :after (op c)
  ;                  (import 'named-readtables:in-readtable :cl-user)
  ;                  )
  )
