; -*- Encoding: utf-8;   -*- 
;;; Правильнее было бы назвать ide-budden-tools
;;; Работа со средой лиспворкса

(asdf:defsystem :editor-budden-tools
  :depends-on (:cl-utilities :budden-tools :buddens-readtable)
  :serial t
  :components (
               (:file "editor-budden-tools-package")
               (:file "editor-budden-tools") ; навигация по коду
               (:file "ppcre-shortcuts") ; сокращения, удобные для работы с ppcre
               (:file "syntax-coloring-mpf") ; shift-f12 - раскрасить fbody
               (:file "message-area") ; подмена строки состояния в листенере и подсказки интерпретатора
               (:file "error-browser-for-paren-reader") ; помощник для поиска ошибок чтения. Пример простого расширения отладчика
               (:file "listener-confirm-destroy-function")
               (:file "layout-debugger")
               (:file "fasl-files-in-temp-dir")
               (:file "find-readtable-for-point")
               )
  :perform (load-op :after (op c)
                    (import 'named-readtables:in-readtable :cl-user)
                    ))
