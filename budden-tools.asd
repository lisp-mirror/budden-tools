;;; -*- Encoding: utf-8; Mode: Lisp -*-

(in-package #:asdf)

(pushnew :building-see-packages *features*)

(defsystem :budden-tools  
  :serial t
  :description "Some utilities by Budden, mostly public domain"
  :depends-on 
  (:alexandria :cl-fad :split-sequence :cl-utilities :named-readtables
   :cl-ppcre		
   :iterate-keywords :swank :decorate-function :closer-mop :trivial-gray-streams)
  :components ( 
    ;(:file "hierarchical-packages"
    ; :doc "port of Allergo's hierarchical packages to some more implementations by Tim Bradshaw with some changes by Budden"
    ; )
   (:file "def-merge-packages"
    :description "defpackage with some new clauses which are useful in conjunction with this library. See def-merge-packages::!"
    )
   (:file "defpackage-l2"
    :description "Упрощённая версия def-merge-package для l2/s2 - осталось только описание, см. внутри файла")
   (:file "package") ; might need
   #+sbcl (:file "sbcl-policy" :description "Возвращает сведения о политике компилятора")
   (:file "package-proga-implementation")
   (:file "def-toplevel-progn")
   (:file "let1" :description "Устарел. Используй perga")
   (:file "def-trivial-test"
    :description "yet another test suite")
   (:file "budden-tools"
    :description "General purpose utilities")
   (:file "tests/test-budden-tools")
   (:file "with-conc-name" :description "Позволяет сократить запись обращений к полям структуры за счёт кодировки обращений в виде префикса в имени символа. Считается устаревшим, но надёжнее, чем ^")
   #+lispworks6.1 (:file "lw-macro-friendly-stepper" :description "put breakpoints in macros without macroexpansion (for stepper)")
   #+lispworks6.1 (:file "lw-macro-friendly-dbg-vars-and-macros" :description "find frame source correction for debugger")
   #+lispworks6.1 (:file "lw-macro-friendly-dbg" :description "find frame source correction for debugger")
   #+lispworks6.1 (:file "res-fun-with-keyargs-in-a-debugger" :description ":restart-with-keywords-risky debugger command - restart frames with &key args")
   (:file "proga" :description "DEPRECATED")
   (:file "proga-test" :description "DEPRECATED") 

   ; inlining perga system to simplify build
   (:file "perga-implementation-package")
   (:file "perga-vars-and-macros")
   (:file "perga-aux-funs")
   (:file "perga")

   (:file "iterate-extensions" :description "some iterate drivers")
   #+sbcl (:file "let-around-compile-file-and-load" :description "Связать заданные переменные вокруг compile-file или load")
               ; отключено для ускорения сборки образа, т.к. всё время его меняем.
   #+sbcl (:file "запретить-неявное-сужение-типа" :description "При определённых настройках компилятора запретить неявное сужение типа") 
   (:file "!закодировать-строку-в-имя-файла")
   (:file "defun-to-file" :description "When named function is created, it can be saved to a file in order to find-source to work")
   (:file "variable-type" :description "Compilation environment related stuff"
   )
   (:file "full-outer-join" :description "Similar to full outer join in sql")
   (:file "pass-by-ref" :description "Pass place to a function by reference"
   )
   (:file "print-hash-table" :description "Print hashtable readably and other hash table utils"
    )
   (:file "потоки-зпт-считающие-буквы-строки-и-колонки")
   ))
