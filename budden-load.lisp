;;; -*- Encoding: utf-8; -*-

(in-package :cl-user)

#+(and (not quicklisp) win32) (load "c:/lisp/quicklisp/setup.lisp")
(ql:quickload '("cl-fad" "cl-ppcre" "md5" "alexandria" "cl-utilities" "named-readtables" "swank" "split-sequence"))


#+lispworks (defun the-listener () 
  (loop :for pop :in capi-win32-lib::*top-level-windows* 
        :for identity = (slot-value pop 'win32::element) 
        :when (= 0 (search "Listener " (slot-value identity 'capi::title)))
        :do (return (slot-value identity 'capi:editor-pane))))


(pushnew :russian *features*) ; докстринги будут по-Русски 

(defvar *lisp-root* #+win32 "c:/lisp/def-symbol-readmacro/" 
        #-win32 
        (namestring 
         (merge-pathnames 
          (make-pathname :directory '(:relative "def-symbol-readmacro")) 
          (car quicklisp:*local-project-directories*)))
        "Название переменной весьма одиозное, но не можем сразу поменять, т.к. оно используется в defun-to-file. FIXME
Нормальное название этой переменной должно быть *def-symbol-readmacro-dir*, хотя она вообще не нужна, а каталог для
defun-to-file надо настраивть отдельно"
        )

(pushnew *lisp-root* asdf:*central-registry*)
(load (concatenate 'string *lisp-root* "asdf2-tools.lisp"))
(export 'asdf::load-system-by-asd-file :asdf) ; это и правда нужно? 

(asdf::! :decorate-function) ; приспособление для переопределения уже существующих функций
(asdf::! :iterate-keywords) ; версия :iterate с keyword-ами
(asdf::! :budden-tools)  ; def-merge-packages::! и разные полезные функции
(asdf::! :see-packages)  ; таблица чтения с local-nicknames,symbol-readmacros,custom token parsers
(asdf::! :russian-budden-tools) ; поддержка Русских букв (upcase, downcase, =)


; устанавливаем, что symbol-readmacros могут начинаться с Русских букв
(setf budden-tools::*def-symbol-reamacro-additional-name-starting-characters*
      (append budden-tools::*def-symbol-reamacro-additional-name-starting-characters*
              russian-budden-tools::*cyrillic-characters*))

; добавляем расширения к экспериментальной таблице чтения
(budden-tools::ENABLE-BUDDENS-READTABLE-EXTENSIONS :buddens-readtable)

; включаем чтение CamelCase 
(setf (budden-tools::readtable-case-advanced :buddens-readtable) :upcase-if-uniform)

(budden-tools::in-readtable :buddens-readtable)

(setf *print-case* :downcase)
#+lispworks6 (setf SYSTEM:*PRINT-SYMBOLS-USING-BARS* t)
