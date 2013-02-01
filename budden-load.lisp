;;; -*- Encoding: utf-8; -*-

(in-package :cl-user)

#+(and (not quicklisp) win32) (load "c:/lisp/quicklisp/setup.lisp")
(ql:quickload '("cl-fad" "cl-ppcre" "md5" "alexandria" "cl-utilities" "named-readtables" "swank" "split-sequence"))


#+lispworks (defun the-listener () 
  (loop :for pop :in capi-win32-lib::*top-level-windows* 
        :for identity = (slot-value pop 'win32::element) 
        :when (= 0 (search "Listener " (slot-value identity 'capi::title)))
        :do (return (slot-value identity 'capi:editor-pane))))


(EVAL-WHEN (:EXECUTE :LOAD-TOPLEVEL :COMPILE-TOPLEVEL) 
  (SETQ *READTABLE* (COPY-READTABLE *READTABLE*))
  (SETF (READTABLE-CASE *READTABLE*) :UPCASE))

(pushnew :russian *features*)
(pushnew :forbidden-symbol-names *features*) 

(defvar *lisp-root* #+win32 "c:/lisp/def-symbol-readmacro/" 
        #-win32 
        (namestring 
         (merge-pathnames 
          (make-pathname :directory '(:relative "def-symbol-readmacro")) 
          (car quicklisp:*local-project-directories*))))
(defun at-lisp-root (path)
  #+russian "Путь, смещённый относительно корня конфигурации"
  #-russian "Path merged with *lisp-root*"
  (declare (type string path))
  (concatenate 'string *lisp-root* path))

(pushnew *lisp-root* asdf:*central-registry*)

(load (at-lisp-root "asdf2-tools.lisp"))
(export 'asdf::load-system-by-asd-file :asdf) ; это и правда нужно? 

(asdf::! :decorate-function)
(asdf::! :iterate-keywords)
(asdf::! :budden-tools)
(asdf::! :see-packages)
(asdf::! :russian-budden-tools)


; устанавливаем, что symbol-readmacros могут начинаться с кириллических букв
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
