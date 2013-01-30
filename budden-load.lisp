(load "c:/lisp/quicklisp/setup.lisp")
(ql:quickload '("cl-fad" "cl-ppcre" "md5" "alexandria" "cl-utilities" "named-readtables" "swank" "split-sequence"))


(defun the-listener () 
  (loop :for pop :in capi-win32-lib::*top-level-windows* 
        :for identity = (slot-value pop 'win32::element) 
        :when (= 0 (search "Listener " (slot-value identity 'capi::title)))
        :do (return (slot-value identity 'capi:editor-pane))))


(EVAL-WHEN (:EXECUTE :LOAD-TOPLEVEL :COMPILE-TOPLEVEL) 
  (SETQ *READTABLE* (COPY-READTABLE *READTABLE*))
  (SETF (READTABLE-CASE *READTABLE*) :UPCASE))

(pushnew :budden *features*)
(pushnew :russian *features*)
(pushnew :forbidden-symbol-names *features*) 

(load "c:/lisp/def-symbol-readmacro/config.lisp")
(defun at-lisp-root (path) "Путь, смещённый относительно корня конфигурации"
  (concatenate 'string *lisp-root* path))

(defmacro portably-without-package-locks (&body body)
`(#+sbcl sb-ext:without-package-locks
#+allegro excl::without-package-locks
#+cmu ext:without-package-locks
#+lispworks let 
#+lispworks 
((lw:*handle-warn-on-redefinition* :warn)
 (*packages-for-warn-on-redefinition* nil))
#+clisp ext:without-package-lock #+clisp ()
#-(or allegro lispworks sbcl clisp cmu) 
progn
,@body))

;(defun asdf::! (&rest args) (apply 'asdf:load-system args))
(push "c:/lisp/def-symbol-readmacro/" asdf:*central-registry*)

(load "c:/lisp/def-symbol-readmacro/asdf2-tools.lisp")
(export 'asdf::load-system-by-asd-file :asdf)

(asdf::! :decorate-function)
(asdf::! :iterate-keywords)

(defpackage :iterk
  (:nicknames :iterate-keywords-tiny-export)
  (:use #:cl #:iterate-keywords)
  (:export #:iter #:dsetq))

(asdf::! :see-packages)
(asdf::! :budden-tools)
(asdf::! :russian-budden-tools)

;(load "load-js.lisp")
;(break)
;(asdf::! :anaphora)

(setf budden-tools::*def-symbol-reamacro-additional-name-starting-characters*
      (append budden-tools::*def-symbol-reamacro-additional-name-starting-characters*
              russian-budden-tools::*cyrillic-characters*))

(budden-tools::ENABLE-BUDDENS-READTABLE-EXTENSIONS :buddens-readtable)
(budden-tools::ENABLE-BUDDENS-READTABLE-EXTENSIONS :buddens-readtable-a)

;(setf (budden-tools::readtable-case-advanced :buddens-readtable) :upcase-if-uniform)
;(budden-tools:see-packages-on :buddens-readtable-a)
(setf (budden-tools::readtable-case-advanced :buddens-readtable-a) :upcase-if-uniform)
(budden-tools::in-readtable :buddens-readtable-a)
(setf *print-case* :downcase)
#+lispworks6 (setf SYSTEM:*PRINT-SYMBOLS-USING-BARS* t)
