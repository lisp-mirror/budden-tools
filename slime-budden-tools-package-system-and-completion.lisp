;;; -*- Encoding: utf-8; system :see-packages -*-
;;; Some SWANK symbols are decorated here. This code may be sbcl-specific
 
(in-package :budden-tools)
(in-readtable nil)

(defun decorated-swank--tokenize-symbol-thoroughly (fn string)
  "Use sbcl-reader-budden-tools-lispworks machinery if appropriate"
  (cond
    ((packages-seen-p *readtable*)
     (perga-implementation:perga
       (let sbcl-reader-budden-tools-lispworks::*return-package-and-symbol-name-from-read* t)
       (let ps (ignore-errors (read-from-string string)))
       (cond
         ((null ps)
          nil)
         (t
          ;(budden-tools:show-expr (sbcl-reader-budden-tools-lispworks:potential-symbol-qualified ps))
          (values
           (sbcl-reader-budden-tools-lispworks:potential-symbol-casified-name ps)
           (package-name (sbcl-reader-budden-tools-lispworks::potential-symbol-package ps))
           (/= 1 (sbcl-reader-budden-tools-lispworks:potential-symbol-qualified ps)))
          ))))
    (t
     (funcall fn string))))

(decorate-function:decorate-function 'swank::tokenize-symbol-thoroughly #'decorated-swank--tokenize-symbol-thoroughly)


(defun decorated-swank--all-completions (fn prefix package)
  (perga-implementation:perga
    (let completions nil)
    (flet set-completions (x) (setf completions x))
  (cond
    ((packages-seen-p *readtable*)
     (break "test me")
     (do-complete-symbol-with-budden-tools prefix package 'error #'set-completions))
    (t
     (funcall fn prefix package)))))


#|           (strings (loop for sym in syms
                          for str = (unparse-symbol sym)
                          when (prefix-match-p name str) ; remove |Foo|
                          collect str)))
      (swank::format-completion-set strings intern pname)))) |#
