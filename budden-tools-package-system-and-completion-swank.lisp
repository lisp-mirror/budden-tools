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


(defun decorated-swank--completion-output-case-converter (fn input &optional with-escaping-p)
  "Return a function to convert strings for the completion output.
INPUT is used to guess the preferred case."
  (cond
    ((and (packages-seen-p *readtable*)
          (eq (readtable-case-advanced *readtable*) :upcase-if-uniform))
     (cond
       ((or with-escaping-p
            (and (plusp (length input))
                 (eq (all-ascii-chars-in-same-case-p input) :uppercase)))
        #'identity)
       (t #'string-downcase-ascii)))
    (t
     (funcall fn input with-escaping-p)
     )))

(decorate-function:decorate-function 'swank::completion-output-case-converter
                                     #'decorated-swank--completion-output-case-converter)

(defun decorated-swank--symbol-completion-set (fn name package-name package internal-p matchp)
  (perga-implementation:perga
    (let completions nil)
    (flet set-completions (x) (setf completions x) "")
    (cond
      ((packages-seen-p *readtable*)
       (let single-completion
         (do-complete-symbol-with-budden-tools
           name package 'error #'set-completions :yes-or-no-p-fn (constantly nil)))
       (show-expr completions)
       (cond
         (single-completion
          (list single-completion))
         (t completions)))
      (t
       (funcall fn name package-name package internal-p matchp)))))


(decorate-function:decorate-function 'swank::symbol-completion-set #'decorated-swank--symbol-completion-set)



#|(defun decorated-swank--all-completions (fn prefix package)
  (perga-implementation:perga
    (let completions nil)
    (flet set-completions (x) (setf completions x))
  (cond
    ((packages-seen-p *readtable*)
     (break "test me")
     (do-complete-symbol-with-budden-tools prefix package 'error #'set-completions))
    (t
     (funcall fn prefix package)))))|#


#|           (strings (loop for sym in syms
                          for str = (unparse-symbol sym)
                          when (prefix-match-p name str) ; remove |Foo|
                          collect str)))
      (swank::format-completion-set strings intern pname)))) |#
