;;;; -*- Mode: Lisp; indent-tabs-mode: nil; system :see-packages ; -*-

(def-merge-packages::! :oduvanchik-regexp-synonyms
                       (:always t)
  (:nicknames oduresy)
  (:use :cl :cl-ppcre)
  (:export #:canonical-case
           #:canonicalize-slave-package-name
           #:canonicalize-slave-readtable-name
           #:casify-char
           #:find-readtable-change-in-string
           #:find-package-change-in-string
           )
  )

(in-package :oduvanchik-regexp-synonyms)


(define-parse-tree-synonym char-maybe-cyr (:regex "[a-zA-Zа-яА-Я]"))

(define-parse-tree-synonym usual-lisp-identifier-char (:alternation char-maybe-cyr "-" "_" "/" "!" :digit-class))
(define-parse-tree-synonym usual-lisp-identifier-char-or-dot (:alternation usual-lisp-identifier-char "."))

(define-parse-tree-synonym optional-whitespaces (:non-greedy-repetition 0 nil :whitespace-char-class))

(define-parse-tree-synonym whitespaces (:non-greedy-repetition 1 nil :whitespace-char-class))

(define-parse-tree-synonym usual-lisp-identifier (:non-greedy-repetition 1 nil usual-lisp-identifier-char))
(define-parse-tree-synonym usual-lisp-identifier-with-dots (:non-greedy-repetition 1 nil usual-lisp-identifier-char-or-dot))

(define-parse-tree-synonym keyword-or-uninterned-symbol-prefix
                                    (:alternation
                                     "#:"
                                     ":"))

(define-parse-tree-synonym keyword-or-uninterned-symbol
                                    (:sequence
                                     keyword-or-uninterned-symbol-prefix
                                     usual-lisp-identifier
                                     (:alternation
                                     "#:"
                                     ":")))



(define-parse-tree-synonym optional-package-prefix
                                    (:greedy-repetition
                                     0 1
                                     (:sequence 
                                      usual-lisp-identifier-with-dots
                                      (:greedy-repetition
                                       1 2
                                       ":"))))

(defmacro cache-scanner (regex &rest more-args)
  ;; help compilers that don't support compiler macros
  `(load-time-value (ppcre:create-scanner ,regex ,@more-args)))


#|(register-groups-bind
 (package)
 ((ppcre:create-scanner
   '(:sequence :start-anchor
               "("
               optional-package-prefix
               "in-package"
               whitespaces
               (:sequence
                keyword-or-uninterned-symbol-prefix
                (:register usual-lisp-identifier))
               optional-whitespaces
               ")"
               optional-whitespaces
               :end-anchor)
   ; "^\\((?:[a-zA-Z]+:)?in-package \\:*([^)]*)\\)"
   )
  "(x:in-package #:swank)")
 (when package
   (print
    (cons package (oduvanchik::canonicalize-slave-package-name package)))))
|#


(defun casify-char (char)
  "Convert CHAR accoring to readtable-case."
  #-scl
  (char-upcase char)
  #+scl
  (if (eq ext:*case-mode* :upper)
      (char-upcase char)
      (char-downcase char))
  ;; fixme: need to do this on the slave side
  #+nil
  (ecase (readtable-case *readtable*)
    (:preserve char)
    (:upcase   (char-upcase char))
    (:downcase (char-downcase char))
    (:invert (if (upper-case-p char)
                 (char-downcase char)
                 (char-upcase char)))))


(defun canonical-case (string)
  #-scl
  (nstring-upcase string)
  #+scl
  (if (eq ext:*case-mode* :upper)
      (nstring-upcase string)
      (nstring-downcase string)))

(defun canonicalize-slave-package-name (str)
  (cl-ppcre:regex-replace "^SB!" (canonical-case str) "SB-"))

(defun canonicalize-slave-readtable-name (str)
  "We assume that readtable names are always in upper case"
  (assert (stringp str))
  (intern (string-upcase str) (find-package :keyword)))

(defun find-package-change-in-string (string)
  "If the line contains in-package form, return package name"
  (cl-ppcre:register-groups-bind
   (package1 package2)
   ((cache-scanner
     '(:sequence
       :start-anchor
       "("
       optional-package-prefix
       "in-package"
       whitespaces
       (:alternation
        (:sequence
         #\"
         (:register usual-lisp-identifier-with-dots)
         #\")
        (:sequence
         (:greedy-repetition
          0 1
          keyword-or-uninterned-symbol-prefix)
         (:register usual-lisp-identifier-with-dots)))
       optional-whitespaces
       ")"
       ;oduresy::optional-whitespaces
       ;:end-anchor
       )
     :case-insensitive-mode t
     )
    string)
   (let ((p (or package1 package2)))
     (when p
       (canonicalize-slave-package-name p)))))

(def-trivial-test::! test-find-package-change-in-string-1
    (find-package-change-in-string "(in-package :some-strange-package)")
    "SOME-STRANGE-PACKAGE")

(defun find-readtable-change-in-string (string)
  "If the string contains in-readtable form, return (values t readtable-name), otherwise returns (values nil nil). Readtable name must be a keyword or nil"
  (cl-ppcre:register-groups-bind
   (just-nil readtable)
   ((cache-scanner
     '(:sequence
       :start-anchor
       "("
       optional-package-prefix
       "in-readtable"
       whitespaces
       (:alternation
        (:register "nil") 
        (:sequence
         ":"
         (:register 
          usual-lisp-identifier-with-dots)))
       optional-whitespaces
       ")"
       optional-whitespaces
       :end-anchor)
     :case-insensitive-mode t
     )
    string)
   (values
    (when (or readtable just-nil) t)
    (if just-nil
        nil
        (canonicalize-slave-readtable-name readtable)))
   ))

(def-trivial-test::! test-find-readtable-change-in-string-1
    (multiple-value-list
     (find-readtable-change-in-string "(in-readtable :some-strange-readtable)"))
    '(t :SOME-STRANGE-READTABLE))
