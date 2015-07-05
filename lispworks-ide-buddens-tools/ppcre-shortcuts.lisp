;; -*- Encoding : utf-8 ; -*- ;; 

;; some ppcre parse-tree synonyms
(asdf::of-system :editor-budden-tools)
(in-package :ppcre-shortcuts)


(define-parse-tree-synonym constituent-char
                           (:inverted-char-class :whitespace-char-class #\( #\) #\{ #\} #\[ #\] #\# #\, #\' #\" #\` #\; #\| ))

; deprecated
(define-parse-tree-synonym lisp-identifier
                           (:greedy-repetition 1 nil constituent-char))

; безопасный идентификатор лиспа - внутренний или квалифицированный внешний символ. 
; FIXME - когда-нибудь запретить неквалифицированные символы
(define-parse-tree-synonym safe-lisp-identifier
                           (:sequence 
                            (:regex "[a-zA-Z_абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНУПРСТУФХЦЧШЩЪЫЬЭЮЯ\\-0-9]*")
                            (:greedy-repetition 0 1
                             (:sequence #\:
                              (:regex "[a-zA-Z_абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНУПРСТУФХЦЧШЩЪЫЬЭЮЯ\\-0-9]*")))))

(define-parse-tree-synonym filename-with-path
                           (:sequence 
                            (:regex "[a-zA-Z_абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНУПРСТУФХЦЧШЩЪЫЬЭЮЯ\\-0-9\./\:]*")
                            (:greedy-repetition 0 1
                             (:sequence #\:
                              (:regex "[a-zA-Z_абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНУПРСТУФХЦЧШЩЪЫЬЭЮЯ\\-0-9\./\:]*")))))



(define-parse-tree-synonym @identifier (:sequence (:flags :case-insensitive-p) (:sequence :word-boundary (:regex "[a-zA-Z_][a-zA-Z_0-9]*") :word-boundary)))
(define-parse-tree-synonym @non-negative-decimal (:sequence :word-boundary (:regex "[0-9]+")))
(define-parse-tree-synonym @reg-identifier (:register @identifier))  ; (:sequence :non-word-char-class (:register identifier) :non-word-char-class))
(define-parse-tree-synonym @whitespace (:greedy-repetition 0 nil :whitespace-char-class))
(define-parse-tree-synonym @maybe-everything (:non-greedy-repetition 0 nil :everything))
(define-parse-tree-synonym @sql-short-comment (:sequence @whitespace  "--" (:register @maybe-everything) :end-anchor))
(define-parse-tree-synonym @reg-whitespace (:register @kwhitespace))


(def-trivial-test::! #:constituent-char.1
                     (cl-ppcre::scan 'ppcre-shortcuts::constituent-char ",()[]\"`;|m") 9)

(def-trivial-test::! qualified-lisp-identifier.1
                     (cl-ppcre::scan 'PPCRE-SHORTCUTS:safe-lisp-identifier "foo:bar ")
                     0)

(def-trivial-test::! qualified-lisp-identifier.2
                     (cl-ppcre::scan 'PPCRE-SHORTCUTS:safe-lisp-identifier "just-a-symbol ")
                     0)


(def-trivial-test::! #:lisp-identifier.1
                     (iterate-keywords:iter
                       (do-register-groups (it) ('(:register ppcre-shortcuts::lisp-identifier) "(there is `[,a 'foo-bar])")
                         (:collect it))
                       (:finish))
                     '("there" "is" "a" "foo-bar") 
                     :test 'equalp)
