;;; -*- Encoding: utf-8; system :see-packages -*-

#|
Here document like in shells. In readtable which supports
symbol-readmacros, the following will work:

> buddens-here-document:s[ abcd] 
"abcd"
> buddens-here-document:here-document ok Krym is Russian ok 
"Krym is Russian"
> buddens-here-document:here-document OK Krym is RussianOK
"Krym is Russian"
> buddens-here-document:here-documentc :Krym is Russian:
"Krym is Russian"
> buddens-here-document:here-documentc <Krym is Russian>
"Krym is Russian"

And so on. 

Beware strings in non-conventional syntax can cause a malfunction 
of your IDE. 

|#


(def-merge-packages::! :buddens-here-document
                       (:documentation "Here documents - an alternative string syntax. See comment in the beginning of source file")
                       (:use :cl :budden-tools :iterate-keywords)
                       (:always t)
                       (:export "HERE-DOCUMENT" "HERE-DOCUMENTC"
                       ;;; deprecated.
                       ;;; See buddens-readtable:enable-triple-quote-reader
                                "S[" "S{" "S<<<"
                        ))

(in-package :buddens-here-document)

(defun text-up-to-terminator-lexer (stream terminator)
  "Считывает текст до вхождения подстроки terminator и возвращает, не включая эту строку. 
Подстрока проверяется с учётом регистра"
  (let1 terminator-length (length (the* string terminator))
    (assert (> terminator-length 0) () "Terminator should be non-empty, of a fixnum length")
    (iter 
      (declare (character c) (fixnum index) (string terminator))
      (:with index = 0)
      (:for look-for = (aref terminator index))
      (:for c = 
            (handler-bind 
                ((end-of-file #'(lambda (c) (declare (ignore c)) (error "terminator ~S not found in ~S till eof" terminator stream))))
              (read-char stream)))
      (flet ((coll (x) (:collect x into collected)))
        (cond
         ((eql c look-for)
          (incf index)
          (when (= index terminator-length)
            (return-from text-up-to-terminator-lexer (coerce collected 'string)))
          )
         ((> index 0)
          (iter (:for i from 0 to (1- index))
            (coll (aref terminator i)))
          (coll c)
          (setf index 0))
         (t 
          (coll c))
         )))
    )
  )

#| (defstruct 
    (here-document-result 
     (:print-function 
      (lambda (x s n) 
        (declare (ignore n))
        (print-unreadable-object (x s :type nil)
          (format s "~A ~A~A" 
                  (here-document-result-terminator x)
                  (here-document-result-string x)
                  (here-document-result-terminator x))))))
  terminator string) |#

(defun here-document-readmacro-reader (stream symbol)
  (declare (ignore symbol))
  (budden-tools:it-is-a-car-symbol-readmacro
   (let1 terminator (string (read-preserving-whitespace stream))
     (when (characterp terminator) 
       (setf terminator (make-string 1 :initial-element terminator)))
     (read-char stream)
     (text-up-to-terminator-lexer stream terminator)
     #+nil (make-here-document-result :terminator terminator 
                                      :string
                                      (text-up-to-terminator-lexer stream terminator)))))


(defun here-documentc-inner-readmacro-reader (stream skip-char terminator)
  (when skip-char (read-char stream))
  (let1 terminator (or terminator (make-string 1 :initial-element (read-char stream)))
    (setf terminator (or (CASE1::NATURAL-CLOSE-BOUNDARY terminator) 
                         terminator)
          )
    (text-up-to-terminator-lexer stream terminator)))

(defun here-documentc-readmacro-reader (stream symbol)
  (declare (ignore symbol))
  (budden-tools:it-is-a-car-symbol-readmacro
   (here-documentc-inner-readmacro-reader stream t nil)))

(def-symbol-readmacro |HERE-DOCUMENT| #'here-document-readmacro-reader)
(def-symbol-readmacro |HERE-DOCUMENTC| #'here-documentc-readmacro-reader)

(def-symbol-readmacro |S[| #'(lambda (stream symbol) (declare (ignore symbol)) (here-documentc-inner-readmacro-reader stream t "[")))

(def-symbol-readmacro |S{| #'(lambda (stream symbol) (declare (ignore symbol)) (here-documentc-inner-readmacro-reader stream t "{")))

(def-symbol-readmacro |S<<<| #'(lambda (stream symbol) (declare (ignore symbol)) (here-documentc-inner-readmacro-reader stream t ">>>")))

