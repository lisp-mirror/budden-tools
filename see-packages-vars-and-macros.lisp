; -*- coding: windows-1251-dos; -*- 

(in-package :budden-tools)
(setf *readtable* (copy-readtable nil))

(defparameter *my-readtable* nil "Readtable for testing see-packages")
(defparameter *good-readtable* (copy-readtable nil) "Sample initial readtable for tests")

(defparameter *cached-default-readtable* (copy-readtable nil))

(defvar *keyword-package* (find-package :keyword))

(defvar *package-seen-packages* (make-weak-key-hash-table :test 'eq)
  "Additional packages to see from that package")

(defvar *my-readtable-to-good-readtable* (make-weak-key-hash-table :test 'eq)
  "Maps altered readtable with reader extensions to their unaltered counterparts")

(defvar *my-readtable-to-colon-readtable* (make-weak-key-hash-table :test 'eq)
  "Maps altered readtable to their colon-readtable"
  )

(defvar *readtable-uses-sbcl-reader-budden-tools-lispworks* (make-weak-key-hash-table :test 'eq)
  "Readtable implements reader extensions via SBCL reader")

(defvar *readtable-case-is-upcase-if-uniform* (make-weak-key-hash-table :test 'eq)
  "Если readtable - в этой таблице, то для него действуют особые правила поиска имён. При этом readtable-case нужно поставить в preserve"
  )

(defvar *record-paren-locations* nil
  "When t, records locations of all objects starting from opening paren - not implemented, but see 
  PAREN-READER-WITH-CLOSING-PAREN-NOTIFICATION where file locations are stored on stack while reading is accomplished"
  )


(defparameter *def-symbol-reamacro-additional-name-starting-characters* nil
  "By default, if first character of symbol/package (nick)name is constituent, 
\(with some exclusions) and its char-code <= 255, this token is processed by 
the library's machinery. You can list some more characters in the variable. 
E.g., national characters of your language. Do not try to include #\. 
reading would be broken.")

(defmacro with-my-readtable-0 (&body body)
  `(let1 *readtable* *my-readtable*
     ,@body))

(defmacro with-good-readtable-0 (&body body) 
  "Использовать только для тестирования!"
  `(let1 *readtable* *good-readtable* ,@body))

(defmacro with-good-readtable-2 ((&key (ensure-this-is-a-bad-one t)) &body body)
  "переданная readtable должна быть получена с помощью see-packages-on"
  (with-gensyms (good)
    `(proga
       (let ,good (gethash *readtable* *my-readtable-to-good-readtable*))
;       (print '(:good-readtable-is ,good))
       ,@(when ensure-this-is-a-bad-one
           `((assert ,good nil "with-good-readtable: ~A is not a hp-readtable" *readtable*)))
       (let *readtable* (or ,good *readtable*))
       ,@body
       )))

(defvar *print-normalize-seen-symbols* t "Print seen symbols with package
prefix even if they can be read without the prefix")

(defmacro with-xlam-package (&body body) "Заменить на with-xlam-package-2"
  `(let ((*real-package* *package*)
         (*package* *xlam-package*)) 
     (break "with-xlam-package не должен вызываться")
     (setf *last-used-real-package* *real-package*)
     ,@body))

(defmacro with-xlam-package-2 (temp-rt &body pbody)
  `(proga
     (break "with-xlam-package-2 не должен вызываться")
     (let real-rt *readtable* *real-package* *package*)
     (let *readtable* ,temp-rt)
     (let *package* *xlam-package*)
     (setf *last-used-real-package* *real-package*)
     (pllet1 (readtable-case *readtable*)
         (xlam-package-readtable-case real-rt))
     ,@pbody))


(defmacro get-non-persistent-object-locations (object)
  `(gethash ,object *nplm))


