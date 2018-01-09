;;; -*- Encoding: utf-8; system :buddens-readtable; -*-

(named-readtables::in-readtable nil)

(def-merge-packages::! :buddens-readtable-test
                      (:always t)
                      (:use :cl :buddens-readtable))

(in-package :buddens-readtable-test)

;;; тесты для enable-triple-quote-reader
(defun read-t-q-s (s)
  "Заменяет плюсики на кавычки и читает строку с включёнными тройными кавычками"
  (let ((*readtable* (copy-readtable nil)))
    (enable-triple-quote-reader *readtable*)
    (read-from-string (substitute #\" #\+ s))))

(def-trivial-test::! test-triple-quote-reader.1
                  "sd"
  (read-t-q-s "+++sd+++"))

(def-trivial-test::! test-triple-quote-reader.2
                  "s\"\"\"d"
  (read-t-q-s "+++s++++++d+++"))

(def-trivial-test::! test-triple-quote-reader.3
                  "s\"\"d\"f"
  (read-t-q-s "+++s++d+f+++"))

(def-trivial-test::! test-triple-quote-reader.4
                     "s\""
  (ignore-errors (read-t-q-s "+++s++++")))

(def-trivial-test::! test-triple-quote-reader.5
                     "s\"\""
  (ignore-errors (read-t-q-s "+++s+++++")))

;;; Проверим, что ридер не читает лишнего. 
(defun read-t-q-s-2 (s)
  "Заменяет плюсики на кавычки и два раза читает строку с включёнными тройными кавычками, возвращая второе значение"
  (let ((*readtable* (copy-readtable nil))
        (ss (substitute #\" #\+ s)))
    (enable-triple-quote-reader *readtable*)
    (with-input-from-string (in ss)
      (read in)
      (read in))))

(def-trivial-test::! test-triple-quote-reader.6
   1
  (read-t-q-s-2 "+++s+++1"))

(def-trivial-test::! test-triple-quote-reader.7
   1
  (read-t-q-s-2 "+++s++++1"))
