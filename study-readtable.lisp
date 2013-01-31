;;; -*- Encoding: utf-8; -*-
 
(in-package :budden-tools)


(defun test-does-not-terminate-token (c) 
  "Экспериментально выясняем, не завершает ли cимвол чтение токена?"
  (ignore-errors
    (return-from test-does-not-terminate-token
      (let* ((s (format nil "the-~A-symbol" c))
             (ss (str+ ":" s))
             (sym (intern s :keyword)))
         (eq sym 
             (dynamic-let1 (readtable-case *readtable*) :preserve
               (read-from-string ss))))))
  nil)

(defun test-whitespace[2]p (c)
  "Экспериментально выясняем, что это - whitespace 2. Не знаю, правильно ли?"
  (ignore-errors 
    (return-from test-whitespace[2]p 
      (equalp '(a b)
              (read-from-string (format nil "(a~Ab)" (the character c))))))
  nil)

(defun test-multiple-escape-p (c)
  "Экспериментально выясняем, что это - multiple escape"
  (ignore-errors 
    (let1 good-symbol 
        (with-good-readtable-0
          (dynamic-let1 (readtable-case *readtable*) :preserve
                        (read-from-string "qQ")))
      (return-from test-multiple-escape-p 
        (eq good-symbol 
            (dynamic-let1 (readtable-case *readtable*) :upcase
              (read-from-string (format nil "~AqQ~A" c c)))))))
  nil)


(defun test-single-escape-p (c)
  "Экспериментально выясняем, что это - single escape"
  (ignore-errors 
    (let1 good-symbol 
        (with-good-readtable-0
          (read-from-string "\\'"))
      (return-from test-single-escape-p 
        (eq good-symbol 
            (read-from-string (format nil "~A'" c))))))
  nil)


(defun test-macro-char-p (c)
  "Проверяем, что тут есть macro-char. Возвращаем values из macro-char. 
В Аллегро делаем исключение для read-token"
  #+allegro (unless 
                (eql (get-macro-character c) #'excl::read-token)
              (get-macro-character c))
  #-allegro (get-macro-character c))

(defparameter *char-table* (make-array 256 :initial-element nil))

(defun fill-char-table ()
  (with-good-readtable-0
    (iter:iter ; какие символы у нас составляют токен? 
      (:for i :from 0 to 255) 
      (:for c := (code-char i))
      (setf (elt *char-table* i)
            (cond
             ((test-macro-char-p c)
                (multiple-value-list (test-macro-char-p c)))
             #-pascal-reader ((eql c #\.) :dot)
             ((eql c #\:) :colon)           ; здесь тоже неплохо написать тест
             ; ((eql c #\() :open-brace)    ; для него есть readmacro 
             ((test-does-not-terminate-token c) :does-not-terminate-token)
             ((test-whitespace[2]p c) :whitespace[2])
             ((test-multiple-escape-p c) :multiple-escape)
             ((test-single-escape-p c) :single-escape)
             )
            ))))

(defun whitespace[2]p (c) (eq (elt *char-table* (char-code c)) :whitespace[2]))

