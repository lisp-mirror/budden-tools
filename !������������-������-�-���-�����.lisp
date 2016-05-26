;;; -*- system :budden-tools; -*-

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz. Все права зарезервированы
;;; Copyright (c) 2016, Денис Будяк. Все права зарезервированы

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :budden-tools)

(defun |Закодировать-строку-в-имя-файла| (string)
  "Похоже на URL-encode, но :
    Русские буквы оставляет на месте.
    Восклицательный знак, звёздочку, точку, запятую и одинарную кавычку кодирует кодом
    Если заглавных букв меньше половины от всех букв, то перед заглавными буквами ставит восклицательный знак. 
    Если заглавных букв больше половины от всех букв, то в начале имени ставит !!, а перед строчными буквами ставит запятую
    Все буквы при этом приводятся к нижнему регистру в любом случае
   "
  (with-output-to-string (s)
    (let ((число-заглавных 0)
          (число-строчных 0)
          (какие-буквы-получат-префикс :upcase))
      (loop for б across string do
        (when (alpha-char-p б)
          (when (upper-case-p б) (incf число-заглавных))
          (when (lower-case-p б) (incf число-строчных))))
      (when (> число-заглавных число-строчных)
        (setf какие-буквы-получат-префикс :downcase)
        (write-string "!!" s))
      (loop for б across string
        do (cond ((or (char<= #\0 б #\9)
                      (find б "$-_()" :test #'char=))
                  (write-char б s))
                 ((or 
                   (char<= #\A б #\Z)
                   (char<= #\А б #\Я))
                  (when (eq какие-буквы-получат-префикс :upcase)
                    (write-char #\! s))
                  (write-char (char-downcase б) s))
                 ((or 
                   (char<= #\a б #\z)
                   (char<= #\а б #\я))
                  (when (eq какие-буквы-получат-префикс :downcase)
                    (write-char #\, s))
                  (write-char б s))
                 (t (format s "%~2,'0x" (char-code б))))))))

(def-trivial-test::! тест--char-downcase--понижаем-Я--ожидаем-я
  #\я
  (char-downcase #\Я))

(def-trivial-test::! тест--Закодировать-строку-в-имя-файла--Закодировать-строку-в-имя-файла
  "!закодировать-строку-в-имя-файла"
  (|Закодировать-строку-в-имя-файла| "Закодировать-строку-в-имя-файла")
  )

(def-trivial-test::! тест--Закодировать-строку-в-имя-файла--2-ЗАКОДИРОВАТЬ
  "!!закодировать"
  (|Закодировать-строку-в-имя-файла| "ЗАКОДИРОВАТЬ")
  )

(def-trivial-test::! тест--Закодировать-строку-в-имя-файла--3-ЗАКОДИРовать
  "!!закодир,о,в,а,т,ь"
  (|Закодировать-строку-в-имя-файла| "ЗАКОДИРовать")
  )

(def-trivial-test::! тест--Закодировать-строку-в-имя-файла--2
  "яr!Я!Р%2F%3A%2A%3F()%21%2E"
  (|Закодировать-строку-в-имя-файла| "яrЯР/:*?()!.")
  )
