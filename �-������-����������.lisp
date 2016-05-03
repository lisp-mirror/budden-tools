;;; -*- Encoding: utf-8; -*-
;;; Попытка создать блокировку на уровне системы с помощью создания 
;;; файла блокировки. Не работает,т.к. файл в SBCL открывается
;;; с разделяемой блокировкой.

(def-merge-packages::! :С-ФАЙЛОМ-БЛОКИРОВКИ
  (:always t)
  ;(:nicknames :с-файлом-блокировки)
  (:use :cl :budden-tools)
  (
   :export "
   С-ФАЙЛОМ-БЛОКИРОВКИ:С-ФАЙЛОМ-БЛОКИРОВКИ-ФУНК
   "
                            ))

(in-package :С-ФАЙЛОМ-БЛОКИРОВКИ)

(in-readtable nil)

(defun забить-файл-пробелами (поток)
  (let ((л (file-length поток)))
    (file-position поток 0)
    (format поток "~A" (make-string л :initial-element #\ ))
    (finish-output поток)))

(defun с-файлом-блокировки-функ (имя-файла таймаут-блокировки время-сна функция)
  (perga-implementation:perga
   (check-type таймаут-блокировки fixnum)
   (check-type время-сна number)
   (assert (> время-сна 0))
   (let время-начала-первой-попытки (get-universal-time))
   (let время-наступления-таймаута (if таймаут-блокировки (+ время-начала-первой-попытки
                                                             таймаут-блокировки)
                                       0))
   (flet захватить-файл-блокировки (п)
     (file-position п 0)
     (format п "~A" (sb-posix:getpid))
     (finish-output п))
     
     
   (loop ; в цикле пытаемся заблокировать файл
     (:@ multiple-value-bind
         (поток error)
         (ignore-errors
          (open имя-файла :direction :io :if-does-not-exist :create :if-exists :overwrite)))
     (cond
      ((typep error 'error)
       (let текущее-время (get-universal-time))
       (cond
        ((= 0 время-наступления-таймаута)
         (sleep время-сна)
         )
        ((< текущее-время время-наступления-таймаута)
         (sleep время-сна)
         )
        (t ; раз мы сюда попали, наступил таймаут
         (cerror "Подождать ещё" "Таймаут ожидания файла блокировки ~S" имя-файла)
         (incf время-наступления-таймаута таймаут-блокировки)
         )))
      (t ; удалось открыть файл
       (unwind-protect
           (perga-implementation:perga
            (let файл-пустой (= 0 (file-length поток)))
            (let содержимое-файла
              (cond
               (файл-пустой "")
               (t
                (file-position поток 0)
                (read-line поток))))
            (let предыдущая-операция-завершена
              (and (not файл-пустой)
                   (find #\. содержимое-файла)))
            #|(unless (or файл-пустой предыдущая-операция-завершена)
              ; файл не захвачен, но операция не завершена. Что-то не так
              (cerror "Захватить файл блокировки" "Нужный нам файл блокироки ~S был когда-то захвачен, а позднее отпущен процессом с PID=~A, но операция не помечена завершённой. Возможно, процесс ~A был убит" имя-файла содержимое-файла (copy-seq содержимое-файла))
              )|#
            (unless файл-пустой
              (забить-файл-пробелами поток))
            (захватить-файл-блокировки поток)
            (return (funcall функция)))
         (format поток ".")
         (close поток)
         (ignore-errors
          (delete-file имя-файла))
         )
       )))))


(defun тест-создать-тестовый-поток (i)
  (let ((фб "c:/clcon/kkk.txt")
        (щ *standard-output*))
    (bt:make-thread
     (lambda ()
       (let ((i i)
             (*standard-output* щ))
         (format t "~%Начинаем поток ~S~%" i)
         (multiple-value-bind
             (result error)
             (С-ФАЙЛОМ-БЛОКИРОВКИ:С-ФАЙЛОМ-БЛОКИРОВКИ-ФУНК фб 2 0.7
              (lambda ()
                (format t "~%Поток ~S заблокировал файл ~%" i)
                (sleep 0.2)
                ))
           (format t "~%Поток ~S завершился: рез ~S, ошибка ~S~%" i result error))))           
     :name (format nil "~A" i))))

#| ; test 
 (dotimes (i 4)
   (тест-создать-тестовый-поток i))
|#
