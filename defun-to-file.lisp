;;; -*- Encoding: utf-8; -*-
(in-package :budden-tools)
(in-readtable :buddens-readtable)

(defparameter *defun-to-file-directory* #+win32 (str+ cl-user::*lisp-root* "sw/defun-to-file") #-win32 "/home/denis/sw/defun-to-file")

(defun maybe-add-slash (string)
  #+russian "Добавляет / в конец имени папки, если его там ещё нет"
  (cond
   ((string= string "") string)
   ((string= (sequence-last string) "/") string)
   (t (str+ string "/"))))

(defmacro defun-to-file (name &rest more)
  #+russian "Определяет функцию с таким исходником в файле с именем *defun-to-file-directory*/имя-функции.
Имя функции должно быть допустимым именем файла и не должно содержать всяких мерзких символов. 
Нужно бы добавить сюда ещё имя пакета, но пока не сделано. Для лиспворкс 4 в функции не могут быть gensyms, т.к. они криво
печатаются (что, в общем-то сводит всю идею на нет). Возвращает два значения - имя функции и имя файла"
  (proga 
    (assert (every #L(not (find !1 "\\/.?* ")) (string name)))
    (let filename (str+ (maybe-add-slash *defun-to-file-directory*) name))
    (proga
      (with-open-file out (str+ filename ".lisp") :direction :output
        :if-does-not-exist :create :if-exists :supersede)
      #+lispworks6 (let *print-circle* t *print-readably* t *print-pretty* t)
;      (let *print-circle* t *print-pretty* t)
      #-lispworks6 (let *print-pretty* t)
      (print `(in-package ,(keywordize (package-name *package*))) out)
      (print `(in-readtable :buddens-readtable-a) out)
      (print `(defun ,name ,@more) out)
      )
    (assert (compile-file (str+ filename ".lisp")))
    `(values (load ,filename) ,filename)))


(defun eval-with-file (code) 
  "Создаёт временную функцию в файле и выполняет её"
  (proga 
    (let func-name (gentemp "eval-with-file-fn"))
    (multiple-value-bind (success filename) 
        (eval `(defun-to-file ,func-name () ,code))
      (unwind-protect
          (progn 
            (assert success nil "Код ~S не загрузился" code)
            (funcall func-name))
        (delete-file filename)
        (unintern func-name)
        ))))
            
      
          
