;;; -*- Encoding: utf-8; -*-
(def-merge-packages::! :defun-to-file
                       (:always t)
                       (:use :cl :budden-tools)
                       (:export
                        "
  budden-tools:*defun-to-file-directory*
  budden-tools:defun-to-file
   "
                        ))

(in-package :defun-to-file)
(in-readtable nil)

(defparameter *defun-to-file-directory* #+win32 (budden-tools::pathname-as-directory (merge-pathnames
                                                                        (budden-tools::pathname-as-directory *default-pathname-defaults*)
                                                                        "defun-to-file"))
  #-win32 (error "Not defined *defun-to-file-directory* variable for not win32 systems")
  )

(defun maybe-add-slash (string)
  #+russian "Добавляет / в конец имени папки, если его там ещё нет"
  (cond
   ((string= string "") string)
   ((string= (sequence-last string) "/") string)
   (t (str+ string "/"))))

#|

незаконченный проект доделанной печати с gensym. Идея в следующем - создать во время чтения переменную, содержащую 
псевдопакет и заменить все генсимы на некоторые объекты, которые при печати и последующем чтении превращаются
в такие генсимы. В этом случае, генсимы, используемые только в пределах файла, будут печататься нормально. 

отложено из-за цейтнота. 

(defclass printable-gensym ()
  ((name :initarg name :accessor printable-gensym-name)
   (var :initarg var :accessor printable-gensym-var))
  )

(defmethod print-object ((o printable-gensym) out-stream)
  (format nil "#.~S" `(ggsym ,(printable-gensym-var o) ,(printable-gensym-name o))))

(defmacro ggsym (pseudo-package name)
  `(let ((result (cdr (assoc ,name ,pseudo-package :test 'string=))))
     (assert result "ggsym: name ~S not found in pseudo-package ~S" ,name ',pseudo-package)))

(defun convert-if-gensym (x)
  (typecase 


(defun make-gensyms-printable (tree var-name)
  #+russian "Есть некий список форм верхнего уровня с gensym-ами. Хотим сделать, чтобы он печатался читабельно. 
   Для этого gensym-ы заменяем читабельными выражениями, и создаём отдельную форму, к-рая их порождает.
   Возвращает новый список, в который спереди добавлены необходимые определения.  
   Например 
   (with-gensyms (a) `(,a)) 
   => (#:a173607)
   => `((defparameter *my-gensyms* '((\"A173607\" . (make-symbol \"A173607\")))) `(,(ggsym *my-gensyms* \"A173607\")))"
  (let* ((result-tree (maptree


Пусть дан список форм, который потом будет записан в файл. Обходим формы и находим в них все gensym-ы. Создаём переменную var-name, в которую записываем порождение этих gensym-ов, а при использовании заменяем gensym-ы на форму, которая их читает. 

|#


(defmacro defun-to-file (name &rest more)
  #+russian "Определяет функцию с таким исходником в файле с именем *defun-to-file-directory*/имя-функции.
Имя функции должно быть допустимым именем файла и не должно содержать всяких мерзких символов. 
Нужно бы добавить сюда ещё имя пакета, но пока не сделано. Для лиспворкс 4 в функции не могут быть gensyms, т.к. они криво
печатаются (что, в общем-то сводит всю идею на нет). Возвращает два значения - имя функции и имя файла. Если определить таким способом ф-ю CamelCase, а затем CAMELCASE, то определение CamelCase пропадёт, поскольку имена файлов в Windows нечувствительны к регистру."
  #-russian "Works like defun, but writes source to file named *defun-to-file-directory*/function-name. 
 Function name must be a valid filename. Returns two values: function name and filename. Beware lowercase vs uppercase
 problem: if you defun-to-file CamelCase function and then CAMELCASE, first function will be overwritten as filenames coincide in Windows" 
  (perga-implementation:perga
    (assert (every (lambda (!1) (not (find !1 "\\/.?* "))) (string name)))
    (let filename (str+ (namestring *defun-to-file-directory*) name))
    (perga-implementation:perga
      (:@ with-open-file (out (str+ filename ".lisp") :direction :output
        :if-does-not-exist :create :if-exists :supersede
        :external-format :utf-8))
      #+lispworks6 (let *print-circle* t *print-readably* t *print-pretty* t)
;      (let *print-circle* t *print-pretty* t)
      #-lispworks6 (let *print-pretty* t)
      #+lispworks6 (format out
                           ";;; -*- Encoding: utf-8; -*-~%;;; generated with budden-tools::defun-to-file from ~S~%"
                           (LISPWORKS:current-pathname))
      (print `(in-package ,(def-merge-packages:keywordize-package-designator
                            (package-name *package*))) out)
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
            
      
          
