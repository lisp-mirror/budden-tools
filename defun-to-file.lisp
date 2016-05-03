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

(defparameter *defun-to-file-directory* (budden-tools::pathname-as-directory (merge-pathnames
                                                                        (budden-tools::pathname-as-directory *default-pathname-defaults*)
                                                                        "defun-to-file"))
  )

(defun maybe-add-slash (string)
  #+russian "Добавляет / в конец имени папки, если его там ещё нет"
  (cond
   ((string= string "") string)
   ((string= (sequence-last string) "/") string)
   (t (str+ string "/"))))


(defvar *идентификатор-образа*
  (str++ (get-universal-time) "-" (sb-posix:getpid)))

(defvar *идентифицировать-бездомные-символы-при-печати* nil
  "Если истина, то при печати бездомный символ идентифицируется. При чтении ранее идентифицированный бездомныйсимвол всегда идентифицируется")

(defvar *все-сериализуемые-бездомные-символы* (make-hash-table :test 'equal)
  "Сюда заносятся бездомные символы, идентифицированные при чтении или печати. Они могут перестать впоследствии быть бездомными, поэтому данное название не совсем корректно")

(defvar *счётчик-сериализуемых-бездомных-символов* 0)

(defclass печатаемый-представитель-символа ()
  ((имя :initarg имя :accessor печатаемый-представитель-символа-имя)
   (ид-образа :initarg ид-образа :accessor печатаемый-представитель-символа-ид-образа)
   (код-внутри-образа :initarg код-внутри-образа :accessor печатаемый-представитель-символа-код-внутри-образа))
  )

(defmethod print-object ((o печатаемый-представитель-символа) out-stream)
  (cond
   (*идентифицировать-бездомные-символы-при-печати*
     (format out-stream "#.~S" `(ggsym ,(slot-value o 'ид-образа)
                                       ,(slot-value o 'код-внутри-образа)
                                       ,(slot-value o 'имя))))
   (t
    (call-next-method))))

(defmacro ggsym (ид-образа код-внутри-образа имя)
  `',(ggsym-fun ид-образа код-внутри-образа имя))

(defun ключ-таблицы-сериализуемых-бездомных-символов (ид-образа код-внутри-образа)
  (str++ ид-образа "!" код-внутри-образа))

(defun ggsym-fun (ид-образа код-внутри-образа имя)
  (or (gethash (ключ-таблицы-сериализуемых-бездомных-символов ид-образа код-внутри-образа)
               *все-сериализуемые-бездомные-символы*)
      (make-symbol имя)))

(defun получить-печатаемый-представитель-символа (символ)
  (or
   (get символ 'печатаемый-представитель-символа)
   (let* ((ид-образа *идентификатор-образа*)
          (код-внутри-образа (incf *счётчик-сериализуемых-бездомных-символов*))
          (ключ (ключ-таблицы-сериализуемых-бездомных-символов ид-образа код-внутри-образа))
          (представитель
            (make-instance 'печатаемый-представитель-символа
                           'имя (string символ)
                           'ид-образа ид-образа
                           'код-внутри-образа код-внутри-образа)))
     (setf (get символ 'печатаемый-представитель-символа) представитель)
     (setf (gethash ключ *все-сериализуемые-бездомные-символы*) символ)
     представитель
    )))

(defun decorated-output-symbol (fn object stream)
  (cond
   ((symbol-package object)
    (funcall fn object stream))
   (*идентифицировать-бездомные-символы-при-печати*
    (sb-impl::output-object 
     (получить-печатаемый-представитель-символа object)
     stream))
   (t 
    (funcall fn object stream))))

(decorate-function:portably-without-package-locks
 (decorate-function:decorate-function
  'sb-impl::output-symbol
  #'decorated-output-symbol))


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
      (let *print-circle* t
        *print-readably* t
        *print-pretty* t
        *идентифицировать-бездомные-символы-при-печати* t)
      (format out
              ";;; -*- Encoding: utf-8; -*-~%;;; generated with budden-tools::defun-to-file from ~S~%"
              (or *compile-file-pathname* *load-pathname*))
      (print `(in-package ,(def-merge-packages:keywordize-package-designator
                            (package-name *package*))) out)
      (print `(in-readtable :buddens-readtable-a) out)
      (print (let ((sb-walker::*walk-form-expand-macros-p* t))
               (sb-walker:walk-form `(defun ,name ,@more)))
             out)
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
            
;;; tests
(defun тест-печатаемый-представитель-символа (&optional (sym (gensym)))
  (let* ((str (with-output-to-string (ou)
                (let ((*идентифицировать-бездомные-символы-при-печати* t))
                  (print sym ou)))))
    ;(print str)
    (assert
     (eql sym (read-from-string str)))))


#|(let ((sym1 (gensym)))
  (dolist (sym (list sym1 'cons nil 123 sym1))
    (тест-печатаемый-представитель-символа sym)))|#


;; (defun-to-file aabbyy () (loop thereis t))

