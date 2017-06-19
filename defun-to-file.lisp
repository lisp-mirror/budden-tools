;;; -*- Encoding: utf-8; system :budden-tools; -*-
(def-merge-packages::! :defun-to-file
                       (:always t)
                       (:use :cl :budden-tools)
                       (:export
                        "
  budden-tools:*defun-to-file-directory*
  budden-tools:defun-to-file ; устарело
  budden-tools:defun-to-file-2
  budden-tools:defun-to-file-macroexpanded ; устарело
  budden-tools:defun-to-file-me-no-pe ; устарело
  budden-tools:ggsym
  budden-tools:ПЕЧАТАЕМЫЙ-ПРЕДСТАВИТЕЛЬ-СИМВОЛА
  defun-to-file:get-tfi-symbol ; будет определена в defun-or-defun-tfi
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


#| Идея состояла в том, чтобы gensym-ы, созданные в рамках одного образа и включенные в defun-to-file, ссылались на одно и то же. Это может быть достигнуто лишь частично. Именно, 

(defparameter sss (gensym))
(eval `(defun-to-file uuu () (cons ',sss ',sss)))
(let ((res (uuu))) (eq (car res) (cdr res))) ; да
; но
(eval `(defun-to-file vvv () (cons ',sss ',sss)))
(eq (car (uuu)) (car (vvv)))
; и
(eq (car (uuu)) sss)

;; потеря информации происходит в момент компиляции: файловый компилятор получает в исходном тексте не представитель символа, хранящий его точный идентификатор, а лишь его печатаемое представление. 
;; В компилируемывй файл сохраняется это представление и при загрузке fasl-а оно превращается в новый одноимённый символ. 

|#

(defvar *идентификатор-образа*
  (str++ (get-universal-time) "-" (sb-posix:getpid)))

(defvar *идентифицировать-бездомные-символы-при-печати* nil
  "Если истина, то при печати бездомный символ идентифицируется. При чтении ранее идентифицированный бездомныйсимвол всегда идентифицируется")

(defvar |*заменять-символы-на-их-Tfi-эквиваленты*| nil
  "Используется в defun-and-defun-tfi, чтобы заменить обращения к символам, именующим компилируемые ф-ии, на обращения к символам, именующим интерпретируемые ф-ии")

(defvar *все-сериализуемые-бездомные-символы* (make-hash-table :test 'equal)
  "Сюда заносятся бездомные символы, идентифицированные при чтении или печати. Они могут перестать впоследствии быть бездомными, поэтому данное название не совсем корректно")

(defvar *счётчик-сериализуемых-бездомных-символов* 0)

(defclass ПЕЧАТАЕМЫЙ-ПРЕДСТАВИТЕЛЬ-СИМВОЛА ()
  ((имя :initarg имя :accessor печатаемый-представитель-символа-имя)
   (ид-образа :initarg ид-образа :accessor печатаемый-представитель-символа-ид-образа)
   (код-внутри-образа :initarg код-внутри-образа :accessor печатаемый-представитель-символа-код-внутри-образа))
  )

(defmethod print-object ((o ПЕЧАТАЕМЫЙ-ПРЕДСТАВИТЕЛЬ-СИМВОЛА) out-stream)
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
      (setf (gethash (ключ-таблицы-сериализуемых-бездомных-символов ид-образа код-внутри-образа)
               *все-сериализуемые-бездомные-символы*) (make-symbol имя))))

(defun получить-печатаемый-представитель-символа (символ)
  (or
   (get символ 'ПЕЧАТАЕМЫЙ-ПРЕДСТАВИТЕЛЬ-СИМВОЛА)
   (let* ((ид-образа *идентификатор-образа*)
          (код-внутри-образа (incf *счётчик-сериализуемых-бездомных-символов*))
          (ключ (ключ-таблицы-сериализуемых-бездомных-символов ид-образа код-внутри-образа))
          (представитель
            (make-instance 'ПЕЧАТАЕМЫЙ-ПРЕДСТАВИТЕЛЬ-СИМВОЛА
                           'имя (string символ)
                           'ид-образа ид-образа
                           'код-внутри-образа код-внутри-образа)))
     (setf (get символ 'ПЕЧАТАЕМЫЙ-ПРЕДСТАВИТЕЛЬ-СИМВОЛА) представитель)
     (setf (gethash ключ *все-сериализуемые-бездомные-символы*) символ)
     представитель
    )))

(defun print-symbol-with-readmacro-readably (object stream)
  "Символ должен иметь домашний пакет, см. вызовы. Сделано по аналогии с print.lisp/output-symbol"
  (let ((name (symbol-name object))
        (package (symbol-package object)))
    (unless (budden-tools::symbol-is-in-package object *package* nil)
      (let ((prefix (package-name package)))
        (sb-impl::output-symbol-name prefix stream))
      (if (eq :external (nth-value 1 (find-symbol name package)))
          (write-char #\: stream)
          (write-string "::" stream)))
    (sb-impl::output-quoted-symbol-name name stream))
  nil)

(declaim (ftype (function (t t t) t) get-tfi-symbol))

(defun decorated-output-symbol (fn object stream)
  (cond
   ((and *escape-symbol-readmacros*
         (symbol-package object)
         (symbol-readmacro object))
    (print-symbol-with-readmacro-readably object stream))
   ((and |*заменять-символы-на-их-Tfi-эквиваленты*|
         ; эта ф-я будет определена в defun-or-defun-tfi
         (get-tfi-symbol object nil nil))
    (let ((|*заменять-символы-на-их-Tfi-эквиваленты*| nil))
      (funcall fn (get-tfi-symbol object nil nil) stream)))
   ((symbol-package object)
    (funcall fn object stream))
   (*идентифицировать-бездомные-символы-при-печати*
     (let ((*print-circle* nil)
           (*print-readably* nil))
       (sb-impl::output-object 
        (получить-печатаемый-представитель-символа object)
        stream)))
   (t 
    (funcall fn object stream))))

(decorate-function:portably-without-package-locks
 (decorate-function:decorate-function
  'sb-impl::output-symbol
  #'decorated-output-symbol))


(defparameter |*декларации-оптимизации-пошаговой-отладки*| 
  '(declaim
    (optimize (debug 3) (space 2) (compilation-speed 2) (speed 2) (safety 3))))

(defmacro defun-to-file-macroexpanded (name &rest more)
  "То же, что defun-to-file, но вызывает walk-form с полным макрорасширением над телом, а также настраивает steppable код. Есть риск ошибок в этой конструкции из-за отсутствия локальных переменных в контексте во время прогулок по лямбда-выражению"
  (defun-to-file-fn 'defun-to-file-macroexpanded name more :walk-form t
    :preambula |*декларации-оптимизации-пошаговой-отладки*|))

(defmacro defun-to-file-me-no-pe (name &rest more)
  "Вызывает walk-form с полным макрорасширением над телом и без print-circle"
  (defun-to-file-fn 'defun-to-file-me-no-pe name more :walk-form t :print-circle nil
    :preambula |*декларации-оптимизации-пошаговой-отладки*|))

(defun careful-transform-defun-form (name more)
  (perga-implementation:perga
   (let args (car more))
   (let body (cdr more))
   (:@ multiple-value-bind (forms decls doc) (sb-impl::parse-body body t))
   (let lambda-guts `(,@decls (block ,(sb-impl::fun-name-block-name name) ,@forms)))
   (let lambda `(lambda () ,@lambda-guts))
   (let walked-lambda 
     (let ((sb-walker::*walk-form-expand-macros-p* t))
       (sb-walker:walk-form lambda)))
   `(defun ,name ,args ,@(when doc (list doc)) (funcall ,walked-lambda))))

(defmacro defun-to-file (name &rest args-docstring-decls-body)
  #+russian "Определяет функцию с таким исходником в файле с именем *defun-to-file-directory*/имя-функции.
Имя функции должно быть допустимым именем файла и не должно содержать всяких мерзких символов. 
Нужно бы добавить сюда ещё имя пакета, но пока не сделано. Возвращает два значения - имя функции и имя файла. Если определить таким способом ф-ю CamelCase, а затем CAMELCASE, то определение CamelCase пропадёт, поскольку имена файлов в Windows нечувствительны к регистру."
  #-russian "Works like defun, but writes source to file named *defun-to-file-directory*/function-name. 
 Function name must be a valid filename. Returns two values: function name and filename. Beware lowercase vs uppercase
 problem: if you defun-to-file CamelCase function and then CAMELCASE, first function will be overwritten as filenames coincide in Windows"
  (defun-to-file-fn 'defun-to-file name args-docstring-decls-body :walk-form nil))

(defmacro defun-to-file-2 (name args options &rest docstring-decls-body)
  "Копирует определение функции в файл и выполняет файл. Смысл в том, что функция может быть генерируемой, в этом случае её исходник в виде файла не существует, а это затрудняет отладку. 
  (defun-to-file-2 имя-функции
     (&key walk-form (package *package*) (print-circle t) preambula)
     (арг1 ...)
     докстрока декларации . тело). 
     walk-form - Обработать с помощью sb-walker:walk-form.
     preambula - код, вставляемый в файл перед телом функции.
     См. также defun-to-file"
  (defun-to-file-fn-with-options 'defun-to-file-2 name options (cons args docstring-decls-body))) 

(defun defun-to-file-fn-with-options (definer-name name options args-docstring-decls-body)
  (apply 'defun-to-file-fn definer-name name args-docstring-decls-body options))

(defun defun-to-file-fn (definer-name name args-docstring-decls-body
                          &key
                          walk-form
                          (package *package*)
                          (print-circle t)
                          (preambula |*декларации-оптимизации-пошаговой-отладки*|)
                          (|компилировать| t)
                          (|заменять-символы-на-их-Tfi-эквиваленты| nil))
  "Описание см. в defun-to-file-2"
  (let* 
      ((|имя-директории-пакета| (|Закодировать-строку-в-имя-файла| (package-name (symbol-package name))))
       (директория-пакета (str+ (namestring *defun-to-file-directory*) "/" |имя-директории-пакета| "/"))
       (имя-файла (|Закодировать-строку-в-имя-файла| (symbol-name name)))
       (полное-имя-файла (str+ директория-пакета имя-файла ".lisp"))
       (*readtable* (find-readtable nil)))
    (ensure-directories-exist директория-пакета)
    (perga-implementation:perga
      (:@ with-open-file (out полное-имя-файла :direction :output
        :if-does-not-exist :create :if-exists :supersede
        :external-format :utf-8))
      (let *print-circle* print-circle
        *print-readably* t
        *print-pretty* t
        *идентифицировать-бездомные-символы-при-печати* t)
      (format out
              ";;; -*- Encoding: utf-8; -*-~%;;; Автоматически создано ~S из ~S~%"
              definer-name
              (or *compile-file-pathname* *load-pathname*))
      (print `(named-readtables:in-readtable nil) out)
      (print `(CL:IN-PACKAGE ,(package-name (the* not-null (find-package package)))) out)
      (etypecase preambula
        (null)
        (string (format out "~%~A~%" preambula))
        (cons
         (print preambula out)))
      ; Закомментаренный код - для defun-tfi, см. теги в репозитории Яр и budden-tools - этот код временно удалён, но в целом полезен
      ;(let normal-symbol-from-tfi-symbol (get-tfi-symbol name nil t))
      ;(when normal-symbol-from-tfi-symbol
      ;  (format out ";; См. также ~S~%" normal-symbol-from-tfi-symbol))
      (let |*заменять-символы-на-их-Tfi-эквиваленты*| |заменять-символы-на-их-Tfi-эквиваленты|)

      (let processed-definition
        (ecase walk-form
          (t
           (let ((sb-walker::*walk-form-expand-macros-p* t))
             (sb-walker:walk-form `(defun ,name ,@args-docstring-decls-body))))
          (:careful
           (careful-transform-defun-form name args-docstring-decls-body))
          ((nil)
           `(defun ,name ,@args-docstring-decls-body))))
      (print processed-definition out)
      )
    (cond
     (|компилировать| 
      (multiple-value-bind (имя-фасл-файла ошибки неудача)
                           (compile-file полное-имя-файла)
        (when неудача
              (cerror "Продолжить и попытаться загрузить"
                      "~S: неудача при компиляции функции ~S в файл: ~S" definer-name name ошибки))
        `(values (load ,имя-фасл-файла) ,имя-фасл-файла)))
     (t
      `(load ,полное-имя-файла)))))


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


(let ((sym1 (gensym)))
  (dolist (sym (list sym1 'cons nil 123 sym1))
    (тест-печатаемый-представитель-символа sym)))

; здесь степпер работает
;(defun-to-file:defun-to-file aabby (y) (let ((x y)) (break) (loop while (< x 5) do (incf x)) x))

; а здесь - нет. Но виной тому то, во что расширяется loop
; (defun-to-file:defun-to-file-macroexpanded aabby (y) (let ((x y)) (break) (loop while (< x 5) do (incf x)) x))

