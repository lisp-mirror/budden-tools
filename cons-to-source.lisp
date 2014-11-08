; -*- Encoding: utf-8; -*- 

; формат вывода списка
; (" "  items)->через пробел
; (";(" items) в скобках, через ;
; (".'" items) в один. кавычках, через .
; формат вывода
; (:s x &optional formatter) - строка
; (:e x) escape
; (:f formatter x) with-formatter
; (" " .  items)->через пробел
; ("" . items) -> без пробела.
; (\(; . items) в скобках, через ;
; ("'." . items) в один. кавычках, через .
; (symbol . items) - берется значение родовой ф-ии symbol-processor (в частности, может быть symbol-function, а это плоховато)
; (:progn . code) - выполняется code с помощью eval (progn ,@code) и печатает результат (не пользоваться)
; x - format ~A


#|
Вопросы для документирования:
1.  когда производятся вычисления
2.  Как выводится "исходник в исходнике", т.е. конструкция, когда нужно вывести фрагмент исходника
в строку на некотором языке, например:
'select * from "my table" where '+myWhereClause+' and somemore(id)'

Что нужно для этого знать?
-в строку на каком языке должен превратиться исходник
-какой язык будет исполнять исходник
-как обрабатываеть escape типа myWhereClause

|#
;(asdf::of-system :budden-tools)

(def-merge-packages::! :cons-to-source-formatters
  (:documentation "Separate package to list all formatters. Import whatever formatters you need")
  (:use)
  (:always t)
  (:export "
   CONS-TO-SOURCE-FORMATTERS:*PASCAL*
   CONS-TO-SOURCE-FORMATTERS:*FIREBIRD-SQL-NO-UTF*
   CONS-TO-SOURCE-FORMATTERS:*sql*
   CONS-TO-SOURCE-FORMATTERS:*c-formatter*
   CONS-TO-SOURCE-FORMATTERS:*nod-formatter*
   CONS-TO-SOURCE-FORMATTERS:*csv-formatter*
   "))
   


(def-merge-packages::! :cons-to-source
                       (:documentation "cons formatting - code generation from lisp trees")
                       (:nicknames :case1)
                       (:use :cl :cons-to-source-formatters
                        :budden-tools :iterate-keywords)
                       (:local-nicknames :bu :budden-tools)
                       (:always t)
                       (:export "
                        CONS-TO-SOURCE:PD
                        CONS-TO-SOURCE:PDS
                        CONS-TO-SOURCE:*CODEGEN-STREAM*
                        ; CONS-TO-SOURCE:|.|
                        CONS-TO-SOURCE:EMIT-FUNCALL
                        CONS-TO-SOURCE:LONG-COMMENT
                        CONS-TO-SOURCE:SHORT-COMMENT
                        CONS-TO-SOURCE:FORMATTER-DATA
                        CONS-TO-SOURCE:*FORMATTER-STACK*
                        CONS-TO-SOURCE:X-TO-SYN
                        CONS-TO-SOURCE:INDENT
                        CONS-TO-SOURCE:WITH-FORMATTER-STACK
                        CONS-TO-SOURCE:NATURAL-CLOSE-BOUNDARY
                        CONS-TO-SOURCE:*CODEGEN-STREAM*
                        " 
                            )
                       )

(in-package :cons-to-source)

(declaim (optimize speed))

(defclass formatter-data nil 
  ((name :initarg :name :initform nil :accessor name^)
   (open-boundary :initarg :open-boundary :initform nil :accessor open-boundary^) 
   (close-boundary :initarg :close-boundary :initform nil :accessor close-boundary^) 
   (string-boundary-output-style :initarg :string-boundary-output-style :initform nil
                                 :accessor string-boundary-output-style^
                                 :documentation ":double или :escape"
                                 ) 
   (escape-character-output-style :initarg :escape-character-output-style :initform :as-is
                                  :accessor escape-character-output-style^
                                  :documentation ":double или :as-is")
   (eol-in-string :initarg :eol-in-string :initform nil :accessor eol-in-string^)
   (string-formatter :initarg :string-formatter :initform nil :accessor string-formatter^ 
                     :documentation "Описание формата строки. Форматтер")
   (string-concatenator :initarg :string-concatenator :initform nil :accessor string-concatenator^
                        :documentation "то, чем складывают строки (для превращения выражений в строку, в т.ч., для Escape)")
   (short-comment-formatter :initarg :short-comment-formatter :initform nil :accessor short-comment-formatter^)
   (long-comment-formatter :initarg :long-comment-formatter :initform nil :accessor long-comment-formatter^)
   (transformer :initarg :transformer :initform nil :accessor transformer^ :documentation "Преобразует выражение к строке")
   (delimiter :initarg :delimiter :initform nil :accessor delimiter^ :documentation "for lists")
   (indentation :initarg :indentation :initform 0 :accessor indentation^ :documentation "Величина отступа")
   )
  (:documentation "string formatter, language formatter and list formatter. String and language formatters are in *formatter-stack* while list formatters are not. Ain't it a bug?"))

(defmethod print-object ((x formatter-data) stream) 
  (print-unreadable-object (x stream :type t :identity t) 
    (format stream "~A" (slot-value x 'name))
    ))

(defmethod get-close-boundary ((x formatter-data))
  (with-slots (open-boundary close-boundary) x
    (cond
     (close-boundary close-boundary)
     (t (natural-close-boundary open-boundary)))))

(defmacro def-formatter-data (name &rest formatter-data-initargs)
  `(defparameter ,name (make-instance 'formatter-data :name ',name ,@formatter-data-initargs)))

#+lispworks 
(dspec:define-dspec-alias def-formatter-data (name &rest formatter-data-initargs)
  (setf formatter-data-initargs formatter-data-initargs)
  `(defparameter ,name))


(def-formatter-data *pascal-string* :open-boundary "'" :string-boundary-output-style :double :transformer 'string-transformer
                   :eol-in-string "'#13#10
+'")

 
(def-formatter-data *sql-string* :open-boundary "N'" :close-boundary "'" :string-boundary-output-style :double :transformer 'string-transformer
                :eol-in-string 
                "
")

 
(def-formatter-data *sql-no-N-string* :open-boundary "'" :string-boundary-output-style :double :transformer 'string-transformer
                :eol-in-string 
                "
")

(def-formatter-data *c-string* :open-boundary "\"" :string-boundary-output-style :escape
              :escape-character-output-style :double
              :transformer 'string-transformer
              :eol-in-string "\\\n")
(def-formatter-data *command-line-string* :open-boundary "\"" :string-boundary-output-style :double :transformer 'string-transformer) 
(def-formatter-data *short-pascal-comment* :open-boundary "{")
(def-formatter-data *long-pascal-comment* :open-boundary "(*")
(def-formatter-data *pascal* :string-concatenator "+" :short-comment-formatter *short-pascal-comment*
            :long-comment-formatter *long-pascal-comment* :string-formatter *pascal-string*)
(def-formatter-data *long-sql-comment* :delimiter " " :open-boundary "/*"  :string-formatter *sql-no-N-string*)
(def-formatter-data *short-sql-comment* :open-boundary "--")
(def-formatter-data *sql* :string-concatenator "+" :long-comment-formatter *long-sql-comment* :string-formatter *sql-string*
         :short-comment-formatter *short-sql-comment*)
(def-formatter-data *firebird-sql-no-utf* :string-concatenator "||" :long-comment-formatter *long-sql-comment* :string-formatter *sql-no-N-string*
         :short-comment-formatter *short-sql-comment*)
(def-formatter-data *long-c-comment* :delimiter " " :open-boundary "/*" :string-formatter *c-string*)
(def-formatter-data *short-c-comment* :open-boundary "//")
(def-formatter-data *c-formatter* :string-concatenator "\" \"" :long-comment-formatter *long-c-comment* :string-formatter *c-string*
                 :short-comment-formatter *short-c-comment*)
(def-formatter-data *short-lisp-comment* :open-boundary ";" :close-boundary "
")
(def-formatter-data *nod-string* :open-boundary "\"" 
                :string-boundary-output-style :escape 
                :escape-character-output-style :double
                :transformer 'string-transformer
                :eol-in-string "
")
(def-formatter-data *nod-formatter* :string-concatenator "\" \"" :long-comment-formatter *long-c-comment* :string-formatter *nod-string*
                   :short-comment-formatter *short-lisp-comment*)
(def-formatter-data *csv-string* :open-boundary "\"" :string-boundary-output-style :double :transformer 'string-transformer
                :eol-in-string "
")
(def-formatter-data *csv-formatter* :string-formatter *csv-string*)
  ; (*wil-long-comment-transformer* :transformer 'wil-long-comment-transformer)
  ; (*wil-formatter* :string-concatenator *wil-string-concatenator* :long-comment-formatter )



(defvar *formatter-stack* (list *pascal*))
(defun current-formatter () (car *formatter-stack*))
(defparameter *codegen-stream* *debug-io*) ; *standard-output*)

(defmacro with-formatter-stack (formatter &body body) 
  "В отличие от with-formatter, который задает уровни вывода исходника в исходнике, этот макрос 
  может служить для обработки нескольких не связанных между собой операций вывода."
  `(let1 *formatter-stack* (list ,formatter)
     ,@body))

(defconstant eol (format nil "~A" #\newline))

(defparameter *natural-close-boundaries* 
  '(
    ("(" . ")")    ("{" . "}")    ("[" . "]")    ("(*" . "*)")    ("/*" . "*/")
    ("\"" . "\"")    ("--" . eol)    ("//" . eol)    ("'" . "'")    ("<" . ">")  ("begin" . "end")))


(defun natural-close-boundary (open-boundary) 
  (cdr (assoc open-boundary *natural-close-boundaries* :test #'equal)))

(defstruct syntree expr data code)

(defgeneric x-to-syn (x formatter)
  (:documentation 
   "пусть в дереве для print-data (также известном как pd,pds) попался некий объект. Если не заданы другие способы печати, а задан метод x-to-syn, то данный объект преобразуется в дерево, которое печатается вместо объекта. В частности, в fbody и pbody x-to-syn отвечает за преобразование ~~лисп-объект. См. также budden::x-to-inline-sql"
   ()))

(defmethod x-to-syn ((x syntree) formatter) (syntree-expr x))

(defstruct syntax-printable-object "Если сделать такой объект, то pprint будет печатать его 
  прямо как порождаемый им код"
  code
  formatter)

; а вообще надо тут весь вывод переделать на pretty-printing. 
(defun pprint-syntax-printable-object (s o) 
  (let1 *codegen-stream* s
    (with-formatter-stack (syntax-printable-object-formatter o)
      (print-data (syntax-printable-object-code o)))))

(set-pprint-dispatch 'syntax-printable-object 'pprint-syntax-printable-object)

(defun indentation-string (&optional (formatter (current-formatter))) 
"Выдает строчку для отступа"
(make-sequence 'string (slot-value formatter 'indentation) :initial-element #\ ))

(defun dprint-eol ()
  (terpri *codegen-stream*)
  (dprint (indentation-string)))

(defun faster-princ (x stream)
  "FIXME проэкспортируй меня"
  (typecase x
    (string (write-string x stream))
    (character (write-char x stream))
    (t (princ x stream)))
  )

(defun dprint (x &optional (formatter-stack *formatter-stack*))  "Печатает атом. Делает отступы, преобразует строку в исходнике"
  (declare (optimize speed))
  (when (eq x 'eol) (break))
  (let* (x-with-location)
    (when x
      (typecase x
        (string)
        (t (setf x-with-location x x (with-output-to-string (v) (faster-princ x v)))))
      (let ((dst-beg (file-position *codegen-stream*))
            (delegate (gethash *codegen-stream* budden-tools::*slmd)))
        ; (when (>= dst-beg 11883) (break))
        (cond
         (formatter-stack 
          (let1 transformer (slot-value (car formatter-stack) 'transformer)
            (cond (transformer (setf x (funcall transformer x (car formatter-stack)))) ; вот здесь дерьмо с нахождением исходника. Трансформеру следует об этом позаботиться (например, вызывать l/str+ вместо str+)
                  ((and (stringp x) (/= 0 (slot-value (car formatter-stack) 'indentation))) ; и здесь тоже
                   (setf x (budden-tools::l/substitute-subseq x eol (str+ eol (indentation-string)))))
                  )
            (dprint x (cdr formatter-stack))))

         (t (faster-princ x *codegen-stream*)))
        (let ((dst-end (file-position *codegen-stream*)))
          (when delegate
            (budden-tools::l/add-to-location-map delegate dst-beg dst-end (or x-with-location x))))
        )))) ; по идее, нужен цикл, и при встречании символа конца строки - 
                                             ; добавлять indentation.



(defun list-formatter-by-string-or-list (s)
  (let (delimiter open-boundary close-boundary)
    (cond 
     ((listp s) (apply #'make-instance 'formatter-data s))
     ((stringp s)
      (case (length s)
        (0) ; nothing
        (1 ; delimiter
         (setf delimiter s))
        (2 ; delimiter open-brace
         (setf delimiter (subseq s 0 1) open-boundary (subseq s 1 2) 
               close-boundary (natural-close-boundary open-boundary)))
        (t (error "only length 1 or 2 is supported by list-formatter-by-string-or-list")))
      (make-instance 'formatter-data 
                     :open-boundary open-boundary :close-boundary close-boundary
                     :delimiter delimiter))
     (t (error "unknown data ~S in list-formatter-data-by-string-or-list" s)))))


(defun switch-formatter (formatter upward)
  "When not upward,  it means that formatting is being closed. The formatter is not the current formatter anyway"
  (assert (not (eq formatter (current-formatter))))
  (let1 brace 
      (if upward (get-close-boundary formatter)
        (slot-value formatter 'open-boundary))
    (print-data brace)))



(defmacro with-formatter (formatter &body body)
  "If formatter=nil this temporarily pops *formatter-stack*. Here the boundaries are printed"
  (with-gensyms 
      (fmtr upward sav-formatter)
    `(let* ((,fmtr ,formatter) (,upward (not ,fmtr)) ,sav-formatter)
       (cond
        ((not ,upward)
         (switch-formatter ,fmtr nil)
         (push ,fmtr *formatter-stack*))
        (t 
         (setf ,sav-formatter (pop *formatter-stack*))
         (unless *formatter-stack* (error "with-formatter upward move: *formatter-stack* is empty!"))
         (switch-formatter ,sav-formatter t)))
       (unwind-protect
           (progn ,@body)
         (cond
          ((not ,upward)
           (pop *formatter-stack*)
           (switch-formatter ,fmtr t))
          (t 
           (switch-formatter ,sav-formatter nil)
           (push ,sav-formatter *formatter-stack*)))))))


#|   
Examples:
 (let ((s (make-string-output-stream)))
   (write-string "testing... " s)
   (prin1 1234 s)
   (get-output-stream-string s))
=>  "testing... 1234"
|#


(defun string-transformer (s formatter)  
  "Занимается только Escapa-ми. Есть проблема с need-bounds"
  (assert formatter)
  (unless s (return-from string-transformer nil))
  (with-slots (string-boundary-output-style eol-in-string escape-character-output-style)
      formatter
    (let1 close-boundary (get-close-boundary formatter)
      (cond 
       ((member s '(eol :|EOL| :|eol|)) (str+ eol-in-string (indentation-string)))
       (t 
        (unless (stringp s) (setf s (format nil "~A" s)))
        (let (res)
          (iter (:for c :in-string s)
            (cond 
             ((eql c #\\)
              (ecase escape-character-output-style
                (:double (setf res (list* c c res)))
                (:as-is (setf res (list* c res)))
                ))
             ((or nil #| (eq c (elt open-boundary 0)) |# ; Проблема с буквой N.
                  (eql c (elt close-boundary 0)))
              (ecase string-boundary-output-style 
                (:double (setf res (list* c c res)))
                (:escape (setf res (list* c #\\ res)))
                ))
             ((eql c #\newline)
              (setf res (append (reverse (concatenate 'list eol-in-string))
                                res)))
             (t (push c res))))
          (map 'string #'identity (reverse res))))))))


(defun format-list (list list-formatter)
  "Prints a list"
  (with-slots (open-boundary delimiter) list-formatter
    (print-data open-boundary)
    (let ((firsttime t) (close-boundary (get-close-boundary list-formatter)))
      (dolist (x list) 
        (when x 
          (if firsttime (setf firsttime nil) (print-data delimiter))
          (print-data x)))
      (print-data close-boundary))))

(defun symbol-processor-wrapper (sym tail &key (formatter (car *formatter-stack*)))
  "Для списка, начинающегося с символа, выполняет обработку, вызывая symbol-processor"
  (symbol-processor sym (name^ formatter) tail))


(defgeneric symbol-processor (symbol formatter-name tail)
  (:documentation "В печати print-data, для списка, начинающегося с такого символа, выполняет обработку этого списка")
  (:method (symbol formatter-name tail)
   (assert (symbolp symbol))
   (if
       (fboundp symbol) 
       (apply (symbol-function symbol) tail)
     (error "~S" `(не-понимаю-символ ,symbol :formatter ,formatter-name
                                     :tail ,tail))))
  (:method ((s (eql 'and)) (fnm (eql '*firebird-sql-no-utf*)) ta)
   (cond 
    ((atom ta) ta)
    ((cdr ta)
     `((:open-boundary "(" :delimiter ") and (") ,@ta))
    (t (car ta))))
  (:method ((s (eql 'or)) (fnm (eql '*firebird-sql-no-utf*)) ta)
   (cond 
    ((atom ta) ta)
    ((cdr ta)
     `((:open-boundary "(" :delimiter ") or (") ,@ta))
    (t (car ta))))
  (:method ((s (eql 'not)) (fnm (eql '*firebird-sql-no-utf*)) ta)
   `("" "not (" ,(car ta) ")"))
  (:method ((s (eql 'cond)) (fnm (eql '*firebird-sql-no-utf*)) ta)
   (cond
    ((atom ta) (error "Wrong cond body in pd"))
    (t
     `(if ,(caar ta) 
          (progn ,@(cdar ta))
        ,(when (cdr ta)
           `("" (:indent -2) 
                (cond ,@(cdr ta))
                (:indent 2)))
        )))
   )
  (:method ((s (eql 'when)) (fnm (eql '*firebird-sql-no-utf*)) ta)
   (cond
    ((atom ta) (error "Wrong cond body in pd"))
    (t `(if ,(car ta) (progn ,@(cdr ta))))))
  )

(defmethod x-to-syn ((p number) formatter)
  (format nil "~A" p)
  )

(defmethod x-to-syn ((p float) (formatter (eql *pascal*)))
  (format nil "~F" p)
  )


(defun replace-dot-with-comma (s)
  (substitute #\, #\. s))

(defmethod x-to-syn ((p float) (formatter (eql *csv-formatter*)))
  (replace-dot-with-comma (format nil "~F" p)))

(defmethod symbol-processor ((s (eql 'progn)) fnm ta)
  (case fnm
    ((*firebird-sql-no-utf* *pascal*)
     `("" (:indent 2) :eol
          begin :eol
          ((:delimiter :eol) ,@ta) :eol
          end ,@(when (eq fnm '*pascal*) '(";")) (:indent -2) :eol)
     )
    ))


(defmethod symbol-processor ((s (eql 'setf)) fnm ta)
  (proga
    (labels inner (tail)
      (when tail
        (let1 (var val . tail) tail
          `(,var ,(ecase fnm 
                    (*firebird-sql-no-utf* '=)
                    (*pascal* ":="))
                 ,val ";" ,@(inner tail)))))
    `("" ,@(inner ta) :eol)))


(defmethod symbol-processor ((s (eql 'funcall))
                             (fnm (eql '*firebird-sql-no-utf*))
                             ta)
  (let1 (proc &rest args) ta
    `("" "execute procedure " ,proc ,@(when args `((",(" ,@args))) ";")))

(defmethod symbol-processor ((s (eql 'funcall)) (fnm (eql *sql*)) ta)
  (let1 (proc &rest args) ta
    `(" " exec ,proc ((:delimiter ",") ,@args) ";")))

(defmethod symbol-processor ((s (eql 'assert))
                             (fnm (eql '*firebird-sql-no-utf*))
                             ta)
  "places должно быть nil, datum - это выражение для pds, которое заключается в (:s . )"
  (let1 (test-form &optional places datum &rest arguments) ta
    (print places)
    (assert (null places))
    (assert (null arguments))
    `(unless ,test-form
       (funcall prtkl_zapisat nil 'assert (:s ,datum))
       (:l exception e_debug (:s ,datum)))
    ))

          
(defmethod symbol-processor ((s (eql 'if))
                             fnm
                             ta)
  (let1 (test if-true &optional if-false) ta
    (cond ((eq test t) if-true)
          ((eq test nil) if-false)
          (t (case fnm
               (*firebird-sql-no-utf*
                `("" if "(" ,test ")" then " " (:indent 2) 
                     ,if-true 
                     (:indent -2) " "
                     ,@(when if-false
                         `(else " " (:indent 2) 
                                ,if-false (:indent -2) " "
                                )))))))))

(defmethod symbol-processor ((s (eql 'unless))
                             fnm
                             ta)
  (let1 (test &rest body) ta
    `(if (not ,test) (progn ,@body))))


(defun print-data (code)
  (declare (special *print-right-margin*))
  ;(assert (member (car *formatter-stack*) `(,*firebird-sql-no-utf* ,*sql-no-N-string* ,*long-sql-comment*)))
  (let1 *print-right-margin* nil
    (cond 
     ((null code))
     ((member code '(eol :|eol| :|EOL|)) (dprint-eol))
     ((or (symbolp code) (stringp code) (characterp code)) (dprint code))
     ((atom code) (print-data (x-to-syn code (current-formatter)))) ; класс, для которого может быть определен метод печати.
     ((member (car code) '(quote)) ; строка
      (with-formatter (slot-value (current-formatter) 'string-formatter)
        (dprint (cadr code))))
     ((member (car code) '(s :s))
      (print-data `(f ,(slot-value (current-formatter) 'string-formatter) ,@(cdr code))))
;    `(f ,(slot-value (current-formatter) 'string-formatter) ,@(cdr code)))
     ((member (car code) '(e :e))  ; имеем в виду, что ESC вообще то из строки
      (with-formatter nil 
        (with-slots (string-concatenator) (current-formatter)
          (when string-concatenator (dprint string-concatenator))
          (print-data (cadr code))
          (when string-concatenator (dprint string-concatenator)))))
     ((member (car code) '(f :f))
      (with-formatter 
          (let ((x (cadr code)))
            (when (symbolp x) (setf x (symbol-value x)))
            (assert x) 
            x) 
        (print-data (caddr code))))
     ((member (car code) '(indent :indent))
      (with-slots (indentation) (current-formatter)
        (incf indentation (cadr code))
        (when (< indentation 0) (setf indentation 0))))
     ((member (car code) '(l :|l| :|L|))
      (print-data `(" " ,@(cdr code))))
     ;((member (car code) '(\. :\.))
     ; (print-data `("." ,@(cdr code))))
     ((eq (car code) :progn) (print-data (eval `(progn ,@(cdr code)))))
     ((symbolp (car code))
      (print-data (symbol-processor-wrapper (car code) (cdr code))))
;                   (or )
;                   (symbol-function (car code)) (cdr code))))
     ((or (stringp (car code)) (listp (car code)))
      (format-list (cdr code) (list-formatter-by-string-or-list (car code))))
     (t (error "Print-data: strange car ~S" (car code))))))

(defmacro pd (&rest args) `(print-data ,@args)) ; ЧЧтобы ручки не отсохли.

(DEFMACRO PDS (&REST ARGS) (if budden-tools::*record-locations* 
                               `(BUDDEN-TOOLS::L/WITH-OUTPUT-TO-STRING (*CODEGEN-STREAM*) (PRINT-DATA ,@ARGS))
                             `(with-output-to-string (*codegen-stream*) (print-data ,@args))
                             ))



; (defun l (&rest x) `(" " ,@x))

(defun long-comment (&rest args)
  `(:f ,(slot-value (current-formatter) 'long-comment-formatter) (" " ,@args)))
(defun short-comment (&rest args)
  `(:f ,(slot-value (current-formatter) 'short-comment-formatter) (" " ,@args)))

#+test
(progn 
 (print (print-data '(s asdf)))
CL-USER 80 > (print-data '(long-comment a b c d))
(*A B C D*)
CL-USER 54 > (print-data '(sql-in-pas "select" (ee null) eol "from x"))
'select '+NULL+' '#13#10' from x'
NIL
)


(defmethod x-to-syn ((p pathname) formatter) `',(namestring p))




