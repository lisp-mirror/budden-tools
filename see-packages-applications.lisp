(in-package :budden-tools)
(in-readtable nil) 

; ������-�� �� ��������, ������ ��-�� ������� ������ readtable
(def-symbol-readmacro |/WITH-PACKAGE/|
                      (lambda (stream symbol)
                        (declare (ignore symbol))
                        (let* ((*package* (find-package (read stream)))
                               (res (read stream)))
                          (print res)
                          (it-is-a-car-symbol-readmacro res))))



(defpackage :package-for-read-symbol-name (:use))
(defparameter +package-for-read-symbol-name+ 
  (find-package :package-for-read-symbol-name))

(defun read-symbol-name (stream)
  "������ ������, �� ���������� ������ ��� ���. ���� ������ � ������ �� ����� �������������, �� ����� ������ ������ �� ����� ������"
  (let1 symbol-or-string (let1 *package* +package-for-read-symbol-name+
                 (read stream t))
    (etypecase symbol-or-string
      (string symbol-or-string)
      (symbol
       (assert (eq (symbol-package symbol-or-string) +package-for-read-symbol-name+))
       (unintern symbol-or-string +package-for-read-symbol-name+)
       (symbol-name symbol-or-string)))))



#+nil (defun ^-reader-internal (stream read-object object)
  "���� read-object=nil, �� �� ��� ������� ������ � ������ ������ ��, ��� ��� ����� ����"
  (let* ((object (if read-object (read stream t) object))
         (field-name (read-symbol-name stream))
         (args (read-delimited-list #\) stream t))) ; ������ ��� �����, � ����� �� ������. 
    (unread-char #\) stream) ; ����� �����������.
    `(|^| ,object ,field-name ,@args)))


#|�-� ���� ������ ��� ������� (defun guess-where-i-am (object)
  "������ ���, ��� ��� �������� ������ ������� ���������, �������� �� ���� ������ ����� ������.
  ���� ������ �������� ��� ������, �� ������ �� ����������"
  (when *reading-parens* 
    (budden-tools::push-function-to-call-when-paren-is-closing
     (lambda (result stream)
       (assert (consp result) () 
         "Something wrong with symbol readmacro: list reader on ~S returned atom ~S" stream result)
       (cond ((eq object (car result)) 
              (format *trace-output* "~S: I am a car" object)
              result)
             (t 
              (format *trace-output* "~S: i am not a car" object)
              result))
       )))
  object)|#

(defun splice-later-if-a-car (object)
  "��� �������� ������, ���� ���� ������ �������� ����� ������, �� �������, ��� �� ��� �������� �������. 
�������� � ��� ����� ����� ������������ ������ � ��������� �� ���� �������. �������� 
 (a^b c) ---/custom-token-parser/---> ((^ a b) c) ---/splice-if-a-car/---> (^ a b c)"
  (when *reading-parens* 
    (budden-tools::push-function-to-call-when-paren-is-closing
     (lambda (result stream)
       (assert (consp result) () 
         "Something wrong with symbol readmacro: list reader on ~S returned atom ~S" stream result)
       (cond ((eq object (car result)) 
              ;(format *trace-output* "~S: I am a car" object)
              (assert (listp object) () "splice-later-if-a-car: object ~S was expected to be a list" object)
              (append object (cdr result)) ; ����� ��� �������� ������
              )
             (t 
              ;(format *trace-output* "~S: i am not a car" object)
              result ; ����� ��� �������� ������
              )))))
  object)

#|(defun ^-reader-internal-2 (stream read-object object read-field-name field-name)
  "�� ���� �������������� ���� read-object=nil, �� �� ��� ������� ������ � ������ ������ ��, ��� ��� ����� ����"
  (let* ((object (if read-object (read stream t) object))
         (field-name (if read-field-name (read-symbol-name stream) field-name))
         (symbol (make-symbol (str+ "(^ " (if (string-designator-p object)
                                                (string object) "#<...>")
                                    " " field-name ")")))
         (args (make-symbol "args")))
    (eval `(defmacro ,symbol (&rest ,args) 
             `(|^| ,',object ,',field-name ,@,args)))
    (eval `(define-symbol-macro ,symbol (|^| ,object ,field-name)))
    symbol
    ))|#


(defun ^-reader-internal-2 (stream read-object object read-field-name field-name)
  "���� read-object=nil, �� �� ��� ������� ������ � ������ ������ ��, ��� ��� ����� ����"
  (let* ((object (if read-object (read stream t) object))
         (field-name (if read-field-name (read-symbol-name stream) field-name))
         (ret `(|^| ,object ,field-name))
         )
    (splice-later-if-a-car ret)
    ret))


(defun ^-reader-internal-3 (stream read-object object read-field-name field-name)
  "���� read-object=nil, �� �� ��� ������� ������ � ������ ������ ��, ��� ��� ����� ����"
  (let* ((object (if read-object (read stream t) object))
         (field-name (if read-field-name (read-symbol-name stream) field-name))
         
         )
    (list '|^| object field-name)))


(defun closing-paren-splice-cdr-into-car (readmacro-returned)
  "��� symbol-readmacro, ������������ �������������� ������ ����� ���� �� �������� ������. 
symbol-readmacro ������ ������� ������. cdr ���������� ������ ����������� � ����� ����
������, ������� ������ symbol-readmacro."
  ;(assert (null *functions-to-call-when-paren-is-closing*)
  ;    () "Wrong call to closing-paren-splice-cdr-into-car")
  (push-function-to-call-when-paren-is-closing 
        (lambda (list stream)
          (declare (ignore stream))
          (let1 car (car list)
            (assert (eq car readmacro-returned) () "Symbol-readmacro is not in the first position in a list ~S" list)
            (append car (cdr list))
            )))
  readmacro-returned)
  

;; FIXME ��� ������-������ ^ ����������� ��������. ������������� ^ �� ���-��, ���� ��� ������ 
;; (� ������ ��� ����� ����, ���� ������ '(a b c --> d e f), ���� ��������� �� ����� ������ - ������ ������
(defun ^-reader (stream symbol)
  "��. ����� ������ ����������� � ������ ����������� ������� ^"
  (declare (ignore symbol))
;  (it-is-a-car-symbol-readmacro (^-reader-internal-2 stream t nil t nil))
  (closing-paren-splice-cdr-into-car (^-reader-internal-3 stream t nil t nil))
  )



(def-symbol-readmacro |^| '^-reader)
; ������� ^ ���������� � variable-type.lisp

;(def-symbol-readmacro def-merge-packages:|EXPORT2| 'def-merge-packages::export2-reader)                            

(defun convert-carat-to-^ (stream symbol-name package)
  "���������� ��������� ^ � (^ a b). ��� ����� ����������� ���������
������������ ������ ����� ����, ��� �� ��������.
��� ��������� ������ ��� (�^fun arg), ��� � o^field (� �� (o^field)). 
����� ���� ��� �� ������ �������� ��������: ���� ��������� ���������� � ���� 
������� �������� ������� (��������, �������� ������ eval), � �� ��� ��������
� �����, ���������� ����������, � ���� _������_ �������� ������ ��� ������, �� �� ���
����������� ��������. ��� ����������� ������ �����������
������� � ������� �������� � ��������� ������ ���� '������������� ���������� #:(\^ ... ...)'.
��������� �� ������� ��, ��� ��� ������ ���������, ������� ��, ��� ��� ���������
�� ������. ��� ������� ��������, � ���� ������.
���� ������� ������������ load-time-value, �� ��� ������� ������ ���� ���.
����� ��� �����������:
make-load-form
������, � ����� ����� ������ �� ���������. 
"
  (let* ((p (position #\^ symbol-name :from-end t)))
    (cond 
     ((null p) (values nil nil))
     ((= (+ p 1) (length symbol-name)) (values nil nil))
     (t ; (break "~A" symbol-name)
        (let ((beg (subseq symbol-name 0 p))
              (end (subseq symbol-name (+ p 1))))
          (values (let ((*package* package))
                    (funcall '^-reader-internal-2 
                             (make-concatenated-stream (make-string-input-stream (str+ beg " ")) stream)
                             t nil 
                             nil end))
                  t))))))

; (setf (get-custom-token-parsers-for-package :budden) nil)
; (pushnew 'convert-carat-to-^ (get-custom-token-parsers-for-package :budden))


;; (/with-readtable-case/ :preserve '(foo bar))
;; Note that readtable case is evaluated at read-time 
(def-symbol-readmacro |/WITH-READTABLE-CASE/|
                      (lambda (stream symbol)
                        (declare (ignore symbol))
                        (let1 new-case (read stream)
                          (print new-case)
                          (pllet1 (readtable-case (packages-seen-p *readtable*)) new-case
                            (pllet1 (readtable-case *readtable*) new-case
                              (let1 colon-readtable (or (gethash *readtable* *my-readtable-to-colon-readtable*) *readtable*)
                                (pllet1 (readtable-case colon-readtable) new-case
                                  (it-is-a-car-symbol-readmacro (read stream)))))))))

(defparameter *essential-binding-checkers* 
  '(boundp fboundp) ;  ap5:rboundp - ����-�� � ������ ���� ��������
  "List of function names. Either function recieves one parameter, a symbol. If symbol is essential
and should be uninterned with caution, some of functions return true. E.g., if symbol denotes ap5 relation,
it is essential from ap5 viewpoint"
  )
                                     

; FIXME - ������ �� �� ����� ��������� ����� �������� � ����������
; ������������
(defmacro with-proplist-carat (var &body body)
  "� var ���������� property-list (:key value :key2 value2). ����� var^key � var^KEY ���������� � 
��� � getf* ��� ������ value"
  `(with-custom-carat-implementation 
    (,var (o f &rest mo) 
          `(prog1
               (getf* ,o ,f :test 'string-equal)
             (assert (assoc-getf* ,o ,f :test 'string-equal))))
    ,@body))

(setf (get 'with-proplist-carat 'proga-implementation::proga-transformer) 
      'proga-implementation::open-up)



