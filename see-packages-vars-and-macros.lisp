; -*- coding: windows-1251-dos; -*- 

(in-package :budden-tools)
(setf *readtable* (copy-readtable nil))

(defparameter *my-readtable* nil "Readtable for testing see-packages")
(defparameter *good-readtable* (copy-readtable nil) "Sample initial readtable for tests")

(defvar *keyword-package* (find-package :keyword))

(defvar *package-seen-packages* (make-hash-table :test 'eq)
  "Additional packages to see from that package")

(defvar *my-readtable-to-good-readtable* (make-hash-table :test 'eq)
  "Maps altered readtable with reader extensions to their unaltered counterparts"
  )

(defvar *my-readtable-to-colon-readtable* (make-hash-table :test 'eq)
  "Maps altered readtable to their colon-readtable"
  )


(defvar *record-paren-locations* nil
  "When t, records locations of all objects starting from opening paren"
  )

(defmacro with-my-readtable-0 (&body body)
  `(let1 *readtable* *my-readtable*
     ,@body))

(defmacro with-good-readtable-0 (&body body) 
  "������������ ������ ��� ������������!"
  `(let1 *readtable* *good-readtable* ,@body))

(defmacro with-good-readtable-2 ((&key (ensure-this-is-a-bad-one t)) &body body)
  "���������� readtable ������ ���� �������� � ������� see-packages-on"
  (with-gensyms (good)
    `(proga
       (let ,good (gethash *readtable* *my-readtable-to-good-readtable*))
;       (print '(:good-readtable-is ,good))
       ,@(when ensure-this-is-a-bad-one
           `((assert ,good nil "with-good-readtable: ~A is not a hp-readtable" *readtable*)))
       (let *readtable* (or ,good *readtable*))
       ,@body
       )))


(defmacro with-fallback-reader-behaviour (&body body) 
  "�� ��������� ������ with-xlam-package � ���������� readtable. ��� ����� ��������� ���� � ��������, �����
�� ��� �� ���������. ��� ����� ��� ������ ��������� � lispworks"
  `(with-good-readtable-2 (:ensure-this-is-a-bad-one nil)
     (let1 *package* (or *real-package* *package*)
       ,@body)))

(defpackage :xlam-package (:use))
(defpackage :xlam-package-for-output (:use))
(defparameter *xlam-package* (find-package :xlam-package))
(defparameter *xlam-package-for-output* (find-package :xlam-package-for-output))

(defvar *in-with-xlam-package* nil)
(defvar *real-package* nil "�� ����� with-xlam-package ���� ����������� *package* � ������� let")
(defvar *last-used-real-package* nil "�� ����� with-xlam-package ���� ������������� ��������� ��������
*package* � ������� setf. ����������, ��������, �������")

(defvar *package-stack* nil "���� �������. ��� foo:: ����� <#package foo> �� ������� �����")
(defvar *colon-no-stack* nil "����, ������������ ����� �������, � �-��� ������� ����� �������")
(defvar *intern-to-qualified-package-silently* t "foo::bar ����� ��������� �� ����� �������")

(defvar *print-normalize-seen-symbols* t "Print seen symbols with package
prefix even if they can be read without the prefix")

#+nil (defmacro maybe-bind-package (pack &body body)
  "���� pack - ������, �� ��������� ����, �������� *package* � pack. 
��� ����, ������ �������� *pack* ��������� � *real-package*, � �����
������������ � ������� setf ���������� *last-used-real-package*. 
���� �� *package* = *xlam-package*, �� �� ������ *real-package* � *last-used-real-package*"
  (once-only (pack)
    `(let* ((*real-package* 
             (cond
              ((eq *package* *xlam-package*) *real-package*)
              (,pack *package*)
              (t *real-package*)))
            (*package* (or ,pack *package*)))
       (if *real-package* (setf *last-used-real-package* *real-package*))
       ,@body)))

(defmacro maybe-bind-package (pack &body body)
  "�������� ��� ������ pack::symbol, ��� ������� ������
pack �� ������������ ����� ����������
  ���� pack - ������, �� ��������� ����, �������� *package* � pack. 
��� ����, ������ �������� *pack* ��������� � *real-package*, � �����
������������ � ������� setf ���������� *last-used-real-package*. 
���� �� *package* = *xlam-package*, �� �� ������ *real-package* � *last-used-real-package*"
  (once-only (pack)
    `(let* ((*real-package* 
             (cond
              ((eq *package* *xlam-package*) *real-package*)
              (,pack *package*)
              (t *real-package*)))
            (*package* (or ,pack *package*)))
       (if *real-package* (setf *last-used-real-package* *real-package*))
       ,@body)))

#+nil
(defmacro with-xlam-package (&body body)
  `(maybe-bind-package *xlam-package*
     (proga
       (let *in-with-xlam-package* t)
       ,@body)))

(defmacro with-xlam-package (&body body)
  `(let ((*real-package* *package*)
         (*package* *xlam-package*)) ; FIX1 ��������� pllet (readtable-case *readtable*) :preserve
     (setf *last-used-real-package* *real-package*)
     ,@body))

(defmacro with-xlam-package-for-output (&body body)
  `(let ((*real-package* *package*)
         (*package* *xlam-package-for-output*))
     ,@body))
  


#+nil (defun force-find-package (package-designator)
  "Makes sure that package is found. If not, gives sane error message"
  (iter 
    (:for package = (find-package package-designator))
    (when package (return package))
    (cerror "Retry" "Tried to see non-existent package ~A" package-designator)
    ))


(defmacro %seen-package-list (package)
  `(gethash ,package *package-seen-packages*))

(defmacro package-seen-packages-list (package-designator)
  `(%seen-package-list (the* package (find-package ,package-designator))))

(defmacro see-packages (&rest packages)
  "������������� � ��������� ������������ in-package ������� � ���, ��� 
��� ������ � ��-�� ��� ����� ������������ ������ ������"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (see-packages-on *readtable*)
     (setf (%seen-package-list *package*) (mapcar 'force-find-package ',packages))
     ; (format t "~%Now seeing packages ~S" ',packages)
     )
  )


(defvar *number-of-colons*) ; ����� ������� � ��������� �������������, �-��� �� �������
(defvar *in-careful-token-reader* nil)
(defvar *reading-up-to-colons* nil) ; ������, ��� �� ������������, ��� ������. ���� ����� ���������, ��� ��� ������� ������
(defparameter *colon-readtable* (copy-readtable nil))



(defstruct*mc package-metadata
;  write-lock ; if t, attempt to create a symbol creates continuable error
;  read-lock ; if t, attempt to read a symbol creates a error
  custom-reader ; custom reader is a function with the same args as read. It is called 
                ; when reader is read in a context of package:: syntax. 
  custom-token-parsers ; Custom token parsers is a list of custom token parsers. 
                       ; Custom token parser is a function of 
                       ; (stream potential-symbol-name package) which 
                       ; returns two values. First value is t if token is 
                       ; parsed and nil otherwise. Second value is parsed token itself.
                       ; If custom token parsers are defined, package 
                       ; protection is not accomplished. 
                       ; Stream is at the end of the token at the time of the call.
                       ; Parsers are called from left to right until some parser returns t as its
                       ; secondary value. If no parser returns t, 
  )

(defparameter *per-package-metadata* (make-hash-table :test 'eq)
  "Mapping of keywordized package names to their metadata"
  )

(defmacro get-non-persistent-object-locations (object)
  `(gethash ,object *nplm))



#| �� ��� �� ��������
(defun def-symbol-readmacro-symbol ()
  (intern (symbol-name '#:def-symbol-readmacro) :budden-tools))

(defmacro do-def-symbol-readmacro (symbol (streamvar) &body body)
  "It is impossible to read symbol naming symbol-readmacro directly, so there is a hack. See def-symbol-readmacro-reader below"
  `(,(def-symbol-readmacro-symbol) ,symbol (,streamvar) ,@body))

(eval 
 `(dspec:define-form-parser ,(def-symbol-readmacro-symbol) (name &rest args)
    (declare (ignore ,(def-symbol-readmacro-symbol) args))
    name))

(eval 
 `(defmacro ,(def-symbol-readmacro-symbol) (symbol (streamvar) &body body)
    "It is a temporary macro binding. It is a symbol-readmacro indeed. See def-symbol-readmacro-reader"
    (with-gensyms (sym)
      `(dspec:def ,symbol 
         (setf (symbol-readmacro ',symbol)
               (lambda (,streamvar ,sym) 
                 (declare (ignore ,sym))
                 ,@body))))))
|#