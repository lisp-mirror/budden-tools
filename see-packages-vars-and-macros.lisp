; -*- coding: windows-1251-dos; -*- 

(in-package :budden-tools)
(setf *readtable* (copy-readtable nil))

(defparameter *my-readtable* nil "Readtable for testing see-packages")
(defparameter *good-readtable* (copy-readtable nil) "Sample initial readtable for tests")

(defvar *keyword-package* (find-package :keyword))

(defvar *package-seen-packages* (make-weak-key-hash-table :test 'eq)
  "Additional packages to see from that package")

(defvar *my-readtable-to-good-readtable* (make-weak-key-hash-table :test 'eq)
  "Maps altered readtable with reader extensions to their unaltered counterparts"
  )

(defvar *my-readtable-to-colon-readtable* (make-weak-key-hash-table :test 'eq)
  "Maps altered readtable to their colon-readtable"
  )

(defvar *readtable-case-is-upcase-if-uniform* (make-weak-key-hash-table :test 'eq)
  "���� readtable - � ���� �������, �� ��� ���� ��������� ������ ������� ������ ���. ��� ���� readtable-case ����� ��������� � preserve"
  )

(defvar *record-paren-locations* nil
  "When t, records locations of all objects starting from opening paren - not implemented, but see 
  PAREN-READER-WITH-CLOSING-PAREN-NOTIFICATION where file locations are stored on stack while reading is accomplished"
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

; FIXME. �� ����� ����, ��� ����������� ������������������. 
; ����� ���-�� ������. ��������, ������������ property-list �������� (�� ���������������)
; ��� ������ read ��������-�����������. ������ readmacro-character �������� ������ ����������� � ��������� ��� ����������. 
; � ����� ��������� ���������� *��-������-������-����*
; ����������, ���������� read ���������� � ���������� *��-������-������-����* �, � ����������� �� �����
; ���� ���� ��-�������. ��� �������� - ������ � ���, ��� �� ��������� *package*? 
(defmacro with-fallback-reader-behaviour (&body body) 
  "�� ��������� ������ with-xlam-package � ���������� readtable. ��� ����� ��������� ���� � ��������, �����
�� ��� �� ���������. ��� ����� ��� ������ ��������� � lispworks."
  `(with-good-readtable-2 (:ensure-this-is-a-bad-one nil)
     (let1 *package* (or *real-package* *package*)
       ,@body)))

(defpackage :xlam-package (:use))
(defpackage :xlam-package-for-output (:use))
(defparameter *xlam-package* (find-package :xlam-package))
(defparameter *xlam-package-for-output* (find-package :xlam-package-for-output))

(defvar *in-with-xlam-package* nil "FIXME - �� ������������, ��������� � �������")
(defvar *real-package* nil "�� ����� with-xlam-package ���� ����������� *package* � ������� let")
(defvar *last-used-real-package* nil "�� ����� with-xlam-package ���� ������������� ��������� ��������
*package* � ������� setf. ����������, ��������, �������")

(defvar *have-colon* nil "���� ��������� ��������� ����� - ��� ���������, �������� � ������������ ������, �� t")
(defvar *package-stack* nil "���� �������. ��� foo:: ����� ������� �����, � ����� <#package foo> �� ������� �����")
(defvar *colon-no-stack* nil "����, ������������ ����� �������, � �-��� ������� ����� �������")
; (defvar *intern-to-qualified-package-silently* nil "foo::bar ����� ��������� �� ����� �������") - moved to per-package-metadata

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

(defmacro with-xlam-package (&body body) "�������� �� with-xlam-package-2"
  `(let ((*real-package* *package*)
         (*package* *xlam-package*)) 
     (setf *last-used-real-package* *real-package*)
     ,@body))

(defmacro with-xlam-package-2 (temp-rt &body pbody)
  `(proga
     (let real-rt *readtable* *real-package* *package*)
     (let *readtable* ,temp-rt)
     (let *package* *xlam-package*)
     (setf *last-used-real-package* *real-package*)
     (pllet1 (readtable-case *readtable*)
         (xlam-package-readtable-case real-rt))
     ,@pbody))

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


(defmacro in-readtable-upcase-if-uniform (rt-designator)
  `(progn
     (in-readtable ,rt-designator)
     (setf (readtable-case-advanced *readtable*) :upcase-if-uniform)))

(defvar *number-of-colons*) ; ����� ������� � ��������� �������������, �-��� �� �������
(defvar *in-careful-token-reader* nil)
(defvar *reading-up-to-colons* nil) ; ������, ��� �� ������������, ��� ������. ���� ����� ���������, ��� ��� ������� ������
(defparameter *colon-readtable* (copy-readtable nil))





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