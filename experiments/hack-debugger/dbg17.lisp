;; ����� hack-debugger 5. 
;; ������� ���������: ����� �������� ����, �� ��� 
;; �������������� � ������������� �������.
;; ���� ������������ ������������. 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :dbg17
    (:use :cl :lispworks)
    (:export #:set-source-location-substitution
     #:with-source-location-substitutions
     #:*interesting-function-name*
     #:*w-table*
     #:*w-form*
     #:*address-substitution-table*
     #:END-SOURCE-LOCATION-SUBSTITUTIONS-FN
     #:BEGIN-SOURCE-LOCATION-SUBSTITUTIONS-FN
     #:*with-source-location-substitutions-level*
     )))

(in-package :dbg17)

(defmacro compiler-break ()
  (break "compiler-break ~A" COMPILER:*function-name*))

(defvar *frame* nil)

(defadvice (system::dbg-edit-frame-internal hap-frame :around)
    (frame pane)
  (setf *frame* frame)
  (call-next-advice frame pane))


;; ����, �� ����� ��������� ���� ��� ����������� ���� (�� � ��������, ��� �������� ���� ��������� � dbg5). 
;; � ��� �� ����� ���? 
    

#|
��������, ��� 
���������� COMPILER::*source-level-form-table* ���������� �
COMPILER::in-process-forms-in-file. 

����� compiler::process-form (��������� �����) - ��� �� ��������� ��������. 
������, ��� ���� � ������� around. �������? 

�� �������, �.�. �� ����� ��� ���������� �������� � ������� �������������. 
� ��� ����� ��������� �� �����. 

����� �������� �� compiler::wombat-2 
� ����� �������� � ��� �����������. 

�������� �������, ����� ���� �� �� ����� ��������. 
� ���������� wombat-2 ����� �������� � ���������� ���� � 

|#

;(defadvice (compiler::process-form hap-source-level-form-table :around)
;    (i-form)
;  (let ((result (call-next-advice i-form)))
;    (print COMPILER::*source-level-form-table* *trace-output*)
;    (capture-source-level-form-table)
;    result))

#| ��������� ���� ������� �� ������� � ������������������ �������
    (when (and first-cons-value second-cons-value)
      (maphash 
       (lambda (key value)
         (typecase value
           (number
            (when (eq key *second-cons*)
              (break "ura")
              (setf (gethash key hash) first-cons-value))
            (when (eq key *first-cons*) (setf (gethash key hash) second-cons-value))
            )
           (COMPILER::multiple-transforms-record
           ; ���������� ���
            )
           (t
           ; ���-�� ����������, ����� ������� warning
            )
          ))
       hash))))
|#

; �������� *first-cons*, ����� ����� �����, ��� �� ��� ���������
(defparameter *first-cons* nil)
(defmacro first-cons (&whole form x) (setf *first-cons* form) `(break ,x))

(defparameter *second-cons* nil)
(defmacro second-cons (&whole form x) (setf *second-cons* form) `(break ,x))

(defparameter *w-form* nil "����������� � wombat-2 �����")
(defparameter *w-table* nil "����������� � wombat-2 �������")

(defparameter *address-substitution-table*
  (make-hash-table :test 'eql)
  "���� - ����� ��������� ����, �������� - ����� �����, ��� ���� ��������. ���� �������� ������ ��� �����
 ������� *interesting-function-name*"
  )

(defparameter *interesting-function-name* 'y)

(defadvice (dbg::call-frame-edit-path hack-path
                                      :around
                                      :documentation
                                      "������ ������� ���� �������������� ��� ���������� �������")
    (c)
  (let (
        (original-path (call-next-advice c))
        maybe-new-path)
    (when (eq *interesting-function-name*
              (ignore-errors
                (slot-value *frame* 'dbg::function-name)))
      (setf maybe-new-path (gethash original-path *address-substitution-table*)))
    (or maybe-new-path 
        original-path)))

(defadvice (compiler::wombat-2 hack-two-conses :around)
    (form &optional (table COMPILER::*source-level-form-table*))
  (setf *w-table* table
        *w-form* form)
  (call-next-advice form table))


(defun find-source-address-in-a-hash (source)
  (gethash source *w-table*)) 

(defun particular-fill-address-substitution-table ()
  (let ((first-cons-address (find-source-address-in-a-hash *first-cons*))
        (second-cons-address (find-source-address-in-a-hash *second-cons*)))
    (assert (and first-cons-address second-cons-address))
    (setf (gethash first-cons-address *address-substitution-table*) second-cons-address)
    (setf (gethash second-cons-address *address-substitution-table*) first-cons-address)
    ))
           
(defun y () (first-cons "find-source ������� ������ �����")
  (second-cons "��, � ������ ������� ��������")
  ;(compiler-break)
  )


(defun test ()
  (break "����������� � ����� y() � ����� ��������")
  (particular-fill-address-substitution-table)
  (y)
  )


; ������� ����������� compiler::wombat-2 � �������������� f (����� compile defun)
