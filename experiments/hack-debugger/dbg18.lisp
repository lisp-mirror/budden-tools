;; ������� ������� dbg17.lisp
;; �������� ���������� � �������. 
(in-package :cl-user)

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

; �������� *first-cons*, ����� ����� �����, ��� �� ��� ���������
(defparameter *first-cons* nil)

(defvar *compile-time-substitution-table* nil
  "���� ������� - ����� � ��������� ���������, �������� - �� �����, �������
   ����� �� ���� ����� ������������")

(defun set-source-location-substitution (real-code source-place)
  "source-place - ����� � ��������� ���������, 
   real-code - �����, �-��� ����� ��� �����������"
  (typecase *compile-time-substitution-table*
    (hash-table
     (setf (gethash source-place *compile-time-substitution-table*) real-code)
     )
    (t (warn "set-source-location-substitution is out of with-source-location-substitutions scope"))
    ))

(defmacro first-cons-1 (&whole form x)
  "���� ������ ������� ������� � ��������"
  (setf *first-cons* form) `(break ,x))

(defmacro second-cons-1 (&whole form x)
  "� ����� �������� ������� ��������"
  (set-source-location-substitution *first-cons* form) `(list ,x))


(defun end-source-location-substitutions-fn ()
  "Converts map between conses to map between numeric addresses"
  (setf *address-substitution-table* (make-hash-table :test 'eq))
  (maphash
   (lambda (source-place real-code)
     (let ((source-place-address (find-source-address-in-a-hash source-place)))
       (typecase source-place-address
         (integer
          (let ((real-code-address (find-source-address-in-a-hash real-code)))
            (typecase real-code-address
              (integer
               (setf (gethash real-code-address *address-substitution-table*)
                     source-place-address))
              (t #+nil (warn "code-address not found for ~S" real-code)))))
         (t #+nil (warn "source-place address not found for ~S" source-place)))))
   *compile-time-substitution-table*)
  nil)



#|  (let ((first-cons-address (find-source-address-in-a-hash *first-cons*))
        (second-cons-address (find-source-address-in-a-hash *second-cons*)))
    (assert (and first-cons-address second-cons-address))
    (setf (gethash first-cons-address *address-substitution-table*) second-cons-address)
    (setf (gethash second-cons-address *address-substitution-table*) first-cons-address)
    )) |#
      
(defadvice (COMPILER::process-form bind-compile-time-substitution-table :around
                                   :documentation "Isolates *compile-time-substitution-table* variable from other processes")
    (i-form)
  (let ((*compile-time-substitution-table* *compile-time-substitution-table*))
    (call-next-advice i-form)))
     

(defun begin-source-location-substitutions-fn ()
  (setf *compile-time-substitution-table*
        (or *compile-time-substitution-table* (make-hash-table :test 'eq)))
  )

(defmacro begin-source-location-substitutions ()
  (begin-source-location-substitutions-fn)
  )

(defmacro end-source-location-substitutions ()
  (end-source-location-substitutions-fn)
  (print *address-substitution-table*)
  )

(setf *interesting-function-name* 'r)

#|(defun r ()
  (begin-source-location-substitutions)
  (progn
    (first-cons-1 "find-source ������� ������ �����")
    (second-cons-1 "��, � ������ ������� ��������"))
  (end-source-location-substitutions))|#
  

(defmacro with-source-location-substitutions (form)
  `(progn 
     (begin-source-location-substitutions)
     (multiple-value-prog1
         ,form
       (end-source-location-substitutions))))

(defun r ()
  (with-source-location-substitutions
   (with-source-location-substitutions
    (progn
      (first-cons-1 "find-source ������� ������ �����")
      (second-cons-1 "��, � ������ ������� ��������")
      ))))
  
