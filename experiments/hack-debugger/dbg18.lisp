;; ������� ������� dbg17.lisp
;; �������� ���������� � �������. 
(in-package :dbg17)

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
  "���� ������� - ����� �� ���������������� ����, 
   �������� - ����� � ��������� �� �����, �� ������� ����� ����� ������������")

(defvar *with-source-location-substitutions-level* nil "���� ������� �� 0, �� ����������� ���������")

(defun set-source-location-substitution (source-place real-code)
  "source-place - ����� � ��������� ���������, 
   real-code - �����, �-��� ����� ��� �����������.
   "
  (typecase *compile-time-substitution-table*
    (hash-table
     (unless (eq source-place real-code)
       (setf (gethash real-code *compile-time-substitution-table*) source-place)
       t
       ))
    (t
     (warn "set-source-location-substitution is out of with-source-location-substitutions scope")
     nil)
    )
  )

(defmacro first-cons-1 (&whole form x)
  "���� ������ ������� ������� � ��������"
  (setf *first-cons* form) `(break ,x))

(defmacro second-cons-1 (&whole form x)
  "� ����� �������� ������� ��������"
  (set-source-location-substitution form *first-cons*) `(list ,x))


(defun end-source-location-substitutions-fn ()
  "Converts map between conses to map between numeric addresses"
  (incf *with-source-location-substitutions-level* -1) 
  (unless (= *with-source-location-substitutions-level* 0)
    (return-from END-SOURCE-LOCATION-SUBSTITUTIONS-FN nil))
  ;(break)
  (setf *address-substitution-table* (make-hash-table :test 'eq))
  (maphash
   (lambda (real-code source-place)
     (let ((source-place-address (find-source-address-in-a-hash source-place)))
       (typecase source-place-address
         (integer
          (let ((real-code-address (find-source-address-in-a-hash real-code)))
            (typecase real-code-address
              (integer
               (unless (eql real-code-address source-place-address)
                 (setf (gethash real-code-address *address-substitution-table*)
                       source-place-address)))
              (t #+nil (warn "code-address not found for ~S" real-code)))))
         (COMPILER::MULTIPLE-TRANSFORMS-RECORD
          (warn "source-place-address=~S" source-place-address)
          )
         (t #+nil (warn "source-place address not found for ~S" source-place)))))
   *compile-time-substitution-table*)
  (print `("number of substitutions is" ,(hash-table-count *address-substitution-table*)) *trace-output*)
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
  (let ((*compile-time-substitution-table* *compile-time-substitution-table*)
        (*with-source-location-substitutions-level* (or *with-source-location-substitutions-level* 0)))
    (call-next-advice i-form)))
     

(defun begin-source-location-substitutions-fn ()
  (setf *compile-time-substitution-table*
        (or *compile-time-substitution-table* (make-hash-table :test 'eq)))
  (incf *with-source-location-substitutions-level*) 
  )

(defmacro begin-source-location-substitutions ()
  (begin-source-location-substitutions-fn)
  )

(defmacro end-source-location-substitutions ()
  (end-source-location-substitutions-fn)
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

#| ��� ����������� COMPILER::MULTIPLE-TRANSFORMS-RECORD

(COMPILER::TRANSFORM-FORM COMPILER::GET-FORM-PATH COMPILER::FIND-NODE-SOURCE-PATH 
COMPILER::FIND-NODE-FORM DBG::GENERATE-SCL-INFO DBG17:END-SOURCE-LOCATION-SUBSTITUTIONS-FN 
COMPILER::COMPILER-SOURCE-LEVEL-TRANSFORMATION-A COMPILER::NODE-REPLACING-NODE)

|#

(defun hack-source-level-form-table (real-code)
  "������� real-code � *COMPILE-TIME-SUBSTITUTION-TABLE* � ����������� �����
�� source-place � ����. ���������� ��� ��� ���������� ���, � ������ ��������� - t, ���� ������� ���������"
  (let* ((maybe-source-form
          (typecase *COMPILE-TIME-SUBSTITUTION-TABLE*
            (HASH-TABLE
             (gethash real-code *COMPILE-TIME-SUBSTITUTION-TABLE* real-code))
            (t
             real-code)))
         (maybe-other-address
          (and maybe-source-form
               (typecase COMPILER::*SOURCE-LEVEL-FORM-TABLE*
                 (hash-table
                  (gethash maybe-source-form COMPILER::*SOURCE-LEVEL-FORM-TABLE*
                           ))))))
    (typecase COMPILER::*SOURCE-LEVEL-FORM-TABLE*
      (hash-table
       (cond ((and (numberp maybe-other-address)
                   (not (eq MAYBE-SOURCE-FORM real-code))
                   (not (eq (gethash real-code COMPILER::*SOURCE-LEVEL-FORM-TABLE*)
                            MAYBE-OTHER-ADDRESS)))
              (setf (gethash real-code COMPILER::*SOURCE-LEVEL-FORM-TABLE*)
                    maybe-other-address)
              (values maybe-source-form t))
             (t
              (values maybe-source-form nil))))
      (t
       (values MAYBE-SOURCE-FORM nil)))))


(defadvice (compiler::get-form-path take-code-from-compile-time-substitution-table :around)
    (form)
  (CALL-NEXT-ADVICE (HACK-SOURCE-LEVEL-FORM-TABLE form))
  )

#|
(defadvice (COMPILER::FIND-NODE-SOURCE-PATH take-code-from-compile-time-substitution-table :around)
    (node)
  (when
      (and (SLOT-EXISTS-P node 'compiler::source)
           (SLOT-BOUNDP node 'compiler::source)
           )
    (HACK-SOURCE-LEVEL-FORM-TABLE (slot-value node 'compiler::source)))
  (CALL-NEXT-ADVICE node))

(remove-advice 'COMPILER::FIND-NODE-SOURCE-PATH 'take-code-from-compile-time-substitution-table)
|#
;(remove-advice 'compiler::get-form-path 'take-code-from-compile-time-substitution-table)
; 'DBG::GENERATE-SCL-INFO