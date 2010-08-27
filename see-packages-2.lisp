; -*- coding: windows-1251-dos; -*-

(in-package :budden-tools)
(setf *readtable* (copy-readtable nil))

#+pascal-reader 
(defmacro see-packages (&rest packages)
  "������������� � ��������� ������������ in-package ������� � ���, ��� 
��� ������ � ��-�� ��� ����� ������������ ������ ������"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     #+ignore (unless (eq *readtable* *my-readtable*)
                (warn "setting right readtable for see-packages...")
                (setf *readtable* *my-readtable*))
     (setf *my-packages* (mapcar 'find-package ',packages))
     (format t "~%Now seeing packages ~S" ',packages))
  )

#+pascal-reader (defun reintern (pack num-of-colons nm)
  "������� ������ � ����� �� ������ �������� *package* � *my-packages*
���� ��� �� ������, �� ���������� ��� ��� ����. ���� ��������, �� ��������.
package-sym ���������� ������� ������, � ������� �� ������� ���. num-of-colons
����������, ������� ��������� ��������� ��� ������� �� ����� ������"
  (macrolet ((reject (strongp &rest ctrlstring-and-args)
               `(return-from reintern
                  (values nil ,strongp
                          ,(when ctrlstring-and-args
                             `(format nil ,@ctrlstring-and-args)))))
             (accept (type token)
               `(return-from reintern (values t ,type ,token))))
    (ecase num-of-colons
      (0  ; ����� �� ������. ���� ������ �� ���� �������, �� ������� �� ������ ������ � �����. 
       (iter
         (:with sym-found = nil)
         (:for p in (cons *package* *my-packages*))
         (:for (values p-sym storage-type) = (find-symbol nm p))
         (when (and p-sym  ; ������ 
                    (or (eq storage-type :external) ; ������ ���� ������� 
                        (:first-time-p)  ; ��� �� ������� � *package* � ����� �� ����� ���� ���������� ����
                        ))
          ; ���� � ��� ��������� ��������, �� ��� ����� ���������. 
           (unless (eq p-sym sym-found) ; ���� �� ���������, �� ��� ������� ����� ������ ����.
             (setf sym-found p-sym)
             (:count 1 :into cnt))
           (:collect p :into packs-found)
           )
         (:finally
          (return
           (case cnt
             (0 (accept 'symbol (intern nm *package*)))
             (1 (accept 'symbol sym-found))
             (t (reject t "symbol name ~A is ambigious between ~S" nm packs-found)))))))
      ((1 2)
       (multiple-value-bind (target-sym storage-type)
           (find-symbol nm pack)
         (cond
          ((null target-sym)
           (reject t "Symbol ~A::~A does not exist" pack nm))
          ((and (= num-of-colons 1)
                (not (eq storage-type :external)))
           (reject t "~A is not an external symbol in ~A" nm pack))
          (t (accept 'symbol target-sym))))))))


#+pascal-reader (setf (symbol-function 'reader::intern-symbol-with-package) #'reintern)

