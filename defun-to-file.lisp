; -*- coding: windows-1251-dos; -*-
(in-package :budden-tools)
(in-readtable :buddens-readtable)

(defparameter *defun-to-file-directory* #+win32 (str+ cl-user::*lisp-root* "sw/defun-to-file") #-win32 "/home/denis/sw/defun-to-file")

(defun maybe-add-slash (string)
  #+russian "��������� / � ����� ����� �����, ���� ��� ��� ��� ���"
  (cond
   ((string= string "") string)
   ((string= (sequence-last string) "/") string)
   (t (str+ string "/"))))

(defmacro defun-to-file (name &rest more)
  #+russian "���������� ������� � ����� ���������� � ����� � ������ *defun-to-file-directory*/���-�������.
��� ������� ������ ���� ���������� ������ ����� � �� ������ ��������� ������ ������� ��������. 
����� �� �������� ���� ��� ��� ������, �� ���� �� �������. ��� ��������� 4 � ������� �� ����� ���� gensyms, �.�. ��� �����
���������� (���, � �����-�� ������ ��� ���� �� ���). ���������� ��� �������� - ��� ������� � ��� �����"
  (proga 
    (assert (every #L(not (find !1 "\\/.?* ")) (string name)))
    (let filename (str+ (maybe-add-slash *defun-to-file-directory*) name))
    (proga
      (with-open-file out (str+ filename ".lisp") :direction :output
        :if-does-not-exist :create :if-exists :supersede)
      #+lispworks6 (let *print-circle* t *print-readably* t *print-pretty* t)
;      (let *print-circle* t *print-pretty* t)
      #-lispworks6 (let *print-pretty* t)
      (print `(in-package ,(keywordize (package-name *package*))) out)
      (print `(in-readtable :buddens-readtable-a) out)
      (print `(defun ,name ,@more) out)
      )
    (assert (compile-file (str+ filename ".lisp")))
    `(values (load ,filename) ,filename)))


(defun eval-with-file (code) 
  "������ ��������� ������� � ����� � ��������� �"
  (proga 
    (let func-name (gentemp "eval-with-file-fn"))
    (multiple-value-bind (success filename) 
        (eval `(defun-to-file ,func-name () ,code))
      (unwind-protect
          (progn 
            (assert success nil "��� ~S �� ����������" code)
            (funcall func-name))
        (delete-file filename)
        (unintern func-name)
        ))))
            
      
          