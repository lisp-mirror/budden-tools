; -*- coding: windows-1251-dos; -*-
(in-package :budden-tools)
(in-readtable :buddens-readtable)

(defparameter *defun-to-file-directory* #+win32 "l:/sw/defun-to-file" #-win32 "/home/denis/sw/defun-to-file")

(defun maybe-add-slash (string)
  #+russian "��������� / � ����� ����� �����, ���� ��� ��� ��� ���"
  (if (string= (sequence-last string) "/")
    string
    (str+ string "/")))

(defmacro defun-to-file (name &rest more)
  #+russian "���������� ������� � ����� ���������� � ����� � ������ *defun-to-file-directory*/���-�������.
��� ������� ������ ���� ���������� ������ ����� � �� ������ ��������� ������ ������� ��������. 
����� �� �������� ���� ��� ��� ������, �� ���� �� �������. � ������� �� ����� ���� gensyms, �.�. ��� �����
���������� (���, � �����-�� ������ ��� ���� �� ���). ���������� ��� �������� - ��� ������� � ��� �����"
  (proga 
    (assert (every #L(not (find !1 "\\/.?* ")) (string name)))
    (let filename (str+ (maybe-add-slash *defun-to-file-directory*) name))
    (proga
      (with-open-file out (str+ filename ".lisp") :direction :output
        :if-does-not-exist :create :if-exists :supersede)
;      (let *print-circle* t *print-readably* t *print-pretty* t)
;      (let *print-circle* t *print-pretty* t)
      (let *print-pretty* t)
      (print `(in-package ,(keywordize (package-name *package*))) out)
      (print `(in-readtable :buddens-readtable-a) out)
      (print `(defun ,name ,@more) out))
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
            
      
          