(in-package :dbg17)

(defun r ()
  (with-source-location-substitutions
   (with-source-location-substitutions
    (progn
      (first-cons-1 "find-source ������� ������ �����")
      (second-cons-1 "��, � ������ ������� ��������")
      ))))
  
(defun test ()
  (setf *INTERESTING-FUNCTION-NAME* 'r)
  (break "����������� r � ������� compile-defun � ��������")
  (r))
