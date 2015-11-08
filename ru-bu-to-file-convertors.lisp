;;; -*- Encoding: utf-8; -*-; 
;;; Преобразования файлов 

(in-package :russian-budden-tools)

(defun convert-file-code-page-to-utf (filename)
  (cl-fad:COPY-FILE filename (budden-tools:str++ filename ".bak"))
  (let ((s (budden-tools:READ-FILE-INTO-STRING filename :external-format :windows-1251)))
    (budden-tools:__f budden-tools:str++ ";; -*- encoding : utf-8; coding : utf-8-*- 
" s)
    (setf s (budden-tools::SEARCH-AND-REPLACE-SEQ 'string s (budden-tools::str++ (code-char 13)) "" :all t))
    (budden-tools:SAVE-STRING-TO-FILE s filename :external-format :utf-8)))

(defun convert-file-utf-remove-13 (filename)
  (cl-fad:COPY-FILE filename (budden-tools:str++ filename ".bak"))
  (let ((s (budden-tools:READ-FILE-INTO-STRING filename :external-format :utf-8)))
    (setf s (budden-tools::SEARCH-AND-REPLACE-SEQ 'string s (budden-tools::str++ (code-char 13)) "" :all t))
    (budden-tools:SAVE-STRING-TO-FILE s filename :external-format :utf-8)))

(defun convert-file-remove-utf-bom (filename)
  (cl-fad:COPY-FILE filename (budden-tools:str++ filename ".bak"))
  (perga-implementation:perga
    (:@ with-open-file (f1 filename :element-type '(unsigned-byte 8)))
    (let f1l (file-length f1))
    (let s (make-array (list f1l) :element-type '(unsigned-byte 8)))
    (read-sequence s f1)
    (close f1)
    (:@ with-open-file (f2 filename :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede))
    (write-sequence s f2 :start 3)
    filename
    ))
