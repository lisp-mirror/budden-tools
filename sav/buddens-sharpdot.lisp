;;; -*- Encoding: utf-8; -*-
;; read-eval-stream
(in-package :buddens-readtable)
(in-readtable :buddens-readtable)

;(defvar *read-eval-stream* "Something's wrong with #.? " "Bound to current stream in the scope of #. ") 
;
;(defvar *old-sharpsign-dot* (get-dispatch-macro-character #\# #\.));
;
;(defun new-sharpsign-dot (stream sub-char numarg)
;  (let ((*read-eval-stream* stream))
;    (funcall *old-sharpsign-dot* stream sub-char numarg)))
;
;(set-dispatch-macro-character #\# #\. #'new-sharpsign-dot)

