;; -*- coding: utf-8 ; -*- ;; 

(def-merge-packages::!
 :multipaste
 (:always t)
 (:use :cl :budden-tools :editor-budden-tools :iterate-keywords)
 (:export 
  "MULTIPASTE:*CLIP*
  MULTIPASTE:DO-MULTIPASTE
 ")
 (:local-nicknames :bu :budden-tools :b :budden)
 )

(in-package :multipaste)
(in-readtable :buddens-readtable-a)

(defparameter *clip* 
  '(
   
   ))    
  
(defun nth-name-from-0 (n) 
  (make-string 1 :initial-element (code-char (+ n (char-code #\0)))))

(defun safe-beginning-of-string-for-wil (s)
  (coerce 
   (iter (:for c :in-string s)
     (:for i :from 0)
     (:while (< i 50))
     (:for cc = (case c
                  (#\Tab #\~)
                  (#\Newline #\~)
                  (#\" #\~)
                  (t c)))
     (:collect cc))
   'string))


(defun make-list-for-ask-from-strings (list-of-strings)
  (iter 
    (:for s :in list-of-strings)
    (:for i :from 0)
    (:collecting (str+ (nth-name-from-0 i) " " (safe-beginning-of-string-for-wil s)))
    ))
    
    

(defun do-multipaste ()
  (proga body
    (let choice-list `(" push current cliboard contents" ,@(make-list-for-ask-from-strings *clip*)))
    (let data (capi:prompt-with-list choice-list "Paste"))
    (unless data (return-from body nil))
    (let pos (position data choice-list :test 'equal))
    (cond 
     ((= pos 0)
      (setf *clip* `(,(CAPI-WIN32-LIB::GET-CLIPBOARD-TEXT) ,@(subseq1 *clip* 0 8)))
      (print `(defparameter *clip* ',*clip*)))
     (t 
      (let real-data (elt *clip* (- pos 1)))
      (text-to-clipboard real-data)))))

