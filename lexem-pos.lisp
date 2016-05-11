;; -*- encoding : utf-8; coding : utf-8; system :see-packages; -*- 


(def-merge-packages::! :LEXEM-POS
  (:always t)
  (:documentation "Положение лексемы")
  (:use :cl  
        )
  (:export "  
   LEXEM-POS:LEXEM-POS
   LEXEM-POS:MAKE-LEXEM-POS
   LEXEM-POS:LEXEM-POS-P
   LEXEM-POS:LEXEM-POS-START
   LEXEM-POS:LEXEM-POS-END
   LEXEM-POS:LEXEM-POS-FILE-NAME
   LEXEM-POS:LEXEM-POS-START-LINE-NUMBER
   "))

(in-package :lexem-pos)
(named-readtables::in-readtable nil)

(defstruct lexem-pos file-name start end start-line-number) ; FIXME - записпользовать

(defmethod cl:make-load-form ((self lexem-pos) &optional env)
  (make-load-form-saving-slots self :environment env))

