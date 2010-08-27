(in-package :budden-tools)
;;; with-conc-name 0.4
;;; Written by Denis Budyak, 2009-12-28
;;; Dedicated to my sister Ksenya
;;; Code is in the public domain

;;; (with-conc-name x conc-name- &body body) 
;;; Looks for symbols *package*::x.foo or :x.bar in body
;;; For each symbol found, if symbol conc-name-foo exist, 
;;; replaces x.foo with (conc-name-foo x)
#|
; examples (ccl-1.4 under windows xp) 
? (macroexpand-1 '(with-conc-name x symbol x.name))
(PROGN (SYMBOL-NAME X))

; you can also put var.accessor symbol to a keywords package 
? (macroexpand-1 '(with-conc-name x symbol- (list x.name :x.package)))
(PROGN (LIST (SYMBOL-NAME X) (SYMBOL-PACKAGE X)))

? (defstruct very-long-structure-type-name fld1 fld2)
VERY-LONG-STRUCTURE-TYPE-NAME
? (macroexpand-1 
    '(with-conc-name x very-long-structure-type-name-
        (list x.fld1 x.fld2)))
(PROGN (LIST 
        (VERY-LONG-STRUCTURE-TYPE-NAME-FLD1 X) 
        (VERY-LONG-STRUCTURE-TYPE-NAME-FLD2 X)))
T
|#

(defmacro with-conc-name (var conc-name &body body)
  (let* ((svar (string var))
         (len (length svar))
         (string-concname (string conc-name)))
   (labels 
       ((maybe-replace-var-with-conc-name (symbol)
          (let ((pkg (symbol-package symbol)))
            (or
              (and 
               (or (eq pkg *package*)
                   (eq pkg #.(find-package :keyword)))
               (let* ((sname (string symbol))
                      (snamelen (length sname))
                      )
                (and 
                 (> snamelen len)
                 (eql (mismatch sname svar) len)
                 (eql (elt sname len) #\.)
                 (let ((accessor-name
                       (find-symbol
                        (concatenate 'string 
                           string-concname
                           (subseq sname (1+ len)))
                           *package*
                           )))
                    (when accessor-name                                                       
                 `(,accessor-name
                   ,var))))))
               symbol)))
        (process-form (x)
         (typecase x
          (null nil)
          (symbol (maybe-replace-var-with-conc-name x))
          (cons
           `(,(process-form (car x))
             ,@(process-form (cdr x))))
          (t x))))
       `(progn ,@(process-form body))
       )))
