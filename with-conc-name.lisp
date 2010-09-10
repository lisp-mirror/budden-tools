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

; TODO.
; можно ли сделать вызовы ф-й более одного параметра?
; (foo.add x) -> (foos-type-add foo x) ??? 
; или же (foo->add x) -> (foos-type-add foo x)

; TODO with-conc-type = with-conc-name + декларация типа
; вспомнить про null object pattern
#+old (defmacro with-conc-name (var conc-name &body body)
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


#+very-nil (defmacro with-conc-name (var conc-name &body body)
  (let* ((svar (string var))
         (len (length svar))
         (string-concname (string conc-name)))
   (labels 
       ((conc-name-p (symbol) ; returns either nil or (values accessor-name var)
          (let ((pkg (symbol-package symbol)))
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
                    (values accessor-name var))
                  ))))))
        (maybe-replace-var-with-conc-name (symbol)
          (print `(2 ,symbol))
          (multiple-value-bind (accessor-name var)
              (conc-name-p symbol)
            (if accessor-name
                `(,accessor-name ,var)
              symbol)))
        (maybe-replace-call-with-conc-name-call (call)
          (print `(1 ,call))
          (destructuring-bind (symbol &rest args) call
            (cond ((symbolp symbol)
                   (multiple-value-bind (accessor-name var)
                       (conc-name-p symbol)
                     (if accessor-name
                         `(,accessor-name ,var ,@args)
                       `(,@call))))
                  (t `(,@call)))))
        (process-form (x)
          (typecase x
            (null nil)
            (symbol (maybe-replace-var-with-conc-name x))
            (cons
             (setf x (maybe-replace-call-with-conc-name-call x))
             `(,(process-form (car x))
               ,@(process-form (cdr x))))
            (t x))))
       `(progn ,@(process-form body))
       )))


(defmacro with-conc-name (var conc-name &body body)
  (let* ((svar (string var))
         (len (length svar))
         (string-concname (string conc-name))
         (bindings1 nil) ; variable bindings (for symbol-macrolet)
         (bindings2 nil) ; function bindings (for macrolet)
         )
   (labels 
       ((conc-name-p (symbol) ; returns either nil or (values accessor-name var)
          (let ((pkg (symbol-package symbol)))
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
                    (values accessor-name var))
                  ))))))
        (maybe-note-symbol (symbol)
          (multiple-value-bind (accessor-name var) 
              (conc-name-p symbol)
            (when accessor-name
              (pushnew `(,symbol (,accessor-name ,var)) ; macrolet
                       bindings1 :test 'equalp)
              (with-gensyms (args)
                #+nil (pushnew `(,symbol (&rest ,args)
                                   `(,',accessor-name ,',var ,',args))
                         bindings2 :test 'equalp)
                (pushnew `(,symbol (&rest ,args)
                                   `(,',accessor-name ,',var ,@,args))
                         bindings2 :test 'equalp)
                 )))
          symbol)
        (process-form (x)
          (typecase x
            (null nil)
            (symbol (maybe-note-symbol x))
            (cons
             `(,(process-form (car x))
               ,@(process-form (cdr x))))
            (t x))))
     (process-form body))
   `(progn (macrolet ,bindings2 (symbol-macrolet ,bindings1 ,@body))))
  )


(defmacro let-with-conc-type (var type value &body body)
  "type must be an atom"
  `(let ((,var ,value))
     (declare (type ,type ,var))
     (with-conc-name ,var ,(symbol+ type '-)
       ,@body)))


(trivial-deftest::! #:let-with-conc-type.1
                    (proga (let-with-conc-type x string "asdf") 
                      `(,(x.equal "asdf") ,(x.upcase) ,(x.equal x.upcase))
                      )
                    '(T "ASDF" T))
