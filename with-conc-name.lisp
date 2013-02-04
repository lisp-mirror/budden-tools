;;; -*- Encoding: utf-8; -*-
;; DO NOT USE, USE ^ EXPANDER INSTEAD

(in-package :budden-tools)
;;; with-conc-name 0.4
;;; Written by Denis Budyak, 2009-12-28
;;; Dedicated to my sister Ksenya
;;; Code is in the public domain

;;; (with-conc-name x conc-name- &body body) 
;;; Looks for symbols *package*::x.foo or :x.bar in body
;;; For each symbol found, if symbol conc-name-foo exist, 
;;; replaces x.foo with (conc-name-foo x), (x.foo . args) with (conc-name-foo x . args)


#+new-no-success (defmacro with-conc-name (var conc-name &body body)
  ; iterate не понимает macrolet и symbol-macrolet
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

(defmacro with-conc-name (var conc-name &body body)
  "Теперь оно пытается игнорировать регистр символов "
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
                (eql (mismatch sname svar :test 'char-equal) len)
                (eql (elt sname len) #\.)
                (let ((accessor-name
                       (find-symbol-with-advanced-readtable-case
                        (string-upcase
                         (concatenate 'string 
                                      string-concname
                                      (subseq sname (1+ len))))
                        *package*
                        *readtable*
                        nil ; хм? FIXME разобраться, что значит этот nil. 
                        )))
                  (when accessor-name                                                       
                    (values accessor-name var))
                  ))))))
        (maybe-replace-var-with-conc-name (symbol)
          (multiple-value-bind (accessor-name var)
              (conc-name-p symbol)
            (if accessor-name
                `(,accessor-name ,var)
              symbol)))
        (maybe-replace-call-with-conc-name-call (call)
          `(maybe-replace-call-with-conc-name-call ,call)
          (destructuring-bind (symbol &rest args) call
            (cond ((symbolp symbol)
                   (multiple-value-bind (accessor-name var)
                       (conc-name-p symbol)
                     (if accessor-name
                         `(,accessor-name ,var ,@args)
                       `(,@call))))
                  (t `(,@call)))))
        (process-cdr (x)
          `(process-cdr ,x)
          (typecase x
            (null nil)
            (symbol (maybe-replace-var-with-conc-name x))
            (cons
             `(,(process-form (car x))
               ,@(process-cdr (cdr x))))))
        (process-form (x)
          `(process-form ,x)
          (typecase x
            (null nil)
            (symbol (maybe-replace-var-with-conc-name x))
            (cons
             (setf x (maybe-replace-call-with-conc-name-call x))
             `(,(process-form (car x))
               ,@(process-cdr (cdr x))))
            (t x))))
       `(progn ,@(loop :for form :in body :collect (process-form form)))
       )))


; TODO вспомнить про null object pattern
(defmacro let-with-conc-type (var type value &body body)
  "type must be an atom"
  (assert (typep type 'symbol) () "let-with-conc-type: ожидали имя типа, получили ~S" type)
  `(let ((,var ,value))
     (declare (type ,type ,var))
     (with-conc-name ,var ,(symbol+ type '-)
       ,@body)))


; #+see-packages
;(trivial-deftest::! #:let-with-conc-type.1
;                    (let-with-conc-type x string "asdf"
;                      `(,(x.equal "asdf") ,(x.upcase) ,(x.equal x.upcase))
;                      )
;                    '(T "ASDF" T))

; FIXME - тест требует see-packages, но они ещё не загружены
