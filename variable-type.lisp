;;; -*- Encoding: utf-8; -*-
(in-package :budden-tools)

(named-readtables:in-readtable nil)

; (defmacro pvi (var &ENVIRONMENT env) (break) (print (list (hcl:variable-information var env) (hcl:declaration-information 'type))) `',var)

(defmacro print-environment (&optional break &ENVIRONMENT env) (print env) (if break (break)))

#+sbcl 
(defun normalize-type (type)
  (cond
    ((SB-KERNEL:STRUCTURE-CLASSOID-P type)
     (sb-kernel:classoid-name type))
    ((sb-kernel:named-type-p type)
     (sb-kernel:named-type-name type))
    ((sb-kernel:union-type-p type)
     (sb-kernel:type-specifier type))
    (t
     type)))

  

(defun variable-type-or-class (VAR ENV) 
  "Возвращает тип ИЛИ класс значения. Непонятно, зачем нужен класс - наверняка всегда достаточно типа для ^ (FIXME)"
  #+sbcl 
  (cond
   ((constantp var env)
    (normalize-type (type-of var))) ; в лиспворксе здесь возвращается класс... 
   (env
    (assert (typep env 'sb-kernel:lexenv))
    (let* ((vars (sb-c::lexenv-vars env))
           (this-var (cdr (assoc var vars))))
      (when this-var
        (etypecase this-var
          (sb-c::lambda-var
           (let ((result (sb-c::lambda-var-type this-var)))
             (normalize-type result)))
          (sb-c::global-var
          ; there is sb-c::global-var-defined-type also
           (let ((result (sb-c::global-var-type this-var)))
             (normalize-type result)))
           
          )))))
  #+:LISPWORKS4.4
  (cond
   ((constantp VAR ENV)
    (class-of var))
   (ENV
    (let1 remote-environment (slot-value env 'lexical::remote-environment)
      (when remote-environment 
        (let1 variable-types
            (iter (:for venv in (slot-value remote-environment 'compiler::venv))
              (ignored (struct-to-alist venv))
              (:collect `(,(slot-value venv 'compiler::name)
                          ,(slot-value venv 'type))))
          (cadr (assoc var variable-types))))
      )))
  #+lispworks6
  (cond 
   ((constantp var env)
    (class-of var))
   (env
    (let1 venvs (slot-value env 'compiler::venv)
      (when venvs
        (let1 variable-types
            (iter (:for venv in venvs)
              (ignored (struct-to-alist venv))
              (:collect `(,(slot-value venv 'compiler::name)
                          ,(slot-value venv 'type))))
          (cadr (assoc var variable-types))))))
   );cond
  #-(or :LISPWORKS4.4 :lispworks6 sbcl)
  (when (constantp var env) (class-of var))
  )


(defmacro kind-of-variable-via-augmented-environment (VAR &ENVIRONMENT ENV)
  "Из http://www.lispworks.com/documentation/lw50/CLHS/Issues/iss342_w.htm, пока не используется, не возвращает
декларацию типа. Вроде заработало в 6.1"
  #+lispworks (MULTIPLE-VALUE-BIND (KIND BINDINGP DECLS)
      (HCL:VARIABLE-INFORMATION VAR ENV)
    `(LIST ',VAR ',KIND ',BINDINGP ',DECLS)))
#|test 

 (progn (defun foo (x) (declare (integer x)) (print (budden-tools::kind-of-variable-via-augmented-environment x)) x)
  (compile 'foo) (foo 45))

|#

(defmacro print-variable-type-or-class (var &environment env)
  (print (variable-type-or-class var env))
  var)

(defmacro print-variable-information (var &environment env)
  #+lispworks (print (hcl:variable-information var env))
  #-lispworks (warn "print-variable-information not implemented in this CL implementation")
  )


; (defun foo (x) (declare (integer x)) (print-variable-type-or-class x))


(defun conc-prefix-by-type-or-class-name (type class)
  (let* ((type-package (symbol-package type)))
    (cond
     ((null class) (values (str+ type "-") type-package)) ; might fail
     (t 
      (let1 class-name (class-name class)
        (cond
         ((typep class 'structure-class)
          (values (str+ class-name "-") type-package))
         ((subtypep class (find-class 'string))
          (values (str+ 'string "-") (find-package :common-lisp)))
         ((subtypep class (find-class 'character))
          (values (str+ 'char "-") (find-package :common-lisp)))
         ((eq class-name 'package)
          (values (str+ 'package "-") (find-package :common-lisp)))
         ((subtypep class (find-class 'array))
          (values (str+ 'array "-") (find-package :common-lisp)))
         ((typep class (find-class 'standard-class))
          (values (str+ class-name "-") type-package))
         (t (error "conc-prefix for class ~S is undefined" class-name))
         ))))))

(defun conc-prefix-by-type-or-class (type-or-class)
  "Возвращает два значения: префикс и пакет, в котором надо искать символ с таким префиксом"
  (multiple-value-bind (type class)
      (typecase type-or-class
        (symbol ; это - тип
         (values type-or-class nil))
        (class ; это - класс
         (values (class-name type-or-class) type-or-class))
        (t ; хм.
         (break)
         (error "can't derive conc-prefix for type ~S" type-or-class)))
    (conc-prefix-by-type-or-class-name type class)))
  

(trivial-deftest::! #:conc-prefix-by-class-1
                    (mapcar 
                     (lambda (x) 
                       (multiple-value-bind (s p) (conc-prefix-by-type-or-class x)
                         (list s (package-name p))))
                     (list (class-of "asdf")
                           (find-class 'pathname)
                           (class-of #(1 3 4))
                           (class-of #\q)))
                    '(("STRING-" "common-lisp" )
                      ("PATHNAME-" "common-lisp" )
                      ("ARRAY-" "common-lisp")
                      ("CHAR-" "common-lisp"))
                    :test 'equalp)


; ---------------------------------------------------------------- runtime^ --------------------------------------------------------------

; должна возвращать два значения - 1 имя функции. 2 t, если нужно передать вторым параметром имя поля
(defgeneric function-symbol-for-^ (type-or-class field-name))

(defmethod function-symbol-for-^ (type-or-class field-name)
  "возвращает функцию для выполнения ^"
  (multiple-value-bind (prefix package) (conc-prefix-by-type-or-class type-or-class)
    (let* ((target-symbol-name (str+ prefix field-name))
           (target-symbol (find-symbol target-symbol-name package)))
      (assert target-symbol () "(runtime^ ~S ~S): symbol name ~S not found in ~S" 
        type-or-class field-name target-symbol-name package)
      (unless (fboundp target-symbol) (warn "(runtime^ ~S ~S): ~S should be a function" 
                                            type-or-class field-name target-symbol))
        ; а если это макрос? (assert (null (macro-function target-symbol)))?
      target-symbol
      )))
                            
; we need this as we attach symbol-readmacro on ^ so that it can't be 
(defun runtime^ (object field-name &rest args)
  "Вызывается, если на этапе компиляции не удалось определить тип объекта"
  (assert object () "(runtime^ nil ~S): Sorry, object can't be null!" field-name)
  (let* ((class (class-of object)))
    (multiple-value-bind (target-function-symbol field-p)
        (function-symbol-for-^ class field-name)
      (apply target-function-symbol object
             (if field-p (cons field-name args)
               args))
      )))

(defmacro common-carat-implementation (object field-name &rest args)
  "See also def-compiler-macro common-carat-implementation"
  `(runtime^ ,object ',field-name ,@args))

(define-compiler-macro common-carat-implementation (object field-name &rest args &environment env)
  "See also defmacro common-carat-implementation"
  (let ((variable-type-or-class (variable-type-or-class object env)))
    (case variable-type-or-class
      ((t nil) `(runtime^ ,object ',field-name ,@args))
      (t
       (multiple-value-bind (function-symbol field-p)
           (function-symbol-for-^ variable-type-or-class field-name)
         `(,function-symbol ,object ,@(when field-p `(,field-name)) ,@args))
       ))))

(defmacro carat-implementation (object field-name &rest args)
  "This implementation can be shadowed in with-custom-carat-implementation"
  `(common-carat-implementation ,object ,field-name ,@args))

(defun setf-apply (new-value function-name args)
  "Позволяет присвоить значение, имея на входе известное в момент исполнения имя функции для получения места, например, 
  (let1 v '(1 3 4)
             (bu::setf-apply 0 'car v)
             v) => (0 3 4).
  Вызывает eval!!!"
  (eval `(setf (,function-name ,@args) ',new-value)))

(defsetf runtime^ (object field-name &rest args) (new-value)
  "Вызывает eval в runtime!"
  ;(assert (constantp object env))
  (with-gensyms (o target-function-symbol class)
    (once-only (object)
      `(progn
         (let* ((,o ,object)
                (,class (class-of ,o))
                (,target-function-symbol (function-symbol-for-^ ,class ,field-name)))
           (assert ,o () "(^set nil ~S): Sorry, object can't be null!" ,field-name)
           (setf-apply ,new-value ,target-function-symbol (cons ,o ,args))
           )))))

#| Скорость:
(progn
  (defstruct qq Aa)
  (defparameter -v- (make-qq))
  (defun setit2 () (setf (QQ-Aa -v-) 234234))
  (defun setit () (setf -v-^Aa 234234))
  (compile 'setit)
  (compile 'setit2)
  (time (dotimes (i 100000) (setit)))
;user time    =      0.703
  (time (dotimes (i 100000) (setit2)))
;user time    =      0.015
  (/ 0.703 0.015)
;46.86666666666667
  )
|#




;--------------------------------------------------------------      Читалка   ^ -----------------------------------------------------------
(defparameter ^-reader nil "Этот параметр нужен только для тогО, чтобы было возможно с помощью Alt-. Alt-, найти определение функции ^,которую невозможно прочитать")

(defmacro |^| (object field-name &rest args)
  "See also ^-READER"
  `(carat-implementation ,object ,field-name ,@args))
      
#|(defmacro with-the1 (var type object &body body)
  "Combines type declaration, type check and binding. See also :lett perga clause" 
  `(let ((,var (the* ,type ,object)))
     (declare (type ,type ,var))
     ,@body))|#

(defun assert-special-subtype-for-with-the1 (var env new-type) 
  "If variable is special, checks that with-the1 does not redeclare it with incompatible subtype"
  (multiple-value-bind (kind localp decls)
      (#+lispworks
       hcl:variable-information
       #+sbcl
       sb-cltl2:variable-information
       var env)
    (declare (ignore localp))
    (when (eq kind :special)
      (let ((global-declared-type (cdr (assoc 'type decls))))
        (when global-declared-type
          (assert (subtypep new-type global-declared-type)
              ()
            "Special variable ~S was declared as having type ~S in surrounding scope, but is being bound to incompatible type ~S"
            var global-declared-type new-type))))))


#|
SBCL
sb-c::lambda-var - параметр таков и локальная перемненая тоже
sb-c::global-var :kind :special - связанная глоб.перем
но у нас есть и sb-cltl2:variable-information
|#

(proclaim '(notinline keep-var-for-debug-fn))

(defun keep-var-for-debug-fn (object)
  object)

(defmacro keep-var-for-debug (object)
  "Пытаемся гарантировать, чтобы в конструкции
   (let ((z (keep-var-for-debug x)))
     (break)) 
  переменная z была видна в отладчике при политике отладки >=2"
  (if (>= (or (sbcl-policy-level 'debug) 0) 2)
    `(keep-var-for-debug-fn ,object)
    object))

(defmacro with-the1 (var type object &body body &environment env)
  "Combines type declaration, type check and binding. See also :lett perga clause" 
  (assert-special-subtype-for-with-the1 var env type)
  (let ((expr 
         #-sbcl
         (let ((the-symbol (gensym (format nil "~A" var))))
           `(let ((,the-symbol ,object)) ; expanded `(the* ,type ,object)
              (assert (typep ,the-symbol ',type))
              (the ,type ,the-symbol)))
         #+sbcl
         (keep-var-for-debug object)))
    `(let ((,var ,expr))
       (declare (type ,type ,var)) ; нужно ли это в SBCL? 
       ,@body)))


(setf (get 'with-the1 'proga-implementation::proga-transformer) 
      'proga-implementation::open-up-if-4
      )
; see (def-perga-clause :lett) in perga.lisp

     

(defmacro with-freezed-env (&environment env &body body) 
  "Благодаря наличию именованного символа с замороженным env, можно сослаться на 
затенённый экземпляр макросов"
  `(symbol-macrolet 
       ((the-freezed-env ,env)) 
     ,@body))
  
(defmacro with-custom-carat-implementation ((object lambda-list &body expansion) &body body)
  "when object is eq to a given symbol, runs custom expansion"
  (assert (symbolp object))
  `(with-freezed-env
     (macrolet ((carat-implementation (&whole form ,@lambda-list)
                  (if (eq ',object ,(car lambda-list))
                      (progn ,@expansion)
                    (macroexpand-1 form the-freezed-env))))
       ,@body)))

(def-trivial-test::! custom-carat-implementation.1 
                     (let1 *readtable* 
                         (find-readtable nil)
                       (eval 
                        (read-from-string 
                         "(with-custom-carat-implementation 
                              (a (o f &rest mo) `(str+ ',o ',f))
                            (|^| a b))"))
                       )
                     "AB")


(defmacro with-conc-namec (var conc-name &body body)
  "var^something превращается в conc-name-something var"
  `(with-custom-carat-implementation
    (,var (o f &rest mo)
          (let* ((target-name (str+ ',conc-name "-" f))
                 (target-package (symbol-package ',conc-name))
                 (target-symbol (budden-tools-find-symbol target-name target-package)))
            (unless target-symbol 
              (error "with-conc-namec: symbol ~S not found in ~S" target-name target-package))
            `(,target-symbol ,o ,@mo)))
    ,@body))


(setf (get 'with-conc-namec 'proga-implementation::proga-transformer) 
      'proga-implementation::open-up)


        
(defmacro deftvar (var type &key documentation (initial-value nil initial-value-supplied-p))
  "def-typed-var. variable is proclaimed to have a type give. 
  In Lispworks the following facts would hold:
  1. Setting or binding to an incompatible type in a compiled code would give a runtime error (may vary dependent on safety compiler setting). 
     No checks in interpreted code. 
  2. with-the1 won't allow rebinding with another type. let + declare would allow. 
  If this is an insufficient protection, consider giving variable a functional form. E.g. 
  
  (defmacro def-pseudo-var (name inner-name)
    `(progn
       (defvar ,inner-name)
       (defconstant ,name nil)
       (defun ,name () ,inner-name)
       (defun setname (new-value) (do-check-type) (setf ,inner-name new-value))
       (defsetf ,name setname)))
  "  
  `(progn
     (defvar ,var ,@(when initial-value-supplied-p `((the* ,type ,initial-value))))
     (proclaim '(type ,type ,var))
     ,@(when documentation `((setf (documentation ',var 'variable) ,documentation)))
     ',var))

#+lispworks6
(dspec:define-dspec-alias deftvar (name) `(defvar ,name))

(defmacro deftparameter (var type &key documentation (initial-value nil initial-value-supplied-p))
  "def-typed-parameter. See detfvar"  
  `(progn
     (defparameter ,var ,@(when initial-value-supplied-p `((the* ,type ,initial-value))))
     (proclaim '(type ,type ,var))
     ,@(when documentation `((setf (documentation ',var 'variable) ,documentation)))
     ',var))

#+lispworks6
(dspec:define-dspec-alias deftparameter (name) `(defvar ,name))



; example (deftvar *test1* integer :initial-value 123 :documentation "asfd")


