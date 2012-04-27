(in-package :budden-tools)

; (defmacro pvi (var &ENVIRONMENT env) (break) (print (list (hcl:variable-information var env) (hcl:declaration-information 'type))) `',var)

(defmacro print-environment (&optional break &ENVIRONMENT env) (print env) (if break (break)))

(defun variable-type-or-class (VAR ENV) 
  "Возвращает тип ИЛИ класс значения"
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
  #-:LISPWORKS4.4
  (error "You lose"))


(defmacro print-variable-type-or-class (var &environment env)
  (print (variable-type-or-class var env))
  var)

(defmacro print-variable-information (var &environment env)
  (print (hcl:variable-information var env)))


; (defun foo (x) (declare (integer x)) (print-variable-type-or-class x))


(defun conc-prefix-by-type-or-class-name (type class)
  (let* ((type-package (symbol-package type)))
    (cond
     ((null class) (values (str+ type "-") type-package)) ; might fail
     (t 
      (let1 class-name (class-name class)
        (cond
         ((typep class 'structure-class)
          (let1 mc-metadata (defstruct-meta:defstruct*m-slot-names-and-accessors class-name :NO-ERROR t)
            (cond 
             (mc-metadata
              (error "напиши меня"))
             (t (values (str+ class-name "-") type-package)))))
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

(defun function-symbol-for-^ (type-or-class field-name)
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
                            
(defun runtime^ (object field-name &rest args)
  (assert object () "(runtime^ nil ~S): Sorry, object can't be null!" field-name)
  (let* ((class (class-of object))
         (target-function-symbol (function-symbol-for-^ class field-name)))
    (apply target-function-symbol object args)
    ))

; we need this as we attach symbol-readmacro on ^ so that it can't be 

(defmacro carat-implementation (object field-name &rest args)
  "This implementation can be shadowed in with-custom-carat-implementation"
  `(common-carat-implementation ,object ,field-name ,@args))

(defmacro common-carat-implementation (object field-name &rest args)
  "One more level of indirection due to presence of compiler-macro"
  `(runtime^ ,object ',field-name ,@args))

(define-compiler-macro common-carat-implementation (object field-name &rest args &environment env)
  (let1 variable-type-or-class (variable-type-or-class object env)
    (case variable-type-or-class
      ((t nil) `(runtime^ ,object ',field-name ,@args))
      (t 
       `(funcall ',(function-symbol-for-^ variable-type-or-class field-name) ,object ,@args))
      )))
      
(defmacro |^| (object field-name &rest args)
  "See also ^-READER"
  `(carat-implementation ,object ,field-name ,@args))
      
(defmacro with-the1 (var type object &body body)
  `(let ((,var ,object))
     (declare (type ,type ,var))
     ,@body))
     

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
                            (^ a b))"))
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


        
