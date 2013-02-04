;;; -*- Encoding: utf-8; -*-
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
  #-(or :LISPWORKS4.4 :lispworks6)
  (when (constantp var env) (class-of var)))


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
                            
; we need this as we attach symbol-readmacro on ^ so that it can't be 
(defun runtime^ (object field-name &rest args)
  "Вызывается, если на этапе компиляции не удалось определить тип объекта"
  (assert object () "(runtime^ nil ~S): Sorry, object can't be null!" field-name)
  (let* ((class (class-of object))
         (target-function-symbol (function-symbol-for-^ class field-name)))
    (apply target-function-symbol object args)
    ))

(defmacro common-carat-implementation (object field-name &rest args)
  "See also def-compiler-macro common-carat-implementation"
  `(runtime^ ,object ',field-name ,@args))

(define-compiler-macro common-carat-implementation (object field-name &rest args &environment env)
  "See also defmacro common-carat-implementation"
  (let1 variable-type-or-class (variable-type-or-class object env)
    (case variable-type-or-class
      ((t nil) `(runtime^ ,object ',field-name ,@args))
      (t 
       `(,(function-symbol-for-^ variable-type-or-class field-name) ,object ,@args))
      )))

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

(defsetf runtime^ (object field-name &rest args &environment env) (new-value)
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
      
(defmacro with-the1 (var type object &body body)
  `(let ((,var (the* ,type ,object)))
     (declare (type ,type ,var))
     ,@body))

(setf (get 'with-the1 'proga-implementation::proga-transformer) 
      'proga-implementation::open-up-if-4
      )
     

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


        
