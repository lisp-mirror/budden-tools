;;; -*- Encoding: utf-8; system :budden-tools; -*-
(in-package :budden-tools)

(named-readtables:in-readtable nil)

; (defmacro pvi (var &ENVIRONMENT env) (break) (print (list (hcl:variable-information var env) (hcl:declaration-information 'type))) `',var)

(defmacro print-environment (&optional break &ENVIRONMENT env) (print env) (if break (break)))

;; отключаем пошаговую отладку
(declaim  (optimize (debug 2) (space 2) (compilation-speed 2) (speed 2) (safety 3)))


#+sbcl 
(defun normalize-type (type)
  (cond
    ((SB-KERNEL:STRUCTURE-CLASSOID-P type)
     (sb-kernel:classoid-name type))
    ((sb-kernel:named-type-p type)
     (sb-kernel:named-type-name type))
    ((sb-kernel:union-type-p type)
     (sb-kernel:type-specifier type))
    ((sb-kernel::built-in-classoid-p type)
     (sb-kernel:classoid-name type))
    (t
     type)))

  

(defun variable-type-or-class (VAR ENV) 
  "Возвращает тип ИЛИ класс значения. Непонятно, зачем нужен класс - наверняка всегда достаточно типа для ^ (FIXME)"
  #+sbcl
  (cond
   ((constantp var env)
    (normalize-type (type-of var))) ; в лиспворксе здесь возвращается класс...
   ((not (symbolp var)) ; если это не переменная, то про тип ничего и не знаем
    t)         
   (env
    (assert (typep env 'sb-kernel:lexenv))
    (let ((type-cltl2
           (multiple-value-bind
               (binding-type local-p declarations) (sb-cltl2:variable-information var env)
             (declare (ignore binding-type local-p))
             (let ((declared-type (cdr (assoc 'type declarations))))
               (or declared-type ; если тип не задан, то и нечего декларировать
                   t))))
          (hacked-type
           (let* ((vars (sb-c::lexenv-vars env))
                  (this-var (cdr (assoc var vars))))
             (cond
              ((null this-var) ; а странно... но считаем, что тип t
               t)
              (t
               (etypecase this-var
                 (sb-c::lambda-var
                  (let ((result (sb-c::lambda-var-type this-var)))
                    (normalize-type result)))
                 (sb-c::global-var
                  ; there is sb-c::global-var-defined-type also
                  (let ((result (sb-c::global-var-type this-var)))
                    (normalize-type result)))))))))
      (unless (equalp type-cltl2 hacked-type)
        (warn "variable-type-or-class есть куда улучшить: type-cltl2 = ~A, hacked-type = ~A"
              type-cltl2 hacked-type))
      hacked-type)))
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
  #-(or :lispworks6 sbcl)
  (if
   (constantp var env)
   (class-of var)
   t)
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

(defgeneric symbol-from-yar-p (symbol) (:documentation "Будет определён around метод при загрузке Яра"))

(defmethod symbol-from-yar-p ((symbol symbol)) nil)

(defun dash-after-symbol (symbol)
  (if (symbol-from-yar-p symbol) "ₒ" "-"))

;; почему мы отключаем определение функций во время компиляции?
;; а потому, что нам придётся тогда воспроизвести всю работу, в т.ч. определять сигнатуры
;; функций доступа. 
(defun structure-or-class-slots-maybe-compile-time (type-name)
  "Для SBCL, умеем проведать о слотах во время компиляции. Для других реализаций будем брать метаданные, известные из прошлых компиляций. См. также structure-or-class-slot-accessors-maybe-compile-time. ПРАВЬМЯ на самом деле работает только для структур"
  #+(and SBCL BUDDEN-TOOLS--VARIABLE-TYPE--COMPILE-TIME-METADATA)
  (let* ((layout (sb-kernel::compiler-layout-or-lose type-name))
         (info (sb-kernel::layout-info layout))
         (slots (sb-kernel::dd-slots info))
         (names (mapcar 'sb-kernel::dsd-name slots)))
    names)
  #-(and SBCL BUDDEN-TOOLS--VARIABLE-TYPE--COMPILE-TIME-METADATA)
  (let* ((slots (closer-mop:class-slots (find-class type-name nil)))
         (names (mapcar 'closer-mop:slot-definition-name slots)))
    names))

(defun structure-or-class-slot-readers-maybe-compile-time (type-name)
  "Аналогично structure-or-class-slots-maybe-compile-time, возвращает ИМЕНА функций - это нетривиальный факт, т.к. в классе, наверное, могут быть специализированные ФУНКЦИИ, а не имена"
  #+(and SBCL BUDDEN-TOOLS--VARIABLE-TYPE--COMPILE-TIME-METADATA)
  (let* ((layout (sb-kernel::compiler-layout-or-lose type-name))
         (info (sb-kernel::layout-info layout))
         (slots (sb-kernel::dd-slots info))
         (names (mapcar 'sb-kernel::dsd-accessor-name slots)))
    names)
  #+(and SBCL (not BUDDEN-TOOLS--VARIABLE-TYPE--COMPILE-TIME-METADATA))
  (cond
   ((typep (find-class type-name) 'structure-class)
    (let* ((struct-description (sb-kernel:find-defstruct-description type-name))
           (slots (sb-kernel::dd-slots struct-description))
           (names (mapcar 'sb-kernel::dsd-accessor-name slots)))
      names))
   (t
    (let* ((slots (closer-mop:class-slots (find-class type-name nil)))
           (names-lists (mapcar 'closer-mop:slot-definition-readers slots))
           (names (mapcar 'car slots)))
      names)))
  #-SBCL ; вряд ли сработает для структур, см. случаи для SBCL, а также budden-tools:struct-to-alist и ищите, как
         ; это работает в данной реализации лиспа.
  (let* ((slots (closer-mop:class-slots (find-class type-name nil)))
         (names-lists (mapcar 'closer-mop:slot-definition-readers slots))
         (names (mapcar 'car slots)))
    names))

(defun field-or-function-by-type-or-class-and-field-name (type-or-class field-name)
  "Третье значение для function-symbol-for-^"
  (cond
   ((not (typep type-or-class 'symbol))
    :bad ; если у нас всё в порядке, мы в Яре не увидим этого значения
    )
   (t
    (let* ((class (find-class type-or-class nil)))
      (cond
       ((null class)
        nil)
       ((and (typep class 'structure-class)
             (member field-name (structure-or-class-slots-maybe-compile-time type-or-class)
                     :test 'string=))
        :field)
       (t
        :function))))))

(defun conc-prefix-by-type-or-class-name (type class suffix)
  (let* ((type-package (symbol-package type)))
    (cond
     ((null class) (values (str+ type (dash-after-symbol type)) type-package)) ; might fail
     (t 
      (let1 class-name (class-name class)
        (cond
         ((typep class 'structure-class)
          (values (str+ class-name suffix) type-package))
         ((subtypep class (find-class 'string))
          (values (str+ 'string suffix) (find-package :common-lisp)))
         ((subtypep class (find-class 'character))
          (values (str+ 'char suffix) (find-package :common-lisp)))
         ((member class-name '(symbol package))
          (values (str+ class-name suffix) (find-package :common-lisp)))
         ((subtypep class (find-class 'array))
          (values (str+ 'array suffix) (find-package :common-lisp)))
         ((typep class (find-class 'standard-class))
          (values (str+ class-name (dash-after-symbol class-name)) type-package))
         ((eql class-name 'pathname)
          (values (str+ class-name (dash-after-symbol class-name)) :common-lisp))
         (t (error "conc-prefix for class ~S is undefined" class-name))
         ))))))

(defun conc-prefix-by-type-or-class (type-or-class suffix)
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
    (conc-prefix-by-type-or-class-name type class suffix)))
  

(trivial-deftest::! #:conc-prefix-by-class-1
                    (mapcar 
                     (alexandria:named-lambda conc-prefix-by-class-1-lambda (x) 
                       (multiple-value-bind (s p) (conc-prefix-by-type-or-class x "-")
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

(defgeneric function-symbol-for-^ (type-or-class suffix field-name)
  (:documentation "
    Определяет, как преобразовывать выражение а^б (или (|^| а б)) по известному типу а и по имени поля б. suffix - это разделитель между именем типа и поля, к-рый должен
  быть в результирующей ф-ии. Должна возвращать 3 значения
 - 1: имя функции
 - 2: t, если нужно передать вторым параметром имя поля
 - 3: должен ли это в Яре быть синтаксис вызова функции или синтаксис обращения к полю:
   -- :function 
   -- :field
   -- nil - не решено, должен ли это быть синтаксис вызова ф-ии или обращения к полю.
   -- :bad - ошибка в алгоритме (такой код не должен вызываться для Яра)
"
))


(defmethod function-symbol-for-^ ((type-or-class symbol) suffix field-name)
  (multiple-value-bind (name pass-field-name field-or-function)
                       (call-next-method)
    (values name pass-field-name field-or-function)))

(defmethod function-symbol-for-^ (type-or-class suffix field-name)
  (let ((common-lisp:*break-on-signals* t))
    (multiple-value-bind (prefix package) (conc-prefix-by-type-or-class type-or-class suffix)
     (let* ((target-symbol-name (str+ prefix field-name))
           (target-symbol (find-symbol target-symbol-name package))
           (target-field-or-function (field-or-function-by-type-or-class-and-field-name type-or-class field-name)))
           
      (assert target-symbol () "(^ ~S ~S): symbol name ~S not found in ~S" 
        type-or-class field-name target-symbol-name package)
      (unless (or (fboundp target-symbol)
                  (member target-symbol (structure-or-class-slot-readers-maybe-compile-time type-or-class)))
        (warn "(^ ~S ~S): ~S должен быть функцией. Если вы используете а.б в том же модуле, где определена функция, она может быть не видна. В Яре этот код должен быть не нужен, но если он нужен, то разбейте модуль, вставив код или специальную форму завершения модуля. В лиспе вынесите определение функции в другой файл, выше по течению сборки" 
              type-or-class field-name target-symbol))
        ; а если это макрос? (assert (null (macro-function target-symbol)))?
      (values target-symbol nil target-field-or-function)
      ))))
                            
; Мы не можем прямо определить ф-ю ^, т.к. у нас уже есть symbol-readmacro с именем ^
(defun runtime^ (object suffix field-name &rest args)
  "Вызывается, если на этапе компиляции не удалось определить тип объекта"
  (assert object () "(runtime^ nil ~S ~S): Прошу прощения, но объект должен быть не null!" suffix field-name)
  (let* ((class (class-of object)))
    (multiple-value-bind (target-function-symbol field-p field-or-function)
        (function-symbol-for-^ class suffix field-name)
      ;(warn "Здесь должна стоять проверка на field-or-function, да и вообще вряд ли оно оживёт!")
      (assert target-function-symbol) ; или нужно здесь внятно обругаться? 
      (apply target-function-symbol object
             (if field-p (cons field-name args)
               args))
      )))

(defmacro common-carat-implementation (object field-name &rest args)
  "See also def-compiler-macro common-carat-implementation"
  `(runtime^ ,object "-" ',field-name ,@args))

(define-compiler-macro common-carat-implementation (object field-name &rest args &environment env)
  "See also defmacro common-carat-implementation, а также strict-carat-implementation"
  (let ((variable-type-or-class (variable-type-or-class object env)))
    (case variable-type-or-class
      ((t nil) `(runtime^ ,object "-" ',field-name ,@args))
      (t
       (multiple-value-bind (function-symbol field-p)
           (function-symbol-for-^ variable-type-or-class "-" field-name)
         `(,function-symbol ,object ,@(when field-p `(,field-name)) ,@args))
       ))))

(defmacro strict-carat-implementation (object field-name &rest args &environment env)
  "Для ^^ . См. также .апр {.°} "
  (let ((variable-type-or-class (variable-type-or-class object env)))
    (case variable-type-or-class
      ((t nil) (error "Для ~S ^^ ~S - не понял тип аргумента слева от ^^" object field-name))
      (t
       (multiple-value-bind (function-symbol field-p)
           (function-symbol-for-^ variable-type-or-class "-" field-name)
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

(defsetf runtime^ (object suffix field-name &rest args) (new-value)
  "Вызывает eval в runtime!"
  ;(assert (constantp object env))
  (with-gensyms (o target-function-symbol class)
    (once-only (object)
      `(progn
         (let* ((,o ,object)
                (,class (class-of ,o))
                (,target-function-symbol (function-symbol-for-^ ,class ,suffix ,field-name)))
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




;--------------------------------------------------------------      Читалка   ^ и ^^ -----------------------------------------------------------
(defparameter ^-reader nil "Этот параметр нужен только для тогО, чтобы было возможно с помощью Alt-. Alt-, найти определение функции ^,которую невозможно прочитать")

(defparameter ^^-reader nil "Аналогично ^-reader")

(defmacro |^| (object field-name &rest args)
  "See also ^-READER"
  `(carat-implementation ,object ,field-name ,@args))

(defmacro |^^| (object field-name &rest args)
  "See also ^-READER"
  `(strict-carat-implementation ,object ,field-name ,@args))

     
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


