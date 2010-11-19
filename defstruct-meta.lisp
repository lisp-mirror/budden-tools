
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html
;;;   (C) Denis Budyak 2009

(in-package :defstruct-meta)

(defun str-cat (&rest args) (apply 'concatenate 'string args))
(defun concat-pnames (&rest names)
  (intern (apply 'str-cat (mapcar 'string names))))

(defvar %defstructs% (make-hash-table))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro smlet (&rest args) `(symbol-macrolet ,@args)))
(defmacro neq (&rest args) `(not (equal ,@args)))
(defun fixnump (x) (typep x 'fixnum)) 

(defmacro while (test &body body) `(loop :unless ,test :do (loop-finish) :do (progn ,@body)))

(defstruct ssd name initform offset read-only slot-type)

(defmacro defstruct*m (options &rest slots &environment env)
  "Version DEFSTRUCT which saves slot names and slot accessor names. Currenty do not handle included slots, 
but handles conc-name. See also defstruct*m-slot-names-and-accessors"
  (prog (struct-name type conc-name constructor copier predicate include
         print-function print-object  named initial-offset boa-constructors print-p
         documentation (slot-list ()) (offset 0) superclasses sd
         refnames copy-of-options copy-of-slots)
    ;Parse options
    (setf copy-of-options (copy-tree options)
          copy-of-slots (copy-tree slots))
    (if (atom options)
      (setq struct-name options options ())
      (setq struct-name (pop options)))
    (unless (symbolp struct-name) (error "Defstruct: structure name ~S is not a symbol" struct-name))
    (let (name args constructor-p predicate-p)
      (while options
        (if (atom (car options))
          (setq name (car options) args ())
          (setq name (caar options) args (cdar options)))
        (case name
          (:conc-name
           (when conc-name (go dup-options))
           (when (cdr args) (go bad-options))
           (setq conc-name (or args (list nil))))
          (:constructor
           (when (cddr args) (go bad-options))
           (cond ((cdr args) (push args boa-constructors))
                 (t (when constructor (go dup-options))
                    (unless (symbolp (car args)) (go bad-options))
                    (setq constructor-p t constructor args))))
          (:copier
           (when copier (go dup-options))
           (when (or (cdr args) (not (symbolp (car args)))) (go bad-options))
           (setq copier args))
          (:predicate
           (when predicate (go dup-options))
           (when (or (cdr args) (not (symbolp (car args)))) (go bad-options))
           (setq predicate-p t predicate args))
          (:include
           (when include (go dup-options))
           (when (or (null args) (not (symbolp (car args)))) (go bad-options))
           (setq include args))
          ((:print-function :print-object)
           (when print-function (go dup-options))
           (when (or (cdr args)
                     (not (or (symbolp (car args))
                              (and (consp (car args)) (eq (caar args) 'lambda)))))
             (go bad-options))
           (setq print-p t
		 print-function (car args)
		 print-object (eq name :print-object)))
          (:type
           (when type (go dup-options))
           (when (cdr args) (go bad-options))
           (unless (eq (setq type (car args)) 'list)
             (when (eq type 'vector) (setq type '(vector t)))
             (when (or (atom type) (neq (car type) 'vector) (cdr (cdr type)))
               (go bad-options))))
          (:named
           (when args (go bad-options))
           (setq named t))
          (:initial-offset
           (when initial-offset (go dup-options))
           (when (or (cdr args) (not (fixnump (car args))) (< (car args) 0))
             (go bad-options))
           (setq initial-offset (car args)))
          (t (go bad-options)))
        (setq options (cdr options)))
      ;Options parsed!  Do defaulting and some consistency checking.
      (cond (type
             #+ignore (when (null (defstruct-reftype type)) ;e.g. (vector NIL)
               (bad-named-arg :type type))
             (when print-p
               (error "Cannot specify ~S with ~S" :print-function :type))
             (if (and named (consp type) (eq (car type) 'vector)
                      (cadr type) (not (subtypep 'symbol (cadr type))))
               (error "Cannot specify ~S with type: ~S" :named type))
             )
            #+ignore ((built-in-type-p struct-name)
             (error "Cannot redefine built-in type ~S" struct-name))
            (initial-offset
             (error "Cannot use ~S without ~S" :initial-offset :type))
            (t (setq named t)))
      (if (not named)
        (when predicate-p
          (unless (null (setq predicate (car predicate)))
            (error "Cannot specify :PREDICATE for an unnamed structure")))
        (setq predicate (if (null predicate)
                          (concat-pnames struct-name "-P")
                          (car predicate))))
      (setq conc-name
            (if (null conc-name) (str-cat (symbol-name struct-name) "-")
                (if (car conc-name) (string (car conc-name)))))
      (unless (and boa-constructors (not constructor-p))
        (setq constructor
              (if (null constructor)
                (concat-pnames "MAKE-" struct-name) (car constructor))))
      (setq copier
            (if (null copier) (concat-pnames "COPY-" struct-name) (car copier))))
    ;Process included slots
    #+ignore 
    (when include
      (let* ((included-name (car include))
             (sub-sd (or (let* ((defenv (definition-environment env)))
                          (when defenv (cdr (assq included-name (defenv.structures defenv)))))
                         (gethash included-name %defstructs%)))
            (slots (cdr include))
            name args ssd)
        (unless sub-sd (error "No such structure: ~S" (cons :include include)))
        (unless (eq (defstruct-reftype type)
                    (defstruct-reftype (sd-type sub-sd)))
          (error "Incompatible structure type ~S for ~S"
                 (sd-type sub-sd) (cons :include include)))
        (dolist (ssd (sd-slots sub-sd)) (push
					 (let* ((new-ssd (copy-ssd ssd)))
					   (ssd-set-inherited new-ssd)
					   new-ssd)
					   slot-list))
        (while slots
          (if (atom (car slots))
            (setq name (car slots) args ())
            (setq name (caar slots) args (cdar slots)))
          (unless (symbolp name) (error "defstruct: not a symbol: ~S" name))
          (unless (setq ssd (named-ssd name slot-list))
            (error "~S has no ~S slot, in ~S"
                   (sd-name sub-sd) name (cons :include include)))
          #+nil (ssd-set-initform ssd (pop args))
          (while args
            (when (atom (cdr args)) (error "~S is not a proper list" (cdr args)))
            (cond ((eq (car args) :type) )
                  ((eq (car args) :read-only)
                   (when (and (not (cadr args)) #+nil (ssd-r/o ssd))
                     (error "Slot ~S in ~S must be read-only" name (sd-name sub-sd)))
                   (when (cadr args) #+nil (ssd-set-r/o ssd)))
                  (t (error "~S must be  (member :type :read-only)." (car args))))
            (setq args (cddr args)))
          (setq slots (cdr slots)))
        (setq offset (sd-size sub-sd))
        (setq superclasses (sd-superclasses sub-sd))))
    (push struct-name superclasses)
    ;Now add own slots
;    (setq offset (%i+ offset (or initial-offset 0)))
    #+ignore  
    (when (and named (or type (not include)))
      (push (make-ssd :name 0 :initform (if type `',struct-name `',superclasses) :offset offset :read-only t) slot-list)
      (setq named offset offset (%i+ offset 1)))
    (when (stringp (car slots))
      (setq documentation (car slots) slots (cdr slots)))
    (let (name args read-only initform slot-type)
      (while slots
         (if (atom (car slots))
           (setq name (car slots) args ())
           (setq name (caar slots) args (cdar slots)))
         (unless (symbolp name) (go bad-slot))
         (setq read-only nil initform (pop args) slot-type t)
         (while args
            (when (atom (cdr args)) (go bad-slot))
            ;; To do: check for multiple/incompatible options.
            (cond ((eq (car args) :type)
                   (setq slot-type (cadr args)))
                  ((eq (car args) :read-only)
                   (setq read-only (cadr args)))
                  (t (go bad-slot)))
            (setq args (cddr args)))
         (push (make-ssd :name name :initform initform :offset offset :read-only read-only :slot-type slot-type) slot-list)
         (setq slots (cdr slots) #+nil offset #+nil (%i+ offset 1))))

    (setq slot-list (nreverse slot-list))
    (when (and (null type) include)
      #+nil (ssd-set-initform (car slot-list) `',superclasses))
    (progn ;when conc-name
      (dolist (slot slot-list)
        (unless (fixnump (ssd-name slot))
          (push (if conc-name
                  (concat-pnames conc-name (ssd-name slot))
                  (ssd-name slot))
                refnames)))
      (setq refnames (nreverse refnames)))
    (setq sd (vector type slot-list superclasses offset constructor () refnames))
    (print slot-list)
    (print refnames)
    (return
     `(progn
        (remhash ',struct-name %defstructs%)
        (defstruct ,copy-of-options ,.copy-of-slots)
        (setf (gethash ',struct-name %defstructs%)
              ',(loop 
                   :for slot :in slot-list
                   :for slot-accessor-name :in refnames
                   :collect `(,(ssd-name slot) ,slot-accessor-name)))
;       (remove-structure-defs  ',struct-name) ; lose any previous defs
;        ,.(defstruct-slot-defs sd refnames env)
;        ,.(if constructor (list (defstruct-constructor sd constructor)))
;        ,.(defstruct-boa-constructors sd boa-constructors)
;        ,.(if copier (defstruct-copier sd copier env))
;        ,.(if predicate (defstruct-predicate sd named predicate env))
        #+nil (eval-when (:compile-toplevel)
          (define-compile-time-structure 
            ',sd 
            ',refnames 
            ,(if (and predicate (null (sd-type sd))) `',predicate)
            ,env))        
       ;; Wait until slot accessors are defined, to avoid
       ;; undefined function warnings in the print function/method.
        ',struct-name))

    dup-options
     (error "Duplicate ~S options not allowed" (car options))
    bad-options
     (error "Bad defstruct option ~S." (car options))
    bad-slot
    (error "Bad defstruct slot spec ~S." (car slots))))


(defun defstruct*m-slot-names-and-accessors (struct-name &key NO-ERROR)
  "Returns list of (slot-name accessor-name) pairs for defstruct defined with metainfo (e.g. defstruct*m)"
  (or 
   (gethash struct-name %defstructs%)
   (unless NO-ERROR
     (error "~S is either not a defstruct or was not defined with meta-extensions" struct-name))))


(defmacro defstruct*mc (&rest args)
  "Defines a structure at compile time with meta-info. Use this if you want to use with-struct"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defstruct*m ,@args)))

(defmacro with-struct (binding type-name &body body)
  "Binds structure accessors"
  (let* ((atom (atom binding))
         (instance (if atom binding (car binding)))
         (type-atom (atom type-name))
         (type (if type-atom type-name (car type-name))))
    (or 
     (and atom (symbolp binding))
     (and (list binding) 
          (symbolp instance)
          (null (third binding)))
     (error "with-struct: incorrect binding ~S" binding))
    (or 
     (and type-atom (symbolp type))
     (and (list type-name)
          (null (cdr type-name))
          (symbolp type))
     (error "with-struct: wrong typespec ~S" type-name))
    (let1 sm-bindings
        (loop 
           :for (name accessor) 
           :in (defstruct*m-slot-names-and-accessors type)
           :collect `(,(if type-atom
                           (concat-pnames instance "." name)
                           (concat-pnames name "."))
                       (,accessor ,instance))
           )
      (if atom
          `(symbol-macrolet 
               ,sm-bindings
             ,@body)
          `(let1 ,@binding
               (symbol-macrolet 
               ,sm-bindings
             ,@body))))))



#|
(defstruct*mc s1 f1)
(defstruct*mc (s2 (:conc-name asdf-) (:include s1)) f2)

(let ((i1 (make-s1 :f1 1))
      (i2 (make-s2 :f1 1.1 :f2 2)))
  (assert
   (equalp
    (list 
     (with-struct i1 s1 (setf i1.f1 8) i1.f1)
     (with-struct i2 s2 
       (list #+nil i2.f1 i2.f2))
     (with-struct i2 (s2)
       (list f2.))
     (with-struct (i3 (make-s1 :f1 5)) s1 i3.f1)
     )
    (list
     8
     (list #+nil (s2-f1 i2) (asdf-f2 i2))
     (list (asdf-f2 i2))
     (s1-f1 (make-s1 :f1 5))))))



(defstruct*mc foo bar baz)
(let ((x (make-foo :baz 5)))
  (with-struct x foo
    (setf x.bar 4)
    (print (+ x.bar x.baz))))
|#
