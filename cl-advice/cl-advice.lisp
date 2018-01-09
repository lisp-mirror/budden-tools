;;; -*- coding: utf-8; System :cl-advice;  -*-

(in-package :cl-advice)

; disable stepping if SBCL
#+SBCL (declaim (optimize (debug 3) (compilation-speed 3) (safety 3)))

#-(or SBCL CCL)
(defstruct function-decoration
  "All we know about this decoration"
  (name (error "mandatory slot name missing") :type symbol)
  (old-function nil :type (or null function))
  old-function-source-location 
  ;; We generate lambda when decorating
  ;; new-function and new-function-source-location store decorator, not the lambda
  ;; which is actually written to (symbol-function name)
  (new-function nil :type (or symbol function))
  ;; also we store new-lambda which is actually assigned to (symbol-function name)
  ;; thus we are able to track the situation when the name was redefined and we are trying
  ;; to apply decoration again
  new-lambda 
  )

#-(or SBCL CCL)
(defvar *function-decorations*
  (swank/backend:make-weak-key-hash-table :test 'eq)
  "Name of original function -> function-decoration")

; maps symbol to (symbol . original-macro-function)
; symbol is an uninterned symbol to which original macro definition is copied while
; macro is being decorated
(defvar *undecorated-macros* (swank/backend:make-weak-key-hash-table :test 'eq))

#+CCL
(defun install-advice (symbol decorator-fn &key (advice-name 'default))
  (warn "In CCL, install-advice uses eval ! Consider define-advice instead")
  (eval `(define-advice ,symbol ',decorator-fn :advice-name ,advice-name)))

#-(or CCL SBCL)
(defun install-advice (symbol decorator-fn &key (advice-name 'default))
  "See example"
  (declare (ignorable advice-name)) ; FIXME - implement through advice where available
  (assert (eq advice-name 'default) () "Sorry, but for this CL implementation we don't support advices with different names")
  (check-type symbol symbol)
  (assert (fboundp symbol) () "It hardly makes sense to decorate non-functions")
  (check-type decorator-fn (and (not null) (or symbol function)))
  (let* (#+lispworks
         (lispworks:*handle-warn-on-redefinition* nil)
         (old-entry (gethash symbol *function-decorations*))
         (old-ok (and old-entry (eq (function-decoration-new-lambda old-entry)
                                    (symbol-function symbol))))
         (old-fn (if old-ok
                     (function-decoration-old-function old-entry)
                     (symbol-function symbol)))
         (new-lambda (lambda (&rest args) (apply decorator-fn old-fn args)))
         (new-entry
          (make-function-decoration
           :name symbol
           :old-function old-fn
           ;; this does not make sense because we're under #-SBCL already,
           ;; but it is kept just for history
           #+sbcl :old-function-source-location
           #+sbcl (sb-introspect::find-definition-source old-fn)
           :new-function decorator-fn
           :new-lambda new-lambda)))
    ;(print `(old-ok ,old-ok old-fn ,old-fn symbol-function-of-symbol ,(symbol-function symbol) old-entry ,old-entry))
    (setf (symbol-function symbol)
          new-lambda)
    (setf (gethash symbol *function-decorations*)
          new-entry)
    (symbol-function symbol)))


#+SBCL
(defun install-advice (symbol decorator-fn &key (advice-name 'default))
  "Deprecated. Use install-advice instead"
  (when (sb-int:encapsulated-p symbol advice-name)
    (sb-int:unencapsulate symbol advice-name))
  (sb-int:encapsulate symbol advice-name decorator-fn))

(defun record-decoration-definition-location (function-name advice-name location)
  (setf
   (getf
    (get function-name 'advice-definition-location-indicator)
    advice-name)
   location))

(defun delete-decoration-definition-location (function-name advice-name)
  (remf
   (get function-name 'advice-definition-location-indicator)
   advice-name))


(defmacro define-advice (function-name decorator-fn &key (advice-name 'default))
  "Defines a decoration for the function. Arguments:
   function name - symbol, not evaluated
   decorator-fn - function designator, evaluated. Must accept the same args as function-name + first parameter is a previous encapsulation
   advice-name - name of advice, not evaluated. (function-name advice-name) is a key to identify a piece of advice"
  (check-type function-name symbol)
  (check-type advice-name symbol)
  #-(or CCL SBCL)
  (assert (eq advice-name 'default) () "Sorry, but for this CL implementation we don't support advices with different names")
  (build-the-install-advice-macroexpansion function-name decorator-fn advice-name
                                           #+SBCL :source-location
                                           #+SBCL (sb-c:source-location)))

#+CCL
(defun build-the-install-advice-macroexpansion (function-name decorator-fn advice-name)
  (let ((prev-advice-or-original-fn (make-symbol-for-expression `(prev-advice-or-original-fn ,function-name ,advice-name)))
        (args (gensym "ARGS")))
    `(ccl:advise
      ,function-name
      (flet ((,prev-advice-or-original-fn (&rest ,args) (apply #'ccl::call-next-advice ,args)))
        (apply ,decorator-fn #',prev-advice-or-original-fn ccl:arglist))
      :name ,advice-name
      :when :around)))

#-CCL
(defun build-the-install-advice-macroexpansion (function-name decorator-fn advice-name &key source-location)
  (declare (ignorable source-location))
  (let (#+sbcl (source-location-sym (gensym (string 'sb-c:source-location))))
    `(prog1
         (install-advice
          ',(the symbol function-name)
          (the (and (not null) (or symbol function)) ,decorator-fn)
          :advice-name
          ',(the symbol advice-name))
       ;; see also sbcl--find-definition-sources-by-name--patch.lisp
       #+sbcl (let ((,source-location-sym ,source-location))
         (when ,source-location-sym
           (record-decoration-definition-location
            ',function-name ',advice-name ,source-location-sym))))))
    
(defun decorate-macro-get-undecorated-invoker (symbol)
  (car (gethash symbol *undecorated-macros*)))

(defun decorate-macro-check-redefinition (symbol)
  "Checks if macro was redefined and errs if it was. Returns decoration entry"
  (let ((decoration-entry (gethash symbol *undecorated-macros*)))
    (cond
     ((null decoration-entry) decoration-entry)
     ((not (string= symbol (car decoration-entry)))
      (error "name mismatch between '~A with saved macro symbol '~A" symbol (car decoration-entry)))
     ((not (eq (macro-function symbol) (cdr decoration-entry)))
      (cerror "make macro undecorated. Current definition will be kept"
              "macro '~A was decorated and then redefined" symbol)
      (remhash symbol *undecorated-macros*)
      nil)
     (t decoration-entry)
     )))

(defun decorate-macro (symbol decorator-macro)
  "See example"
  (let (#+lispworks (lispworks:*handle-warn-on-redefinition* nil)
                    (entry (decorate-macro-check-redefinition symbol)))
    (macrolet ((mf (s) `(macro-function ,s)))
      (cond
       (entry 
        (setf (mf symbol) (mf decorator-macro)
              (cdr entry) (mf decorator-macro)))
       (t 
        (let ((old-def-symbol (make-symbol (symbol-name symbol))))
          (setf (mf old-def-symbol) (mf symbol))
          (setf (gethash symbol *undecorated-macros*)
                (cons old-def-symbol (mf decorator-macro)))
          (setf (mf symbol) (mf decorator-macro))))
       ))))

(defun undecorate-macro (symbol)
  (let ((decoration-entry (decorate-macro-check-redefinition symbol)))
    (when decoration-entry
      (setf (macro-function symbol) (macro-function (car decoration-entry)))
      (remhash symbol *undecorated-macros*))))

#+(or CCL SBCL)
(defun uninstall-advice (function-name &key (advice-name 'default))
  #+CCL (ccl::%unadvise-1 function-name :around advice-name)
  #+SBCL (delete-decoration-definition-location (the symbol function-name) (the symbol advice-name))
  #+SBCL (sb-int:unencapsulate function-name advice-name)
  )

#-(or SBCL CCL)
(defun uninstall-advice (symbol &key (advice-name 'default))
  (declare (ignore advice-name))
  (let ((decoration (gethash symbol *function-decorations*)))
    (assert decoration () "Function ~S is not decorated" symbol)
    (remhash symbol *function-decorations*)
    (setf (symbol-function symbol) (function-decoration-old-function decoration))
    ))

(defmacro |С-декорированной-функцией| (|Функция| |Декоратор| &body |Тело|)
  "Функция и декоратор - это символы, хотя декоратор может быть и лямбдой. Вычисляются.
ПРАВЬМЯ - этому место в пакете install-advice. Не стоит питать иллюзий на тему безопасности этйо конструкции - её действие, хоть и временное, но распространяется на все треды. Возвращает nil"
  (let ((|Функция-однократно| (gensym "Функция-однократно")))
    `(let ((,|Функция-однократно| ,|Функция|))
       (unwind-protect
           (progn
             (cl-advice:install-advice ,|Функция-однократно| ,|Декоратор|)
             ,@|Тело|)
         (cl-advice:uninstall-advice ,|Функция-однократно|)))))

;; see cl-advice-tests for examples and tests
