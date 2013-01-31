;;; -*- Encoding: utf-8; -*-
(in-package :budden-tools)

;; Extract name of a function being compiled. 
;; Very basic draft, but works
;; sometimes (in a toplevel defuns)
;; http://paste.lisp.org/display/113545

#+sbcl (defun find-lambda-in-sbcls-info (info-element)
  (subst-if
   nil
   (lambda (x) 
     (and 
      (consp x)
      (member (first x) '(defun SB-INT:NAMED-LAMBDA))
      (second x)
      (symbolp (second x))
      (return-from find-lambda-in-sbcls-info (second x))))
   info-element))
      

(defun fn-being-compiled ()
  "Returns the name of the function being compiled, if it can. 
Call this from the macroexpander code. If you need 
to know function name in function's body itself, use fn-being-compiled-m"
  #+lispworks 
  (let ((info compiler:*function-name*))
    (etypecase info
      (symbol info)
      (cons (second info))))
  #+sbcl 
  (let* ((info 
          (sb-c::file-info-forms 
           (sb-c::source-info-file-info sb-c::*source-info*)))
         (info-len (length info)))
    (progn ; ignore-errors 
      (find-lambda-in-sbcls-info
       (elt info (1- info-len)))))
  #+ccl
  ccl::*nx-cur-func-name*
  #-(or sbcl lispworks ccl)
  (error "not implemented"))

(defmacro fn-being-compiled-m () 
  "Returns the name of the function being compiled. To use in a function body"
  `',(fn-being-compiled)
  )

#| sample session
; first example, evident, but pretty useless
CL-USER> (defun xxxx () (fn-being-compiled-m))
XXXX
CL-USER> (xxxx)
XXXX
; a bit more useful example
CL-USER> (defvar *functions-i-know* nil)
*FUNCTIONS-I-KNOW*
CL-USER> (defmacro note-function () (push (fn-being-compiled) *functions-i-know*))
NOTE-FUNCTION
CL-USER> (defun fooo () (note-function))
FOOO
CL-USER> *functions-i-know*
|#



(defvar *tags* (make-hash-table :test 'equalp) "для каждого тега - список его функций") 


(defmacro documentation-tags (&rest tags)
  "Определяет теги для ф-ии, которую мы сейчас компилируем"
  (mapcar 
   (lambda (tag)
     (let ((tags-cell (gethash tag *tags*))
           (fn (fn-being-compiled-fun)))
       (when fn
         (pushnew fn tags-cell)
         (print tags-cell)
         (setf (gethash tag *tags*) tags-cell))))
   tags)
  nil)

(defun aprotags (tag)
  (let ((tagged-fns (gethash tag *tags*)))
    ;(mapcar 'print (butlast tagged-fns))
    ;(car (last tagged-fns)))
    tagged-fns))

(defun all-tags ()
  (iter:iter
    (declare (ignore v))
    (:for (k v) :in-hashtable *tags*)
    (:collecting k)))
