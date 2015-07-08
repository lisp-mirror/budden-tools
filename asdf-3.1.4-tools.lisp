;;; -*- Encoding: utf-8; -*-
;;; Written by Denis Budyak 2008-2015. This file is in public domain or covered by asdf licens or maybe 
;;; under some other license. It conatains modified parts of asdf so I don't know. 
;;; Load this file (w/o compilation)
;;; This file is not a part of any asdf system

(in-package :asdf)

;; we don't export anything as this could offend someone. But if not, just uncomment this form.
#+nil (export 
        'of-system ; declare that file is in system
        'package-file 
        )


;; package file
(defclass package-file (cl-source-file) 
  (
   (packages :initarg :packages))
  (:documentation "Represents file with package definitions. List files as a value of :packages initarg"))


#| :asdf3.1 - test me (defun load-package-file (system &key (component-name "package")) 
  "Loads only one component of a system as a source. Intention is a load ahead package.lisp 
to resolve circular references between systems"
  (perform (make-instance 'load-source-op) (find-component (find-system system) component-name))
  ) |#


#+asdf3.1
(defvar *current-component*)

(defmacro of-system (keywordized-system-name)
  "Declares and asserts that file belongs to system"
  `(eval-when (:compile-toplevel :load-toplevel)
     (when (boundp '*current-component*)
       (assert
           (member ,keywordized-system-name
                   (asdf::component-find-path asdf::*current-component*) :test 'string-equal)
           ()
         "current file must be loaded through ~S system" ,keywordized-system-name))))


;; Enable editing component source after compilation or load error
(defun edit-file (filename)
  (or 
   #+lispworks (editor:find-file-command nil filename)
   #+sbcl (when (find-package "SWANK")
            (funcall (find-symbol "ED-IN-EMACS" "SWANK") ; swank:ed-in-emacs
                     filename))
  (format *debug-io* "Still don't know how to open file for editing in this CL implementation")
  ))

(defgeneric edit-component-source (c))
(defmethod edit-component-source ((c source-file)) (edit-file (slot-value c 'absolute-pathname)))
(defmethod edit-component-source ((c module)) (edit-component-source (component-parent c)))
(defmethod edit-component-source ((c system)) (edit-file (system-source-file c)))

(defmethod edit-component-source ((c t))
  (format *debug-io* "Still don't know how to edit ~S source" c))
(define-symbol-macro e (edit-component-source *current-component*))
(define-symbol-macro ep (edit-component-source (component-parent *current-component*)))


#+asdf3.1
(defmethod perform-with-restarts :around (operation component)
    (loop
     (let ((*current-component* component))
       (restart-case
           (return (call-next-method))
         (edit-component ()
           :report
           (lambda (s)
             (let ((parent (component-parent component)))
               (if parent
                   (format s "~@<To edit ~A, eval asdf::e, to edit ~A, eval asdf::ep~@:>" (component-name component) (component-name parent))
                 (format s "~@<To edit ~A, eval asdf::e~@:>" (component-name component))
                 ))))
         #-lispworks (retry ()
                       :report
                       (lambda (s)
                         (format s (compatfmt "~@<Retry ~A.~@:>")
                                 (action-description operation component))))
         #-lispworks (accept ()
                       :report
                       (lambda (s)
                         (format s (compatfmt "~@<Continue, treating ~A as having been successful.~@:>")
                                 (action-description operation component)))
                       (mark-operation-done operation component)
                       (return))))))


;; Enable finding system definitions from IDE

(defun keywordize (symbol) "From iterate" ; see also careful-keywordize
  (intern (symbol-name symbol) :keyword))

#+lispworks
(dspec:define-dspec-class defsystem nil 
  "Defined system"
  :definedp 
  #'(lambda (name)
      ;; Find any object that def-saved-value recorded
      (asdf::system-registered-p name))
  :undefiner #'(lambda (&rest ignore) 
                 (declare (ignore ignore)) 
                 (warn "Don't know how to undefine asdf::defsystem")
                 (constantly t)
                 )
  :object-dspec
  #'(lambda (obj)
      (and (typep obj 'system)
           `(defsystem ,(component-name obj)))))


#+lispworks
(dspec:define-form-parser defsystem (name &rest args)
  (declare (ignore defsystem args))
  `(defsystem ,name))

    
#+lispworks  
(lispworks:defadvice (defsystem defsystem-record-definition :around)
    (call environment)
  ;(print call)
  (let ((system-name (second call)))
    (cond
     (system-name
      `(dspec:def (defsystem ,system-name)
         (progn
           (dspec::record-definition `(defsystem ,',system-name)
                                     (dspec:location))
           ,(lispworks:call-next-advice call environment))))
     (t     
      (lispworks:call-next-advice call environment)))))



#+sbcl
(defmacro decorate-defsystem (name &body options)
  "Enable finding system definition with slime-edit-definition"
  (let ((source-location-sym (make-symbol (string 'sb-c:source-location))))
    `(prog1
         (,(decorate-function::decorate-macro-get-undecorated-invoker 'defsystem)
          ,name ,@options)
       (let ((,source-location-sym (sb-c:source-location)))
       (sb-c:with-source-location (,source-location-sym)
         (setf (sb-c::info :source-location :constant ,(keywordize name)) ,source-location-sym))
       ))))

#+sbcl
(decorate-function::decorate-macro 'defsystem 'decorate-defsystem)

