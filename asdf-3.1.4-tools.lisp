;;; -*- Encoding: utf-8; -*-
;;; Written by Denis Budyak 2008-2009. This file is in public domain or maybe 
;;; under some other license. It conatains modified parts of asdf so I don't know. 
;;; Load this file (w/o compilation)
;;; This file is not a part of any asdf system

(in-package :asdf)

;; we don't export anything as this could offend someone. But if not, just uncomment this form.
#+nil (export 'package-file     ; component class to declare package.lisp and packages it contains
        'dependable-file  ; we want something would happen if dependable-file was touched. 
        'loadsys          ; write asdf:loadsys instead of asdf:oos 'asdf:load-op
        'lo               ; write asdf:lo      instead of asdf:oos 'asdf:load-op
        'asdf-help        ; some useful info in its docstring
        'run-shell-command-here-to-stdout ; running shell commands 
        'run-shell-command-here           ; running shell commands
        'lock-tabooed-systems             ; deliverance from recompiling (and braking) sbcl on :force loads
        'walk-op                          ; traverse systems as load does but call *walk-function* instead of loading
        '*walk-op-function*               ; function which is called by walk-op 
        'dump-all-md5-and-write-dates-in-system ; useful to compare two installations
        'undefsystem      ; clear system's output files, packages and make asdf forget about system. Unfinished, dangerous
         ; asdf systems, modules and components form a tree. Next two functions work with the "paths" to components in the tree
        'dump-component-asdf-path ; return path of compoment, e.g. ("bordeaux-threads" "src" "bordeaux-threads")
        'find-asdf-component-by-path ; return component by its path.
        'load-package-file ; load only "package" file as a source
        'of-system ; declare that file is in system
        )

;(eval-when (:compile-toplevel)
;  (error "load this file as a source, don't compile it!"))

'mark-operation-done ; let's put this symbol here temporary

#|(defmacro of-system (keywordized-system-name)
  "Asserts that file belongs to system"
  `(eval-when (:compile-toplevel :load-toplevel)
     (when (boundp '*current-system*)
       (assert
           (eq (asdf-keywordize (component-name *current-system*))
               ,keywordized-system-name)
           ()
         "current file must be loaded through ~S system" ,keywordized-system-name))))|#

(defmacro of-system (keywordized-system-name)
  "Asserts that file belongs to system"
  `(eval-when (:compile-toplevel :load-toplevel)
     (when (boundp '*current-component*)
       (assert
           (member ,keywordized-system-name
                   (asdf::dump-component-asdf-path asdf::*current-component*) :test 'string-equal)
           ()
         "current file must be loaded through ~S system" ,keywordized-system-name))))


(defun ! (&rest args) "One more shortcut for (asdf:oos 'asdf:load-op ,@args)"
       (apply 'oos 'load-op args)
       (values))

#| :asdf3.1 (defun load-package-file (system &key (component-name "package")) 
  "Loads only one component of a system as a source. Intention is a load ahead package.lisp 
to resolve circular references between systems"
  ;(operate 'load-source-op (find-component (find-system system) component-name))
  (perform (make-instance 'load-source-op) (find-component (find-system system) component-name))
  ) |#

#| :asdf3.1 - does not recommend to load files as a source 
; from asdf-binary-locations
(defclass load-only-file-mixin ()
  ())


(defclass load-only-cl-source-file (load-only-file-mixin cl-source-file)
  ())

(defmethod perform :around ((op compile-op) (component load-only-file-mixin))
  nil)

(defmethod perform ((op load-op) (component load-only-cl-source-file))
  (load (component-pathname component)))


(defmethod output-files ((op compile-op) (c load-only-cl-source-file))
  (declare (ignorable op c))
  nil)

(defmethod operation-done-p ((op compile-op) (c load-only-cl-source-file))
  (declare (ignorable op c))
  t)

(defmethod input-files ((operation load-op) (c load-only-cl-source-file))
  (declare (ignorable op))
  (list (component-pathname c)))

|#

#| asdf3.1 - might be outdated

;;; the following are non-functions. In fact
(defvar *asdf-help* "Read my docstring"
  "Code snippets to use with asdf. 
To load only a source and not to compile a file,
change a type of a component: 
...
\(:load-only-cl-source-file \"your-file\") ; stolen from asdf-binary-locations

To return a system-object by a system name:
\(asdf:find-system system-name)

To return a component by system and component name
\(find-component (find-system :hallo) \"grovel-hallo\")

To learn a system's path
\(slot-value system-object 'asdf::relative-pathname))

Or (with this tools file)
\(find-asdf-component-by-path '(:system :module :submodule :name))

To learn an .asd version 
\(slot-value system-object 'asdf::version)

File have failed to load. We have perform on the stack. How do we learn what system is failed? 
\(slot-value asdf::c 'asfd::parent) ; asdf::c may be of type asdf::cl-source-file

Define dependency on file which is not compiled directly. E.g. we read some file with direct 
call to load
\(:module grovel-hallo-module
   :pathname \"\" ; define module in a same dir as a parent system
   :components
   ((:static-file \"resolved_types.h\")
    (cffi-grovel:grovel-file \"grovel-hallo\"
                :depends-on (\"resolved_types.h\"))
    )
   )
 ))

Find a path to fasls
\(asdf:apply-output-translations (asdf:system-source-directory (asdf:find-system :appserver)))
")

|#

(defun read-stream-to-string (in)
  (with-output-to-string (out)
    (let ((eof (gensym)))
      (do ((line (read-line in nil eof)
                 (read-line in nil eof)))
          ((eq line eof))
        (format out "~A~%" line)))))
 

#| :asdf2 does not work with cache (defun delete-fasls-on-system (system-name fasl-type)
  "Deletes all fasl files of system (w/o dependents)"
  (unless (member system-name *tabooed-system-names* :test 'equalp)
    (let ((dir (make-pathname :directory 
                              (butlast 
                               (pathname-directory (component-relative-pathname (asdf:find-system system-name)))) )))
      #+ignore (cl-fad:walk-directory dir (lambda (n) (when (cl-fad:directory-pathname-p n) (print n)))
                             :directories t 
                             :test (lambda (p) (string= (pathname-type p) fasl-type)))
      (when 
          (cl-fad:walk-directory dir 'print       :directories t :test (lambda (p) (string= (pathname-type p) fasl-type)))
        (break "Continue if you believe list of fasls is correct")
        (cl-fad:walk-directory dir 'delete-file :directories t :test (lambda (p) (string= (pathname-type p) fasl-type))))))) |#

#| :asdf3.1 - was not used, needs more testing
(defclass stat-op (operation) ()
  (:documentation "stat-op is intended for traversing all dependencies of the system and collecting some info on them. It finds dependencies just as load-op does. Do not forget :force t when invoking"))

(defmethod component-depends-on ((o stat-op) (c component))
  (let ((what-would-load-op-do (cdr (assoc 'load-op
                                           (slot-value c 'in-order-to)))))
    (mapcar (lambda (dep)
              (if (eq (car dep) 'load-op)
                  (cons (class-name (class-of o)) (cdr dep))
                  dep))
            what-would-load-op-do)))

(defparameter *stat-op-files* nil) 
(defvar *stat-op-component* nil)
(defvar *walk-op-function* nil) 


(defmethod perform ((o stat-op) (c source-file))
  (dolist (f (input-files o c))
    (account-for-file o c f)))

(defclass walk-op (stat-op) ((walk-function :initarg :walk-function 
					    :documentation "walker function accepts operation as its arguments and component"))
  (:documentation "walk-op walks along all dependencies as load would do and calls walk-function on two arguments: operation and component. 
To call, do (operate 'walk-op :am-util :force t :walk-function (lambda (x y) (print (list y))))"))


(defmethod perform ((o walk-op) (c component))
  (funcall (slot-value o 'walk-function) o c)
  #+ignore (funcall *walk-op-function* o c)) 

(defun account-for-file (operation c f)
  (declare (ignorable operation))
  (push
   `(,(dump-component-asdf-path c)
     ,(md5:md5sum-file f)
     ,(file-write-date f)
     )
   *stat-op-files*)) 

|#

(defun dump-component-asdf-path (component) 
  "Dumps a 'path' to this component in a global asdf systems namespace"
  (labels ((inner (c) 
	     (cons (component-name c)
		   (when (component-parent c)
		     (inner (component-parent c))))))
    (nreverse (inner (the component component)))))

(defun find-asdf-component-by-path (path &key (errorp t))
  "Returns a component with a path given by path"
  (block nil 
    (let (module)
      (labels ((inner (pa) 
;                 (print `(inner ,pa))
                 (cond
                   ((null pa) (return module)) ; error "no path specified"))
                   ((atom pa) 
                    (return (find-component module path)))
                  ; ((null (cdr pa)) (return module))
                   (t 
                    (setf module (find-component module (car pa)))
                    (cond 
                      (module (return (inner (cdr pa))))
                      (errorp (error "path not found: ~A" path))
                      (t (return nil)))))))
        (inner path)))))
                  


; edit-file, operate was located at asdf.lisp itself to be awailable as early as possible. We duplicate it here
; so that it won't be lost when upgrading asdf.lisp

(defun edit-file (filename)
  #+lispworks (editor:find-file-command nil filename)
  #+sbcl (swank:ed-in-emacs filename)
  #-(or lispworks sbcl) (format *debug-io* "Still don't know how to open file for editing in this CL implementation")
  )
(defgeneric edit-component-source (c))
(defmethod edit-component-source ((c source-file)) (edit-file (slot-value c 'absolute-pathname)))
(defmethod edit-component-source ((c module)) (edit-component-source (component-parent c)))
(defmethod edit-component-source ((c system)) (edit-file (system-source-file c)))

(defmethod edit-component-source ((c t))
  (format *debug-io* "Still don't know how to edit ~S source" c))
(define-symbol-macro e (edit-component-source *current-component*))
(define-symbol-macro ep (edit-component-source (component-parent *current-component*)))


#+asdf3.1
(defvar *current-component*)

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
                   (format s "~@<To edit ~A, type in asdf::e, to edit ~A, type in asdf::ep~@:>" (component-name component) (component-name parent))
                 (format s "~@<To edit ~A, type in asdf::e~@:>" (component-name component))
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



#| asdf3 - needs renewal
(defmethod operate (operation-class system &rest args
                    &key ((:verbose *asdf-verbose*) *asdf-verbose*) version force
                    &allow-other-keys)
  (declare (ignore force))
  (let* ((*package* *package*)
         (*readtable* *readtable*)
         (op (apply #'make-instance operation-class
                    :original-initargs args
                    args))
         (*verbose-out* (if *asdf-verbose* *standard-output* (make-broadcast-stream)))
         (system (if (typep system 'component) system (find-system system))))
    (unless (version-satisfies system version)
      (error 'missing-component-of-version :requires system :version version))
    (let ((*current-system* system)
          (steps (traverse op system)))
      (print `(operate ,*current-system*))
      (with-compilation-unit ()
        (loop :for (op . component) :in steps :do
          (loop
           (let ((*current-component* component))
             (restart-case
                 (progn
                   (perform-with-restarts op component)
                   (return))
               (edit-component ()
                               :report
                               (lambda (s)
                                 (let ((parent (component-parent component)))
                                   (if parent
                                       (format s "~@<To edit ~A, type in asdf::e, to edit ~A, type in asdf::ep~@:>" (component-name component) (component-name parent))
                                     (format s "~@<To edit ~A, type in asdf::e~@:>" (component-name component))
                                     ))))
               (retry ()
                      :report
                      (lambda (s)
                        (format s "~@<Retry ~A.~@:>" (operation-description op component))))
               (accept ()
                       :report
                       (lambda (s)
                         (format s "~@<Continue, treating ~A as having been successful.~@:>"
                                 (operation-description op component)))
                       (setf (gethash (type-of op)
                                      (component-operation-times component))
                             (get-universal-time))
                       (return)))))))
        (values op steps)))) |#



#| asdf3.1 hopefully not needed (defmacro def-cl-file-with-external-format-class (external-format class-name &key (predecessor 'cl-source-file))
  "Defines new file type which compiles with an external-format specified. In a simplest form, args are like :windows-1251 'asdf::windows-1251-file.
Copy-paste from predecessor's perform method"
  `(progn
     (defclass ,class-name (,predecessor) ())
     (defmethod perform ((operation compile-op) (c ,class-name))
       #-:broken-fasl-loader
       (let ((source-file (component-pathname c))
	     (output-file (car (output-files operation c))))
	 (multiple-value-bind (output warnings-p failure-p)
	     (compile-file source-file :output-file output-file :external-format ,external-format)
	   (when warnings-p
	     (case (operation-on-warnings operation)
	       (:warn (warn
		       "~@<COMPILE-FILE warned while performing ~A on ~A.~@:>"
		       operation c))
	       (:error (error 'compile-warned :component c :operation operation))
	       (:ignore nil)))
	   (when failure-p
	     (case (operation-on-failure operation)
	       (:warn (warn
		       "~@<COMPILE-FILE failed while performing ~A on ~A.~@:>"
		       operation c))
	       (:error (error 'compile-failed :component c :operation operation))
	       (:ignore nil)))
	   (unless output
	     (error 'compile-error :component c :operation operation)))))
     (defmethod perform ((o load-op) (c ,class-name))
       (dolist (f (input-files o c)) (load f :external-format ,external-format))
       )))

; (def-cl-file-with-external-format-class :windows-1251 asdf::windows-1251-file)

|#

#| asdf3.1 - depends on stat-op which is commented out
 (defun dump-all-md5-and-write-dates-in-system (system-name) 
  "Run this to get md5 sum and write date of all files that are required by your system. Required
is meant in terms of load operation"
  (let (*stat-op-files*)
    (oos 'stat-op system-name :force t) 
    *stat-op-files*)) |#

#| seem to be unused 
 (defclass around-load-op-mixin () () 
  (:documentation "Mixin to be able to do some actions around load-op")) 


 (defmethod perform :around ((o operation) (c around-load-op-mixin))
  (let ((*readtable* (copy-readtable nil)))
    (call-next-method)
    ))  

|#

#| maybe-later
 (defmethod print-object ((c source-file) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (ignore-errors
      (format stream "~A ~S" (dump-component-asdf-path c) (component-pathname c))
      )))
|#


(defun keywordize (symbol) "From iterate" ; see also careful-keywordize
  (intern (symbol-name symbol) :keyword))

(defun lock-system (name) 
  "Loads system and then forget about its contents. This prevents the system from being affected by any further asdf operations, but
if you undefsystem it"
  #+asdf3 (break "didn't tested in asdf3")
  (oos 'load-op name)
  (let ((system (find-system name)))
    (dolist (slot-name '(components do-first in-order-to))
              (setf (slot-value system slot-name) nil))))

(defun lock-tabooed-systems () "Utility function which locks all tabooed systems"
  #+asdf3 (break "didn't tested in asdf3")
  #-asdf3 (dolist (name *tabooed-system-names*) (lock-system name)))

;    (when (ignore-errors (find-system "sb-cover")) 
;      (lock-system name))))

(defun undefsystem (name &key (feature t) (reload nil) (fasls "fasl") packages dependents-too)
  "Removes the system from memory. This is required when:
- you relocate your asd file
- you change class of some component
"
                                        ;  (break "This function is completely untested. It deletes files! It is not too late to return")
  #+asdf3 (break "didn't tested in asdf3")
  #-asdf3
  (progn
    (assert (not packages) () ":packages not implemented")
    (assert (not dependents-too) () ":dependents-too not implemented") 
    (assert (not (eq fasls 't)) () "undefsystem says: RTFM")
    (when (member name *tabooed-system-names* :test 'equalp) (return-from undefsystem nil))
    (let ((system (find-system name)))
      (unless system
        (warn "system ~S not found" name)
        (return-from undefsystem nil))
      (let ((system-asdf-name (component-name system))
            (system-kw (keywordize name)))
					;  (assert (not (eq *package* (find-package :sw))))
        #+ignore
        (when (find-package :sw)
          (delete-package :sw)
          )
                                        ;      (when fasls (delete-fasls-on-system name fasls))
        (when feature (setf *features* (delete system-kw *features*)))
        (remhash system-asdf-name asdf::*defined-systems*)
        (when reload (oos 'load-op name))
        ))))


(defun run-shell-command-here (control-string &rest args) "In SBCL, run-shell-command ignores *default-pathname-defaults*. Let's fix it and also bind verbose-out to standard-output"
       (let ((command #-sbcl command
                      #+sbcl 
                      (format nil "cd ~A; ~A" *default-pathname-defaults*
                              (apply 'format nil control-string args)))
             )
         (asdf:run-shell-command command)))

(defun run-shell-command-here-to-stdout (control-string &rest args) 
  "run-shell-command-here to *standard-output*"
  (let ((asdf::*verbose-out* *standard-output*)) (apply 'run-shell-command-here control-string args)))


(defclass dependable-file (source-file) () 
          (:documentation "Just a file to depend on. No activity goes on when it is being 'compiled' or 'loaded',
  but other components can depend on it so they're recompiled when the file changes. It is highly recommended to give it a pathname option"))

(defmethod source-file-type ((c dependable-file) s) nil)
(defmethod perform ((op compile-op) (c dependable-file)) (print "compiled resolved_types.h") nil)
(defmethod perform ((op load-op) (c dependable-file)) (print "loaded resolved_types.h") nil)


#| asdf3.1; seem to be unused
 (defun load-system-by-asd-file (asd-pathname &key (add-path-to-central-registry t))
  (flet ((path-to-a-file (filename) "d:/foo/file.ext --> d:/foo/" 
           (let ((p (pathname filename)))
             (make-pathname 
              :host (pathname-host p) 
              :directory (pathname-directory p)))))
    (let ((dir (path-to-a-file asd-pathname))
          (name (pathname-name asd-pathname)))
      (cond
       (add-path-to-central-registry
        (pushnew dir *central-registry* :test 'equalp)
        (load-system name))
       (t
        (let ((*central-registry* (cons dir *central-registry*)))
          (load-system name)))))))
|#

(defclass package-file (cl-source-file) 
  (
   (packages :initarg :packages))
  (:documentation "Represents file with package definitions. List files as a value of :packages initarg"))

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



;(defmacro my-macro (x) `,x)
;(defmacro decorate-

 ; `(call-next-advice ,name ,@options))



#|(defmacro defsystem (name &body options)
  #-lispworks `(defsystem-inner ,name ,options)
  #+lispworks `(dspec:def (defsystem ,(asdf-keywordize name))
                 (when (dspec::record-definition `(defsystem ,',(asdf-keywordize name))
                                          (dspec:location))
                   (defsystem-inner ,name ,@options))
                 ))|# 
