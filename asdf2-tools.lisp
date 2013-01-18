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

(eval-when (:compile-toplevel)
  (error "load this file as a source, don't compile it!"))

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
       (apply 'oos 'load-op args))

(defun load-package-file (system &key (component-name "package")) 
  "Loads only one component of a system as a source. Intention is a load ahead package.lisp 
to resolve circular references between systems"
  ;(operate 'load-source-op (find-component (find-system system) component-name))
  (perform (make-instance 'load-source-op) (find-component (find-system system) component-name))
  )


;;; the following are non-functions. In fact
(defvar *asdf-help* "Read my docstring"
  "Code snippets to use with asdf. 
To load only a source and not to compile a file: 
change a type of a component: 
\(asdf::! :asdf-binary-locations) 
...
\(asdf-binary-locations-system::load-only-cl-source-file \"your-file\")

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
")

(defun read-stream-to-string (in)
  (with-output-to-string (out)
    (let ((eof (gensym)))
      (do ((line (read-line in nil eof)
                 (read-line in nil eof)))
          ((eq line eof))
        (format out "~A~%" line)))))
 

#-asdf2 (defun %command-output (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell,
returns (VALUES string-output error-output exit-status)"
  (let ((command (apply #'format nil control-string args)))
    #+sbcl
    (let* ((process (sb-ext:run-program
                    "/bin/sh"
                    (list "-c" command)
                    :input nil :output :stream :error :stream))
           (output (read-stream-to-string (sb-impl::process-output process)))
           (error (read-stream-to-string (sb-impl::process-error process))))
      (close (sb-impl::process-output process))
      (close (sb-impl::process-error process))
      (values
       output
       error
       (sb-impl::process-exit-code process)))


    #+(or cmu scl)
    (let* ((process (ext:run-program
                     "/bin/sh"
                     (list "-c" command)
                     :input nil :output :stream :error :stream))
           (output (read-stream-to-string (ext::process-output process)))
           (error (read-stream-to-string (ext::process-error process))))
      (close (ext::process-output process))
      (close (ext::process-error process))

      (values
       output
       error
       (ext::process-exit-code process)))

    #+allegro
    (multiple-value-bind (output error status)
        (excl.osi:command-output command :whole t)
      (values output error status))

    #+lispworks
    ;; BUG: Lispworks combines output and error streams
    (let ((output (make-string-output-stream)))
      (unwind-protect
          (let ((status
                 (system:call-system-showing-output
                  command
                  :prefix "" ; added by budden
                  :show-cmd nil ; added by budden
                  :current-directory (namestring (user-homedir-pathname)) ; added by budden
                  :shell-type "/bin/sh"
                  :output-stream output)))
            (values (get-output-stream-string output) nil status))
        (close output)))

    #+clisp
    ;; BUG: CLisp doesn't allow output to user-specified stream
    (values
     nil
     nil
     (ext:run-shell-command  command :output :terminal :wait t))

    #+(and openmcl (not windows))
    (let* ((process (ccl:run-program
                     "/bin/sh"
                     (list "-c" command)
                     :input nil :output :stream :error :stream
                     :wait t)
                    )
           (output (read-stream-to-string (ccl::external-process-output process)))
           (error (read-stream-to-string (ccl::external-process-error process))))
      (close (ccl::external-process-output process))
      (close (ccl::external-process-error process))
      (values output
              error
              (nth-value 1 (ccl::external-process-status process))))
    #+(and openmcl windows)
    (let (out err status)
      (with-output-to-string (output)
        (with-output-to-string (error)
          (setf status
                (ccl:external-process-status 
                 (ccl:run-program "cmd /c" (list command) :output output :error error
                                  )))
          (setf out (copy-seq (get-output-stream-string output)))
          (setf err (get-output-stream-string error))
          ))
        (values out err status))
    #-(or openmcl clisp lispworks allegro scl cmu sbcl)
    (error "COMMAND-OUTPUT not implemented for this Lisp")
    ))

(defparameter *sysdef-central-registry-search-by-lnk-files-memo* nil)

#+(or (and lispworks win32) (and allegro microsoft-32) (and ccl windows))
(defun sysdef-central-registry-search-by-lnk-files (system)
  "Finds a system.asd.lnk in a central registry directories resolves symlink with gnu readlink to a system name"
  (let*
      ((name (asdf::coerce-name system))
       (memo-val (assoc name *sysdef-central-registry-search-by-lnk-files-memo* :test 'string-equal)))
    (if memo-val (cdr memo-val)
      (let ((result 
             (block nil
               (dolist (dir asdf::*central-registry*)
                 (let* ((defaults (eval dir))
                        (file (and defaults
                                   (concatenate 'string (namestring defaults) name ".asd.lnk"))))
                   (when (and file (probe-file file))
;              (with-input-from-string 
;                  (command-stdout 
;                   (with-output-to-string (asdf::*verbose-out*) 
;                     (%command-output
;                      "readlink ~S" (namestring file))))
;                (read-line command-stdout) ; skip first line where command is printed out
;                #+lispworks (progn
;                              (read-line command-stdout) ; on the second line, too
;                              (dotimes (i 2) (read-char command-stdout)) ; prefix "; "
;                              )
;                     (print file)
                     (with-input-from-string (command-stdout 
                                                (%command-output "readlink ~S" file))
                         (return (pathname #-(and ccl windows) 
                                           (read-line command-stdout nil nil)
                                           #+(and ccl windows) 
                                           (remove #\Return (read-line command-stdout nil nil))))))))
               )))
        (push (cons name result) *sysdef-central-registry-search-by-lnk-files-memo*)
        result))))


; make sure asdf-install:sysdef-source-dir-search is not here!
#+(and (not asdf2) (or (and lispworks win32) (and allegro microsoft-32) (and ccl windows)))
(pushnew 'sysdef-central-registry-search-by-lnk-files *system-definition-search-functions*)

; (trace sysdef-central-registry-search-by-lnk-files)


(oos 'load-op :md5)
(oos 'load-op :cl-fad)

(defparameter *tabooed-system-names*
  #+sbcl '(:asdf-install :sb-aclrepl :sb-bsd-sockets :sb-cltl2 #+ignore :sb-cover :sb-grovel :sb-md5 :sb-posix :sb-rotate-byte :sb-rt :sb-simple-streams)
  #-sbcl nil
  "Systems which should never be undefined as they're part of implemetation source")

(defun delete-fasls-on-system (system-name fasl-type)
  "Deletes all fasl files of system (w/o dependents)"
  (break "don't run me. I'm harmful")
  (unless (member system-name *tabooed-system-names* :test 'equalp)
    (let ((dir (make-pathname :directory 
                              (butlast 
                               (pathname-directory (component-relative-pathname (asdf:find-system system-name)))) )))
      #+ignore (cl-fad:walk-directory dir (lambda (n) (when (cl-fad:directory-pathname-p n) (print n)))
                             :directories t 
                             :test (lambda (p) (string= (pathname-type p) fasl-type)))
      (cl-fad:walk-directory dir 'print       :directories t :test (lambda (p) (string= (pathname-type p) fasl-type)))
      #+ignore (cl-fad:walk-directory dir 'delete-file :directories t :test (lambda (p) (string= (pathname-type p) fasl-type))))))

(defun delete-fasls-in-directory (directory fasl-type)
  "Sometimes it looks more safe than recursive deletion of fasls on system"
  (cl-fad:walk-directory directory 'print :directories t :test (lambda (p) (string= (pathname-type p) fasl-type)))
  (cl-fad:walk-directory directory 'delete-file :directories t :test (lambda (p) (string= (pathname-type p) fasl-type)))
  )

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
                  

(defun account-for-file (operation c f)
  (declare (ignorable operation))
  (push
   `(,(dump-component-asdf-path c)
     ,(md5:md5sum-file f)
     ,(file-write-date f)
     )
   *stat-op-files*))



; edit-file, operate was located at asdf.lisp itself to be awailable as early as possible. We duplicate it here
; so that it won't be lost when upgrading asdf.lisp

(defun edit-file (filename)
  #+lispworks (editor:find-file-command nil filename)
  #-lispworks (format *debug-io* "Still don't know how to open file for editing in this CL implementation")
  )
(defgeneric edit-component-source (c))
(defmethod edit-component-source ((c source-file)) (edit-file (slot-value c 'absolute-pathname)))
(defmethod edit-component-source ((c module)) (edit-component-source (component-parent c)))
(defmethod edit-component-source ((c system)) (edit-file (system-source-file c)))

(defmethod edit-component-source ((c t))
  (format *debug-io* "Still don't know how to edit ~S source" c))
(define-symbol-macro e (edit-component-source *current-component*))
(define-symbol-macro ep (edit-component-source (component-parent *current-component*)))

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
        (values op steps))))


#+maybe-not-needed
(defmethod operation-done-p ((o compile-op) (c component))
  "Copy of original operation-done-p to account load-op as compile-op"
  (flet ((fwd-or-return-t (file)
           ;; if FILE-WRITE-DATE returns NIL, it's possible that the
           ;; user or some other agent has deleted an input file.  If
           ;; that's the case, well, that's not good, but as long as
           ;; the operation is otherwise considered to be done we
           ;; could continue and survive.
           (let ((date (file-write-date file)))
             (cond
               (date)
               (t
                (warn "~@<Missing FILE-WRITE-DATE for ~S: treating ~
                       operation ~S on component ~S as done.~@:>"
                      file o c)
                (return-from operation-done-p t))))))
    (let ((out-files (output-files o c))
          (in-files (input-files o c)))
      (cond ((and (not in-files) (not out-files))
             ;; arbitrary decision: an operation that uses nothing to
             ;; produce nothing probably isn't doing much
             t)
            ((not out-files)
             (let ((op-done
                    (or 
                     (gethash 'load-op (component-operation-times c))
                     (gethash (type-of o) (component-operation-times c)))))
               (and op-done
                    (>= op-done
                        (apply #'max
                               (mapcar #'fwd-or-return-t in-files))))))
            ((not in-files) nil)
            (t
             (and
              (every #'probe-file out-files)
              (> (apply #'min (mapcar #'file-write-date out-files))
                 (apply #'max (mapcar #'fwd-or-return-t in-files)))))))))


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

(defmacro def-cl-file-with-external-format-class (external-format class-name &key (predecessor 'cl-source-file))
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

(def-cl-file-with-external-format-class :windows-1251 asdf::windows-1251-file)

(defun dump-all-md5-and-write-dates-in-system (system-name) 
  "Run this to get md5 sum and write date of all files that are required by your system. Required
is meant in terms of load operation"
  (let (*stat-op-files*)
    (oos 'stat-op system-name :force t) 
    *stat-op-files*))

(defclass around-load-op-mixin () () 
  (:documentation "Mixin to be able to do some actions around load-op"))

;;; !!! Redefining asdf class!
#-asdf2 (defclass cl-source-file (around-load-op-mixin source-file) ())


#+maybe-later
(defmethod print-object ((c source-file) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (ignore-errors
      (format stream "~A ~S" (dump-component-asdf-path c) (component-pathname c))
      )))


(defmethod perform :around ((o operation) (c around-load-op-mixin))
  (let ((*readtable* (copy-readtable nil)))
    (call-next-method)
    ))  

(defun keywordize (symbol) "From iterate" ; see also careful-keywordize
  (intern (symbol-name symbol) :keyword))

(defun lock-system (name) 
  "Loads system and then forget about its contents. This prevents the system from being affected by any further asdf operations, but
if you undefsystem it"
  (oos 'load-op name)
  (let ((system (find-system name)))
    (dolist (slot-name '(components do-first in-order-to))
              (setf (slot-value system slot-name) nil))))

(defun lock-tabooed-systems () "Utility function which locks all tabooed systems"
  (dolist (name *tabooed-system-names*) (lock-system name)))

;    (when (ignore-errors (find-system "sb-cover")) 
;      (lock-system name))))

(defun undefsystem (name &key (feature t) (reload nil) (fasls "fasl") packages dependents-too)
  "Removes the system from memory. This is required when:
- you relocate your asd file
- you change class of some component
"
;  (break "This function is completely untested. It deletes files! It is not too late to return")
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
      )))


(defun delete-fasls-on-system-force-t (system-name fasl-type) 
;  (break "Ne nado! This will destroy your sbcl installation!")
  (break "Don't run me! I'm harmful!")
  (asdf::operate 'asdf::walk-op system-name
		 :force t :walk-function
		 (lambda (o c) 
                   (declare (ignore o))
                   (when (eq (type-of c) 'asdf:system) 
                     (asdf::delete-fasls-on-system (asdf::component-name c) fasl-type)))))


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


(defun load-system-by-asd-file (asd-pathname)
  (flet ((path-to-a-file (filename) "d:/foo/file.ext --> d:/foo/" 
           (let ((p (pathname filename)))
             (make-pathname 
              :host (pathname-host p) 
              :directory (pathname-directory p)))))
    (let* ((dir (path-to-a-file asd-pathname))
           (asdf:*central-registry* (cons dir asdf:*central-registry*)))
      (! (pathname-name asd-pathname)))))

(defclass package-file (cl-source-file) 
  (
   (packages :initarg :packages))
  (:documentation "Represents file with package definitions. List files as a value of :packages initarg"))

    
  