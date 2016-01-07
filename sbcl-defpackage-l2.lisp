;;;; (C) Денис Будяк 2016
;;;; Это переделка DEFPACKAGE из SBCL
#| 
  Изменения:

  Можно только использовать другие пакеты через :use, нет :import и :shadowing-import
  Конфликт символов разрешается автоматически запретом конфликтующих
  Нельзя экспортировать символы, полученные через :use

|#

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package :defpackage-l2)

(defmacro defpkg2 (package &rest options)
  #.(format nil
  "Defines a new package called PACKAGE. Each of OPTIONS should be one of the
   following: ~{~&~4T~A~}
   All options except ~{~A, ~}and :DOCUMENTATION can be used multiple
   times."
  '((:use "{package-name}*")
    (:export "{symbol-name}*")
    (:local-nicknames "{local-nickname actual-package-name}*")
    #+sb-package-locks (:lock "boolean")
    (:documentation "doc-string")
    (:size "<integer>")
    )
  '(:size #+sb-package-locks :lock))
  (let ((local-nicknames nil)
        (size nil)
        (use nil)
        (use-p nil)
        (exports nil)
        (lock nil)
        (doc nil))
    (dolist (option options)
      (unless (consp option)
        (error 'simple-program-error
               :format-control "Неизвестная опция DEFPKG2: ~S"
               :format-arguments (list option)))
      (case (car option)
        (:local-nicknames
         (setf local-nicknames
               (append local-nicknames
                       (mapcar (lambda (spec)
                                 (destructuring-bind (nick name) spec
                                   (cons (stringify-package-designator nick)
                                         (stringify-package-designator name))))
                               (cdr option)))))
        (:size
         (cond (size
                (error 'simple-program-error
                       :format-control "can't specify :SIZE twice."))
               ((and (consp (cdr option))
                     (typep (second option) 'unsigned-byte))
                (setf size (second option)))
               (t
                (error
                 'simple-program-error
                 :format-control ":SIZE is not a positive integer: ~S"
                 :format-arguments (list (second option))))))
        (:use
         (setf use (append use (stringify-package-designators (cdr option)) )
               use-p t))
        (:export
         (let ((new (stringify-string-designators (cdr option))))
           (setf exports (append exports new))))
        #+sb-package-locks
        (:lock
         (when lock
           (error 'simple-program-error
                  :format-control "multiple :LOCK options"))
         (setf lock (coerce (second option) 'boolean)))
        (:documentation
         (when doc
           (error 'simple-program-error
                  :format-control "multiple :DOCUMENTATION options"))
         (setf doc (coerce (second option) 'simple-string)))
        (t
         (error 'simple-program-error
                :format-control "bogus DEFPACKAGE option: ~S"
                :format-arguments (list option)))))
    `;(eval-when (:compile-toplevel :load-toplevel :execute)
       (%defpackage ,(stringify-string-designator package) ',size
                    ',(if use-p use :default)
                    ',exports ',local-nicknames
                    ',lock (sb-c:source-location)
                    ,@(and doc
                           `(,doc)))
     ;)
  ))

(defun check-disjoint (&rest args)
  ;; An arg is (:key . set)
  (do ((list args (cdr list)))
      ((endp list))
    (loop
      with x = (car list)
      for y in (rest list)
      for z = (remove-duplicates (intersection (cdr x)(cdr y) :test #'string=))
      when z do (error 'simple-program-error
                       :format-control "Parameters ~S and ~S must be disjoint ~
                                        but have common elements ~%   ~S"
                       :format-arguments (list (car x)(car y) z)))))

(defun stringify-string-designator (string-designator)
  (typecase string-designator
    (simple-string string-designator)
    (string (coerce string-designator 'simple-string))
    (symbol (symbol-name string-designator))
    (character (string string-designator))
    (t
     (error "~S does not designate a string" string-designator))))

(defun stringify-string-designators (string-designators)
  (mapcar #'stringify-string-designator string-designators))

(defun stringify-package-designator (package-designator)
  (typecase package-designator
    (simple-string package-designator)
    (string (coerce package-designator 'simple-string))
    (symbol (symbol-name package-designator))
    (character (string package-designator))
    (package (package-name package-designator))
    (t
     (error "~S does not designate a package" package-designator))))

(defun stringify-package-designators (package-designators)
  (mapcar #'stringify-package-designator package-designators))

(defun import-list-symbols (import-list)
  (let ((symbols nil))
    (dolist (import import-list symbols)
      (destructuring-bind (package-name &rest symbol-names)
          import
        (let ((package (sb-impl::find-undeleted-package-or-lose package-name)))
          (mapcar (lambda (name)
                    (push (find-or-make-symbol name package) symbols))
                  symbol-names))))))

(defun use-list-packages (package package-designators)
  (cond ((listp package-designators)
         (mapcar #'sb-impl::find-undeleted-package-or-lose package-designators))
        (package
         ;; :default for an existing package means preserve the
         ;; existing use list
         (package-use-list package))
        (t
         nil)))

(defun update-package (package source-location
                       use
                       exports local-nicknames
                       lock doc-string)
  (declare #-sb-package-locks
           (ignore implement lock))
  ;; 1. :shadow and :shadowing-import-from
  ;;
  ;; shadows is a list of strings, shadowing-imports is a list of symbols.
  ; (shadow shadows package)
  ;; 2. :use
  ;;
  ;; use is a list of package objects.
  (use-package use package)
  ;; 4. :export
  ;;
  ;; exports is a list of strings
  (export (mapcar (lambda (symbol-name) (intern symbol-name package))
                  exports)
          package)
  ;; Everything was created: update metadata
  (sb-c:with-source-location (source-location)
    (setf (sb-impl::package-source-location package) source-location))
  (setf (sb-impl::package-doc-string package) doc-string)
  ;; Handle lock
  #+sb-package-locks
  (setf (sb-impl::package-lock package) lock)
  ;; Local nicknames. Throw out the old ones.
  (setf (sb-impl::package-%local-nicknames package) nil)
  (dolist (spec local-nicknames)
    (sb-impl::add-package-local-nickname (car spec) (cdr spec) package))
  package)

(declaim (type list *on-package-variance*))
(defvar *on-package-variance* '(:warn t)
  #+sb-doc
  "Specifies behavior when redefining a package using DEFPACKAGE and the
definition is in variance with the current state of the package.

The value should be of the form:

  (:WARN [T | packages-names] :ERROR [T | package-names])

specifying which packages get which behaviour -- with T signifying the default unless
otherwise specified. If default is not specified, :WARN is used.

:WARN keeps as much state as possible and causes SBCL to signal a full warning.

:ERROR causes SBCL to signal an error when the variant DEFPACKAGE form is executed,
with restarts provided for user to specify what action should be taken.

Example:

  (setf *on-package-variance* '(:warn (:swank :swank-backend) :error t))

specifies to signal a warning if SWANK package is in variance, and an error otherwise.")

(defun note-package-variance (&rest args &key package &allow-other-keys)
  (let ((pname (package-name package)))
    (destructuring-bind (&key warn error) *on-package-variance*
      (let ((what (cond ((and (listp error) (member pname error :test #'string=))
                         :error)
                        ((and (listp warn) (member pname warn :test #'string=))
                         :warn)
                        ((eq t error)
                         :error)
                        (t
                         :warn))))
        (ecase what
          (:error
           (apply #'error 'package-at-variance-error args))
          (:warn
           (apply #'warn 'package-at-variance args)))))))

(defun update-package-with-variance (package name source-location
                                     use
                                     exports
                                     local-nicknames
                                     lock doc-string)
  (unless (string= (the string (package-name package)) name)
    (error 'simple-package-error
           :package name
           :format-control "~A is a nickname for the package ~A"
           :format-arguments (list name (package-name name))))
  (let ((no-longer-used (set-difference (package-use-list package) use)))
    (when no-longer-used
      (restart-case
          (note-package-variance
           :format-control "~A also uses the following packages:~%  ~A"
           :format-arguments (list name (mapcar #'package-name no-longer-used))
           :package package)
        (drop-them ()
          :report "Stop using them."
          (unuse-package no-longer-used package))
        (keep-them ()
          :report "Keep using them."))))
  (let (old-exports)
    (do-external-symbols (s package)
      (push s old-exports))
    (let ((no-longer-exported (set-difference old-exports exports :test #'string=)))
     (when no-longer-exported
       (restart-case
           (note-package-variance
            :format-control "~A also exports the following symbols:~%  ~S"
            :format-arguments (list name no-longer-exported)
            :package package)
         (drop-them ()
           :report "Unexport them."
           (unexport no-longer-exported package))
         (keep-them ()
           :report "Keep exporting them.")))))
  (update-package package source-location
                  use exports local-nicknames lock doc-string))

(defun %defpackage (name size use exports local-nicknames
                    lock source-location &optional doc)
  (declare (type simple-string name)
           (type list exports)
           (type (or list (member :default)) use)
           (type (or simple-string null) doc))
  (sb-impl::with-package-graph ()
    (let* ((existing-package (find-package name))
           (use (use-list-packages existing-package use))
           )
      (if existing-package
          (update-package-with-variance existing-package name
                                        source-location
                                        use exports
                                        local-nicknames
                                        lock doc)
          (let ((package (make-package name
                                       :use nil
                                       :internal-symbols (or size 10)
                                       :external-symbols (length exports))))
            (update-package package
                            source-location
                            use exports
                            local-nicknames
                            lock doc))))))

(defun find-or-make-symbol (name package)
  (multiple-value-bind (symbol how) (find-symbol name package)
    (cond (how
           symbol)
          (t
           (with-simple-restart (continue "INTERN it.")
             (error 'simple-package-error
                    :package package
                    :format-control "no symbol named ~S in ~S"
                    :format-arguments (list name (package-name package))))
           (intern name package)))))
