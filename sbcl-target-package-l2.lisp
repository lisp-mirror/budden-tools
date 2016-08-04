;;;; Зависит от :def-merge-packages 
;;;; Копия target-package.lisp . 

;;;; Здесь не всё, что было в оригинальном файле. Оставляем то, что поменяли и то,
;;;; что подлежит русификации

;;;; Пытаемся обойтись малой кровью и 
;;;; вместо того, чтобы создавать заново всю систему пакетов, 
;;;; пропатчить систему пакетов SBCL на горячую.

;;;; Это оправдывается тем, что нужно поменять практически весь интерфейс
;;;; Вроде бы у нас получается сделать систему запретных символов путём
;;;; совсем небольших изменений. НО. Она будет работать только при условии,
;;;; что пакеты с запретами ничего не импортируют явно, не запрещают явно.
;;;; Также неясно со shadowing-import. Если нам понадобится что-то иное, 
;;;; мы всегда можем попробовать при каком-то изменении перевыполнять последнюю
;;;; запомненную форму defpackage. 
;;;; 


;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package :sb-impl)

; правильным будет загружать файл внутри unwind-protect с восстановлением
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unlock-package :sb-impl)
  (unlock-package :sb-ext)
  (unlock-package :sb-kernel)
  (unlock-package :common-lisp)
  (unlock-package :sb-int)
  ; а в конце их нужно заблокировать обратно!
  )


(defun call-with-package-graph (function)
  (declare (function function))
  ;; FIXME: Since name conflicts can be signalled while holding the
  ;; mutex, user code can be run leading to lock ordering problems.
  (sb-thread:with-recursive-lock (*package-graph-lock*)
    (funcall function)))

;;; a map from package names to packages
(defvar *package-names*)
(declaim (type hash-table *package-names*))

(defmacro with-package-names ((names &key) &body body)
  `(let ((,names *package-names*))
     (with-locked-system-table (,names)
       ,@body)))

; copied from c:/clcon/sbcl/1.3.0/source/src/code/defpackage.lisp
(eval-when (:compile-toplevel)
  (defparameter *default-package-use-list*
    ;; ANSI says this is implementation-defined. So we make it NIL,
    ;; the way God intended. Anyone who actually wants a random value
    ;; is free to :USE (PACKAGE-USE-LIST :CL-USER) anyway.:-|
    nil))

#+sb-package-locks
(progn

(defun package-lock-violation (package &key (symbol nil symbol-p)
                               format-control format-arguments)
  (let* ((restart :continue)
         (cl-violation-p (eq package *cl-package*))
         (error-arguments
          (append (list (if symbol-p
                            'symbol-package-locked-error
                            'package-locked-error)
                        :package package
                        :format-control format-control
                        :format-arguments format-arguments)
                  (when symbol-p (list :symbol symbol))
                  (list :references
                        (append '((:sbcl :node "Package Locks"))
                                (when cl-violation-p
                                  '((:ansi-cl :section (11 1 2 1 2)))))))))
    (restart-case
        (apply #'cerror "Ignore the package lock." error-arguments)
      (:ignore-all ()
        :report "Ignore all package locks in the context of this operation."
        (setf restart :ignore-all))
      (:unlock-package ()
        :report "Unlock the package."
        (setf restart :unlock-package)))
    (ecase restart
      (:continue
       (pushnew package *ignored-package-locks*))
      (:ignore-all
       (setf *ignored-package-locks* t))
      (:unlock-package
       (unlock-package package)))))

(defun package-lock-violation-p (package &optional (symbol nil symbolp))
  ;; KLUDGE: (package-lock package) needs to be before
  ;; comparison to *package*, since during cold init this gets
  ;; called before *package* is bound -- but no package should
  ;; be locked at that point.
  (and package
       (package-lock package)
       ;; In package or implementation package
       (not (or (eq package *package*)
                (member *package* (package-%implementation-packages package))))
       ;; Runtime disabling
       (not (eq t *ignored-package-locks*))
       (or (eq :invalid *ignored-package-locks*)
           (not (member package *ignored-package-locks*)))
       ;; declarations for symbols
       (not (and symbolp (lexically-unlocked-symbol-p symbol)))))

(defun lexically-unlocked-symbol-p (symbol)
  (member symbol
          (if (boundp 'sb-c::*lexenv*)
              (let ((list (sb-c::lexenv-disabled-package-locks sb-c::*lexenv*)))
                ;; The so-called LIST might be an interpreter env.
                #+sb-fasteval
                (unless (listp list)
                  (return-from lexically-unlocked-symbol-p
                    (sb-interpreter::lexically-unlocked-symbol-p
                     symbol list)))
                list)
              sb-c::*disabled-package-locks*)))

) ; progn

;;;; more package-locking these are NOPs unless :sb-package-locks is
;;;; in target features. Cross-compiler NOPs for these are in cross-misc.

;;; The right way to establish a package lock context is
;;; WITH-SINGLE-PACKAGE-LOCKED-ERROR, defined in early-package.lisp
;;;


;;;; miscellaneous PACKAGE operations

(defmethod print-object ((package package) stream)
  (let ((name (package-%name package)))
    (print-unreadable-object (package stream :type t :identity (not name))
      (if name (prin1 name stream) (write-string "(deleted)" stream)))))

(defun package-local-nicknames (package-designator)
  #+sb-doc
  "Returns an alist of \(local-nickname . actual-package) describing the
nicknames local to the designated package.

When in the designated package, calls to FIND-PACKAGE with the any of the
local-nicknames will return the corresponding actual-package instead. This
also affects all implied calls to FIND-PACKAGE, including those performed by
the reader.

When printing a package prefix for a symbol with a package local nickname, the
local nickname is used instead of the real name in order to preserve
print-read consistency.

See also: ADD-PACKAGE-LOCAL-NICKNAME, PACKAGE-LOCALLY-NICKNAMED-BY-LIST,
REMOVE-PACKAGE-LOCAL-NICKNAME, and the DEFPACKAGE option :LOCAL-NICKNAMES.

Experimental: interface subject to change."
  (copy-tree
   (package-%local-nicknames
    (find-undeleted-package-or-lose package-designator))))

(defun package-locally-nicknamed-by-list (package-designator)
  #+sb-doc
  "Returns a list of packages which have a local nickname for the designated
package.

See also: ADD-PACKAGE-LOCAL-NICKNAME, PACKAGE-LOCAL-NICKNAMES,
REMOVE-PACKAGE-LOCAL-NICKNAME, and the DEFPACKAGE option :LOCAL-NICKNAMES.

Experimental: interface subject to change."
  (copy-list
   (package-%locally-nicknamed-by
    (find-undeleted-package-or-lose package-designator))))

(defun add-package-local-nickname (local-nickname actual-package
                                   &optional (package-designator (sane-package)))
  #+sb-doc
  "Adds LOCAL-NICKNAME for ACTUAL-PACKAGE in the designated package, defaulting
to current package. LOCAL-NICKNAME must be a string designator, and
ACTUAL-PACKAGE must be a package designator.

Returns the designated package.

Signals a continuable error if LOCAL-NICKNAME is already a package local
nickname for a different package, or if LOCAL-NICKNAME is one of \"CL\",
\"COMMON-LISP\", or, \"KEYWORD\", or if LOCAL-NICKNAME is a global name or
nickname for the package to which the nickname would be added.

When in the designated package, calls to FIND-PACKAGE with the LOCAL-NICKNAME
will return the package the designated ACTUAL-PACKAGE instead. This also
affects all implied calls to FIND-PACKAGE, including those performed by the
reader.

When printing a package prefix for a symbol with a package local nickname,
local nickname is used instead of the real name in order to preserve
print-read consistency.

See also: PACKAGE-LOCAL-NICKNAMES, PACKAGE-LOCALLY-NICKNAMED-BY-LIST,
REMOVE-PACKAGE-LOCAL-NICKNAME, and the DEFPACKAGE option :LOCAL-NICKNAMES.

Experimental: interface subject to change."
  (let* ((nick (string local-nickname))
         (actual (find-package-using-package actual-package nil))
         (package (find-undeleted-package-or-lose package-designator))
         (existing (package-%local-nicknames package))
         (cell (assoc nick existing :test #'string=)))
    (unless actual
      (signal-package-error
       package-designator
       "The name ~S does not designate any package."
       actual-package))
    (unless (package-name actual)
      (signal-package-error
       actual
       "Cannot add ~A as local nickname for a deleted package: ~S"
       nick actual))
    (with-single-package-locked-error
        (:package package "adding ~A as a local nickname for ~A"
                  nick actual))
    (when (member nick '("CL" "COMMON-LISP" "KEYWORD") :test #'string=)
      (signal-package-cerror
       actual
       "Continue, use it as local nickname anyways."
       "Attempt to use ~A as a package local nickname (for ~A)."
       nick (package-name actual)))
    (when (string= nick (package-name package))
      (signal-package-cerror
       package
       "Continue, use it as a local nickname anyways."
       "Attempt to use ~A as a package local nickname (for ~A) in ~
        package named globally ~A."
       nick (package-name actual) nick))
    (when (member nick (package-nicknames package) :test #'string=)
      (signal-package-cerror
       package
       "Continue, use it as a local nickname anyways."
       "Attempt to use ~A as a package local nickname (for ~A) in ~
        package nicknamed globally ~A."
       nick (package-name actual) nick))
    (when (and cell (neq actual (cdr cell)))
      (restart-case
          (signal-package-error
           actual
           "~@<Cannot add ~A as local nickname for ~A in ~A: ~
            already nickname for ~A.~:@>"
           nick (package-name actual)
           (package-name package) (package-name (cdr cell)))
        (keep-old ()
          :report (lambda (s)
                    (format s "Keep ~A as local nicname for ~A."
                            nick (package-name (cdr cell)))))
        (change-nick ()
          :report (lambda (s)
                    (format s "Use ~A as local nickname for ~A instead."
                            nick (package-name actual)))
          (let ((old (cdr cell)))
            (with-package-graph ()
              (setf (package-%locally-nicknamed-by old)
                    (delete package (package-%locally-nicknamed-by old)))
              (push package (package-%locally-nicknamed-by actual))
              (setf (cdr cell) actual)))))
      (return-from add-package-local-nickname package))
    (unless cell
      (with-package-graph ()
        (push (cons nick actual) (package-%local-nicknames package))
        (push package (package-%locally-nicknamed-by actual))))
    package))

(defun remove-package-local-nickname (old-nickname
                                      &optional (package-designator (sane-package)))
  #+sb-doc
  "If the designated package had OLD-NICKNAME as a local nickname for
another package, it is removed. Returns true if the nickname existed and was
removed, and NIL otherwise.

See also: ADD-PACKAGE-LOCAL-NICKNAME, PACKAGE-LOCAL-NICKNAMES,
PACKAGE-LOCALLY-NICKNAMED-BY-LIST, and the DEFPACKAGE option :LOCAL-NICKNAMES.

Experimental: interface subject to change."
  (let* ((nick (string old-nickname))
         (package (find-undeleted-package-or-lose package-designator))
         (existing (package-%local-nicknames package))
         (cell (assoc nick existing :test #'string=)))
    (when cell
      (with-single-package-locked-error
          (:package package "removing local nickname ~A for ~A"
                    nick (cdr cell)))
      (with-package-graph ()
        (let ((old (cdr cell)))
          (setf (package-%local-nicknames package) (delete cell existing))
          (setf (package-%locally-nicknamed-by old)
                (delete package (package-%locally-nicknamed-by old)))))
      t)))


(defun find-package (package-designator)
  #+sb-doc
  "If PACKAGE-DESIGNATOR is a package, it is returned. Otherwise PACKAGE-DESIGNATOR
must be a string designator, in which case the package it names is located and returned.

As an SBCL extension, the current package may affect the way a package name is
resolved: if the current package has local nicknames specified, package names
matching those are resolved to the packages associated with them instead.

Example:

  (defpackage :a)
  (defpackage :example (:use :cl) (:local-nicknames (:x :a)))
  (let ((*package* (find-package :example)))
    (find-package :x)) => #<PACKAGE A>

See also: ADD-PACKAGE-LOCAL-NICKNAME, PACKAGE-LOCAL-NICKNAMES,
REMOVE-PACKAGE-LOCAL-NICKNAME, and the DEFPACKAGE option :LOCAL-NICKNAMES."
  (find-package-using-package package-designator
                              (when (boundp '*package*)
                                *package*)))

;;; This is undocumented and unexported for now, but the idea is that by
;;; making this a generic function then packages with custom package classes
;;; could hook into this to provide their own resolution.
(defun find-package-using-package (package-designator base)
  (flet ((find-package-from-string (string)
           (declare (type string string))
           (let* ((nicknames (when base
                               (package-%local-nicknames base)))
                  (nicknamed (when nicknames
                               (cdr (assoc string nicknames :test #'string=))))
                  (packageoid (or nicknamed (gethash string *package-names*))))
             (if (and (null packageoid)
                      ;; FIXME: should never need 'debootstrap' hack
                      (let ((mismatch (mismatch "SB!" string)))
                        (and mismatch (= mismatch 3))))
                 (restart-case
                     (signal 'bootstrap-package-not-found :name string)
                   (debootstrap-package ()
                     (if (string= string "SB!XC")
                         (find-package "COMMON-LISP")
                         (find-package
                          (substitute #\- #\! string :count 1)))))
                 packageoid))))
    (typecase package-designator
      (package package-designator)
      (symbol (find-package-from-string (symbol-name package-designator)))
      (string (find-package-from-string package-designator))
      (character (find-package-from-string (string package-designator)))
      (t (error 'type-error
                :datum package-designator
                :expected-type '(or character package string symbol))))))


(defun make-package (name &key
                          (use '#.*default-package-use-list*)
                          nicknames
                          (internal-symbols 10)
                          (external-symbols 10))
  #+sb-doc
  #.(format nil
     "Make a new package having the specified NAME, NICKNAMES, and USE
list. :INTERNAL-SYMBOLS and :EXTERNAL-SYMBOLS are estimates for the number of
internal and external symbols which will ultimately be present in the package.
The default value of USE is implementation-dependent, and in this
implementation it is ~S." *default-package-use-list*)
  (prog (clobber)
   :restart
     (when (find-package name)
       ;; ANSI specifies that this error is correctable.
       (signal-package-cerror
        name
        "Clobber existing package."
        "A package named ~S already exists" name)
       (setf clobber t))
     (with-package-graph ()
       ;; Check for race, signal the error outside the lock.
       (when (and (not clobber) (find-package name))
         (go :restart))
       (let* ((name (stringify-package-designator name))
              (package
               (%make-package
                name
                (make-package-hashtable internal-symbols)
                (make-package-hashtable external-symbols))))

         ;; Do a USE-PACKAGE for each thing in the USE list so that checking for
         ;; conflicting exports among used packages is done.
         (use-package use package)

         ;; FIXME: ENTER-NEW-NICKNAMES can fail (ERROR) if nicknames are illegal,
         ;; which would leave us with possibly-bad side effects from the earlier
         ;; USE-PACKAGE (e.g. this package on the used-by lists of other packages,
         ;; but not in *PACKAGE-NAMES*, and possibly import side effects too?).
         ;; Perhaps this can be solved by just moving ENTER-NEW-NICKNAMES before
         ;; USE-PACKAGE, but I need to check what kinds of errors can be caused by
         ;; USE-PACKAGE, too.
         (%enter-new-nicknames package nicknames)
         (return (setf (gethash name *package-names*) package))))
     (bug "never")))

;;; Change the name if we can, blast any old nicknames and then
;;; add in any new ones.
;;;
;;; FIXME: ANSI claims that NAME is a package designator (not just a
;;; string designator -- weird). Thus, NAME could
;;; be a package instead of a string. Presumably then we should not change
;;; the package name if NAME is the same package that's referred to by PACKAGE.
;;; If it's a *different* package, we should probably signal an error.
;;; (perhaps (ERROR 'ANSI-WEIRDNESS ..):-)
(defun rename-package (package-designator name &optional (nicknames ()))
  #+sb-doc
  "Changes the name and nicknames for a package."
  (prog () :restart
     (let ((package (find-undeleted-package-or-lose package-designator))
           (name (stringify-package-designator name))
           (found (find-package name))
           (nicks (mapcar #'string nicknames)))
       (unless (or (not found) (eq found package))
         (signal-package-error name
                               "A package named ~S already exists." name))
       (with-single-package-locked-error ()
         (unless (and (string= name (package-name package))
                      (null (set-difference nicks (package-nicknames package)
                                            :test #'string=)))
           (assert-package-unlocked package "rename as ~A~@[ with nickname~P ~
                                             ~{~A~^, ~}~]"
                                    name (length nicks) nicks))
         (with-package-names (names)
           ;; Check for race conditions now that we have the lock.
           (unless (eq package (find-package package-designator))
             (go :restart))
           ;; Do the renaming.
           (remhash (package-%name package) names)
           (dolist (n (package-%nicknames package))
             (remhash n names))
           (setf (package-%name package) name
                 (gethash name names) package
                 (package-%nicknames package) ()))
         (%enter-new-nicknames package nicknames))
       (return package))))

(defun delete-package (package-designator)
  #+sb-doc
  "Delete the package designated by PACKAGE-DESIGNATOR from the package
  system data structures."
  (tagbody :restart
     (let ((package (find-package package-designator)))
       (cond ((not package)
              ;; This continuable error is required by ANSI.
              (signal-package-cerror
               package-designator
               "Ignore."
               "There is no package named ~S." package-designator)
              (return-from delete-package nil))
             ((not (package-name package)) ; already deleted
              (return-from delete-package nil))
             (t
              (with-single-package-locked-error
                  (:package package "deleting package ~A" package)
                (let ((use-list (package-used-by-list package)))
                  (when use-list
                    ;; This continuable error is specified by ANSI.
                    (signal-package-cerror
                     package
                     "Remove dependency in other packages."
                     "~@<Package ~S is used by package~P:~2I~_~S~@:>"
                     (package-name package)
                     (length use-list)
                     (mapcar #'package-name use-list))
                    (dolist (p use-list)
                      (unuse-package package p))))
                #+sb-package-locks
                (dolist (p (package-implements-list package))
                  (remove-implementation-package package p))
                (with-package-graph ()
                  ;; Check for races, restart if necessary.
                  (let ((package2 (find-package package-designator)))
                    (when (or (neq package package2) (package-used-by-list package2))
                      (go :restart)))
                  (dolist (used (package-use-list package))
                    (unuse-package used package))
                  (dolist (namer (package-%locally-nicknamed-by package))
                    (setf (package-%local-nicknames namer)
                          (delete package (package-%local-nicknames namer) :key #'cdr)))
                  (setf (package-%locally-nicknamed-by package) nil)
                  (dolist (cell (package-%local-nicknames package))
                    (let ((actual (cdr cell)))
                      (setf (package-%locally-nicknamed-by actual)
                            (delete package (package-%locally-nicknamed-by actual)))))
                  (setf (package-%local-nicknames package) nil)
                  ;; FIXME: lacking a way to advise UNINTERN that this package
                  ;; is pending deletion, a large package conses successively
                  ;; many smaller tables for no good reason.
                  (do-symbols (sym package)
                    (unintern sym package))
                  (with-package-names (names)
                    (remhash (package-name package) names)
                    (dolist (nick (package-nicknames package))
                      (remhash nick names))
                    (setf (package-%name package) nil
                          ;; Setting PACKAGE-%NAME to NIL is required in order to
                          ;; make PACKAGE-NAME return NIL for a deleted package as
                          ;; ANSI requires. Setting the other slots to NIL
                          ;; and blowing away the PACKAGE-HASHTABLES is just done
                          ;; for tidiness and to help the GC.
                          (package-%nicknames package) nil))
                  (setf (package-%use-list package) nil
                        (package-tables package) #()
                        (package-%shadowing-symbols package) nil
                        (package-internal-symbols package)
                        (make-package-hashtable 0)
                        (package-external-symbols package)
                        (make-package-hashtable 0)))
                (return-from delete-package t)))))))

(defun list-all-packages ()
  #+sb-doc
  "Return a list of all existing packages."
  (let ((res ()))
    (with-package-names (names)
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (pushnew v res :test #'eq))
               names))
    res))

(macrolet ((find/intern (function &rest more-args)
             ;; Both %FIND-SYMBOL and %INTERN require a SIMPLE-STRING,
             ;; but accept a LENGTH. Given a non-simple string,
             ;; we need copy it only if the cumulative displacement
             ;; into the underlying simple-string is nonzero.
             ;; There are two things that can be improved
             ;; about the generated code here:
             ;; 1. if X is known to satisfy STRINGP (generally any rank-1 array),
             ;;    then testing SIMPLE-<base|character>-STRING-P should not
             ;;    re-test the lowtag. This is constrained by the backends,
             ;;    because there are no type vops that assume a known lowtag.
             ;; 2. if X is known to satisfy VECTORP, then
             ;;    (NOT (ARRAY-HEADER-P)) implies SIMPLE-P, but the compiler
             ;;    does not actually know that, and generates a check.
             ;;    This is more of a front-end issue.
             `(multiple-value-bind (name length)
                  (if (simple-string-p name)
                      (values name (length name))
                      (with-array-data ((name name) (start) (end)
                                        :check-fill-pointer t)
                        (if (eql start 0)
                            (values name end)
                            (values (subseq name start end)
                                    (- end start)))))
                (truly-the
                 (values symbol (member :internal :external :inherited nil))
                 (,function name length
                            (find-undeleted-package-or-lose package)
                            ,@more-args)))))

  (defun intern (name &optional (package (sane-package)))
  #+sb-doc
  "Return a symbol in PACKAGE having the specified NAME, creating it
  if necessary."
    (find/intern %intern t))

  (defun find-symbol (name &optional (package (sane-package)))
  #+sb-doc
  "Return the symbol named STRING in PACKAGE. If such a symbol is found
  then the second value is :INTERNAL, :EXTERNAL or :INHERITED to indicate
  how the symbol is accessible. If no symbol is found then both values
  are NIL."
    (find/intern %find-symbol)))


(define-condition name-conflict (reference-condition package-error)
  ((function :initarg :function :reader name-conflict-function-budden)
   (datum :initarg :datum :reader name-conflict-datum-budden)
   (symbols :initarg :symbols :reader name-conflict-symbols-budden))
  (:default-initargs :references (list '(:ansi-cl :section (11 1 1 2 5))))
  (:report
   (lambda (c s)
     (format s "~@<~S ~S causes name-conflicts in ~S between the ~
                following symbols:~2I~@:_~
                ~{~/sb-impl::print-symbol-with-prefix/~^, ~}~:@>"
             (name-conflict-function-budden c)
             (name-conflict-datum-budden c)
             (package-error-package c)
             (name-conflict-symbols-budden c)))))

(defun name-conflict (package function datum &rest symbols)
  (flet ((importp (c)
           (declare (ignore c))
           (eq 'import function))
         (use-or-export-p (c)
           (declare (ignore c))
           (or (eq 'use-package function)
               (eq 'export function)))
         (can-forbid-symbols-manually (c)
           "Разрешаем это только для патченных пакетов, сделанных с помощью defpackage-l2::! А в будущем такого рестарта вообще не должно быть - вместо него конфликт должен разрешаться автоматически"
           (declare (ignore c))
           (and
            (or (eq 'use-package function)
                (eq 'export function)))
           (let ((md (def-merge-packages:get-package-metadata-or-nil package)))
             (and md (def-merge-packages:package-metadata-l2-package-p md))))
         (do-forbid-symbols-manually ()
           (dolist (s (remove-duplicates symbols :test #'string=))
             (format t "~%name-conflict : Запрещаю имя ~S в ~S~%" s package)
             (def-merge-packages::append-package-forbidden-symbol-names
                 package
                 (def-merge-packages::forbid-symbols-simple (list s) package))
             ))
         (old-symbol ()
           (car (remove datum symbols))))
    (let ((pname (package-name package)))
     (cond
      ((can-forbid-symbols-manually nil)
        (do-forbid-symbols-manually))
      (t
       (restart-case
          (error 'name-conflict :package package :symbols symbols
                                :function function :datum datum)
        ;; USE-PACKAGE and EXPORT
        (forbid-it ()
          :report (lambda (s)
                    (ecase function
                      (export
                       (format s "Заменить ~S одноимённым запретным символом в ~A (он затенит ~S)"
                              (old-symbol) pname datum))
                      (use-package
                       (format s "Запретить новые символы в ~A (затенением)"
                                   pname))))
          :test can-forbid-symbols-manually
          (do-forbid-symbols-manually)
          )
        (keep-old ()
          :report (lambda (s)
                    (ecase function
                      (export
                       (format s "Keep ~S accessible in ~A (shadowing ~S)."
                               (old-symbol) pname datum))
                      (use-package
                       (format s "Keep symbols already accessible ~A (shadowing others)."
                               pname))))
          :test use-or-export-p
          (dolist (s (remove-duplicates symbols :test #'string=))
            (shadow (symbol-name s) package)))
        (take-new ()
          :report (lambda (s)
                    (ecase function
                      (export
                       (format s "Make ~S accessible in ~A (uninterning ~S)."
                               datum pname (old-symbol)))
                      (use-package
                       (format s "Make newly exposed symbols accessible in ~A, ~
                                  uninterning old ones."
                               pname))))
          :test use-or-export-p
          (dolist (s symbols)
            (when (eq s (find-symbol (symbol-name s) package))
              (unintern s package))))
        ;; IMPORT
        (shadowing-import-it ()
          :report (lambda (s)
                    (format s "Shadowing-import ~S, uninterning ~S."
                            datum (old-symbol)))
          :test importp
          (shadowing-import datum package))
        (dont-import-it ()
          :report (lambda (s)
                    (format s "Don't import ~S, keeping ~S."
                            datum
                            (car (remove datum symbols))))
          :test importp)
        ;; General case. This is exposed via SB-EXT.
        (resolve-conflict (chosen-symbol)
          :report "Resolve conflict."
          :interactive
          (lambda ()
            (let* ((len (length symbols))
                   (nlen (length (write-to-string len :base 10)))
                   (*print-pretty* t))
              (format *query-io* "~&~@<Select a symbol to be made accessible in ~
                              package ~A:~2I~@:_~{~{~V,' D. ~
                              ~/sb-impl::print-symbol-with-prefix/~}~@:_~}~
                              ~@:>"
                      (package-name package)
                      (loop for s in symbols
                            for i upfrom 1
                            collect (list nlen i s)))
              (loop
                (format *query-io* "~&Enter an integer (between 1 and ~D): " len)
                (finish-output *query-io*)
                (let ((i (parse-integer (read-line *query-io*) :junk-allowed t)))
                  (when (and i (<= 1 i len))
                    (return (list (nth (1- i) symbols))))))))
          (multiple-value-bind (package-symbol status)
              (find-symbol (symbol-name chosen-symbol) package)
            (let* ((accessiblep status)     ; never NIL here
                   (presentp (and accessiblep
                                  (not (eq :inherited status)))))
              (ecase function
                ((unintern)
                 (if presentp
                     (if (eq package-symbol chosen-symbol)
                         (shadow (list package-symbol) package)
                         (shadowing-import (list chosen-symbol) package))
                     (shadowing-import (list chosen-symbol) package)))
                ((use-package export)
                 (if presentp
                     (if (eq package-symbol chosen-symbol)
                         (shadow (list package-symbol) package) ; CLHS 11.1.1.2.5
                         (if (eq (symbol-package package-symbol) package)
                             (unintern package-symbol package) ; CLHS 11.1.1.2.5
                             (shadowing-import (list chosen-symbol) package)))
                     (shadowing-import (list chosen-symbol) package)))
                ((import)
                 (if presentp
                     (if (eq package-symbol chosen-symbol)
                         nil                ; re-importing the same symbol
                         (shadowing-import (list chosen-symbol) package))
                     (shadowing-import (list chosen-symbol) package)))))))))))))

;;; If we are uninterning a shadowing symbol, then a name conflict can
;;; result, otherwise just nuke the symbol.


(defun export (symbols &optional (package (sane-package)))
  #+sb-doc
  "Exports SYMBOLS from PACKAGE, checking that no name conflicts result. Если пакет создан defpkg2, то можем экспортировать только внутренние символы, а не пришедшие через :use"
  (with-package-graph ()
    (let* ((package (find-undeleted-package-or-lose package))
           (symbols (symbol-listify symbols))
           (md (def-merge-packages:get-package-metadata-or-nil package))
           (l2-package-p (and md (def-merge-packages:package-metadata-l2-package-p md)))
           (syms ()))
      ;; Punt any symbols that are already external.
      (dolist (sym symbols)
        (multiple-value-bind (s found)
            (find-external-symbol (symbol-name sym) package)
          (unless (or (and found (eq s sym)) (member sym syms))
            (push sym syms))))
      (with-single-package-locked-error ()
        (when syms
          (assert-package-unlocked package "exporting symbol~P ~{~A~^, ~}"
                                   (length syms) syms))
        (when l2-package-p
          ;; Нехорошо получается, но нам придётся дважды посмотреть, не является ли символ импортированным. В противном случае мы сначала разрешим конфликт, а потом откажем в обновлении пакета и получится, что мы зря разрешали конфликт.
          (let (imports)
            (dolist (sym syms)
              (let ((w (nth-value 1 (find-symbol (symbol-name sym) package))))
                (when (eq w :inherited)
                  (push sym imports))))
            (when imports
              (signal-package-error
               package
               "Пакет ~S создан defpkg2. Нельзя экспортировать inherited (унаследованные) символы ~S" (package-%name package) imports)))
          )

        (when md
          (let (forbiddens)
            (dolist (sym syms)
              (when (def-merge-packages:forbidden-symbol-p sym package)
                (push sym forbiddens)))
            (when forbiddens
              (signal-package-cerror
               package
               "Но можешь попробовать на свой страх и риск"
               "Нельзя экспортировать запретные символы символы ~S из ~S" forbiddens (package-%name package))))
          )

        ;; Find symbols and packages with conflicts.
        (let ((used-by (package-%used-by-list package)))
          (dolist (sym syms)
            (let ((name (symbol-name sym)))
              (dolist (p used-by)
                (multiple-value-bind (s w) (find-symbol name p)
                  (when (and w
                             (not (eq s sym))
                             (not (member s (package-%shadowing-symbols p))))
                    ;; Beware: the name conflict is in package P, not in
                    ;; PACKAGE.
                    (name-conflict p 'export sym sym s)))))))
        ;; Check that all symbols are accessible. If not, ask to import them.
        (let ((missing ())
              (imports ()))
          (dolist (sym syms)
            (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
              (cond ((not (and w (eq s sym)))
                     (push sym missing))
                    ((eq w :inherited)
                     (push sym imports)))))
          (when missing
            (signal-package-cerror
             package
             (format nil "~S these symbols into the ~A package."
                     'import (package-%name package))
             "~@<These symbols are not accessible in the ~A package:~2I~_~S~@:>"
             (package-%name package) missing)
            (import missing package))
          (import imports package))

        ;; And now, three pages later, we export the suckers.
        (let ((internal (package-internal-symbols package))
              (external (package-external-symbols package)))
          (dolist (sym syms)
            (add-symbol external sym)
            (nuke-symbol internal sym))))
      t)))

;;; Check that all symbols are accessible, then move from external to internal.
(defun unexport (symbols &optional (package (sane-package)))
  #+sb-doc
  "Makes SYMBOLS no longer exported from PACKAGE."
  (with-package-graph ()
    (let ((package (find-undeleted-package-or-lose package))
          (symbols (symbol-listify symbols))
          (syms ()))
      (dolist (sym symbols)
        (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
          (cond ((or (not w) (not (eq s sym)))
                 (signal-package-error
                  package
                  "~S is not accessible in the ~A package."
                  sym (package-%name package)))
                ((eq w :external) (pushnew sym syms)))))
      (with-single-package-locked-error ()
        (when syms
          (assert-package-unlocked package "unexporting symbol~P ~{~A~^, ~}"
                                   (length syms) syms))
        (let ((internal (package-internal-symbols package))
              (external (package-external-symbols package)))
          (dolist (sym syms)
            (add-symbol internal sym)
            (nuke-symbol external sym)))
        (dolist (p (package-%used-by-list package))
          (maybe-unforbid-names p))
        )
      t)))

;;; Do stuff to use a package, with all kinds of fun name-conflict checking.
(defun use-package (packages-to-use &optional (package (sane-package)))
  #+sb-doc
  "Add all the PACKAGES-TO-USE to the use list for PACKAGE so that the
external symbols of the used packages are accessible as internal symbols in
PACKAGE."
  (with-package-graph ()
    (let ((packages (package-listify packages-to-use))
          (package (find-undeleted-package-or-lose package)))

      ;; Loop over each package, USE'ing one at a time...
      (with-single-package-locked-error ()
        (dolist (pkg packages)
          (unless (member pkg (package-%use-list package))
            (assert-package-unlocked package "using package~P ~{~A~^, ~}"
                                     (length packages) packages)
            (let ((shadowing-symbols (package-%shadowing-symbols package))
                  (use-list (package-%use-list package)))

              ;; If the number of symbols already accessible is less
              ;; than the number to be inherited then it is faster to
              ;; run the test the other way. This is particularly
              ;; valuable in the case of a new package USEing
              ;; COMMON-LISP.
              (cond
                ((< (+ (package-internal-symbol-count package)
                       (package-external-symbol-count package)
                       (let ((res 0))
                         (dolist (p use-list res)
                           (incf res (package-external-symbol-count p)))))
                    (package-external-symbol-count pkg))
                 (do-symbols (sym package)
                   (multiple-value-bind (s w)
                       (find-external-symbol (symbol-name sym) pkg)
                     (when (and w
                                (not (eq s sym))
                                (not (member sym shadowing-symbols)))
                       (name-conflict package 'use-package pkg sym s))))
                 (dolist (p use-list)
                   (do-external-symbols (sym p)
                     (multiple-value-bind (s w)
                         (find-external-symbol (symbol-name sym) pkg)
                       (when (and w
                                  (not (eq s sym))
                                  (not (member
                                        (find-symbol (symbol-name sym) package)
                                        shadowing-symbols)))
                         (name-conflict package 'use-package pkg sym s))))))
                (t
                 (do-external-symbols (sym pkg)
                   (multiple-value-bind (s w)
                       (find-symbol (symbol-name sym) package)
                     (when (and w
                                (not (eq s sym))
                                (not (member s shadowing-symbols)))
                       (name-conflict package 'use-package pkg sym s)))))))

            (push pkg (package-%use-list package))
            (setf (package-tables package)
                  (let ((tbls (package-tables package)))
                    (replace (make-array (1+ (length tbls))
                              :initial-element (package-external-symbols pkg))
                             tbls)))
            (push package (package-%used-by-list pkg)))))))
  t)

(defun unuse-package (packages-to-unuse &optional (package (sane-package)))
  #+sb-doc
  "Remove PACKAGES-TO-UNUSE from the USE list for PACKAGE."
  (with-package-graph ()
    (let ((package (find-undeleted-package-or-lose package))
          (packages (package-listify packages-to-unuse)))
      (with-single-package-locked-error ()
        (dolist (p packages)
          (when (member p (package-use-list package))
            (assert-package-unlocked package "unusing package~P ~{~A~^, ~}"
                                     (length packages) packages))
          (setf (package-%use-list package)
                (remove p (the list (package-%use-list package))))
          (setf (package-tables package)
                (delete (package-external-symbols p)
                        (package-tables package)))
          (setf (package-%used-by-list p)
                (remove package (the list (package-%used-by-list p)))))
          (maybe-unforbid-names package)
                )
      t)))


;;;; APROPOS and APROPOS-LIST

(defun briefly-describe-symbol (symbol)
  (fresh-line)
  (prin1 symbol)
  (when (boundp symbol)
    (write-string " (bound)"))
  (when (fboundp symbol)
    (write-string " (fbound)")))

;;; support for WITH-PACKAGE-ITERATOR

(defun package-iter-init (access-types pkg-designator-list)
  (declare (type (integer 1 7) access-types)) ; a nonzero bitmask over types
  (values (logior (ash access-types 3) #b11) 0 #()
          (package-listify pkg-designator-list)))

;; The STATE parameter is comprised of 4 packed fields
;;  [0:1] = substate {0=internal,1=external,2=inherited,3=initial}
;;  [2]   = package with inherited symbols has shadowing symbols
;;  [3:5] = enabling bits for {internal,external,inherited}
;;  [6:]  = index into 'package-tables'
;;
(defconstant +package-iter-check-shadows+  #b000100)

(defun package-iter-step (start-state index sym-vec pkglist)
  ;; the defknown isn't enough
  (declare (type fixnum start-state) (type index index)
           (type simple-vector sym-vec) (type list pkglist))
  (declare (optimize speed))
  (labels
      ((advance (state) ; STATE is the one just completed
         (case (logand state #b11)
           ;; Test :INHERITED first because the state repeats for a package
           ;; as many times as there are packages it uses. There are enough
           ;; bits to count up to 2^23 packages if fixnums are 30 bits.
           (2
            (when (desired-state-p 2)
              (let* ((tables (package-tables (this-package)))
                     (next-state (the fixnum (+ state (ash 1 6))))
                     (table-idx (ash next-state -6)))
              (when (< table-idx (length tables))
                (return-from advance ; remain in state 2
                  (start next-state (svref tables table-idx))))))
            (pop pkglist)
            (advance 3)) ; start on next package
           (1 ; finished externals, switch to inherited if desired
            (when (desired-state-p 2)
              (let ((tables (package-tables (this-package))))
                (when (plusp (length tables)) ; inherited symbols
                  (return-from advance ; enter state 2
                    (start (if (package-%shadowing-symbols (this-package))
                               (logior 2 +package-iter-check-shadows+) 2)
                           (svref tables 0))))))
            (advance 2)) ; skip state 2
           (0 ; finished internals, switch to externals if desired
            (if (desired-state-p 1) ; enter state 1
                (start 1 (package-external-symbols (this-package)))
                (advance 1))) ; skip state 1
           (t ; initial state
            (cond ((endp pkglist) ; latch into returning NIL forever more
                   (values 0 0 #() '() nil nil))
                  ((desired-state-p 0) ; enter state 0
                   (start 0 (package-internal-symbols (this-package))))
                  (t (advance 0)))))) ; skip state 0
       (desired-state-p (target-state)
         (logtest start-state (ash 1 (+ target-state 3))))
       (this-package ()
         (truly-the package (car pkglist)))
       (start (next-state new-table)
         (let ((symbols (package-hashtable-cells new-table)))
           (package-iter-step (logior (mask-field (byte 3 3) start-state)
                                      next-state)
                              ;; assert that physical length was nonzero
                              (the index (1- (length symbols)))
                              symbols pkglist))))
    (declare (inline desired-state-p this-package))
    (if (zerop index)
        (advance start-state)
        (macrolet ((scan (&optional (guard t))
                   `(loop
                     (let ((sym (aref sym-vec (decf index))))
                       (when (and (pkg-symbol-valid-p sym) ,guard)
                         (return (values start-state index sym-vec pkglist sym
                                         (aref #(:internal :external :inherited)
                                               (logand start-state 3))))))
                     (when (zerop index)
                       (return (advance start-state))))))
          (declare #-sb-xc-host(optimize (sb-c::insert-array-bounds-checks 0)))
          (if (logtest start-state +package-iter-check-shadows+)
              (let ((shadows (package-%shadowing-symbols (this-package))))
                (scan (not (member sym shadows :test #'string=))))
              (scan))))))

(defun should-name-be-forbidden-in-a-package (package name)
  "budden. Если извне мы получали бы через use несколько разных символов с таким именем, то возвращает истину. Конечно, раз мы можем вызвать эту функцию, у нас сейчас уже есть тень на это имя"
  (let (symbols)
    (dolist (p (package-%use-list package))
      (multiple-value-bind (symbol found)
                           (find-external-symbol name p)
        (when found
          (pushnew symbol symbols :test 'eq))))
    (> (length symbols) 1)))

(defun maybe-unforbid-name (package forbidden-symbol)
  "If there is no more reason to forbid the symbol, unforbid it"
  (unless
      (should-name-be-forbidden-in-a-package package (symbol-name forbidden-symbol))
    (unintern forbidden-symbol package)
    (def-merge-packages:remove-package-forbidden-symbol-name package forbidden-symbol)
    t))

(defun maybe-unforbid-names (package)
  "Looks at all forbidden symbols in package. If there is no reason to forbid them anymore, unforbids them"
  (let ((md (def-merge-packages:get-package-metadata-or-nil package)))
    (unless md
      (return-from maybe-unforbid-names nil))
    (let ((forbidden-symbols
           (def-merge-packages:package-metadata-forbidden-symbol-names md)))
      (dolist (forbidden-symbol forbidden-symbols)
        (maybe-unforbid-name package forbidden-symbol)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (lock-package :sb-impl)
  (lock-package :sb-ext)
  (lock-package :sb-kernel)
  (lock-package :common-lisp)
  (lock-package :sb-int))
