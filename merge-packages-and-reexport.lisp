;;; Written by Denis Budyak, 2009. This code is in public domain
;; requires iterate-keywords. 

(cl:require :iterate-keywords)

;; разобраться с удалением символа. Например, написать ещё функции: разэкспортировать 
;; символ отовсюду, удалить символ вообще. 
;; Добавить никнейм пакету

(cl:defpackage :merge-packages-and-reexport
  (:documentation "defpackage-autoimport makes new package. It automatically 
resolves symbol clashes in packages it uses. It selects some non-clashing
set of symbols from interesting packages and import them symbol-by-symbol.

defpackage-autoimport-2 prefers to use packages and shadowing-import clashes.
"   
   )
  (:use :cl :org.tfeb.hax.hierarchical-packages)
  (:import-from :iter #:iter #:keywordize)
  (:export 
   #:defpackage-autoimport ; see package docstring. Note this symbol is exported to CL
   #:defpackage-autoimport-2 ; particular case of defpackage-autoimport. Uses all listed packages, shadowing-imports first of clashes. Exported to CL
   #:extract-clause ; extract one clause of def... form (e.g. defpackage) by its head
   ;#:!       ; take several packages and export all what we can without clashes. 
   ;          ; There are two ways to manage clashes: either don't export them
   ;          ; or export leftmost symbol according to package order in ! arglist
   ;#:reexport ; For every symbol in to-package2 which is external in 
   ;           ; package1, export it from to-package2
   #:force-find-package ; force-find-package. If package not found, it is a cerror
   #:group-similar-items ; group-similar-items
                  ; Given a list of items, groups similar (in sence of key-args) items into sublists
   #:collect-duplicates-into-sublists ; '(#\a c #\A #\B #\b) :test 'equalp -> ((#\A #\a) (#\b #\B))
   #:find-symbol-in-packages ; like apropos, but searches only exact symbol name and returns a list
   #:package-doctor ; try to diagnose trash symbols, duplicate symbols, etc
   ;  #:find-symbol-extended ; like find-symbol, but also returns if symbol is (f)bound and home package
   ))

(cl:defpackage :merge-packages-and-reexport.forbidden-symbols
  (:use))

(in-package :merge-packages-and-reexport)


(defvar *forbidden-symbols-package* (find-package :merge-packages-and-reexport.forbidden-symbols))

(defun collect-duplicates-into-sublists (list &rest key-args &key key test test-not)
  "Select duplicates from the list and collect them into sublists. Returns list of such sublists. E.g.,
\'(#\a c #\A #\B #\b) :test 'equalp -> ((#\A #\a) (#\b #\B))
  Non-desctructive on original list"
  (declare (ignore key test test-not))          
  (iter nil
    (:for x in list)
    (:for bucket = (apply #'assoc x buckets key-args))
    (if bucket 
        (push x (cdr bucket))
      (:collect `(,x) :into buckets))
    (:finally
     (return
      (iter 
        (:for bucket in buckets)
        (when (cdr bucket)
          (:collect (nreverse bucket))))))))



(defmacro reexport (from-package &optional (to-package *package*))
  "For every symbol in to-package which is external in from-package, export it
from to-package too"
  (let ((s (gensym)))
  `(do-external-symbols (,s ,from-package)
    (when (eq ,s (find-symbol (string ,s) ,to-package))
      (export ,s ,to-package)))))


#| (defmacro ! (new-package source-packages 
             &key 
             use
             dont-warn-on
             (use-first-duplicate t)
             print
             ) ; todo: err on non-existing packages
  "Merge packages and reexport their exports. Package dest will re-export all non-clashing external symbols 
of source-packages. If use-first-duplicate is t, it also reexport first of any set of clashing symbols, by order 
of source-packages. Warns on clashing symbols unless their keywordized symbol-names are put on dont-warn-on list. Additionally, just uses listed packages, but do not try to avoid clashes. If some symbols from used packages are clashing between themselves, or there are clashes between used-packages and source-packages, execution would fail. :use is intended mostly for using cl package. 
Example: (merge-packages-and-reexport :iterate+cl-utilities (:iterate :cl-utilites)
                                      :use (:cl) 
                                      :dont-warn-on (:for))
If you want to specify some custom exports, it is good idea to make an intermediate package
with merge then use it in your package and then reexport from intermidiate package. E.g., if in 
\(defpackage :p1 (:use :cl) (:export :sym :s1 :nth))
\(defpackage :p2 (:use :cl) (:export :sym :s2 :nth))
you want new package with p1:s1 
\(with-compilation-unit () (merge-packages-and-reexport:! :intermediate (:p1 :p2) :dont-warn-on (:sym))
  (defpackage :p1+p2-with-sym-from-p1 
   (:use :intermediate :cl)
   (:shadowing-import-from :p1 :sym)
   (:export :sym))
  (in-package :p1+p2-with-sym-from-p1)
  (merge-packages-and-reexport::reexport :intermediate))
" 
  (let* ((dest (keywordize new-package))
         (sources (mapcar #'keywordize source-packages))
         (srcs (mapcar 'find-package sources))
         #+(and budden forbidden-symbol-names) (cl-user::*forbidden-symbol-names* nil)
         all-symbols
         duplicates
         reported-duplicates
         symbols-not-to-import
         package-definition
         )
    (dolist (p srcs)
      (do-external-symbols (s p)
        (pushnew s all-symbols)))
    (setf duplicates (collect-duplicates-into-sublists all-symbols :test 'string=))
    (setf reported-duplicates 
          (iter 
            (:for bucket :in duplicates)
            (unless (find (car bucket) dont-warn-on :test 'string=)
              (:collect bucket))))
    (when reported-duplicates
      (warn (if use-first-duplicate 
                "merge-packages-and-reexport interns first of every group of clashing symbols(s): ~S"
              "merge-packages-and-reexport declines to import clashing symbol(s): ~S") 
            reported-duplicates))
    (setf symbols-not-to-import
          (apply 'append 
                 (mapcar 
                  (if use-first-duplicate 'cdr 'identity) 
                  duplicates)))
    (setf package-definition 
          `(defpackage ,dest 
             (:use ,@use)
             ,@(iter 
                 (:for p-name :in sources)
                 (:for p :in srcs)
                 (:for maybe-import = 
                  (iter
                    (:for s :in-package p :external-only t)
                    (unless 
                        (find s symbols-not-to-import)
                      (:collect (make-symbol (string s))))))
                 (when maybe-import
                   (:collect `(:import-from ,p-name
                               ,@(sort maybe-import 'string<)) 
                    :into import-clauses)
                   (:appending maybe-import :into exports))
                 (iter:finally ; now you see iterate-keywords is a must :) 
                  (setf exports
                        (iter 
                          (:for s :in (remove-duplicates exports :test 'string=))
                          (:collect (make-symbol (string s)))))
                  (return-from nil `(,@import-clauses
                                     ,@(when exports `((:export ,@(sort exports 'string<)))
                                         )))))))
    (when print
      (let (*print-length* *print-level*) (print package-definition)))
    package-definition
    )) |#

(defun extract-clause (rest-arglist clause-name)
  "Extract a clause from &rest list. Returns (shared with the rest-arglist) tail of clause extracted and a fresh list of other clauses"
  (iter:iter 
    (:with clause-extracted = nil)
    (:with clause-extracted-p = nil)
    (:for elt in rest-arglist)
    (check-type elt list)
    (if (eq (car elt) clause-name)
        (if clause-extracted-p
            (cerror "Ignore it" "Non-unique clause ~S in ~S" elt rest-arglist)
          (setf clause-extracted-p t
                clause-extracted (cdr elt)))
      (:collect elt :into other-clauses))
    (:finally 
     (return (values clause-extracted other-clauses)))))


(defun force-find-package (package-designator)
  "If package is not found, signal a cerror"
  (iter:iter 
    (:for package = (find-package package-designator))
    (when package (return package))
    (cerror "Retry" "Required non-existent package ~A" package-designator)
    ))

(defun group-similar-items (list &rest key-args &key (key 'identity) test test-not)
  #+russian "Недеструктивно группирует (в смысле key-args) значения списка в подсписки"
  #-russian "Given a list of items, groups similar (in sence of key-args) items into sublists"
  (declare (ignorable key test test-not))
  (iter:iter 
    (:for item in list)
    (:for group = (apply 'assoc (funcall key item) groups key-args))
    (if group 
        (push item (cdr group))
      (:collect `(,item) into groups))
    (:finally (return
               (iter (:for (car . cdr) in groups)
                 (if cdr
                     (:collect `(,car ,.(nreverse cdr)))
                   (:collect `(,car))))))))
#|
(defmacro !2 (name &rest clauses) ; todo: err on non-existing packages
  "!2 is like defpackage. It also allows for three additional clauses 
\(:use-first-from . package-designator-list) 
\(:use-non-clashing-from . package-designator-list)
\(:import-first-from . package-designator-list)
\(:aliases . list-of-package-alias-definitions)
where list-of-package-alias-definition ::= (package-alias package-name) 
\(:dont-warn-on . list of symbols names clashes of which we do not report)

If merge clause is present, all packages designated are used. Any clash between the packages
designated is resolved as follows: if use-first-duplicate is true, first of clashing 
symbols from packages listed is shadowing-imported into new package.
Otherwise, symbol from budden-tools::forbidden-symbols package is shadowing-imported
to package.
"
  (multiple-value-bind (use-first-from clauses) (extract-clause clauses :use-first-from)
    (multiple-value-bind (use-non-clashing-from clauses) (extract-clause clauses :use-non-clashing-from)
      (multiple-value-bind (aliases clauses) (extract-clause clauses :aliases)
        (multiple-value-bind (dont-warn-on clauses) (extract-clause clauses :dont-warn-on)
          (iter 
            (labels (
                     (collect-forbidden-symbol (s) 
                       (:collect s :into intern-to-forbidden-symbols))
                     (collect-shadowing-import-clause (c)
                       (:collect c :into my-clauses))
                     (process-one-of-the-clauses (package-designator-list use-first-duplicate)
                       (unless package-designator-list
                         (return-from process-one-of-the-clauses nil))
                       (let* (; (dest (keywordize name))
                              (sources (mapcar #'keywordize package-designator-list))
                              (srcs (mapcar 'force-find-package sources))
                              all-symbols
                              duplicates
                              reported-duplicates
                              )
                         (dolist (p srcs)
                           (do-external-symbols (s p)
                             (pushnew s all-symbols)))
                         (setf duplicates (collect-duplicates-into-sublists all-symbols :test 'string=))
                         (setf reported-duplicates 
                               (iter 
                                 (:for bucket :in duplicates)
                                 (unless (find (car bucket) dont-warn-on :test 'string=)
                                   (:collect bucket))))
                         (when reported-duplicates
                           (:collect `(warn ,(if use-first-duplicate 
                                                 "merge-packages-and-reexport interns first of every group of clashing symbols(s): ~S"
                                               "merge-packages-and-reexport declines to import clashing symbol(s): ~S") 
                                            ',reported-duplicates) :into warnings))
                         (iter:iter 
                           (:for bucket :in duplicates)
                           (:for s = (car bucket))
                           (:for package-name = (keywordize 
                                                 (package-name 
                                                  (if use-first-duplicate
                                                      (symbol-package s) 
                                                    #+forbidden-symbols (package-name *forbidden-symbols-package*)))
                                                 ))
                           (when package-name
                             (:for symbol-designator = (make-symbol (symbol-name s)))
                             #+forbidden-symbols (unless use-first-duplicate (collect-forbidden-symbol symbol-designator))
                             (collect-shadowing-import-clause `(:shadowing-import-from ,package-name ,symbol-designator)))
                           (:collect `(:use ,@sources) :into my-clauses)
                           )))
;              (process-one-of-the-clauses use-first-from t)
;              (process-one-of-the-clauses use-non-clashing-from nil)
                     (return ; this terminates outer iterate. 
                             `(progn
                                ,@(when intern-to-forbidden-symbols 
                                    `((eval-when (:compile-toplevel :load-toplevel :execute)
                                        (dolist (elt ',intern-to-forbidden-symbols) 
                                          (intern (symbol-name elt)
                                                  *forbidden-symbols-package*)))))
                  ; (defpackage ,name ,@clauses)
                                (defpackage ,name ,@my-clauses ,@clauses)
                                ,@warnings
                                (eval-when (:compile-toplevel :load-toplevel :execute)
                                  (setf (gethash (find-package ,name) 
                                                 *per-package-alias-table*)
                                        ',(iter:iter (:for (alias name) :in aliases)
                                            (unless name
                                              (error "No package name given for alias ~A in ~S clause" alias `(:aliases ,@aliases)))
                                            (:collect (cons alias name)))))
                                (find-package ,name)
                                )))))))))
  ) |#

(defun process-local-nicknames (new-package-name list &key to-alist) 
  (do ((a (pop list) (pop list)) 
       (b (pop list) (pop list))
       res) 
      (nil) 
    (check-type a (or symbol string))
    (check-type b (or symbol string))
    (assert (not (string= b new-package-name)))
    (push (if to-alist (cons a b) (list a b)) res)
    (when (null list) (return-from process-local-nicknames (reverse res)))
    )
  )



(defmacro !3 (name &rest clauses) ; todo: err on non-existing packages
  "See docs for defpackage-autoimport"
  (let (auto-import-from auto-import-dont-warn-clashes 
                         print-defpackage-form
                         auto-import-first-clashing
                         ; non-symbols non-hosted-symbols 
                         auto-import-shadowing
                         auto-reexport-from
                         local-nicknames
                         (clauses clauses))
    (multiple-value-setq (auto-import-from clauses) (extract-clause clauses :auto-import-from))
    (multiple-value-setq (auto-import-dont-warn-clashes clauses) (extract-clause clauses :auto-import-dont-warn-clashes))
    (multiple-value-setq (auto-import-first-clashing clauses) (extract-clause clauses :auto-import-first-clashing))
    (multiple-value-setq (auto-import-dont-warn-clashes clauses) (extract-clause clauses :auto-import-dont-warn-clashes))
    (multiple-value-setq (print-defpackage-form clauses) (extract-clause clauses :print-defpackage-form))
    (multiple-value-setq (auto-import-shadowing clauses) (extract-clause clauses :auto-import-shadowing))
    (multiple-value-setq (auto-reexport-from clauses) (extract-clause clauses :auto-reexport-from))
    (multiple-value-setq (local-nicknames clauses) (extract-clause clauses :local-nicknames))
    (assert (subsetp auto-reexport-from auto-import-from :test 'string-equal)
        () "In an defpackage-autoimport, auto-reexport-from should be a subset of auto-import-from")
  
;    (multiple-value-setq (non-symbols clauses) (extract-clause clauses :non-symbols))
;    (multiple-value-setq (non-hosted-symbols clauses) (extract-clause clauses :non-hosted-symbols))    
    (setf auto-import-first-clashing (first auto-import-first-clashing)
          print-defpackage-form (first print-defpackage-form))
    (let* (; (dest (keywordize name))
           (sources (mapcar #'keywordize auto-import-from))
           (srcs (mapcar 'force-find-package sources))
           all-symbols
           duplicates
           reported-duplicates
           package-definition
           symbols-not-to-import
           )
      (dolist (p srcs)
        (do-external-symbols (s p)
          (pushnew s all-symbols)))
      (setf duplicates (collect-duplicates-into-sublists all-symbols :test 'string=))
      (setf reported-duplicates 
            (iter 
              (:for bucket :in duplicates)
              (unless (find (car bucket) auto-import-dont-warn-clashes :test 'string=)
                (:collect bucket))))
      (when reported-duplicates
        (warn (if auto-import-first-clashing 
                  "merge-packages-and-reexport ~A first of every group of clashing symbols(s): ~S"
                "merge-packages-and-reexport declines to ~A clashing symbol(s): ~S")
              (if auto-import-shadowing "shadowing-import" "import")
              reported-duplicates))
      (setf symbols-not-to-import
            (apply 'append 
                   (mapcar 
                    (if auto-import-first-clashing 'cdr 'identity) 
                    duplicates)))
      (setf package-definition 
            `(defpackage ,name
               ,@(iter 
                   (:for p-name :in sources)
                   (:for p :in srcs)
                   (:for maybe-import = 
                    (iter
                      (:for s :in-package p :external-only t)
                      (unless 
                          (find s symbols-not-to-import)
                        (:collect (make-symbol (string s))))))
                   (when maybe-import
                     (:collect `(,(if auto-import-shadowing :shadowing-import-from :import-from)
                                 ,p-name
                                 ,@(sort maybe-import 'string<)) 
                      :into import-clauses)
                     (when (member p-name auto-reexport-from :test 'string-equal)
                       (:appending maybe-import :into exports)
                       ))
                   (:finally 
                    (setf exports
                          (iter 
                            (:for s :in (remove-duplicates exports :test 'string-equal))
                            (:collect (make-symbol (string s)))))
                    (return-from nil `(,@import-clauses
                                       ,@(when exports `((:export ,@(sort exports 'string<))))
                                       ))))
               ,@clauses))
      (when print-defpackage-form
        (let (*print-length* *print-level*) (print package-definition)))
      `(prog1
         ,package-definition
         (eval-when (:load-toplevel :execute)
           ,(if local-nicknames
                `(setf (gethash (find-package ,name) *per-package-alias-table*) 
                       ',(process-local-nicknames name local-nicknames :to-alist t))
              `(remhash (find-package ,name) *per-package-alias-table*))
           )
         ))
    ))
  
(cl-user::portably-without-package-locks
; non-toplevel
(defmacro defpackage-autoimport (&rest body)
    "It is like defpackage. It also allows for additional clauses. Currently every additional clause can only occur once. 
\(:auto-import-from . package-designator-list) - import all non-clashing external symbols + first of every set of clashing symbols. 
\(:auto-import-dont-warn-clashes . symbol-designator-list) - inhibit warning on auto-import clashes. 
\(:auto-import-first-clashing [t | nil]) - if t (the default), then, in auto-import-from, take leftmost of any set of the 
   clashing symbols and import it too. Only one such clause is allowed.
\(:auto-reexport-from . package-designator-list) - reexport all symbols which were imported from package listed in auto-import-from clause. 
   Packages listed must be a subset of auto-import packages. Only one such clause is allowed.
\(:auto-import-shadowing [t | nil]) - if t, use shadowing-import instead of import for clash resolution
\(:print-defpackage-form [t | nil) - if t, print defpackage form
\(:local-nicknames :nick1 :package1 :nick2 :package2 ...) - Refer to package1 as nick1, package2 as nick2 from package being defined. 
\(:non-symbols . symbol-designator-list) - if the symbols with that name is interned to the package, it is a error. NOT IMPLEMENTED
\(:non-hosted-symbols . symbol-designator-list) - if symbols with that name are interned and have their home package = 
                                                                     (find-package :name), it is a error. NOT IMPLEMENTED
"
    `(!3 ,@body))
  #+lispworks 
; non-toplevel
(dspec:define-dspec-alias defpackage-autoimport (name &rest args)
  (setf args args)
  `(defpackage ,name))
; non-toplevel
(import '(defpackage-autoimport defpackage-autoimport-2) :cl)
; non-toplevel
(export '(defpackage-autoimport defpackage-autoimport-2) :cl)

; non-toplevel
(defmacro defpackage-autoimport-2 (name &rest clauses) 
  "particular case of defpackage-autoimport. Uses all listed packages, shadowing-imports first of clashes. Exported to CL"
  (let (use auto-import-shadowing auto-import-first-clashing auto-import-from (clauses clauses))
    (multiple-value-setq (use clauses) (extract-clause clauses :use))
    (multiple-value-setq (auto-import-shadowing clauses) (extract-clause clauses :auto-import-shadowing))
    (assert (null auto-import-shadowing) () 
      ":auto-import-shadowing is always t at defpackage-autoimport-2, you can't pass it")
    (multiple-value-setq (auto-import-first-clashing clauses) (extract-clause clauses :auto-import-first-clashing))
    (assert (null auto-import-first-clashing) () 
      ":auto-import-first-clashing is always t at defpackage-autoimport-2, you can't pass it")
    (multiple-value-setq (auto-import-from clauses) (extract-clause clauses :auto-import-from))
    (assert (null auto-import-from) () 
      ":auto-import-shadowing is assigned from :use at defpackage-autoimport-2, you can't pass it")
    `(defpackage-autoimport ,name
                            (:use ,@use)
                            (:auto-import-from ,@use)
                            (:auto-import-shadowing t) 
                            (:auto-import-first-clashing t)
                            ,@clauses)))

; non-toplevel
#+lispworks 
(dspec:define-dspec-alias defpackage-autoimport-2 (name &rest args)
  (setf args args)
  `(defpackage ,name))
)

#|
tests:
(defpackage-autoimport-2 :p4 (:use :cl) (:local-nicknames :mpar :merge-packages-and-reexport))

(let ((*package* (find-package :p4))) (assert (eq 'defpackage-autoimport (read-from-string "mpar:defpackage-autoimport-2"))))

(assert 
    (nth-value 1 
               (ignore-errors (eval '(defpackage-autoimport-2 :p4 (:use :cl) (:local-nicknames :p :p4))))))



|#



(defun delete-symbols-from-package (pack &rest symbols)
  "Each element of symbols may be a string-designator, or a list. 
If it is a list, first element should be a string-designator. 
Try to delete symbol designated with each string-designator from pack 
and explain if we can't"
  (iter
    (:for sname-or-list in symbols)
    (:for sname = (if (consp sname-or-list) 
                      (car sname-or-list)
                    sname-or-list))
    (:for str = (string sname))
    (multiple-value-bind (sym status) (find-symbol str pack)
      (unless sym
        (warn "Хотели удалить символ ~S из ~A, но он не найден" str pack)
        (:next-iteration))
      (when (eq status :external)
        (format *error-output* "Пытаемся разэкспортировать ~S из ~A" sym pack)
        (unexport sym pack))
      (multiple-value-bind (sym0 status0) (find-symbol str pack)
        (assert (eq sym sym0))
        (ecase status0
          (:inherited (warn "Не получится удалить ~S из ~A, т.к. он унаследован через use-package от ~A" sym0 pack (symbol-package sym0)))
          (:internal
           (unintern sym0 pack)
           (assert (not (find-symbol str pack))))
          )))))


(defun find-symbol-extended (string-designator package &key include-symbol-name) 
  "If include-symbol is nil, then include symbol-name instead of symbol itself"
  (let ((sname (string string-designator)))
    (multiple-value-bind (sym status) (find-symbol sname package)
      (when sym
        `(,@(if include-symbol-name `(,sname) `(,sym))
          ,(keywordize (package-name package))
          ,status
          ,(unless (eq (symbol-package sym) (find-package package)) 
             (keywordize (package-name (symbol-package sym))))
          ,@(when (boundp sym) '(:boundp))
          ,@(when (fboundp sym) '(:fboundp))
          )))))
          

(defun compare-find-symbol-extended-result (x y)
  (flet ((ordering-number (a)
           (+ (* 10 (if (fourth a) 1 0))
              (position (third a) '(:internal :external :inherited)))))
    (< (ordering-number x) (ordering-number y))))

(defun find-symbol-in-packages (string-designator &key (packages (list-all-packages)) excluding-package (show-symbols t)) 
  "Excluding-package allows to exclude one package"
  (setf packages (sort packages #'string< :key #'package-name))
  (when excluding-package (setf packages (remove excluding-package packages :test 'string= :key 'package-name)))
  (let ((result 
         (sort 
          (iter
            (:for sname = (string string-designator))
            (:for p in packages)
            (multiple-value-bind (sym status)
                (find-symbol sname p)
              (declare (ignorable status))
              (when sym (:collect (find-symbol-extended sym p)))))
          #'compare-find-symbol-extended-result
          )))
    (if show-symbols result
      (mapcar 'cdr result))))

(defun all-chars-in-same-case-p (s)
  (let ((all-downs t)
        (all-ups t)
        (up #\a)
        (down #\a))
    (declare (symbol all-ups all-downs))
    (declare (character up down))
    (iter 
      (:while (or all-ups all-downs))
      (:for c :in-string s)
      (declare (character c))
      (when all-ups
        (setf up (char-upcase c))
        (unless (char= up c)
          (setf all-ups nil)))
      (when all-downs 
        (setf down (char-downcase c))
        (unless (char= down c)
          (setf all-downs nil)))
      )
    (cond
     ((and all-ups all-downs) :ignore-case)
     (all-ups :uppercase)
     (all-downs :lowercase)
     (t nil))))


(defmacro package-doctor (package-with-trash &key packages)
  "Returns a form which would likely clear trash symbols"
  (setf packages (or packages (list-all-packages)))
  (iter (:for pckg in (list package-with-trash :keyword))
    (setf packages (remove pckg packages
                                  :test 'string= :key 'package-name)))
  `'(progn
      "Trying to delete symbols which are (f)unbound and have the same name as (f)bound symbols from other packages"
      (delete-symbols-from-package 
       ',package-with-trash
       ,@(sort 
          (iter 
            (:for sym :in-package package-with-trash)
            (:for sname = (symbol-name sym))
            (when (or (boundp sym) (fboundp sym))
              (:next-iteration))
            (flet ((outer-collect (x) (:collect x)))
              (iter 
                (:for pck in packages)
                (multiple-value-bind (sym1 status1)
                    (find-symbol sname pck)
                  (when 
                      (and 
                       sym1
                       (not (eq sym1 sym))
                       (or (boundp sym1) (fboundp sym1))
                       (not (eq status1 :inherited)))
                    (:collect `(,(package-name pck) ,status1) :into explanation)
                    )
                  (:finally 
                   (when explanation
                     (outer-collect `(,sname ,@explanation)))))
                )))
          'string<
          :key 'car))
      "Symbols which may be duplicated definitions. I just note them for you"
      '(,@(sort 
           (iter 
             (:for sym :in-package package-with-trash)
             (:for sname = (symbol-name sym))
             (:for boundp = (boundp sym))
             (:for fboundp = (fboundp sym))
             (when (or boundp fboundp)
               (flet ((outer-collect (x) (:collect x)))
                 (iter 
                   (:for pck in packages)
                   (multiple-value-bind (sym1 status1)
                       (find-symbol sname pck)
                     (when 
                         (and 
                          sym1
                          (not (eq sym1 sym))
                          (or (boundp sym1) (fboundp sym1))
                          (not (eq status1 :inherited)))
                       (:collect (find-symbol-extended sym1 pck) :into explanation)
                       )
                     (:finally 
                      (when explanation
                        (outer-collect `(,sname ,@(mapcar 'cdr explanation)))))
                     )))))
           'string<
           :key 'car))
      "Identical UPPERCASE and lowercase names. Suggest to delete either unbound of them, just list the rest"
      ,@(sort 
         (iter 
           (:for sym :in-package package-with-trash)
           (:for sname = (symbol-name sym))
           (:for case = (all-chars-in-same-case-p sname))
           (:for other-sym = 
            (case case
              (:uppercase (find-symbol (string-downcase sname) package-with-trash))
              (:lowercase (find-symbol (string-upcase sname) package-with-trash))
              (t nil)))
           (:for other-sname = (symbol-name other-sym))
           (when other-sym
             (:for boundp = (boundp sym))
             (:for fboundp = (fboundp sym))
             (:for boundpo = (boundp other-sym))
             (:for fboundpo = (fboundp other-sym))
             (cond ((and (or boundp fboundp)
                         (not (or boundpo fboundpo)))
                    (:collect `(delete-symbols-from-package ,package-with-trash ,other-sname)))
                   ((and (eq (find-package package-with-trash) :keyword)
                         (eq case :uppercase)) ; remove lowercase keyword if there is a conflict
                    (:collect `(delete-symbols-from-package ,package-with-trash ,other-sname)))
                   (t (:collect `(,sname ,other-sname :both-are-bound))))
             ))
         'string<
         :key 'car))
  )


