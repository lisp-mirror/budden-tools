;;; Written by Denis Budyak, 2009. This code is in public domain
;; requires iterate-keywords. 

(cl:require :iterate-keywords)

;; ����������� � ��������� �������. ��������, �������� ��� �������: ����������������� 
;; ������ ��������, ������� ������ ������. 
;; �������� ������� ������

;; ��� �������? 
;;
;; ������ ���� �������� �� (������) ������������ ������, � �� ������ �������. �����, ��� ����������������� clauses
;; :use :auto-import-from :auto-import-all-from 
;; ���������� ������� (API ��� ������ �������, � ����� ������������)
;; 


(cl:defpackage :merge-packages-simple
  (:documentation "defpackage-autoimport makes new package. It  
resolves symbol clashes automatically in packages it uses. It selects some non-clashing
set of symbols from interesting packages and import them symbol-by-symbol.

defpackage-autoimport-2 prefers to use packages and shadowing-import clashes.
"   
   )
  (:nicknames :def-merge-packages)
  (:use :cl :org.tfeb.hax.hierarchical-packages)
  (:import-from :iter #:iter #:keywordize)
  (:export 
   #:package-metadata ; structure 
   #:package-metadata-forbidden-symbol-names ; and
   #:package-metadata-custom-reader ; its
   #:package-metadata-custom-token-parsers ;slots 

   #:set-package-lock-portably
   #:*per-package-metadata* ; variable
   #:package-forbidden-symbol-names ; place of package designator
   #:ensure-package-metadata ; makes sure that *per-package-metadata* entry for package exists
   #:keywordize-package-designator
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

(cl-user::portably-without-package-locks
  (when (find-package :merge-packages-simple.forbidden-symbols)
    (delete-package :merge-packages-simple.forbidden-symbols))
  (cl:make-package :merge-packages-simple.forbidden-symbols :use nil)
  )


(defun set-package-lock-portably (package lock)
  "When t, package designator is locked. Designators are compared with string="
  #+lispworks (if lock 
                  (pushnew package hcl:*packages-for-warn-on-redefinition* :test 'string=)
                (setf hcl:*packages-for-warn-on-redefinition* (remove package hcl:*packages-for-warn-on-redefinition* :test 'string=))
                )
  #-lispworks
  (warn "set-package-lock-portably not implemented for your lisp")
  )

(cl-user::set-package-lock-portably :merge-packages-simple.forbidden-symbols t)

(in-package :merge-packages-simple)


(defvar *forbidden-symbols-package* (find-package :merge-packages-simple.forbidden-symbols))

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


(defun extract-several-clauses (rest-arglist clause-name)
  "Is like extract-clauase. Extracts several clauses from &rest list and returns list of them"
  (iter:iter 
    (:for elt in rest-arglist)
    (check-type elt list)
    (if (eq (car elt) clause-name)
        (:collect (cdr elt) :into clauses-extracted)
      (:collect elt :into other-clauses))
    (:finally 
     (return (values clauses-extracted other-clauses)))))
  


(defun force-find-package (package-designator)
  "If package is not found, signal a cerror"
  (iter:iter 
    (:for package = (find-package package-designator))
    (when package (return package))
    (cerror "Retry" "Required non-existent package ~A" package-designator)
    ))

(defun group-similar-items (list &rest key-args &key (key 'identity) test test-not)
  #+russian "�������������� ���������� (� ������ key-args) �������� ������ � ���������"
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


;; FIXME - ��������� �������� ������� ����� use � auto-import-from, ����������� �� ���� � defpackage-autoimport-2. ���� �� �� ���������� � auto-import-from, 
;; �� ����� �� ����� ��������� ������ ����������. ���� ���-�� ������, ������ ���������, ���
(defmacro !3 (name &rest clauses) ; todo: err on non-existing packages
  "See docs for defpackage-autoimport"
  (let (auto-import-from auto-import-dont-warn-clashes 
                         print-defpackage-form
                         auto-import-first-clashing
                         ; non-symbols non-hosted-symbols 
                         auto-import-shadowing
                         auto-reexport-from
                         local-nicknames
                         always
                         (clauses clauses))
    (multiple-value-setq (auto-import-from clauses) (extract-clause clauses :auto-import-from))
    (multiple-value-setq (auto-import-dont-warn-clashes clauses) (extract-clause clauses :auto-import-dont-warn-clashes))
    (multiple-value-setq (auto-import-first-clashing clauses) (extract-clause clauses :auto-import-first-clashing))
    (multiple-value-setq (auto-import-dont-warn-clashes clauses) (extract-clause clauses :auto-import-dont-warn-clashes))
    (multiple-value-setq (print-defpackage-form clauses) (extract-clause clauses :print-defpackage-form))
    (multiple-value-setq (auto-import-shadowing clauses) (extract-clause clauses :auto-import-shadowing))
    (multiple-value-setq (auto-reexport-from clauses) (extract-clause clauses :auto-reexport-from))
    (multiple-value-setq (local-nicknames clauses) (extract-clause clauses :local-nicknames))
    (multiple-value-setq (always clauses) (extract-clause clauses :always))
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
           process-local-nicknames-form
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
                  "merge-packages-simple ~A first of every group of clashing symbols(s): ~S"
                "merge-packages-simple declines to ~A clashing symbol(s): ~S")
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
      (setf process-local-nicknames-form 
            (if local-nicknames
                `(setf (gethash (find-package ,name) *per-package-alias-table*) 
                       ',(process-local-nicknames name local-nicknames :to-alist t))
              `(remhash (find-package ,name) *per-package-alias-table*)))
      (if always 
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (prog1
                 ,package-definition
               ,process-local-nicknames-form))
        `(prog1
             ,package-definition
           (eval-when (:load-toplevel :execute)
             ,process-local-nicknames-form)))
      )))
  
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
\(:print-defpackage-form [t | nil]) - if t, print defpackage form
\(:local-nicknames :nick1 :package1 :nick2 :package2 ...) - Refer to package1 as nick1, package2 as nick2 from package being defined. 
\(:non-symbols . symbol-designator-list) - if the symbols with that name is interned to the package, it is a error. NOT IMPLEMENTED
\(:non-hosted-symbols . symbol-designator-list) - if symbols with that name are interned and have their home package = 
                                                                     (find-package :name), it is a error. NOT IMPLEMENTED
\(:always [t | nil]) - if always, everything is wrapped into (eval-when (:compile-toplevel :load-toplevel :execute))
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
(defpackage-autoimport-2 :p4 (:use :cl) (:local-nicknames :mpar :merge-packages-simple))

(let ((*package* (find-package :p4))) (assert (eq 'defpackage-autoimport (read-from-string "mpar:defpackage-autoimport-2"))))

(assert 
    (nth-value 1 
               (ignore-errors (eval '(defpackage-autoimport-2 :p4 (:use :cl) (:local-nicknames :p :p4))))))



|#

(defun 1-to-list (x) (if (atom x) `(,x) x))

(defstruct package-metadata
;  write-lock ; if t, attempt to create a symbol creates continuable error
;  read-lock ; if t, attempt to read a symbol creates a error
  custom-reader ; custom reader is a function with the same args as read. It is called 
                ; when reader is read in a context of package:: syntax. 
  custom-token-parsers ; Custom token parsers is a list of custom token parsers. 
                       ; Custom token parser is a function designator of 
                       ; (stream potential-symbol-name package) which 
                       ; returns two values. First value is t if token is 
                       ; parsed and nil otherwise. Second value is parsed token itself.
                       ; If custom token parsers are defined, package 
                       ; protection is not accomplished. 
                       ; Stream is at the end of the token at the time of the call.
                       ; Parsers are called from left to right until some parser returns t as its
                       ; secondary value. If no parser returns t, 
  forbidden-symbol-names ; list of names which can't be read as symbols from that package. 
  )

(defvar *per-package-metadata* (make-hash-table :test 'eq)
  "Mapping of keywordized package names to their metadata"
  )

; FIXME - ������ ��� �������� �������� ������ ����� ���������� NIL. 
; ����� ���� ������ ������� ��������
(defun keywordize-package-designator (package-designator)
  (etypecase package-designator
    (keyword package-designator)
    (package (intern (package-name package-designator) :keyword))
    (symbol (keywordize package-designator))
    ))

(defun ensure-package-metadata (package-designator)
  "Gets package metadata. Creates one if there is no metadata"
  (let ((d (keywordize-package-designator package-designator)))
    (or (gethash d *per-package-metadata*)
        (setf (gethash d *per-package-metadata*) (make-package-metadata)))))

(defun package-forbidden-symbol-names (package)
  "Note that symbol forbidding would work well in buddens readtables extensions only"
  (let ((m (ensure-package-metadata package)))
    (package-metadata-forbidden-symbol-names m)))

(defsetf package-forbidden-symbol-names (package) (names)
  "Note that symbol forbidding would work well in buddens readtables extensions only"
  (let ((m (gensym)))
    `(progn (let ((,m (ensure-package-metadata ,package)))
              (setf (package-metadata-forbidden-symbol-names ,m) ,names))
       )))

(defun forbid-symbols-simple (symbols &optional (package *package*))
  "Forbids existing symbol names. Does that by assigning absurd definitions to that symbol which is supposed to be in a package-shadowing-symbols list of the package. 
Also unexports the symbol from the package. This is a simple and not reliable symbol forbidding tool which allows you both to read and redefine the symbol. The 
only good news is that at least you will be unable to call this function as a symbol or variable by omission. But you are free to redefine it. Use this
while buddens readtable extensions are disabled"
  (let ((symbols-to-forbid
         (iter 
           (:with sh = (package-shadowing-symbols package)) 
           (:for sname in (1-to-list symbols))
           (:for s = (find-symbol (string sname) package))
           (assert s () "Unable to forbid-symbols-simple: ~A is not found in ~A" sname package)
           (assert (member s sh) () "Unable to forbid-symbols-simple: ~A is not shadowing in ~A" s package)
           (:collect s)
           )))
    (iter (:for s in symbols-to-forbid)
      (unexport s package)
      (cl-user::portably-without-package-locks
        (eval 
         `(progn
            (defconstant ,s :forbidden-symbol)
            (define-symbol-macro ,s (error "symbol ~S is forbidden in ~S" ,s ,package))
            (defmacro ,s (&rest ignore) (declare (ignore ignore)) (error "symbol ~S is forbidden in ~S" ,(symbol-name s) ,package))))))))
    


(defmacro !4 (name &rest clauses) ; todo: err on non-existing packages
  "See docs for defpackage-autoimport"
  (macrolet ((get-clause (name)
               `(multiple-value-setq (,name clauses) (extract-clause clauses ,(keywordize name))))
             (length-is-1 (name)
               `(when ,name
                  (assert (= 1 (length ,name)) () "~S clause accept exactly one argument" ,(keywordize name))
                  (setf ,name (first ,name)))
               )
             )
    (let (auto-import-from use
                           print-defpackage-form
                         ; non-symbols non-hosted-symbols 
                           local-nicknames
                           always
                           shadowing-import-from-s
                           (clauses clauses))
      
      (get-clause use)
      (get-clause auto-import-from)
      (get-clause print-defpackage-form)
      (get-clause local-nicknames)
      (get-clause always)
      (multiple-value-setq (shadowing-import-from-s clauses) (extract-several-clauses clauses :shadowing-import-from))
;    (multiple-value-setq (non-symbols clauses) (extract-clause clauses :non-symbols))
;    (multiple-value-setq (non-hosted-symbols clauses) (extract-clause clauses :non-hosted-symbols))    
      (length-is-1 print-defpackage-form)
      (length-is-1 always)
      (assert (null (intersection use auto-import-from))
          () ":use and :auto-import-from clauses must be disjoint")
      (let* (; (dest (keywordize name))
             (sources-for-clashes (mapcar #'force-find-package (append auto-import-from use)))
             (sources-for-import (mapcar #'force-find-package auto-import-from))
             all-symbols-for-clashes
             all-symbols-for-import
             duplicates
             package-definition
             forbidden-symbol-names
             generated-import-clauses
             forbid-symbols-forms
             process-local-nicknames-form
             )
        (dolist (p sources-for-clashes)
          (do-external-symbols (s p)
            (pushnew s all-symbols-for-clashes)))
    
        (setf duplicates (collect-duplicates-into-sublists all-symbols-for-clashes :test 'string=))
        ; duplicates is a list of lists of duplicate symbols

        ; remove explicitly shadowing-imported symbols from it
        (let ((all-shadowing-import-names (apply 'append shadowing-import-from-s)))
          (setf duplicates 
                (iter
                  (:for dup in duplicates)
                  (unless (member (car dup) all-shadowing-import-names :test 'string=)
                    (:collect dup)))))

        (when duplicates
          (warn "!4 forbids clashing symbols ~S" duplicates))
        (dolist (p sources-for-import)
          (do-external-symbols (s p)
            (unless (member s duplicates :key 'car :test 'string=)
              (pushnew s all-symbols-for-import))))
        (setf forbidden-symbol-names 
              (iter 
                (:for (dup) in duplicates)
                (:collect (make-symbol (string dup)))))
        (setf generated-import-clauses
              (iter 
                (:for p :in sources-for-import)
                     (:for maybe-import = 
                      (iter
                        (:for s :in-package p :external-only t)
                        (unless (find s duplicates :test 'string= :key 'car)
                          (:collect (make-symbol (string s))))))
                     (when maybe-import
                       (:collect `(:import-from
                                   ,(package-name p)
                                   ,@(sort maybe-import 'string<)) 
                        ))
                     ))
        (setf package-definition 
              `(defpackage ,name
                 ,@(when forbidden-symbol-names 
                     `((:shadow ,@forbidden-symbol-names)))
                 ,@(when use `((:use ,@use)))
                 ,@generated-import-clauses
                 ,@(iter (:for cl in shadowing-import-from-s) (:collect `(:shadowing-import-from ,@cl)))
                 ,@clauses))
        (setf process-local-nicknames-form 
              (if local-nicknames
                  `(setf (gethash (find-package ,name) *per-package-alias-table*) 
                         ',(process-local-nicknames name local-nicknames :to-alist t))
                `(remhash (find-package ,name) *per-package-alias-table*)))
        (setf forbid-symbols-forms
              `((setf (package-forbidden-symbol-names ,name) '(,@forbidden-symbol-names))
                ,@(when forbidden-symbol-names `((forbid-symbols-simple ',forbidden-symbol-names ,name)))
                ))
        (setf package-definition 
              (if always 
                  `(eval-when (:compile-toplevel :load-toplevel :execute)
                     (prog1
                         ,package-definition
                       ,process-local-nicknames-form
                       ,@forbid-symbols-forms))
                `(prog1
                     ,package-definition
                   (eval-when (:load-toplevel :execute)
                     ,process-local-nicknames-form
                     ,@forbid-symbols-forms))))
        (when print-defpackage-form
          (let (*print-length* *print-level*) (print package-definition)))
        package-definition
        ))))


(dspec:define-dspec-alias !4 (name &rest args)
  (setf args args)
  `(defpackage ,name))


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
        (warn "������ ������� ������ ~S �� ~A, �� �� �� ������" str pack)
        (:next-iteration))
      (when (eq status :external)
        (format *error-output* "�������� ����������������� ~S �� ~A" sym pack)
        (unexport sym pack))
      (multiple-value-bind (sym0 status0) (find-symbol str pack)
        (assert (eq sym sym0))
        (ecase status0
          (:inherited (warn "�� ��������� ������� ~S �� ~A, �.�. �� ����������� ����� use-package �� ~A" sym0 pack (symbol-package sym0)))
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


