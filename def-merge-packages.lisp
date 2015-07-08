;;; -*- Encoding: utf-8; -*-
;;; Written by Denis Budyak, 2009. This code is in public domain
;; requires iterate-keywords. 

;; (cl:require :iterate-keywords)

;; разобраться с удалением символа. Например, написать ещё функции: разэкспортировать 
;; символ отовсюду, удалить символ вообще. 
;; Добавить никнейм пакету


(eval-when (:execute)
  (error "Do not execute the file, use compile/load sequence"))

(eval-when (:load-toplevel :compile-toplevel)
    (setf *readtable* (copy-readtable nil))
    nil)

(cl:defpackage :defpackage-budden
  (:documentation "
See ! docstring for docs. ! is unexported to avoid any symbol clashes, but this is the
function you most likely want to use."   
   )
  (:nicknames :def-merge-packages)
  (:use :cl ;:org.tfeb.hax.hierarchical-packages
   )
  (:import-from :iterate-keywords #:iter #:keywordize)
  (:export 
   #:def-merge-packages ; exported name for !. ! itself is unexported
   ; #:export2 ; smart export clause
   #:package-metadata ; structure 
   #:package-metadata-forbidden-symbol-names ; and
   #:package-metadata-custom-reader ; its
   #:package-metadata-custom-token-parsers ;slots 
   #:package-metadata-allow-qualified-intern
   #:package-metadata-interning-is-forbidden

   #:set-package-lock-portably
   #:*per-package-metadata* ; variable
   #:*per-package-alias-table* ; variable, stolen from hierarchical-packages
   #:package-forbidden-symbol-names ; place of package designator
   #:ensure-package-metadata ; makes sure that *per-package-metadata* entry for package exists
   #:keywordize-package-designator
   ; #:defpackage-autoimport ; УСТАРЕЛ, не пользоваться. See package docstring. Note this symbol is exported to CL
   ; #:defpackage-autoimport-2 ; ПОД ВОПРОСОМ.particular case of defpackage-autoimport. Uses all listed packages, shadowing-imports first of clashes. Exported to CL
   ; пользоваться !4 .
   #:extract-clause ; extract one clause of def... form (e.g. defpackage) by its head
   ;#:reexport ; For every symbol in to-package2 which is external in 
   ;           ; package1, export it from to-package2
   #:reexport-clause-for-package ; for every expternal symbol in package, write an export clause
   #:force-find-package ; force-find-package. If package not found, it is a cerror
   #:group-similar-items ; group-similar-items
                  ; Given a list of items, groups similar (in sence of key-args) items into sublists
   #:collect-duplicates-into-sublists ; '(#\a c #\A #\B #\b) :test 'equalp -> ((#\A #\a) (#\b #\B))
   #:find-symbol-in-packages ; like apropos, but searches only exact symbol name and returns a list
   #:search-and-replace-seq
   #:package-doctor ; try to diagnose trash symbols, duplicate symbols, etc
   ;  #:find-symbol-extended ; like find-symbol, but also returns if symbol is (f)bound and home package
   #:get-custom-reader-for-package
   #:get-custom-token-parsers-for-package

   #:char-upcase-ascii
   #:char-downcase-ascii
   #:string-upcase-ascii
   #:string-downcase-ascii

   #:all-ascii-chars-in-same-case-p

   #:unintern-all-internal-symbols-of-package
   ))

(in-package :defpackage-budden)

(defparameter *ascii-letters* 
  (let ((lowercase-ascii-letters
      "abcdefghijklmnopqrstuvwxyz"))
    (append
     (concatenate
      'list
      lowercase-ascii-letters
      (string-upcase lowercase-ascii-letters)
      ))))

(let* ((numchars (/ (length *ascii-letters*) 2))
       (up (make-hash-table :test #'eql))
       (down (make-hash-table :test #'eql)))
  (check-type numchars integer)
  (iter 
    (:for i from 0 to (- numchars 1))
    (:for j from numchars to (- (* 2 numchars) 1))
    (:for lochar = (elt *ascii-letters* i))
    (:for hichar = (elt *ascii-letters* j))
    (setf (gethash lochar up) hichar)
    (setf (gethash hichar down) lochar))

; non-toplevel
(defun char-upcase-ascii (char) 
  (gethash char up char))

; non-toplevel
(defun char-downcase-ascii (char)
  (gethash char down char))

; non-toplevel
(defun char-equal-ascii (c1 c2) 
  (char-equal (char-upcase-ascii c1)
              (char-upcase-ascii c2)))

; non-toplevel
(defun string-upcase-ascii (s)
    (map 'string 'char-upcase-ascii s))

(defun string-downcase-ascii (s)
  (map 'string 'char-downcase-ascii s))

);let*

(defun all-ascii-chars-in-same-case-p (s)
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
        (setf up 
              (char-upcase-ascii c))
        (unless (char= up c)
          (setf all-ups nil)))
      (when all-downs 
        (setf down 
              (char-downcase-ascii c))
        (unless (char= down c)
          (setf all-downs nil)))
      )
    (cond
     ((and all-ups all-downs) :ignore-case)
     (all-ups :uppercase)
     (all-downs :lowercase)
     (t nil))))

(defun search-and-replace-seq (type seq subseq newseq &key all (test #'equalp))
  (let ((num-matches 0)
        (start2 0))
    (loop 
     (let ((found (search subseq seq :test test :start2 start2)))  
       (when found 
         (setf seq (concatenate
                    type 
                    (subseq seq 0 found)
                    newseq
                    (subseq seq (+ found (length subseq)) (length seq))))
         (setf start2 (+ found (length newseq)))
         (incf num-matches))
       (when (or (not found) (not all))
         (return))))
    (values seq num-matches)))


(defun export2-reader (stream symbol)
  "(:export2 package:name ;comment
    package:camelName ; comment
    )" 
  (declare (ignore symbol))
  (let (#+nil (*readtable* (or (EDITOR-HINTS.NAMED-READTABLES:FIND-READTABLE :buddens-readtable-a)
                        *readtable*)))
    (read stream)
    ))


(defun export-clause (nickname string)
  "DEPRECATED. Generates :export clause from string containing qualified symbol names and comments. 
   nickname: is replaced with #:. So, you can safely navigate to it via 
   your 'find-definition' command. E.g.
  (defpackage pack
   #.(export-clause \"
      pack:sym1 ; this is the first symbol of package
      pack:sym2 ; this is the second one
      other-pack:reexported ; this won't be replaced
      \"))
  " 
  (let* ((nickname (string nickname))
         (expr1 (concatenate 'string nickname "::")) ; package::
         (expr2 (concatenate 'string nickname ":")) ; package:
         (processed1 (search-and-replace-seq 'string string expr1 "#:" :all t :test 'equalp))
         (processed2 (search-and-replace-seq 'string processed1 expr2 "#:" :all t :test 'equalp))
         (clause-string (concatenate 'string "(" processed2 ")")))
    `(:export
      ,@(with-input-from-string (s clause-string)
          (export2-reader s nil)))))

(defun set-package-lock-portably (package lock)
  "When t, package designator is locked. Designators are compared with string="
  #+lispworks (if lock 
                  (pushnew package hcl:*packages-for-warn-on-redefinition* :test 'string=)
                (setf hcl:*packages-for-warn-on-redefinition* (remove package hcl:*packages-for-warn-on-redefinition* :test 'string=))
                )
  #-lispworks
  (warn "set-package-lock-portably not implemented for your lisp")
  )

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

(defun reexport-clause-for-package (package)
  (let* 
      ((p (find-package package))
       (export-string 
        (let* ((*package* (find-package :keyword))
               (indent-string "    "))
          (with-output-to-string (str)
            (iter (:for sym :in-package p :external-only t)
              (format str "~%~A~S" indent-string sym))
            (format str "~%~A" indent-string)))))
    `(:export ,export-string)))

(defmacro reexport (from-package &optional (to-package *package*))
  "DEPRECATED. For every symbol in to-package which is external in from-package, export it
from to-package too"
  (let ((s (gensym)))
  `(do-external-symbols (,s ,from-package)
    (when (eq ,s (find-symbol (string ,s) ,to-package))
      (export ,s ,to-package)))))


(defun extract-clause (rest-arglist clause-name)
  "Extract a clause from &rest list. Returns (shared with the rest-arglist) tail of clause extracted and a fresh list of other clauses"
  (iter 
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
  (iter 
    (:for elt in rest-arglist)
    (check-type elt list)
    (if (eq (car elt) clause-name)
        (:collect (cdr elt) :into clauses-extracted)
      (:collect elt :into other-clauses))
    (:finally 
     (return (values clauses-extracted other-clauses)))))
  


(defun force-find-package (package-designator)
  "If package is not found, signal a cerror"
  (iter 
    (:for package = (find-package package-designator))
    (when package (return package))
    (cerror "Retry" "Required non-existent package ~A" package-designator)
    ))

(defun group-similar-items (list &rest key-args &key (key 'identity) test test-not)
  #+russian "Недеструктивно группирует (в смысле key-args) значения списка в подсписки"
  #-russian "Given a list of items, groups similar (in sence of key-args) items into sublists"
  (declare (ignorable key test test-not))
  (iter 
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
  (assert (evenp (length list)) () "def-merge-packages::! : local package nicknames list must be of even length")
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
  forbidden-symbol-names ; FIXME rename to forbidden-symbols. This is a list of forbidden symbols. Forbidden symbols are internal (and in shadowing-import list) in the package 
                       ; and, if buddens readtable extensions are on,  you can't read them with reader 
  allow-qualified-intern ; with buddens readtable extensions, by default, if package::symbol is being read for non-existent symbol, this is cerror. To return to default cl behaviour, set 
                         ; this variable to t. E.g. (setf (budden-tools::package-metadata-allow-qualified-intern (budden-tools::ensure-package-metadata :my-package)) t)
  interning-is-forbidden ; when this is true, interning via reading is prohibited for the package (in our readtable)
  )

(defvar *per-package-metadata* (make-hash-table :test 'eq)
  "Mapping of keywordized package names to their metadata"
  )


(defvar *per-package-alias-table*
  ;; maps from package -> alist of alias -> real names.
  ;; Lookups are nopt recursive in this list.
  ;; (could this be a constant?)
  (make-hash-table))

#+LispWorks
;;; Try and let entries go away when packages go away
(hcl:set-hash-table-weak *per-package-alias-table* :key) 

(defun get-custom-reader-for-package (package-designator)
  "custom-reader, если он назначен (с помощью setf), имеет те же параметры, что и read. Вызывается для чтения во временном контексте пакета, т.е., после custom-reader-for-package должен учитывать, что его могут вызвать изнутри read, поэтому просто вызов read скорее всего, вызовет безконечную рекурсию"
  (let ((pm (gethash (keywordize-package-designator package-designator) 
                    *per-package-metadata*)))
    (and pm (package-metadata-custom-reader pm))))

(defun get-custom-token-parsers-for-package (package-designator)
  "custom-token-parsers, если назначены (с помощью setf) - это список function designators (для funcall), которые вызываются слева направо над каждым токеном. Они получают на вход: поток, строку и пакет. Возвращают два значения. Первое значение - считанный объект. Второе - t, если объект считан, иначе - nil"
  (let ((pm (gethash (keywordize-package-designator package-designator) 
                     *per-package-metadata*)))
    (and pm (package-metadata-custom-token-parsers pm))))
  

(defsetf get-custom-reader-for-package (package-designator) (new-value)
  (let ((md (gensym)))
    `(progn
       (check-type ,new-value (or null symbol function))
       (let ((,md (ensure-package-metadata ,package-designator)))
         (setf (package-metadata-custom-reader ,md) ,new-value)))))

(defsetf get-custom-token-parsers-for-package (package-designator) (new-value)
  (let ((md (make-symbol "MD"))
        (new-value-v (make-symbol "NEW-VALUE-V"))
        (x (make-symbol "X")))
    `(let ((,new-value-v ,new-value))
       (check-type ,new-value-v (or null cons))
       (dolist (,x ,new-value) (check-type ,x (or symbol function)))
       (let ((,md (ensure-package-metadata ,package-designator)))
         (setf (package-metadata-custom-token-parsers ,md) ,new-value-v)))))

; FIXME - иногда при удалении текущего пакета пакет становится NIL. 
; тогда наша хитрая читалка ломается
(defun keywordize-package-designator (package-designator)
  (etypecase package-designator
    (keyword package-designator)
    (package (intern (package-name package-designator) :keyword))
    (symbol (keywordize package-designator))
    (string (keywordize-package-designator (find-package package-designator)))
    ))

(defun ensure-package-metadata (package-designator)
  "Gets package metadata. Creates one if there is no metadata"
  (let ((d (keywordize-package-designator package-designator)))
    (or (gethash d *per-package-metadata*)
        (setf (gethash d *per-package-metadata*) (make-package-metadata)))))

(defun package-forbidden-symbol-names (package)
  "Note that symbol forbidding would work only when buddens readtables extensions are enabled"
  (let ((m (ensure-package-metadata package)))
    (package-metadata-forbidden-symbol-names m)))

(defsetf package-forbidden-symbol-names (package) (names)
  "Note that symbol forbidding would work well in buddens readtables extensions only"
  (let ((m (gensym)))
    `(progn (let ((,m (ensure-package-metadata ,package)))
              (setf (package-metadata-forbidden-symbol-names ,m) ,names))
       )))

(defun forbid-symbols-simple (symbols &optional (package *package*))
  "Forbid symbols designated in the package. 
Symbols should be a symbol designator list. Shadows all symbol names in the package.
Then installs definitions to that symbols so that they would err as soon as possible.
If buddens readtable extensions are used for the readtable, symbols can't be read by reader.
Also unexports symbols designated from the package.
Returns list of symbols.
"
  (let* ((p (find-package package))
         (symbols-to-forbid
          (iter 
            ; (:with sh = (package-shadowing-symbols p)) 
            (:for sname in symbols)
            (:for ssname = (string sname))
            (shadow ssname p)
            (:for s = (find-symbol ssname p))
            ; (assert (eq (symbol-package s) p))
            ; (assert s () "Unable to forbid-symbols-simple: ~A is not found in ~A" sname p)
            ; (assert (member s sh) () "Unable to forbid-symbols-simple: ~A is not shadowing in ~A" s p)
            (:collect s)
            )))
    (iter (:for s in symbols-to-forbid)
      (unexport s package)
      (decorate-function:portably-without-package-locks
        (eval 
         `(progn
            #+lispworks4.4 
            (defconstant ,s :forbidden-symbol)
            ; lispworks 4.4. is non-conformant. Error is signalled in lispworks 6.0 when
            ; attempting is made to define both variable and symbol-macro,
            ; so we choose define-symbol-macro in other lisps as it is more agressive
            (define-symbol-macro ,s (error "symbol ~S is forbidden in ~S" ,s ,p)) 
            (defmacro ,s (&rest ignore) (declare (ignore ignore)) (error "symbol ~S is forbidden in ~S" ,(symbol-name s) ,p))))))
    symbols-to-forbid))
    

(defparameter +!docstring+ "This form is like defpackage and it has some additional features. 
If some symbols from used packages clash, they are shadowed instead and referred
as 'forbidden'. Error occurs on an attempt to read these symbols unqualified in package created.

It also allows for additional clauses. Currently every additional clause can only occur once. 
\(:forbid . string-designators) - explicitly forbid some symbol names with addition to clashes
\(:print-defpackage-form [ t | nil ]) - if t, print defpackage form
\(:local-nicknames :nick1 :package1 :nick2 :package2 ...) - Refer to package1 as nick1, package2 as nick2 from package being defined. package1, package2 can be undefined at the time of ! form evaluation
\(:always [ t | nil ]) - if always, everything is wrapped into (eval-when (:compile-toplevel :load-toplevel :execute))
\(:allow-qualified-intern [ t | nil ]) - with buddens readtable extensions, by default you can't intern bar to foo typing foo::bar. Set allow-qualified-intern to allow this.
\(:custom-token-parsers custom-token-parser-spec1 ...) where 
custom-token-parser-spec is [ symbol | (:packages &rest package-designators) ] - define custom token parsers for the package. Symbols should be from another package and should be names of functions (stream symbol-name package) => (values processed-value processed-p). Custom token parser functions (including inherited ones) are applied from left to right to any new symbol token just before it is interned. If it processed-p is t, then processed-value is inserted into reader output instead of creating symbol named by token. (:packages &rest package-designator) spec
 causes all custom-token-parsers from the package named to be copied to the package being defined. 
\(:custom-reader symbol) - define custom token reader. 
With buddens readtable extensions enabled, when reader finds \"that-package:\" in the stream, function named by custom-token-reader is invoked with the same signature as READ. 
")



(defmacro ! (name &rest clauses) ; Best variant.: err on non-existing packages
  "see docstring above"
  (macrolet ((get-clause (name)
               `(multiple-value-setq (,name clauses) (extract-clause clauses ,(keywordize name))))
             (length-is-1 (name)
               `(when ,name
                  (assert (= 1 (length ,name)) () "~S clause accept exactly one argument" ,(keywordize name))
                  (setf ,name (first ,name)))
               )
             )
    (let (
          use
          forbid
          print-defpackage-form
                         ;  non-hosted-symbols 
          local-nicknames
          always
          shadowing-import-from-s
          export-s
          allow-qualified-intern
          custom-token-parsers
          custom-reader
          (clauses clauses))
      (get-clause use)
      (get-clause print-defpackage-form)
      (get-clause local-nicknames)
      (get-clause always)
      (get-clause allow-qualified-intern)
      (get-clause forbid)
      (get-clause custom-token-parsers)
      (get-clause custom-reader)
      (multiple-value-setq (shadowing-import-from-s clauses) (extract-several-clauses clauses :shadowing-import-from))
      (multiple-value-setq (export-s clauses) (extract-several-clauses clauses :export))
      (length-is-1 print-defpackage-form)
      (length-is-1 always)
      (length-is-1 allow-qualified-intern)
      (length-is-1 custom-reader)
      (let* (; (dest (keywordize name))
             (sources-for-clashes (mapcar #'force-find-package use))
             (sources-for-import nil)
             all-symbols-for-clashes
             all-symbols-for-import
             duplicates
             package-definition
             forbidden-symbol-names forbid-symbols-forms
             generated-import-clauses
             process-local-nicknames-form
             processed-export-s 
             allow-qualified-intern-form
             custom-token-parsers-form
             custom-reader-form
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
          (warn "def-merge-packages:! forbids clashing symbols ~S" duplicates))
        (dolist (p sources-for-import)
          (do-external-symbols (s p)
            (unless (member s duplicates :key 'car :test 'string=)
              (pushnew s all-symbols-for-import))))
        (setf forbidden-symbol-names 
              (iter 
                (:for (dup) in (append duplicates (mapcar 'list forbid)))
                (assert (or (symbolp dup) (stringp dup)) () "forbidden-symbol-names clause must contain a list of string designators")
                (:collect dup)))
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
        (setf processed-export-s
              (iter
                (:for clause in export-s)
                (cond
                 ((and (= (length clause) 1)
                       (stringp (car clause)))
                  (:collect (export-clause name (car clause))))
                 (t 
                  (:collect `(:export ,@clause))))))
        #-sbcl
        (setf process-local-nicknames-form 
              (if local-nicknames
                  `(setf (gethash (find-package ,name) *per-package-alias-table*) 
                         ',(process-local-nicknames name local-nicknames :to-alist t))
                  `(remhash (find-package ,name) *per-package-alias-table*)))
        #+sbcl
        (setf process-local-nicknames-form
              (when local-nicknames
                `((:local-nicknames ,@(process-local-nicknames name local-nicknames)))))                         
        (setf package-definition 
              `(defpackage ,name
                 ,@(when forbidden-symbol-names 
                     `((:shadow ,@forbidden-symbol-names)))
                 ,@`((:use ,@use))
                 ,@generated-import-clauses
                 ,@(iter (:for cl in shadowing-import-from-s) (:collect `(:shadowing-import-from ,@cl)))
                 ,@processed-export-s
                 ,@clauses
                 #+sbcl ,@process-local-nicknames-form))
        (setf forbid-symbols-forms
              `(; (setf (package-forbidden-symbol-names ,name) '(,@forbidden-symbol-names))
                (setf (package-metadata-forbidden-symbol-names (ensure-package-metadata ,name)) (forbid-symbols-simple ',forbidden-symbol-names ,name)))
                )
        (setf allow-qualified-intern-form `(setf (package-metadata-allow-qualified-intern (ensure-package-metadata ,name)) ,allow-qualified-intern))
        (setf custom-token-parsers-form nil)
        (let ((custom-token-parser-list
               (iter 
                 (:for spec in custom-token-parsers)
                 (cond 
                  ((and (listp spec)
                        (eq (car spec) :packages)) ; it is a (:packages . designators) spec
                   (flet ((outer-append (x) (:appending x)))
                     (iter 
                       (:for pack-name in (cdr spec))
                       (:for pack = (force-find-package pack-name))
                       (outer-append (get-custom-token-parsers-for-package pack)))))
                  ((symbolp spec)
                   (assert (symbol-package spec)
                       () "in custom-token-parsers clause for ~S, symbol ~S should have a home package" 
                     name spec)
                   (unless (fboundp spec)
                     (warn "in custom-token-parsers clause for ~S, symbol ~S should name a function" 
                           name spec))
                   (:collecting spec)
                   )
                  ((functionp spec)
                   (error "specifying function as a custom-token-parser for ~S is (currently) not supported" 
                          name))
                  (t 
                   (error "something unknown ~S is passed as a custom-token-parser spec for ~S" 
                          spec name))))))
          (setf custom-token-parsers-form
                `(setf (get-custom-token-parsers-for-package ,name) ',custom-token-parser-list))
          ); let ((custom-token-parser-list ...))
        (assert (symbolp custom-reader))
        (setf custom-reader-form 
              `(setf (package-metadata-custom-reader (ensure-package-metadata ,name)) ',custom-reader))
        (setf package-definition 
              (if always 
                  `(eval-when (:compile-toplevel :load-toplevel :execute)
                     (prog1
                         ,package-definition
                       #-sbcl ,process-local-nicknames-form
                       ,custom-token-parsers-form
                       ,@forbid-symbols-forms
                       ,allow-qualified-intern-form
                       ,custom-reader-form))
                `(prog1
                     ,package-definition
                   (eval-when (:load-toplevel :execute)
                     #-sbcl ,process-local-nicknames-form
                     ,custom-token-parsers-form
                     ,@forbid-symbols-forms
                     ,allow-qualified-intern-form
                     ,custom-reader-form))))
        (when print-defpackage-form
          (let (*print-length* *print-level*) (print package-definition)))
        package-definition
        ))))

(setf (documentation 'def-merge-packages 'function)
      +!docstring+)


(defmacro def-merge-packages (name &rest clauses)
  "see docstring below"
  `(! ,name ,@clauses)
  )

(setf (documentation 'def-merge-packages 'function)
      (concatenate 'string "This form is identical to def-merge-packages::! . I recommend use def-merge-packages::! whenewer possible,
as it is less verbose. But def-merge-packages is exported so that one could find it easily.
"
                 +!docstring+))


#+lispworks 
(dspec:define-dspec-alias ! (name &rest args)
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
           (:for case = (all-ascii-chars-in-same-case-p sname))
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


(defun unintern-all-internal-symbols-of-package (package)
  "Some cleanup of package. Useful after renaming of things to ensure no references to old names can be created. 
  Beware of type names, variables, advices and so. You should export them if you want predictable behaviour"
  (let ((package (find-package package)))
    (do-symbols (s package)
      (when (eq (symbol-package s) package)
        (unintern s package)))))
