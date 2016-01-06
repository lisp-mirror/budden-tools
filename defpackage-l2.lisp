;;; -*- Encoding: utf-8; -*-
;;; (С) Денис Будяк 2016. MIT лицензия.

;;; Средство определения пакетов. Отличия от defpackage
#|
 1. Есть только use, нет импорта по одной букве
 2. Совпадающие символы запрещаются 
 3. Локальные псевдонимы пакетов 
 4. Русифицировано
 5. Нет shadow и shadowing-import
 6. Используются нестандартные функции работы с системой пакетов, стандартные использовать нельзя (но мы не можем это предотвратить). Нужно поправить нашу таблицу чтения под это.
 7. Нет функций export, unintern, make-package, есть только defpackage
 8. При изменении пакета изменения передаются во все использующие пакеты, поэтому конфликты во время выполнения defpackage исключены.
 9. Обычный пакет не должен использовать "наш" пакет, иначе конфликты при defpackage могут вернуться.
 10. Регистр не преобразуется

Концепция преобразования:

А. Совсем неоптимизированная:
При изменении пакета идти по зависимостям и выполнять все зависимые определения.
Для этого нужно запомнить информацию, достаточную для повторного выполнения определения. 

Б. Более-менее оптимизированная:
Запомнить не только определение, но и множество символов с учётом того, какие должны откуда браться. При повторном определении вычислять новое множество символов и сравнивать новое со старым. Если ничего не поменялось, то и не нужно повторно вычислять определение. Пока что этого НЕ ДЕЛАЕМ - нужно сделать простой вариант и хорошо закрыть его тестами. 

Таким образом, определение нужно запомнить в любом случае.

Избежание конфликтов при переопределении:
1. Появился новый импортируемый символ или clash - нужно стереть внутренний символ, если он уже существовал. 
2. Исчез clash - нужно его unintern.

Минимизация утечек при переопределении:
Перед удалением символа должен быть способ узнать, где он используется. Например, если он используется как данные, неплохо бы хотя бы предупредить о последствиях. 

|#

;; разобраться с удалением символа. Например, написать ещё функции: разэкспортировать 
;; символ отовсюду, удалить символ вообще. 
;; Добавить никнейм пакету


(eval-when (:execute)
  (error "Do not execute the file, use compile/load sequence"))

(eval-when (:load-toplevel :compile-toplevel)
    (setf *readtable* (copy-readtable nil))
    nil)

(cl:defpackage :defpackage-l2
  (:documentation "
See ! docstring for docs. ! is unexported to avoid any symbol clashes, but this is the
function you most likely want to use."   
   )
  (:use :cl ;:org.tfeb.hax.hierarchical-packages
   )
  (:import-from :iterate-keywords #:iter #:keywordize)
  (:import-from :def-merge-packages
   def-merge-packages:package-metadata ; structure 
   def-merge-packages:package-metadata-forbidden-symbol-names ; and
   def-merge-packages:package-metadata-custom-reader ; its
   def-merge-packages:package-metadata-custom-token-parsers ;slots 
   def-merge-packages:package-metadata-allow-qualified-intern
   def-merge-packages:package-metadata-interning-is-forbidden
   def-merge-packages:package-metadata-body-of-last-definition
   def-merge-packages:package-metadata-l2-package-p

   def-merge-packages:set-package-lock-portably
   def-merge-packages:*per-package-metadata* 
   def-merge-packages:*per-package-alias-table*
   def-merge-packages:package-forbidden-symbol-names
   def-merge-packages:ensure-package-metadata
   def-merge-packages:keywordize-package-designator
   def-merge-packages:extract-clause
   def-merge-packages:extract-several-clauses
   def-merge-packages:force-find-package
   def-merge-packages:group-similar-items
   def-merge-packages:collect-duplicates-into-sublists
   def-merge-packages:search-and-replace-seq
   def-merge-packages:get-custom-token-parsers-for-package
   def-merge-packages:this-source-file-directory
   )
  (:export 
   #:декл_пакет_л2 ; exported name for !. ! itself is unexported
   ; #:export2 ; smart export clause
   def-merge-packages:package-metadata ; structure 
   def-merge-packages:package-metadata-forbidden-symbol-names ; and
   def-merge-packages:package-metadata-custom-reader ; its
   def-merge-packages:package-metadata-custom-token-parsers ;slots 
   def-merge-packages:package-metadata-allow-qualified-intern
   def-merge-packages:package-metadata-interning-is-forbidden
   def-merge-packages:package-metadata-body-of-last-definition
   def-merge-packages:package-metadata-l2-package-p

   def-merge-packages:set-package-lock-portably
   def-merge-packages:*per-package-metadata* ; variable
   def-merge-packages:*per-package-alias-table* ; variable, stolen from hierarchical-packages
   def-merge-packages:package-forbidden-symbol-names ; place of package designator
   def-merge-packages:ensure-package-metadata ; makes sure that *per-package-metadata* entry for package exists
   def-merge-packages:keywordize-package-designator
   def-merge-packages:extract-clause ; extract one clause of def... form (e.g. defpackage) by its head
   ;#:reexport ; For every symbol in to-package2 which is external in 
   ;           ; package1, export it from to-package2
   #:reexport-clause-for-package ; for every expternal symbol in package, write an export clause
   def-merge-packages:force-find-package ; force-find-package. If package not found, it is a cerror
   def-merge-packages:group-similar-items ; group-similar-items
                  ; Given a list of items, groups similar (in sence of key-args) items into sublists
   def-merge-packages:collect-duplicates-into-sublists ; '(#\a c #\A #\B #\b) :test 'equalp -> ((#\A #\a) (#\b #\B))
   #:find-symbol-in-packages ; like apropos, but searches only exact symbol name and returns a list
   def-merge-packages:search-and-replace-seq
   #:get-custom-reader-for-package
   def-merge-packages:get-custom-token-parsers-for-package

   #:unintern-all-internal-symbols-of-package

   def-merge-packages:this-source-file-directory
   ))

(in-package :defpackage-l2)

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
  "Generates :export clause from string containing qualified symbol names and comments. 
   nickname: is replaced with #:. So, you can safely navigate to it via 
   your 'find-definition' command. E.g.
  (defpackage pack
   #.(export-clause \"
      pack:sym1 ; this is the first symbol of package
      pack:sym2 ; this is the second one
      other-pack:reexported ; this won't be replaced
      \"))
  Не экспортируется. Используется внутри defpackage-l2::! , когда содержимое кляузы экспорта является строкой."
  (let* ((nickname (string nickname))
         (expr1 (concatenate 'string nickname "::")) ; package::
         (expr2 (concatenate 'string nickname ":")) ; package:
         (processed1 (search-and-replace-seq 'string string expr1 "#:" :all t :test 'equalp))
         (processed2 (search-and-replace-seq 'string processed1 expr2 "#:" :all t :test 'equalp))
         (clause-string (concatenate 'string "(" processed2 ")")))
    `(:export
      ,@(with-input-from-string (s clause-string)
          (export2-reader s nil)))))

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
from to-package too. Была переведена в разряд устаревших, поскольку не декларативна и плохо согласуется с use"
  (let ((s (gensym)))
  `(do-external-symbols (,s ,from-package)
    (when (eq ,s (find-symbol (string ,s) ,to-package))
      (export ,s ,to-package)))))


(defun process-local-nicknames (new-package-name list &key to-alist)
  (assert (evenp (length list)) () "defpackage-l2::! : local package nicknames list must be of even length")
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

#+LispWorks
;;; Try and let entries go away when packages go away
(hcl:set-hash-table-weak *per-package-alias-table* :key) 

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
    

(defparameter +!docstring+ "This form is like defpackage, но у неё кое-что убрано и кое-что добавлено. Убрано:

- import
- shadowing-import
- shadow

Добавлено:

- Совпадающие символы автоматически запрещаются. Вместо них присутствует символ-заместитель с таким же именем, но он называется запрещённым и его нельзя считать читателем лиспа. 

\(:local-nicknames :nick1 :package1 :nick2 :package2 ...) - Refer to package1 as nick1, package2 as nick2 from package being defined. package1, package2 can be undefined at the time of ! form evaluation

 При изменении пакета изменения передаются во все использующие пакеты, поэтому конфликты во время выполнения defpackage исключены.
 Обычный пакет не должен использовать пакет, определённый этим макросом, иначе конфликты при defpackage могут вернуться.

\(:forbid . string-designators) - explicitly forbid some symbol names with addition to clashes
\(:print-defpackage-form [ t | nil ]) - if t, print defpackage form
\(:always [ t | nil ]) - if always, everything is wrapped into (eval-when (:compile-toplevel :load-toplevel :execute))
\(:allow-qualified-intern [ t | nil ]) - with buddens readtable extensions, by default you can't intern bar to foo typing foo::bar. Set allow-qualified-intern to allow this.
\(:custom-token-parsers custom-token-parser-spec1 ...) where 
custom-token-parser-spec is [ symbol | (:packages &rest package-designators) ] - define custom token parsers for the package. Symbols should be from another package and should be names of functions (stream symbol-name package) => (values processed-value processed-p). Custom token parser functions (including inherited ones) are applied from left to right to any new symbol token just before it is interned. If it processed-p is t, then processed-value is inserted into reader output instead of creating symbol named by token. (:packages &rest package-designator) spec
 causes all custom-token-parsers from the package named to be copied to the package being defined. 
")


; 111
(defmacro ! (name &rest clauses) 
  "См. +!docstring+"
  (macrolet ((get-clause (name)
               `(multiple-value-setq (,name clauses) (extract-clause clauses ,(keywordize name))))
             (length-is-1 (name)
               `(when ,name
                  (assert (= 1 (length ,name)) () "~S clause accept exactly one argument" ,(keywordize name))
                  (setf ,name (first ,name)))
               )
             )
    (let (
          (original-definition-body (copy-tree clauses))
          use
          forbid
          print-defpackage-form
                         ;  non-hosted-symbols 
          local-nicknames
          always
          export-s
          allow-qualified-intern
          custom-token-parsers
          import-from-s
          intern-s
          (clauses clauses))
      (get-clause use)
      (get-clause print-defpackage-form)
      (get-clause local-nicknames)
      (get-clause always)
      (get-clause allow-qualified-intern)
      (get-clause forbid)
      (get-clause custom-token-parsers)
      (multiple-value-setq (export-s clauses) (extract-several-clauses clauses :export))
      (multiple-value-setq (import-from-s clauses) (extract-several-clauses clauses :import-from))

      (when import-from-s
        (error "кляуза :import-from запрещена в defpackage-l2::! при попытке определения ~S" name))
      (multiple-value-setq (intern-s clauses) (extract-several-clauses clauses :intern))
      (when intern-s
        (error "кляуза :intern запрещена в defpackage-l2::! при попытке определения ~S (хотя, возможно, она ничему и не мешает)" name))

      (length-is-1 print-defpackage-form)
      (length-is-1 always)
      (length-is-1 allow-qualified-intern)
      (let* (; (dest (keywordize name))
             (sources-for-clashes (mapcar #'force-find-package use))
             all-symbols-for-clashes
             duplicates
             package-definition
             record-package-definition-to-metadata-forms
             forbidden-symbol-names forbid-symbols-forms
             process-local-nicknames-form
             processed-export-s 
             allow-qualified-intern-form
             custom-token-parsers-form
             )
        (dolist (p sources-for-clashes)
          (do-external-symbols (s p)
            (pushnew s all-symbols-for-clashes)))
    
        (setf duplicates (collect-duplicates-into-sublists all-symbols-for-clashes :test 'string=))
        ; duplicates is a list of lists of duplicate symbols

        ; remove explicitly shadowing-imported symbols from it
        (let ((all-shadowing-import-names nil))
          (setf duplicates 
                (iter
                  (:for dup in duplicates)
                  (unless (member (car dup) all-shadowing-import-names :test 'string=)
                    (:collect dup)))))

        ; в обычном def-merge-packages это есть, а нам вроде не нужно
        ;(when duplicates
        ;  (warn "defpackage-l2:! forbids clashing symbols ~S" duplicates))
        (setf forbidden-symbol-names 
              (iter 
                (:for (dup) in (append duplicates (mapcar 'list forbid)))
                (assert (or (symbolp dup) (stringp dup)) () "forbidden-symbol-names clause must contain a list of string designators")
                (:collect dup)))
        (setf processed-export-s
              (iter
                (:for clause in export-s)
                (cond
                 ((and (= (length clause) 1)
                       (stringp (car clause)))
                  (:collect (export-clause name (car clause))))
                 (t 
                  (:collect `(:export ,@clause))))))
        (setf process-local-nicknames-form 
              (if local-nicknames
                  `(setf (gethash (find-package ,name) *per-package-alias-table*) 
                         ',(process-local-nicknames name local-nicknames :to-alist t))
                  `(remhash (find-package ,name) *per-package-alias-table*)))
        (setf package-definition 
              `(defpackage ,name
                 ,@(when forbidden-symbol-names 
                     `((:shadow ,@forbidden-symbol-names)))
                 ,@`((:use ,@use))
                 ,@processed-export-s
                 ,@clauses
                 ))
        (setf record-package-definition-to-metadata-forms
              `(
                (setf (package-metadata-l2-package-p (ensure-package-metadata ,name)) t)
                (setf (package-metadata-body-of-last-definition (ensure-package-metadata ,name)) ',original-definition-body)))
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
        (setf package-definition 
              (if always 
                  `(eval-when (:compile-toplevel :load-toplevel :execute)
                     (prog1
                         ,package-definition
                       ,@record-package-definition-to-metadata-forms
                       ,process-local-nicknames-form
                       ,custom-token-parsers-form
                       ,@forbid-symbols-forms
                       ,allow-qualified-intern-form
                       ))
                `(prog1
                     ,package-definition
                     ,@record-package-definition-to-metadata-forms
                   (eval-when (:load-toplevel :execute)
                     ,@record-package-definition-to-metadata-forms
                     ,process-local-nicknames-form
                     ,custom-token-parsers-form
                     ,@forbid-symbols-forms
                     ,allow-qualified-intern-form
                     ))))
        (when print-defpackage-form
          (let (*print-length* *print-level*) (print package-definition)))
        package-definition
        ))))

(setf (documentation 'декл_пакет_л2 'function)
      +!docstring+)


(defmacro декл_пакет_л2 (name &rest clauses)
  "see docstring below"
  `(! ,name ,@clauses)
  )

(setf (documentation 'декл_пакет_л2 'function)
      (concatenate 'string "This form is identical to defpackage-l2::! . I recommend use defpackage-l2::! whenewer possible,
as it is less verbose. But defpackage-l2 is exported so that one could find it easily.
"
                 +!docstring+))


#+lispworks 
(dspec:define-dspec-alias ! (name &rest args)
  (setf args args)
  `(defpackage ,name))

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


(defun unintern-all-internal-symbols-of-package (package)
  "Some cleanup of package. Useful after renaming of things to ensure no references to old names can be created. 
  Beware of type names, variables, advices and so. You should export them if you want predictable behaviour"
  (let ((package (find-package package)))
    (do-symbols (s package)
      (when (eq (symbol-package s) package)
        (unintern s package)))))
