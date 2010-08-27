;;; Written by Denis Budyak, 2009. This code is in public domain
;; requires iterate. 

#-budden
  (cl:require :iterate)

#-budden
(progn
  ; almost a quote from iterate-keywords 
  (iter:defsynonym :for iter:for)
  (iter:defsynonym :with iter:with)
  (iter:defsynonym :collect iter:collect)
  (iter:defsynonym :appending iter:appending))

#+budden 
  (cl:require :iterate-keywords)

(cl:defpackage :merge-packages-and-reexport
  (:use :cl)
  (:import-from :iter #:iter)
  (:export 
   #:!       ; take several packages and export all what we can without clashes. 
             ; There are two ways to manage clashes: either don't export them
             ; or export leftmost symbol according to package order in ! arglist
   #:reexport ; For every symbol in to-package2 which is external in 
              ; package1, export it from to-package2
   ))

(in-package :merge-packages-and-reexport)


(defun collect-duplicates-into-sublist (list &rest key-args &key key test test-not)
  "List of list of duplicates"
  (declare (ignore key test test-not))          
  (iter nil
    (:for x in list)
    (:for bucket = (apply #'assoc x buckets key-args))
    (if bucket 
        (push x (cdr bucket))
      (:collect `(,x) :into buckets))
    (iter:finally
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


(defmacro ! (new-package source-packages 
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
  (let* ((dest (iter::keywordize new-package))
         (sources (mapcar #'iter::keywordize source-packages))
         (srcs (mapcar 'find-package sources))
         #+budden (cl-user::*forbidden-symbol-names* nil)
         all-symbols
         duplicates
         reported-duplicates
         symbols-not-to-import
         package-definition
         )
    (dolist (p srcs)
      (do-external-symbols (s p)
        (pushnew s all-symbols)))
    (setf duplicates (collect-duplicates-into-sublist all-symbols :test 'string=))
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
    (print symbols-not-to-import)
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
    ))



