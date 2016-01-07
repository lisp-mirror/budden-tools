;;; -*- Encoding: utf-8; -*-
;;; (С) Денис Будяк 2016. MIT лицензия.

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
   def-merge-packages:assign-package-forbidden-symbols

   def-merge-packages:set-package-lock-portably
   def-merge-packages:*per-package-metadata* 
   def-merge-packages:*per-package-alias-table*
   def-merge-packages:package-forbidden-symbol-names
   def-merge-packages:get-package-metadata-or-nil
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

