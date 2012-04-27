(in-package :cl-user) 
#.(progn 
   (setf *readtable* (copy-readtable nil))
   nil)

; deprecate it? вряд ли, потому что struct-to-alist
(defpackage :defstruct-meta (:use :cl)
  (:export #:defstruct*m-slot-names-and-accessors
   #:defstruct*m
   #:defstruct*mc
   #:with-struct)
  )


(merge-packages-simple::!4 :BUDDEN-TOOLS
  (:nicknames "budden-tools")
  (:use :cl :named-readtables :buddens-readtable :defstruct-meta :org.tfeb.hax.hierarchical-packages
   :merge-packages-simple)
  (:import-from :iterate-keywords #:iter #:keywordize #:dsetq)
  (:import-from :alexandria #:with-gensyms #:once-only #:string-designator)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :swank #:*readtable-alist*)
  (:import-from :swank-backend #:pathname-to-filename #:filename-to-pathname #:make-weak-key-hash-table #:make-weak-value-hash-table)
;  (:import-from :merge-packages-and-reexport #:collect-duplicates-into-sublists)
  (:export
   "
;; sequences
   budden-tools:sequence-last
   budden-tools:subseq* 
   budden-tools:rmsubseq
   budden-tools:subseq1 ; does not err when run out of sequence, returns emptyness instead
   budden-tools:search-and-replace-seq ; reexported from :merge-packages-simple
   budden-tools:replace-subseq
   budden-tools:split-sequence ; reexported from :split-sequence

;; trees
   budden-tools:tree-weight
   budden-tools:maptree
   budden-tools:copy-tree-of-structures

;; lists. Some function are likely not new... 
   budden-tools:mapcarcar 
   budden-tools:mapcarcadr
   budden-tools:direct-sum 
   budden-tools:assert-unique-symbols
   budden-tools:1-to-list 
   budden-tools:list-to-1
   budden-tools:collect-duplicates 
   budden-tools:collect-duplicates-into-sublist 
   budden-tools:splice-list
   budden-tools:unsplice-list

   budden-tools:dotted-p ; check if list is dotted and 
              ; returns its parts in two values

;; alists, plists, arglist dispatching
   budden-tools:list-to-alist 
   budden-tools:alist-to-list
   budden-tools:assoc-getf*
   budden-tools:getf* ; setfable
   budden-tools:dispatch-keyarg-simple ; helps to build dispatched arglists with &key arguments
   budden-tools:dispatch-keyargs-simple 
   budden-tools:dispatch-keyargs-full 
   budden-tools:_f
   budden-tools:__f
   budden-tools:symbol-macroletf   

;; hashes
   budden-tools:ensure-gethash-2 ; gethash with default, better than alexandria
   budden-tools:make-weak-key-hash-table ; from swank
   budden-tools:make-weak-value-hash-table ; from swank

;; structures
   budden-tools:struct-to-alist ; lispworks only for now

;; strings and symbols
   budden-tools:str+
   budden-tools:str++ ; converts anything to string with format ~A
   budden-tools:symbol+ 
   budden-tools:dotted-name-to-list 
   budden-tools:splice-list
   budden-tools:unsplice-list
   budden-tools:keywordize ; stolen from iterate
   budden-tools:non-empty-string-p
   budden-tools:string-or ; returns first non empty string from its &rest args
   budden-tools:princ-to-string-delimited-list

   budden-tools:char-upcase-cyr
   budden-tools:char-equal-cyr
   budden-tools:char-downcase-cyr
   budden-tools:string-upcase-cyr
   budden-tools:string-downcase-cyr
   budden-tools:string-equal-cyr
   budden-tools:string-designator
   budden-tools:string-designator-p 
   budden-tools:cyrillic-char-p
   budden-tools:translit-reversibly
   budden-tools:textual-equal-cyr

   budden-tools:char-upcase-ascii
   budden-tools:char-downcase-ascii
   budden-tools:string-upcase-ascii
   budden-tools:string-downcase-ascii

;; evaluation and general shortcuts
;  budden-tools:let1 is interned to CL as it is very necessary :) 
   #:let1 ; but we would better reexport it 
   budden-tools:iter ; reexported from iterate
   budden-tools:with-gensyms ; reexported from alexandria
   budden-tools:once-only ; reexported from alexandria
   budden-tools:ignored 
;   budden-tools:eval-when* ; it is defined internal in cl-user for some unknown reason
   budden-tools:print-if 
   budden-tools:proga
   budden-tools:dsetq ; reexported from iterate
   budden-tools:pllet1 ; 'bind' place
   budden-tools:smlet ; another name for symbol-macrolet
   budden-tools:defun-to-file ; to see source of your generated function. TODO: package name->file name, gensyms vs `,
   budden-tools:eval-with-file ; write code to file, compile it and load
   budden-tools:not-null ; type (not null)
   budden-tools:the* ; errs if type ain't match. Otherwise, returns a thing

   budden-tools:with-byref-params ; declare function to accept some
   ; params by reference
   budden-tools:byref ; pass parameter by reference
  

   budden-tools:mlvl-bind ; multiple-value-bind
   budden-tools:mlvl-call ; multiple-value-call
   budden-tools:mlvl-list ; multiple-value-list
   budden-tools:mlvl-prog1 ; multiple-value-prog1
   budden-tools:mlvl-setq ; multiple-value-setq
   budden-tools:mlvls-limit ; multiple-values-limit

;; typed binding
   budden-tools:with-conc-name  ; obsolete
   budden-tools:let-with-conc-type ; 

   budden-tools:^ 
   budden-tools:with-conc-namec 

;; i/o utilities
   budden-tools:show-hash
   budden-tools:show-expr
   budden-tools:show-exprt
   budden-tools:read-file-into-string
   budden-tools:save-string-to-file  

;; system environment
   budden-tools:map-dir 
   budden-tools:path-to-a-file
   budden-tools:maybe-add-slash ; got a directory name. Make sure it ends with slash
   budden-tools:up-dir
   budden-tools:pathname-to-filename ; from swank-backend
   budden-tools:filename-to-pathname ; from swank-backend
   budden-tools:quit-lisp ; thanks to Thomas A.Russ
   budden-tools:edit-stream-position ; trying to do so at least

;; make input stream available to sharpsign-dot
   budden-tools:*read-eval-stream*

;; see-packages and friends
   budden-tools:*keyword-package*
   budden-tools:see-packages-on ; enable see-packages extensions on a (named) readtable. TODO: rename it
   budden-tools:see-packages
   budden-tools:see-packages-find-symbol ; returns (values ((symbol-found . package-where-found) ...) 
   budden-tools:see-packages-check-bad-clashes
   budden-tools:*per-package-alias-table*
   budden-tools:hp-find-package
   budden-tools:hp-in-package
   budden-tools:hp-alias-map   ; stolen from conduit packages
   budden-tools:delete-hp-alias-map ; stolen from conduit packages

   budden-tools:defreadtable
   budden-tools:find-readtable
   budden-tools:in-readtable
   budden-tools:package-readtable
   budden-tools:package-seen-packages-list
   budden-tools:*readtable-alist*
   budden-tools:unregister-readtable

   budden-tools:get-custom-reader-for-package ; use your own reader in package context instead of common lisp reader
   budden-tools:get-custom-token-parsers-for-package ; parse tokens read with custom parsers (which can, but not encouaraged to have side-effects on stream
   budden-tools:symbol-readmacro ; function of two arguments: symbol and a stream. setf symbol-readmacro to reader switched by the symbol
;   budden-tools:def-symbol-readmacro ; def-symbol-readmacro symbol (stream) reader-body. In an attempt to be able to navigate through symbol-readmacros. 
   budden-tools:it-is-a-car-symbol-readmacro ; put this at the first line of your symbol-readmacro definition
   budden-tools:*print-normalize-seen-symbols*

   budden-tools:defpackage-autoimport
   budden-tools:export-clause
   budden-tools:find-symbol-in-packages
   budden-tools:package-doctor

;; alternative-backquoting
   |`|
   |,|
;; some symbol-readmacros
   budden-tools:/with-package/
   budden-tools:/with-readtable-case/
   "
   ))

;(merge-packages-and-reexport::! :proga-implementation 
;                                )
(defpackage :trivial-deftest 
  (:nicknames :def-trivial-test)
  (:documentation "Very simple test suite you can optionally run at load time. Use ! for brevity")
  (:use :cl)
  (:export #:*run-tests* ; #:deftest
   )
  )



(defpackage :dat1 (:use)) ; package for date reading 
