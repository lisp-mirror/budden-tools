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


(defpackage "BUDDEN-TOOLS"
  (:nicknames "budden-tools" "bu" "BU")
  (:use :cl :named-readtables :buddens-readtable :defstruct-meta :org.tfeb.hax.hierarchical-packages
   :merge-packages-simple)
  (:import-from :iterate-keywords #:iter #:keywordize #:dsetq)
  (:import-from :alexandria #:with-gensyms #:once-only #:string-designator)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :swank #:*readtable-alist*)
  (:import-from :swank-backend #:pathname-to-filename #:filename-to-pathname #:make-weak-key-hash-table #:make-weak-value-hash-table)
;  (:import-from :merge-packages-and-reexport #:collect-duplicates-into-sublists)
  (:export

;; sequences
   #:sequence-last
   #:rmsubseq
   #:search-and-replace-seq
   #:replace-subseq
   #:split-sequence ; reexported from split-sequence

;; trees
   #:tree-weight
   #:maptree

;; lists. Some function are likely not new... 
   #:mapcarcar 
   #:mapcarcadr
   #:direct-sum 
   #:assert-unique-symbols
   #:1-to-list 
   #:list-to-1
   #:collect-duplicates 
   #:collect-duplicates-into-sublist 
   #:splice-list
   #:unsplice-list

   #:dotted-p ; check if list is dotted and 
              ; returns its parts in two values

;; alists, plists, arglist dispatching
   #:list-to-alist 
   #:alist-to-list
   #:assoc-getf*
   #:getf* ; setfable
   #:dispatch-keyarg-simple ; helps to build dispatched arglists with &key arguments
   #:dispatch-keyargs-simple 
   #:dispatch-keyargs-full 
   #:_f
   #:symbol-macroletf   

;; hashes
   #:ensure-gethash-2 ; gethash with default, better than alexandria
   #:make-weak-key-hash-table ; from swank
   #:make-weak-value-hash-table ; from swank

;; structures
   #:struct-to-alist ; lispworks only for now

;; strings and symbols
   #:str+
   #:str++ ; converts anything to string with format ~A
   #:symbol+ 
   #:dotted-name-to-list 
   #:splice-list
   #:unsplice-list
   #:keywordize ; stolen from iterate
   #:non-empty-string-p
   #:string-or ; returns first non empty string from its &rest args

   #:char-upcase-cyr
   #:char-equal-cyr
   #:char-downcase-cyr
   #:string-upcase-cyr
   #:string-downcase-cyr
   #:string-equal-cyr
   #:string-designator
   #:string-designator-p 
   #:cyrillic-char-p
   #:translit-reversibly
   #:textual-equal-cyr

   #:char-upcase-ascii
   #:char-downcase-ascii
   #:string-upcase-ascii
   #:string-downcase-ascii

;; evaluation and general shortcuts
;  #:let1 is interned to CL as it is very necessary :) 
   #:let1 ; but we would better reexport it 
   #:iter ; reexported from iterate
   #:with-gensyms ; reexported from alexandria
   #:once-only ; reexported from alexandria
   #:ignored 
;   #:eval-when* ; it is defined internal in cl-user for some unknown reason
   #:print-if 
   #:proga
   #:dsetq ; reexported from iterate
   #:pllet1 ; "bind" place
   #:smlet ; another name for symbol-macrolet
   #:defun-to-file ; to see source of your generated function. TODO: package name->file name, gensyms vs `,
   #:eval-with-file ; write code to file, compile it and load
   #:not-null ; type (not null)
   #:the* ; errs if type ain't match. Otherwise, returns a thing

   #:mlvl-bind ; multiple-value-bind
   #:mlvl-call ; multiple-value-call
   #:mlvl-list ; multiple-value-list
   #:mlvl-prog1 ; multiple-value-prog1
   #:mlvl-setq ; multiple-value-setq
   #:mlvls-limit ; multiple-values-limit

;; typed binding
   #:with-conc-name 
   #:let-with-conc-type 
   #:--> 

;; i/o utilities
   #:show-hash
   #:show-expr
   #:read-file-into-string
   #:save-string-to-file  

;; system environment
   #:map-dir 
   #:path-to-a-file
   #:maybe-add-slash ; got a directory name. Make sure it ends with slash
   #:up-dir
   #:pathname-to-filename ; from swank-backend
   #:filename-to-pathname ; from swank-backend
   #:quit-lisp ; thanks to Thomas A.Russ

;; make input stream available to sharpsign-dot
   #:*read-eval-stream*

;; see-packages and friends
   #:*keyword-package*
   #:see-packages-on ; enable see-packages extensions on a (named) readtable. TODO: rename it
   #:see-packages
   #:see-packages-find-symbol ; returns (values ((symbol-found . package-where-found) ...) 
   #:see-packages-check-bad-clashes
   #:*per-package-alias-table*
   #:hp-find-package
   #:hp-in-package
   #:hp-alias-map   ; stolen from conduit packages
   #:delete-hp-alias-map ; stolen from conduit packages

   #:defreadtable
   #:find-readtable
   #:in-readtable
   #:package-readtable
   #:package-seen-packages-list
   #:*readtable-alist*
   #:unregister-readtable

   #:get-custom-reader-for-package ; use your own reader in package context instead of common lisp reader
   #:get-custom-token-parsers-for-package ; parse tokens read with custom parsers (which can, but not encouaraged to have side-effects on stream
   #:symbol-readmacro ; function of two arguments: symbol and a stream. setf symbol-readmacro to reader switched by the symbol
;   #:def-symbol-readmacro ; def-symbol-readmacro symbol (stream) reader-body. In an attempt to be able to navigate through symbol-readmacros. 
   #:it-is-a-car-symbol-readmacro ; put this at the first line of your symbol-readmacro definition
   #:*print-normalize-seen-symbols*

   #:defpackage-autoimport
   #:find-symbol-in-packages
   #:package-doctor

;; alternative-backquoting
   "`"
   ","
;; some symbol-readmacros
   #:/with-package/
   #:/with-readtable-case/
   ))

;(merge-packages-and-reexport::! :proga-implementation 
;                                )
(defpackage :trivial-deftest 
  (:documentation "Very simple test suite you can optionally run at load time. Use ! for brevity")
  (:use :cl)
  (:export #:*run-tests* #:deftest)
  )

(defpackage-autoimport :proga-implementation
                       (:use :cl)
                       (:auto-import-from :budden-tools :trivial-deftest :defstruct-meta)
                       (:auto-import-first-clashing t)
                       )

(defpackage :dat1 (:use)) ; пакет для чтения дат. 
