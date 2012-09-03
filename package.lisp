(in-package :cl-user) 


(eval-when (:execute)
  (break "it is preferrable to process the file via compilation, otherwise *readtable* might change"))

#.(progn 
   (setf *readtable* (copy-readtable nil))
   nil)

; deprecate it? вряд ли, потому что struct-to-alist
(defpackage :defstruct-meta (:use :cl)
  (:documentation "defstruct does not record names of accessors. defstruct*m was intended to address this, 
but it is unfinished. Also it seem to violate GNU. So it is likely to be removed some time"
   (:export #:defstruct*m-slot-names-and-accessors
    #:defstruct*m
    #:defstruct*mc
    #:with-struct)
   ))


(merge-packages-simple::! :BUDDEN-TOOLS
  (:nicknames "budden-tools")
  (:documentation "Some tools by budden. See packages definition to find a list of symbols")
  (:use :cl :named-readtables :buddens-readtable :defstruct-meta :org.tfeb.hax.hierarchical-packages
   :merge-packages-simple)
  (:import-from :iterate-keywords #:iter #:keywordize #:dsetq)
  (:import-from :alexandria #:with-gensyms #:once-only #:string-designator #:eswitch)
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
   budden-tools:npushback ; DEPRECATED pushes item at the end of the list
   budden-tools:nenqueue ; pushes item at the end of the list

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
   budden-tools:_f ; apply f to its first argument (which should be a place) and store result in the place. E.g. (_f nconc x '(1)) adds '(1) to x. 
   budden-tools:__f ; apply f to its second argument
   budden-tools:symbol-macroletf   

;; hashes
   budden-tools:ensure-gethash-2 ; gethash with default, better than alexandria
   budden-tools:make-weak-key-hash-table ; from swank
   budden-tools:make-weak-value-hash-table ; from swank

;; structures
   budden-tools:struct-to-alist ; lispworks only for now

;; strings and symbols
   budden-tools:str+ ; concatenate strings
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
   budden-tools:proga ; macro to get rid of some extra parens and nesting levels. Changes lisp syntax significantly. 
                      ; currently it needs to be redesigned. 
   budden-tools:dsetq ; reexported from iterate
   budden-tools:pllet1 ; 'bind' place
   budden-tools:smlet ; another name for symbol-macrolet
   budden-tools:defun-to-file ; to see source of your generated function. TODO: package name->file name, gensyms vs `,
   budden-tools:eval-with-file ; write code to file, compile it and load
   budden-tools:not-null ; type (not null)
   budden-tools:the* ; errs if type ain't match. Otherwise, returns a thing
   budden-tools:eswitch ; reexported from alexandria

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
   budden-tools:let-with-conc-type ; obsolete?
   budden-tools:with-the1

   budden-tools:^ #| There is lispy form (^ A B) and infix form A^B.
       ^ can be considered as a \"dot\" in languages such as C,Java or Basic.
       That is, A^B is similar to A.B in Java.
       To enable it, set custom-token-parsers appropriately. 
       In an infix form, A must be a symbol. B is always a sequence of characters, which is
       used to determine action of ^. 
       Action of (^ a b) might depend on:
     -- environment 
     -- declared type of a
     -- actual type of a
     -- value of b
       Action of a^b also depends on either it is located at the beginning of list or not.
       (x A^B) is expanded to (x (^ A B)), (A^B . args) is expanded to (^ A B . args) at read-time.
       (^ A B) is further processed at compile time.

       Example: 
       If it is known at compile time, that A is a structure of type FOO, A^BAR is 
       expanded into (FOO-BAR A). 
       |#
   budden-tools:with-conc-namec ; set translaction prefix for V^X where V is a given symbol.
   budden-tools:with-proplist-carat  ; another (experimental) example of per-variable assignment of ^ 

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
   budden-tools:edit-stream-position ; trying to find out position in a file stream, open the file at the cursor and edit it

;; make input stream available to sharpsign-dot
   budden-tools:*read-eval-stream*

;; see-packages and friends
   budden-tools:*keyword-package* ; just a keyword package
   budden-tools:see-packages-on ; enable symbol-readmacros, custom-token-parsers, advanced-readtable-case and local-nicknames on a (named) readtable. TODO: rename it
   budden-tools:*per-package-alias-table* ; stolen from \"hierarchical packages\"
   budden-tools:hp-find-package ; stolen from \"hierarchical packages\"
   budden-tools:hp-in-package  ; stolen from \"hierarchical packages\"
   budden-tools:hp-alias-map   ; stolen from conduit packages
   budden-tools:delete-hp-alias-map ; stolen from conduit packages
   budden-tools:*essential-binding-checkers* ; currently unused

   budden-tools:defreadtable ; stolen from named-readtables project
   budden-tools:find-readtable
   budden-tools:in-readtable
   budden-tools:package-readtable
   budden-tools:package-seen-packages-list
   budden-tools:*readtable-alist*
   budden-tools:unregister-readtable

   ; we do not export it anymore, use def-merge-packages::!4 budden-tools:get-custom-reader-for-package ; use your own reader in package context instead of common lisp reader, 
                                              ; e.g. you can do so that dat1:2011-01-01 would read a localtime:timestamp value
   ; we do not export it anymore, use def-merge-packages::!4 budden-tools:get-custom-token-parsers-for-package ; parse tokens read with custom parsers (which can, but not encouaraged to have side-effects on stream
   budden-tools:symbol-readmacro ; function of two arguments: symbol and a stream. setf symbol-readmacro to reader switched by the symbol
   budden-tools:def-symbol-readmacro ; Navigation does not work
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
(defpackage :def-trivial-test 
  (:nicknames :trivial-deftest)
  (:documentation "Very simple test suite you can optionally run at load time. Use ! for brevity")
  (:use :cl)
  (:export #:*run-tests* ; #:deftest
   )
  )

