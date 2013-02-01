;;; -*- Encoding: utf-8; -*-
(defpackage :budden-tools
  (:use :cl :named-readtables :buddens-readtable)
  (:import-from :iterate-keywords #:iter #:keywordize #:dsetq)
  (:import-from :alexandria #:with-gensyms #:once-only)
  (:import-from :split-sequence #:split-sequence)
  
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
   #:direct-sum 
   #:assert-unique-symbols
   #:1-to-list 
   #:list-to-1
   #:collect-duplicates 
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
   

;; structures
   #:struct-to-alist ; lispworks only for now

;; strings and symbols
   #:str+  
   #:symbol+ 
   #:dotted-name-to-list 
   #:splice-list
   #:unsplice-list
   #:keywordize ; stolen from iterate
   #:non-empty-string-p

   #:char-upcase-cyr
   #:char-equal-cyr
   #:char-downcase-cyr
   #:string-upcase-cyr
   #:string-downcase-cyr
   #:string-equal-cyr

;; evaluation 
;  #:let1 is interned to CL as it is very necessary :) 
   #:let1 ; but we would better reexport it 
   #:iter ; reexported from iterate
   #:with-gensyms ; reexported from alexandria
   #:once-only ; reexported from alexandria
   #:ignored 
   #:eval-when*
   #:print-if 
   #:proga
   #:dsetq ; reexported from iterate
   #:pllet1 ; "bind" place

;; i/o utilities
   #:show-hash
   #:show-expr
   #:read-file-into-string
   #:save-string-to-file  

;; system environment
   #:map-dir 
   #:path-to-a-file
   #:up-dir

;; sharpsign-dot
   #:*read-eval-stream*
   ))

(defpackage :trivial-deftest 
  (:use :cl)
  (:export #:*run-tests* #:deftest))

(merge-packages-and-reexport::! :proga-implementation (:budden-tools :trivial-deftest :cl))
