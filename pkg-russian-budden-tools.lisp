;;; -*- Encoding: utf-8; -*-
(in-package :cl-user) 

(eval-when (:execute)
  (error "Do not :execute the file, use compile/load sequence"))
(eval-when (:load-toplevel :compile-toplevel)
    (setf *readtable* (copy-readtable nil))
    nil)

(def-merge-packages::! :RUSSIAN-BUDDEN-TOOLS
  (:nicknames "russian-budden-tools")
  (:documentation "case transformation and comparison of Russian characters")
  (:use :budden-tools :cl :named-readtables :buddens-readtable 
   :defstruct-meta ; :org.tfeb.hax.hierarchical-packages
   :merge-packages-simple :iterk)
  (:import-from :alexandria #:with-gensyms #:once-only #:string-designator #:eswitch)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :swank #:*readtable-alist*)
  (:import-from :swank-backend #:pathname-to-filename #:filename-to-pathname #:make-weak-key-hash-table #:make-weak-value-hash-table)
;  (:import-from :merge-packages-and-reexport #:collect-duplicates-into-sublists)
  (:export
   "
   russian-budden-tools:char-upcase-cyr
   russian-budden-tools:char-equal-cyr
   russian-budden-tools:char-downcase-cyr
   russian-budden-tools:string-upcase-cyr
   russian-budden-tools:string-downcase-cyr
   russian-budden-tools:string-equal-cyr
   russian-budden-tools:string-designator
   russian-budden-tools:string-designator-p 
   russian-budden-tools:cyrillic-char-p
   russian-budden-tools:translit-reversibly
   russian-budden-tools:textual-equal-cyr
   russian-budden-tools:*cyrillic-characters*

   russian-budden-tools:define-open-pipe-character-translators 
   russian-budden-tools:define-open-pipe-character-translators-by-sample
   "
   ))






