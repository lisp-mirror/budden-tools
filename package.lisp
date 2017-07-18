;;; -*- Encoding: utf-8; system :budden-tools; -*-
(in-package :cl-user)


(eval-when (:execute)
  (error "Do not :execute the file, use compile/load sequence"))
(eval-when (:load-toplevel :compile-toplevel)
    (setf *readtable* (copy-readtable nil))
    nil)


(def-merge-packages::! :КАРТЫ-ИСХОДНИКОВ-ЛИЦО
  (:always t)
  (:documentation "Инфраструктура для запоминания и поиска соответствий между генерирующим и генерируемым исходными текстами")
  (:export
   "
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:TRACK-LOCATIONS
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:L/WITH-OUTPUT-TO-STRING
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:l/make-string-output-stream
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:L/PASS-FROM-STREAM-TO-FILE
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:l/pass-from-delegate-to-object
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:L/ADD-TO-LOCATION-MAP
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:L/SUBSEQ
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:L/FIND-SOURCES-IN-FILE
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:L/PRINC
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:L/STR++
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:EXTRACT-SOURCE-FILENAME-FROM-STREAM
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:L/SUBSTITUTE-SUBSEQ
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:L/RORL ; = return-object-recording-location
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:fix-offset-2 ; переводит смещение в файле. полученное из file-position, в положение, рассчитанное в буквах. При этом читает весь файл, т.е. работает медленно.
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:EXTRACT-SOURCE-FILENAME-FROM-STREAM
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:input-stream-position-in-chars
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:stream-get-line-number
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:*RECORD-LOCATIONS*
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:extract-file-position
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:get-stream-location-map-delegate
    ;; КАРТЫ-ИСХОДНИКОВ-ЛИЦО:string-stream-extract-string ; заменено на КАРТЫ-ИСХОДНИКОВ-ЛИЦО:ИСХОДНИК-ЦЕЛИКОМ-В-ВИДЕ-СТРОКИ
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:ИСХОДНИК-ЦЕЛИКОМ-В-ВИДЕ-СТРОКИ
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:|Удалить-карты-для-генерируемого-файла|
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:|Сохранить-карту-для-файла-в-файл|
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:|Имя-файла-карты-по-имени-файла-исходного-текста|
    КАРТЫ-ИСХОДНИКОВ-ЛИЦО:|Сохранить-карту-для-файла-в-файл|
    "
   ))

(def-merge-packages::! :BUDDEN-TOOLS
  (:always t)
  (:nicknames "budden-tools")
  (:documentation "Some tools by budden. See packages definition to find a list of symbols")
  (:use :cl :named-readtables  ; :org.tfeb.hax.hierarchical-packages
   :def-merge-packages :decorate-function :iterk :КАРТЫ-ИСХОДНИКОВ-ЛИЦО)
  (:import-from :alexandria #:with-gensyms #:once-only #:string-designator #:eswitch #:cswitch #:switch alexandria:simple-reader-error alexandria:named-lambda)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :swank #:*readtable-alist*)
  (:import-from :swank-backend #:pathname-to-filename #:filename-to-pathname #:make-weak-key-hash-table #:make-weak-value-hash-table)
  (:import-from :cl-fad #:pathname-as-directory)
;  (:import-from :merge-packages-and-reexport #:collect-duplicates-into-sublists)
  (:export
   "
;; sequences
   budden-tools:sequence-last
   budden-tools:subseq* 
   budden-tools:rmsubseq
   budden-tools:subseq1 ; does not err when run out of sequence, returns emptyness instead
   budden-tools:search-and-replace-seq ; reexported from :defpackage-budden
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
   budden-tools:full-outer-join ; similar to SQL's full outer join, but works on sets represented at lists. Has limitations, see docstring.

;; alists, plists, arglist dispatching
   budden-tools:list-to-alist 
   budden-tools:alist-to-list
   budden-tools:swap-pairs-in-plist ; returns new plist where keys and values are swapped
   budden-tools:assoc-getf*
   budden-tools:getf* ; setfable
   budden-tools:pass-keyargs
   budden-tools:dispatch-keyarg-simple ; helps to build dispatched arglists with &key arguments
   budden-tools:dispatch-keyargs-simple 
   budden-tools:dispatch-keyargs-full 
   budden-tools:_f ; apply f to its first argument (which should be a place) and store result in the place. E.g. (_f nconc x '(1)) adds '(1) to x. 
   budden-tools:__f ; apply f to its second argument
   budden-tools:symbol-macroletf   
   budden-tools:plist-names  ; items of list at odd places
   budden-tools:plist-values  ; items of a list at even places
   budden-tools:collect-into-tail-of-list ; Макрос как collect, требует дополнительную переменную - указатель на хвост

;; hashes
   budden-tools:ensure-gethash-2 ; gethash with default, better than alexandria
   budden-tools:make-weak-key-hash-table ; from swank
   budden-tools:make-weak-value-hash-table ; from swank
   budden-tools:mkhash ; make hash and fill it with data, used for printing hash-tables readably
   budden-tools:hash-table-difference ; like set-difference, but for hash tables

;; structures
   budden-tools:struct-to-alist ; lispworks only for now
   budden-tools:mandatory-slot  ; initarg for defstruct slots which must be initialized explicitly

;; strings and symbols
   def-merge-packages:char-upcase-ascii
   def-merge-packages:char-downcase-ascii
   def-merge-packages:string-upcase-ascii
   def-merge-packages:string-downcase-ascii
   def-merge-packages:all-ascii-chars-in-same-case-p
   def-merge-packages:unintern-all-internal-symbols-of-package


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
   budden-tools:string-designator-p

;; evaluation and general shortcuts
;  budden-tools:let1 is interned to CL as it is very necessary :) 
   #:let1 ; but we would better reexport it 
   ; budden-tools:iter ; reexported from iterate
   budden-tools:with-gensyms ; reexported from alexandria
   budden-tools:once-only ; reexported from alexandria
   budden-tools:ignored 
;   budden-tools:eval-when* ; it is defined internal in cl-user for some unknown reason
   budden-tools:print-if 
   budden-tools:proga ; obsolete
   budden-tools:perga 
   ;budden-tools:dsetq ; reexported from iterate
   budden-tools:pllet1 ; 'bind' place
   budden-tools:smlet ; another name for symbol-macrolet
   budden-tools:*defun-to-file-directory*
   budden-tools:defun-to-file ; to see source of your generated function. TODO: package name->file name, gensyms vs `,
   budden-tools:defun-to-file-2
   budden-tools:defun-to-file-macroexpanded
   budden-tools:defun-to-file-me-no-pe
   budden-tools:ggsym
   budden-tools:ПЕЧАТАЕМЫЙ-ПРЕДСТАВИТЕЛЬ-СИМВОЛА
   budden-tools:defparameter-always ; defparameter at load-toplevel,compile-toplevel,execute
   budden-tools:eval-with-file ; write code to file, compile it and load
   alexandria:named-lambda ; reexporting

   ;; types 
   budden-tools:not-null ; type (not null)
   budden-tools:nullable ; тип 'что-то или nil'
   budden-tools:lisp-bool ; t или nil 
   budden-tools:the* ; errs if type ain't match. Otherwise, returns a thing

   ;; switches and logical operators
   budden-tools:eswitch ; reexported from alexandria
   budden-tools:switch
   budden-tools:cswitch 
   budden-tools:exclusive-or 
   budden-tools:implies

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
   budden-tools:deftvar ; in compiled code, with-the1 and (:lett ) will be protected from re-binding with another type
   budden-tools:deftparameter ; like deftvar, but parameter
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

;; i/o utilities
   budden-tools:show-hash
   budden-tools:show-expr
   budden-tools:show-exprt
   budden-tools:read-file-into-string
   budden-tools:save-string-to-file  

;; system environment
   budden-tools:map-dir 
   budden-tools:path-to-a-file
   budden-tools:name-and-type-of-a-file
   budden-tools:pathname-relative-to-super-dir
   budden-tools:subdir-p ; является ли первый аргумент поддиректорией второго?

   budden-tools:maybe-add-slash ; got a directory name. Make sure it ends with slash
   budden-tools:up-dir
   budden-tools:pathname-to-filename ; from swank-backend
   ;; see also sb-ext:native-namestring (sbcl) 

   budden-tools:filename-to-pathname ; from swank-backend
   budden-tools:quit-lisp ; thanks to Thomas A.Russ

;; streams
   КАРТЫ-ИСХОДНИКОВ-ЛИЦО:fix-offset-2 ; переводит смещение в файле. полученное из file-position, в положение, рассчитанное в буквах. При этом читает весь файл, т.е. работает медленно.
   budden-tools:edit-stream-position ; trying to find out position in a file stream, open the file at the cursor and edit it
   КАРТЫ-ИСХОДНИКОВ-ЛИЦО:EXTRACT-SOURCE-FILENAME-FROM-STREAM
   КАРТЫ-ИСХОДНИКОВ-ЛИЦО:input-stream-position-in-chars
   КАРТЫ-ИСХОДНИКОВ-ЛИЦО:stream-get-line-number
    ;; КАРТЫ-ИСХОДНИКОВ-ЛИЦО:string-stream-extract-string ; заменено на КАРТЫ-ИСХОДНИКОВ-ЛИЦО:ИСХОДНИК-ЦЕЛИКОМ-В-ВИДЕ-СТРОКИ
   КАРТЫ-ИСХОДНИКОВ-ЛИЦО:ИСХОДНИК-ЦЕЛИКОМ-В-ВИДЕ-СТРОКИ

   budden-tools:edit-stream-position ; trying to find out position in a file stream, open the file at the cursor and edit it

;; make input stream available to sharpsign-dot
   budden-tools:*read-eval-stream*
   budden-tools:|Закодировать-строку-в-имя-файла|

;; see-packages and friends
   budden-tools:*keyword-package* ; just a keyword package
   budden-tools:enable-buddens-readtable-extensions ; enable symbol-readmacros, custom-token-parsers, advanced-readtable-case and local-nicknames on a (named) readtable. TODO: rename it
   budden-tools:reset-to-standard-readtable ; undo enable-buddens-readtable-extensions and reset readtable to standar syntax
   budden-tools:*per-package-alias-table* ; stolen from \"hierarchical packages\"
   budden-tools:hp-find-package ; stolen from \"hierarchical packages\"
   budden-tools:hp-alias-map   ; stolen from conduit packages
   budden-tools:*essential-binding-checkers* ; currently unused

   budden-tools:defreadtable ; stolen from named-readtables project
   budden-tools:find-readtable
   budden-tools:in-readtable
   budden-tools:package-readtable
   budden-tools:package-seen-packages-list
   budden-tools:*readtable-alist*
   budden-tools:unregister-readtable

   ; we do not export it anymore, use def-merge-packages::! budden-tools:get-custom-reader-for-package ; use your own reader in package context instead of common lisp reader, 
                                              ; e.g. you can do so that dat1:2011-01-01 would read a localtime:timestamp value
   ; we do not export it anymore, use def-merge-packages::! budden-tools:get-custom-token-parsers-for-package ; parse tokens read with custom parsers (which can, but not encouaraged to have side-effects on stream
   budden-tools:symbol-readmacro ; function of two arguments: symbol and a stream. setf symbol-readmacro to reader switched by the symbol
   budden-tools:def-symbol-readmacro ; Navigation does not work
   budden-tools:it-is-a-car-symbol-readmacro ; put this at the first line of your symbol-readmacro definition
   budden-tools:*escape-symbol-readmacros* 

   budden-tools:*print-normalize-seen-symbols*

   ; budden-tools:defpackage-autoimport ; obsolete
   ; budden-tools:export-clause ; obsolete. See DEF-MERGE-PACKAGES:reexport-clause-for-package (name might change further)
   budden-tools:find-symbol-in-packages ; consider using find-all-symbols
   budden-tools:package-doctor ; see also :smart-resolve-export-conflict debugger command for lispworks

   budden-tools:write-exports-for-defstruct
   budden-tools:|Написать-экспорт-для-структуры|

;; alternative-backquoting
   |`|
   |,|

;; locations
   budden-tools:row-col-offset ; type for storing location in a row-col coordinates
   budden-tools:make-row-col-offset 
   budden-tools:row-col-offset-row 
   budden-tools:row-col-offset-col 
   budden-tools:buffer-offset-to-row-col-offset ; find row-col-offset in an editor
   budden-tools:row-col-offset-to-buffer-offset
   BUDDEN-TOOLS:row-col-offset-b-offset
   budden-tools:def-toplevel-progn ; top-level-form which is locatable in a debugger session
   "
   ))


(def-merge-packages::! :КАРТЫ-ИСХОДНИКОВ-ТЕЛО
  (:always t)
  (:use :КАРТЫ-ИСХОДНИКОВ-ЛИЦО :cl :alexandria :iterate-keywords :budden-tools)
  (:shadowing-import-from :budden-tools budden-tools:read-file-into-string)
  (:import-from :SWANK/BACKEND SWANK/BACKEND:MAKE-WEAK-KEY-HASH-TABLE)
  )

;(merge-packages-and-reexport::! :proga-implementation 
;                                )
(def-merge-packages::! :def-trivial-test 
  (:nicknames :trivial-deftest)
  (:documentation "Very simple test suite you can optionally run at load time. Use ! for brevity")
  (:use :cl)
  (:export "def-trivial-test::*run-tests*
            def-trivial-test::*break-on-test-failure*
            def-trivial-test::|Проверить-правильное-прохождение-теста|
            ")
   )

