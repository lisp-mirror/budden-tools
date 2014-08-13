;;; -*- Encoding: utf-8; -*-
;;; perga macro 

(def-merge-packages::! :perga-implementation
 (:always t)
 (:documentation "perga is an extensible macro which helps to get rid of some unnecessary parens and nesting levels 
which are present in lisp syntax.
E.g. instead of writing 
 (let ((a 5))
   (flet ((f (x) x))
     (with-open-file (fi *file*)
       (f a))))

write just

 (perga
  (let a 5)
  (flet f (x) x)
  (with-open-file fi *file*)
  (f a))

So you have one code nesting level reduced from 4 to 1 and parens count reduced from 20 to 12. 
Currently perga is in need for redesign, so use with caution. let, flet forms will be kept, 
some other might change. Look at perga-transformer usage in perga.lisp file for examples of extending 
perga but beware this way of extending is likely to be broken in the near future.
")
 (:use :cl :iterk :budden-tools #+:LISPWORKS6.1 :lw-macro-friendly-dbg
   #+lispworks6 :lw-macro-friendly-stepper
   )
 (:export "
  perga-implementation:perga ; Macro to get rid of some unnecessary parens and nesting levels which are present in lisp syntax
  perga-implementation:def-perga-clause
  perga-implementation:open-up-if-3
  perga-implementation:open-up-if-4
  perga-implementation:wind-up-tail-if-second-is-atom
  perga-implementation:wind-up-tail-if-3
  perga-implementation:with-unwind 
  "
  )
 )



