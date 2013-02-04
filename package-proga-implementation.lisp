;;; -*- Encoding: utf-8; -*-
(def-merge-packages::!
 :proga-implementation
 (:documentation "proga is an extensible macro which helps to get rid of some unnecessary parens and nesting levels 
which are present in lisp syntax.
E.g. instead of writing 
 (let ((a 5))
   (flet ((f (x) x))
     (with-open-file (fi *file*)
       (f a))))

write just

 (proga
  (let a 5)
  (flet f (x) x)
  (with-open-file fi *file*)
  (f a))

So you have one code nesting level reduced from 4 to 1 and parens count reduced from 20 to 12. 
Currently proga is in need for redesign, so use with caution. let, flet forms will be kept, 
some other might change. Look at proga-transformer usage in proga.lisp file for examples of extending 
proga but beware this way of extending is likely to be broken in the near future.
")
 (:use :cl :iterk :budden-tools)
 (:export 
  #:proga ; Macro to get rid of some unnecessary parens and nesting levels which are present in lisp syntax 
  #:proga-transformer ; assign it to a property list to expand proga. See proga.lisp for examples
  )
 )