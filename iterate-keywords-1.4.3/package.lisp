(defpackage :iterate-keywords 
  (:use #:cl)
  (:documentation "Iterate-keywords. Use (:for ) instead of (for ) and so on for all clauses so you can avoid many clashes") 
  (:nicknames 
   #:iterk
   )
  (:export
   #:iter
   #:keywordize
   #:dsetq)
  #+sbcl (:lock t)	      
  )


(in-package #:iterate-keywords)

;;; work around sbcl's obnoxious standard compliance

(defmacro defconst (name value &optional doc)
   `(eval-when (:compile-toplevel :load-toplevel :execute)
      (unless (boundp ',name)
        ,(if doc
             `(defconstant ,name ,value ,doc)
           `(defconstant ,name ,value)))))





