(in-package :buddens-readtable)

(cl-user::portably-without-package-locks
(defun :buddens-readtable-a () "это не функция, а readtable" (error "this is not a function"))
)

(defreadtable :buddens-readtable 
  (:merge #+ccl :current)
  (:dispatch-macro-char #\# #\L #'sharpl-reader)
  (:dispatch-macro-char #\# #\. #'sbcl-sharp-dot)
  ; (:dispatch-macro-char #\# #\" #'sharp-double-quote-readmacro)
  )


(defreadtable :buddens-readtable-a
  (:merge #+ccl :current)
  (:dispatch-macro-char #\# #\L #'sharpl-reader)
  (:dispatch-macro-char #\# #\. #'sbcl-sharp-dot)
  ; (:dispatch-macro-char #\# #\" #'sharp-double-quote-readmacro)
  )
  

