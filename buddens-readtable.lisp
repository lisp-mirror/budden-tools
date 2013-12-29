;;; -*- Encoding: utf-8; -*-
(in-package :buddens-readtable)

;(portably-without-package-locks
;(defun :buddens-readtable-a () "Dummy function to be able to navigate to readtable definition with M-." (error "this is not a function"))
;)

(defreadtable :buddens-readtable ; deprecated, remove it!
  (:merge :standard)
  (:dispatch-macro-char #\# #\L #'sharpl-reader)
  (:dispatch-macro-char #\# #\. #'sbcl-sharp-dot)
  ; (:dispatch-macro-char #\# #\" #'sharp-double-quote-readmacro)
  )


(defreadtable :buddens-readtable-a 
  (:merge :standard)
  (:dispatch-macro-char #\# #\L #'sharpl-reader)
  (:dispatch-macro-char #\# #\. #'sbcl-sharp-dot)
  ; (:dispatch-macro-char #\# #\" #'sharp-double-quote-readmacro)
  )
  

