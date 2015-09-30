;;; -*- Encoding: utf-8; system :buddens-readtable ; -*-
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
(budden-tools::ENABLE-BUDDENS-READTABLE-EXTENSIONS :buddens-readtable)
(setf (budden-tools::readtable-case-advanced :buddens-readtable) :upcase-if-uniform)

; (defun :buddens-readtable-a () "Anchor to find :buddens-readtable-a")

(defun redefine-buddens-readtable-a ()
  (when (find-readtable :buddens-readtable-a)
    (BUDDEN-TOOLS::reset-to-standard-readtable :buddens-readtable-a))
  (defreadtable :buddens-readtable-a 
    (:merge :standard)
    (:dispatch-macro-char #\# #\L #'sharpl-reader)
    (:dispatch-macro-char #\# #\. #'sbcl-sharp-dot)
  ; (:dispatch-macro-char #\# #\" #'sharp-double-quote-readmacro)
    ) 
  (budden-tools::ENABLE-BUDDENS-READTABLE-EXTENSIONS :buddens-readtable-a)
  (setf (budden-tools::readtable-case-advanced :buddens-readtable-a) :upcase-if-uniform)
  )

(redefine-buddens-readtable-a)
