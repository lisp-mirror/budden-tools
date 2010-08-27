(in-package :buddens-readtable)
(defreadtable :buddens-readtable
  (:merge #+ccl :current)
  (:dispatch-macro-char #\# #\L #'sharpL-reader)
  (:dispatch-macro-char #\# #\. #'sbcl-sharp-dot)
  ; (:dispatch-macro-char #\# #\" #'sharp-double-quote-readmacro)
  )

