;;; -*- Encoding: utf-8; -*-
(in-package :buddens-readtable)
(defreadtable :buddens-readtable
  (:merge)
  (:dispatch-macro-char #\# #\L #'sharpL-reader)
  (:dispatch-macro-char #\# #\. #'sbcl-sharp-dot)
  (:dispatch-macro-char #\# #\" #'sharp-double-quote-readmacro)
  )


