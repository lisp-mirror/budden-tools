;; -*- Encoding: utf-8 ; -*- ;; 

(in-package :asdf)

#+lispworks (defsystem :multipaste
  
  :components ((:file "multipaste"))
  )

#-lispworks (error "multipaste.asd is lispworks-specific")
