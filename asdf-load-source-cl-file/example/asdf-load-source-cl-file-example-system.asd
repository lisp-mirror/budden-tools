;;;; -*- Mode: lisp; indent-tabs-mode: nil; coding: utf-8; -*-

(asdf:defsystem :asdf-load-source-cl-file-example-system
  :defsystem-depends-on (:asdf-load-source-cl-file)
  :serial t
  :components
  ((:load-source-cl-file "file-to-load")
   )) 
