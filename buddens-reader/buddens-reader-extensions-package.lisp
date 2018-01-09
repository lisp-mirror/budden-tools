;;; -*- coding: utf-8; system :see-packages ;  -*-

;;;; Ридер из SBCL-1.3.4, подразумевается, что будет запускаться только из SBCL

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.


(named-readtables:in-readtable nil)


#+LISPWORKS
(def-merge-packages::! :buddens-reader-extensions
 (:nicknames
  :sbcl-reader-budden-tools-lispworks
  :sbcl-reader-budden-tools)
 (:always t)
 (:use :cl 
  :budden-tools 
  )
 (:export "
   sbcl-reader-budden-tools:*return-package-and-symbol-name-from-read*
   sbcl-reader-budden-tools:read-token
   sbcl-reader-budden-tools:potential-symbol
   sbcl-reader-budden-tools:make-potential-symbol
   sbcl-reader-budden-tools:potential-symbol-casified-name
   sbcl-reader-budden-tools:potential-symbol-package
   sbcl-reader-budden-tools:potential-symbol-qualified
   sbcl-reader-budden-tools:potential-symbol-p
   sbcl-reader-budden-tools:constituentp
 "
 ))

#+SBCL
(def-merge-packages::! :buddens-reader-extensions
 (:nicknames
  :sbcl-reader-budden-tools-lispworks
  :sbcl-reader-budden-tools
  :sbcl-reader-budden-tools-sbcl
  :sbcl-reader-budden-tools)
 (:always t)
 (:use :cl 
  :budden-tools
  :sb-impl
  )
 (:shadow
   ; #:read-token
   #:constituentp
   )
 (:import-from :sb-impl
   #:read-token ; это не обычная ф-я чтения, она требует буферов
   #:sharp-colon
   )
 (:export 
   #:*return-package-and-symbol-name-from-read*
   #:read-token
   #:potential-symbol
   #:make-potential-symbol
   #:potential-symbol-casified-name
   #:potential-symbol-package
   #:potential-symbol-qualified
   #:potential-symbol-p
   #:sharp-colon
   #:constituentp ; отключено для SBCL
   ; #:read-preserving-whitespace-2
 )) 

