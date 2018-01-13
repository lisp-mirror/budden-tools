;;; -*- coding: utf-8; system :buddens-reader ;  -*-

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

(def-merge-packages::! :buddens-reader-extensions
 (:nicknames
  :sbcl-reader-budden-tools-lispworks
  :sbcl-reader-budden-tools
  :sbcl-reader-budden-tools-sbcl
  :sbcl-reader-budden-tools)
 (:always t)
 (:use :cl 
  :budden-tools
  #+SBCL :sb-impl
  )
 (:shadow
   #:test-attribute
   )
 #+SBCL
 (:import-from :sb-impl
   #:read-token ; это не обычная ф-я чтения, она требует буферов
   #:sharp-colon
   sb-impl::constituentp
   sb-impl::token-delimiterp
   sb-impl::whitespace[2]p
   )
 (:export 
   #:*return-package-and-symbol-name-from-read*
   #:*escape-symbol-readmacros*
   #:read-token
   #:potential-symbol
   #:make-potential-symbol
   #:potential-symbol-casified-name
   #:potential-symbol-package
   #:potential-symbol-qualified
   #:potential-symbol-p
   #:sharp-colon
   #:constituentp 
   #:token-delimiterp
   #:whitespace[2]p
   ; #:read-preserving-whitespace-2
 )) 

