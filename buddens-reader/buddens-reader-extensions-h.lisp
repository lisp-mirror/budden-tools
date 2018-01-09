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

(in-package :buddens-reader-extensions)

;brt
(defstruct potential-symbol package casified-name (qualified 0 :type (integer 0 2)))

;brt
(defvar *return-package-and-symbol-name-from-read* nil
  "Side branch of read. If this var is t, potential-symbol structures are returned instead of new symbols. Nothing is interned. Existing symbols may be returned as potential symbols are just as symbols")
