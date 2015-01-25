;;; -*- Encoding: utf-8;  Mode: Lisp -*-

(in-package :asdf)

; (pushnew :really-see-packages *features*)
(pushnew :rdr1 *features*)

(defsystem :see-packages
	:serial t
        :depends-on (:cl-utilities :budden-tools :swank :decorate-function :iterate-keywords)	
	:components
        (
	(:file "see-packages-vars-and-macros")
	(:file "study-readtable")
	(:file "careful-token-reader")
        (:file "sbcl-reader-budden-tools-lispworks-consts")
        (:file "sbcl-reader-budden-tools-lispworks")
	(:file "redirect-reader")
	(:file "see-packages-applications")
        #+(and lispworks6 win32)
        (:file "lispworks60-def-symbol-readmacro")
        (:file "package-aliases")	
	(:file "locations")
        (:file "cons-to-source" :description "Alternative to ~{ ~} format to product source code from conses")
	;2012-12-19 (:russian-file "see-packages-test")
        #+(and lispworks6 win32) (:file "test-sbcl-reader-budden-tools-lispworks")
        (:file "here-document" :description "Alternative string syntax - like here documents in shell")
	)
	)

(defmethod perform :after 
  ((op load-op) (c (eql (find-system :see-packages))))
  (setf *features* (delete :building-see-packages *features*)) 
  (provide :see-packages))

(defmethod perform :after 
  ((op compile-op) (c (eql (find-system :see-packages))))
  (setf *features* (delete :building-see-packages *features*))
  (pushnew :see-packages *features*)
  (provide :see-packages))


