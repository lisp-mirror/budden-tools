;; -*- Mode: Lisp -*-

(in-package :asdf)

#.(defclass russian-file
  (#-(and sbcl russian) cl-source-file 
   #+(and sbcl russian) windows-1251-file)
  ())

; (pushnew :really-see-packages *features*)
(pushnew :rdr1 *features*)

(defsystem :see-packages
	:serial t
        :depends-on (:cl-utilities :budden-tools :swank :decorate-function)	
	:components
        (
	(:russian-file "see-packages-vars-and-macros")
	(:russian-file "study-readtable")
	(:russian-file "careful-token-reader")
	(:russian-file "redirect-reader")
	(:russian-file "see-packages-applications")
	#+(and lispworks4 win32) 
        (:russian-file "see-packages-lw-edit")
        #+(and lispworks6.0 win32)
        (:russian-file "lispworks60-def-symbol-readmacro")
        (:file "package-aliases")	
	(:russian-file "locations")
	(:russian-file "see-packages-test")
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


