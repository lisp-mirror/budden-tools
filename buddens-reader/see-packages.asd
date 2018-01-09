;;; -*- Encoding: utf-8;  Mode: Lisp -*-

(in-package :asdf)

; (pushnew :really-see-packages *features*)
(pushnew :rdr1 *features*)

(defsystem :see-packages
	:serial t
        :depends-on (:cl-utilities :budden-tools :russian-budden-tools :swank :cl-advice :iterate-keywords)	
	:components
        (
        (:file "sbcl-reader-budden-tools-package")
        (:file "buddens-reader-extensions-h")
	(:file "see-packages-vars-and-macros") ; ПРАВЬМЯ - слить с предыдущим, вынести в отдельный пакет
        #+ccl (:file "sbcl-in-ccl")
	(:file "study-readtable")
	(:file "careful-token-reader")
        #+lispworks (:file "sbcl-reader-budden-tools-lispworks-consts")
        #+lispworks (:file "sbcl-reader-budden-tools-lispworks")
        #+sbcl (:file "sbcl-reader-patch")
        #+ccl (:file "ccl--l1-reader--budden-tools")
	(:file "redirect-reader")
	(:file "see-packages-applications")
        (:file "package-system-and-completion" :description "Изменения в системе пакетов, а также функции для завершения имени символа (completion)")
        #+(and lispworks6 win32)
        (:file "lispworks60-def-symbol-readmacro")
        (:file "oduvanchik-regexp-synonyms") 
        #+sbcl
        (:file "budden-tools-package-system-and-completion-swank")
        (:file "package-aliases")
        (:file "lexem-pos" :description "Структуры для описания положения буквы и лексемы")
	      (:file "locations")
        (:file "cons-to-source" :description "Alternative to ~{ ~} format to product source code from conses")
        (:file "test-sbcl-reader-budden-tools-lispworks")
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


