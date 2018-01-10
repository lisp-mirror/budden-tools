;;; -*- Encoding: utf-8;  Mode: Lisp -*-

(in-package :asdf)

; (pushnew :really-see-packages *features*)
(pushnew :rdr1 *features*)

(defsystem :buddens-reader
	:serial t
        :depends-on (:cl-utilities :budden-tools :russian-budden-tools :iterate-keywords)	
	:components
        (
        (:file "buddens-reader-extensions-package")
        (:file "buddens-reader-extensions-h")
	(:file "see-packages-vars-and-macros") ; ПРАВЬМЯ - слить с предыдущим, вынести в отдельный пакет
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
        (:file "budden-tools-package-system-and-completion-swank" :description "Декораторы для swank и для find-package")
        (:file "package-aliases")
        (:file "lexem-pos" :description "Структуры для описания положения буквы и лексемы")
	      (:file "locations")
        (:file "cons-to-source" :description "Alternative to ~{ ~} format to product source code from conses")
        (:file "test-sbcl-reader-budden-tools-lispworks")
        (:file "here-document" :description "Alternative string syntax - like here documents in shell")
	)
	)

(defmethod perform :after 
  ((op load-op) (c (eql (find-system :buddens-reader))))
  (setf *features* (delete :building-see-packages *features*)) 
  (provide :buddens-reader))

(defmethod perform :after 
  ((op compile-op) (c (eql (find-system :buddens-reader))))
  (setf *features* (delete :building-see-packages *features*))
  (pushnew :buddens-reader *features*)
  (provide :buddens-reader))


