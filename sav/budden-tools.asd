(in-package #:asdf)

#.(defparameter russian-file
  #-(and sbcl russian) :file 
  #+(and sbcl russian) :windows-1251-file)

; #.(defparameter russian-file #+(and lispworks win32) :file #-(and lispworks win32) :windows-1251-file)

(defsystem budden-tools
  :serial t
  :depends-on 
  (:alexandria :split-sequence :cl-utilities :named-readtables
   :buddens-readtable		
   #+budden :iterate-keywords #-budden :iterate)
  :components
  ((:file "let1")
   (:package-file "package") ; might need 
   (#.russian-file "trivial-deftest")
   (#.russian-file "budden-tools")
   (#.russian-file "proga")
   (:file "iterate-extensions")
   )) 
