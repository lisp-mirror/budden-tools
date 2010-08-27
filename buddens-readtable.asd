(defsystem :buddens-readtable
        :serial t
        :depends-on 
        (:named-readtables)
	:components 
        (
         (:file "buddens-readtable-package")
         (:file "buddens-readmacros")
         (:file "buddens-readtable")
;	 (:file "buddens-sharpdot")
))	
; эгегей!!!!