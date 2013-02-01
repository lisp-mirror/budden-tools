;;; -*- Encoding: utf-8; -*-
(in-package :budden-tools)


(defun canonicalise-package-name (package/name)
  ;; Stolen from Tim Bradshaw's conduit package
  (etypecase package/name
    (package (values (intern (package-name package/name) 
			     (find-package :keyword))
		     package/name))
    ((or string symbol)
     (let ((found (find-package package/name)))
       (values (intern (if found
			   (package-name found)
			   (etypecase package/name
			     (string package/name)
			     (symbol (symbol-name package/name))))
		       (find-package :keyword))
	       found)))))

#|(defmacro defpackage-more (package-name &key readtable package-aliases)
  "Assigns a named readtable to a package. Also, for each (package-name alias) in 
package-aliases make alias to be a local alias for (absolute) package-name in scope
of dst package"
  (iter (:for item in package-aliases)
        (assert (typep (list string-designator
  (with-gensyms (package-name alias)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let* ((cn (canonicalise-package-name ',name)))
         ,@(when package-aliases
                 `((setf (hp-alias-map (find-package ',name))
                         ',(nreverse package-aliases))))
         (recompute-conduits-for ',name))
    
|#