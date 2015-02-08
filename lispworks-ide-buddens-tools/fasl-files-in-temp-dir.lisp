(in-package :cl-user)

(defvar *hide-fasl-files* nil)

(defadvice (editor::compile-file-internal note-we-are-in-editor :around) (&rest args)
  (let ((*hide-fasl-files* t))
    (apply #'call-next-advice args)))

(defadvice (compile-system treat-fasls-normally :around) (&rest args)
  (let ((*hide-fasl-files* nil))
    (apply #'call-next-advice args)))

#+asdf 
(defadvice (ASDF:operate treat-fasls-normally :around) (&rest args)
  (let ((*hide-fasl-files* nil))
    (apply #'call-next-advice args)))

(defadvice (compile-file dont-save-fasl :around) (input-file &rest keyargs)
  (if (or (not *hide-fasl-files*) (getf keyargs :in-memory))
    (apply #'call-next-advice input-file keyargs)
    (apply #'call-next-advice input-file :output-file :temp
           (loop for (a b) on keyargs by #'cddr
                 nconc (unless (eq a :output-file) (list a b))))))

