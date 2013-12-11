(in-package :dbg17)

(defun r ()
  (with-source-location-substitutions
   (with-source-location-substitutions
    (progn
      (first-cons-1 "find-source покажет вторую форму")
      (second-cons-1 "ой, и правда хакнули отладчик")
      ))))
  
(defun test ()
  (setf *INTERESTING-FUNCTION-NAME* 'r)
  (break "скомпилируй r с помощью compile-defun и продолжи")
  (r))
