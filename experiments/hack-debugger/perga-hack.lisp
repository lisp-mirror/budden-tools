(in-package :cl-user)

(defun f1 ()
  (perga-implementation::PERGA
    (:lett i integer (read-from-string "1"))
    (perga-implementation::PERGA
      (:lett k integer (read-from-string "1"))
      (:lett j integer (read-from-string "a"))
      (list k i j)
      )
    ))
    

 



