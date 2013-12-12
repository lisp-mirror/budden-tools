(in-package :perga-implementation)

(setf DBG17:*INTERESTING-FUNCTION-NAME* 'fh1)

(defun fh1 ()
  ;(DBG17::BEGIN-SOURCE-LOCATION-SUBSTITUTIONS)
  (PERGAO
    (:lett i integer (read-from-string "1"))
    (:lett j integer (read-from-string "a"))
    (cons i j)
    )
  ;(DBG17::END-SOURCE-LOCATION-SUBSTITUTIONS)
  )
    

#|(DEFUN FH1 ()
  (LET ((J
         (LET ((J16912 1))
           (TAGBODY
            G16913 (IF (NULL (TYPEP J16912 'INTEGER))
                       (PROGN
                         (CONDITIONS::SIMPLE-ASSERTION-FAILURE '(TYPEP J16912 'INTEGER) 'NIL)
                         (GO G16913))
                     NIL))
           (THE INTEGER J16912))))
    (DECLARE (TYPE INTEGER J))
    (LET ((I
           (LET ((I16914 "0"))
             (TAGBODY
              G16915 (IF (NULL (TYPEP I16914 'INTEGER))
                         (PROGN
                           (CONDITIONS::SIMPLE-ASSERTION-FAILURE '(TYPEP I16914 'INTEGER) 'NIL)
                           (GO G16915))
                       NIL))
             (THE INTEGER I16914))))
      (DECLARE (TYPE INTEGER I))
      (CONS I J))))|# 



