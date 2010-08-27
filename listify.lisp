; интегрировать
(defmacro let1 (a b &body body) `(let ((,a ,b)) ,@body))
(defmacro sm-let1 (a b &body body) `(symbol-macrolet ((,a ,b)) ,@body))

(defvar *listify-visited-objects* nil)

(defgeneric listify (o) 
  (:documentation "Convert everyting to a list structure taking care not to loop. Write a method for your own types. You may (should) use def-listify-method and def-clos-listify-method"))

(defmethod listify :around ((o t)) 
  "Convert every CLOS object to list"
  (let1 *listify-visited-objects*
      (or *listify-visited-objects* (make-hash-table))
    (call-next-method)))

(defmacro def-listify-method ((o type-name) &body body)
  (alexandria:with-gensyms (place)
  `(defmethod listify ((,o ,type-name))
     (sm-let1 ,place (gethash ,o *listify-visited-objects*)
       (flet ((note-visited (alistified-o)
                "Be sure to call note-visited prior to descending to children, otherwise you would loop"
                (setf ,place alistified-o)))
         (or ,place (progn ,@body)))))))


(def-listify-method (o (eql nil)) o)
(def-listify-method (o t) (note-visited o))

(def-listify-method (o cons)
  (cond
    ((and (consp (car o)) 
          (eql (caar o) :alistified))
     (note-visited o))
    (t (let1 res (note-visited (cons nil nil))
         (setf (car res) (listify (car o))
               (cdr res) (listify (cdr o)))
         res))))

(defmacro def-clos-listify-method (class-name slot-names)
  (alexandria:with-gensyms (o)
    `(def-listify-method 
         (,o ,class-name) 
       (let1 res (list (list :alistified ',class-name))
         (note-visited res)
         (setf (cdr res)
               (list
                ,@(loop 
                     :for nm in slot-names
                     :collect `(cons ',nm (if (slot-boundp ,o ',nm)
                                              (listify (slot-value ,o ',nm))
                                              :unbound))
                     )))
         res))))