(defpackage :date1)
(defpackage :date1-implementation (:use :cl :budden-tools :local-time))

(in-package :date1-implementation)

(defreadtable :date1-readtable 
  (:merge :standard)
  )

(see-packages-on :date1-readtable)
(set-syntax-from-char #\: #\a 
                      (find-readtable :date1-readtable)
                      (find-readtable :date1-readtable))

(setf (package-readtable :date1) :date1-readtable)

(defun date1-reader (&optional stream eof-error-p eof-value recursive-p)  
  (iter 
    (:for c = (read-char stream nil t recursive-p))
    (cond
     ((budden-tools::whitespace[2]p c)
      (unread-char c stream)
      (return (parse-rfc3339-timestring (concatenate 'string res))))
     ((eq c t) ; no eof character. So we lose speed here. How to do better? 
      (cond
       (res 
        (return (parse-rfc3339-timestring (concatenate 'string res))))
       (t
        (if
            eof-error-p 
            (read stream t) ; cause end-of-file error
          (return eof-value)) 
        )))
     (t 
      (:collect c into res))
     )
    ))


;  (let1 *readtable* (find-readtable nil)
;    (break)))

;  (declare (ignore stream package))
;  (parse-rfc3339-timestring (string string))
;  )

;(setf (budden-tools::get-custom-reader-for-package :date1) `(,#'date1-reader))
(setf (budden-tools::get-custom-reader-for-package :date1) #'date1-reader)

