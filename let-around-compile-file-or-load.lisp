;;; -*- Encoding: utf-8; -*-

(named-readtables:in-readtable nil)

;;; Любая переменная, упомянутая в соответствующей глобальной, будет пересвязана к самой себе или к значению БЫЛА-НЕ-СВЯЗАНА

(def-merge-packages::! :СВЯЗАТЬ-СПЕЦИАЛЬНЫЕ-ПЕРЕМЕННЫЕ-ВОКРУГ-LOAD-ИЛИ-COMPILE-FILE
                       (:always t)
                       (:use :cl :budden-tools)
                       (:shadow #:БЫЛА-НЕ-СВЯЗАНА #:*СВЯЗАТЬ-ВОКРУГ-COMPILE-FILE* #:*СВЯЗАТЬ-ВОКРУГ-LOAD*)
                       (:export
                        "
   СВЯЗАТЬ-СПЕЦИАЛЬНЫЕ-ПЕРЕМЕННЫЕ-ВОКРУГ-LOAD-ИЛИ-COMPILE-FILE:*СВЯЗАТЬ-ВОКРУГ-COMPILE-FILE*
   СВЯЗАТЬ-СПЕЦИАЛЬНЫЕ-ПЕРЕМЕННЫЕ-ВОКРУГ-LOAD-ИЛИ-COMPILE-FILE:*СВЯЗАТЬ-ВОКРУГ-LOAD*
   СВЯЗАТЬ-СПЕЦИАЛЬНЫЕ-ПЕРЕМЕННЫЕ-ВОКРУГ-LOAD-ИЛИ-COMPILE-FILE:БЫЛА-НЕ-СВЯЗАНА
                         "
                        ))

(in-package :СВЯЗАТЬ-СПЕЦИАЛЬНЫЕ-ПЕРЕМЕННЫЕ-ВОКРУГ-LOAD-ИЛИ-COMPILE-FILE)

(defparameter *СВЯЗАТЬ-ВОКРУГ-COMPILE-FILE* nil "Список переменных, к-рые будут связаны своими значениями вокруг compile-file")

(defun создать-фун-пересвязывающую-и-вызывающую-фун1 (фун1 список-имён)
  "фун1 должно быть объектом-функцией"
  (assert (functionp фун1))
  (flet ((сделать-привязку (ч) `(,ч (if (boundp ',ч) ,ч 'БЫЛА-НЕ-СВЯЗАНА))))
    (let ((список-привязок (mapcar #'сделать-привязку список-имён)))
      (with-gensyms (args)
        (compile
         nil
         `(lambda (&rest ,args)
            (let ,список-привязок
              (apply ,фун1 ,args))))))))

(defun decorated-compile-file-let-around-compile-file-or-load (fn &rest args)
  (let ((фун (создать-фун-пересвязывающую-и-вызывающую-фун1 fn *СВЯЗАТЬ-ВОКРУГ-COMPILE-FILE*)))
    (apply фун args)))

(sb-ext::without-package-locks
 (decorate-function:decorate-function 'compile-file #'decorated-compile-file-let-around-compile-file-or-load)
 )


