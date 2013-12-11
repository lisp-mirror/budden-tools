;;; -*- Encoding: utf-8; -*-
;;; Здесь определены функции, нужно проверять способность 
;;; поставить в них брекпойнты


(def-merge-packages::! :perga-test-2
                       (:always t)
                       (:use :cl :perga-implementation))
  
(in-package :perga-test-2)

#+(and lispworks budden) (editor::bind-key "Toggle Breakpoint" "f4")

(defun f1 ()
  (perga
    (let a (+ 2 2))
    (* a 2)))

(defun f2 ()
  (perga
    (let a (- 1))
    (let b a)
    (- b)))


(defun f3 ()
  (perga block
    (let a (+ 2 2))
    (let ((a 7))
      (print a))
    (let b a)
    ;(compiler-break)
    a
    (print (+ b a))
    (let c b)
    (+ c 5)
    ))

(defun f4 ()
  (perga (symbol-macrolet c a)
    (let a 5)
    (let b (/ 4 c (read-from-string "0")))
    b))


(defun f7 ()
  (perga 
    (:labels
        (f (x)
           (let y x)
           (- y))
      (g (x) (f x)))
    (g 1))
  )

(def-trivial-test::! perga.f7
                  (f7) -1)

(defun f7e ()
  (labels (
           (f (x)
             (let ((y x)) (- y)))
           (g (x) (f x)))
    (g 1)))


(defun f5 () 
  (perga name
    (flet f (x) (let y x) (print "a") (/ 3 y x) (print "b"))
    (f (read-from-string "1"))))

(def-trivial-test::! perga.f5
                     (perga all
                       (:& with-output-to-string *standard-output*)
                       (return-from all (f5)))
                     "b")


(defun f5e ()
  (progn (flet ((f (x) (+ x 5))) (f 4))))


(defun f6 ()
  (perga (let i 1) (tagbody (go a)(incf i) a) (let j i) j))

(def-trivial-test::! perga.f6
                     (f6)
                     1)

(def-trivial-test::! perga.7
                     (macroexpand-1 
                      '(perga (destructuring-bind ((a &optional (b 'bee)) one two three) `((alpha) ,@(iota 3)))
                         (list a b three two one)))
                     '(destructuring-bind ((a &optional (b 'bee)) one two three) `((alpha) ,@(iota 3))
                        (list a b three two one)))

(defun f7b ()
  (perga
    (destructuring-bind ((a &optional (b 'bee)) one two three) '((alpha) 1 2 3))
    (list a b three two one)))


(defun perga-16-when-fn () (perga (when t (let a 5) a)))
(def-trivial-test::! perga-16-when
         (perga-16-when-fn) 
         5)


(defun case-break ()
  (perga
    (typecase (read-from-string "a")
      (list t)
      (symbol
       (let a 4)
       a))))

(defun simple-cond-break ()
  (cond
   ((= 1 (read-from-string "1")) 1)
   (t 0)))
    
(defun perga-12-fun ()
  (perga
    (flet the-best-dotted-list-p-1 (l)
      (perga 
        (let res nil)
        (:labels f (sub) ;changed vs proga
          (print "a")
          (cond
           ((consp sub)
            (let tail (f (cdr sub)));changed vs proga
            (cons (car sub) tail))
           ((null sub)
            (return-from the-best-dotted-list-p-1 (values nil l)))
           (t 
            (setf res sub)
            nil)))
        (let res2 (f l))
        (values res res2)))
    (mapcar #'the-best-dotted-list-p-1
            '(nil '(nil) '(1 2) '(1 . 2) '(1 2 3) '(1 2 . 3)))
    ))


(defun perga-add-character-test ()
  (perga
    (:@ with-input-from-string (var "0"))
    (let a (read-from-string var))
    a))


(defmacro finally-do (cleanup &body body)
  `(unwind-protect
       (perga ,@body)
     ,cleanup))

(def-trivial-test::! perga.finally-do
                     (macroexpand 
                      '(perga all
                         (:lett x fixnum 1)
                         (:@ finally-do (return-from all x))
                         (:lett y fixnum x)
                         (setf x (+ y 1))
                         ))
                     '(block all
                        (budden-tools:with-the1 x fixnum 1
                          (finally-do (return-from all x)
                            (budden-tools:with-the1 y fixnum x (setf x (+ y 1)))))))
