;;; -*- Encoding: utf-8; -*-
#|
Отличия perga от proga.
Цель нововведений:
- поддержка степпера
- сделать синтаксис таким, чтобы нетривиальные случаи визуально отделялись от тривиальных
- сделать возможным применение сокращённых имён без использования :budden-tools

Конкретно:
1. Вместо 
 (setf (get 'sym 'proga-transformer) 'func)
делаем
 (def-perga-clause sym 'func)
2. let1, pllet1, with-conc-name, let-with-conc-type, with-open-file,
  with-input-from-string, with-proplist-carat не имеют clause.
  Вместо этого пользоваться :@, :&
3. вместо labels - :lables, т.к. может быть несколько выражений. 
4. вместо with-the1  - :lett
5. block не обрабатывается


|#



(in-package :perga-implementation)

#+(and lispworks budden) (editor::bind-key "Toggle Breakpoint" "f4")

(defun f1 ()
  (perga
    (let a (+ 2 2))
    (* a 2)))

(def-trivial-test::! perga.f1 (f1) 8)


(defun f2 ()
  (perga
    (let a (- 1))
    (let b a)
    (- b)))


(defun f3 ()
  (perga block
    (let a (+ 2 2))
    (let ((a 7))
      (list a))
    (let b a)
    ;(compiler-break)
    a
    (list (+ b a))
    (let c b)
    (+ c 5)
    ))

(defun i1 ()
  (iter (:for i from 1 to 10) (:collect (list i))))

(def-trivial-test::! perga.f3 (f3) 9)

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

#|(defun fu4 ()
  (symbol-macrolet ((c a))
    (let ((a 5))
      (let ((b (/ 4 c (read-from-string "0"))))
        b))))|#



(defun f5 () 
  (perga name
    (flet f (x) (let y x) (print "a") (/ 3 y x) (print "b"))
    (:& with-output-to-string *standard-output*)
    (return-from name (f (read-from-string "1")))))

(def-trivial-test::! perga.f5
                     (f5)
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
     
(defun perga-12-fun ()
  (perga
    (flet the-best-dotted-list-p-1 (l)
      (perga 
        (let res nil)
        (:labels f (sub) ;changed vs proga
          (list "a")
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

(def-trivial-test::! perga.12
         (perga
           (flet the-best-dotted-list-p (l)
             (let ((res nil))
               (labels ((f (sub)
                          (cond
                           ((consp sub)
                            (let ((tail (f (cdr sub))))
                              (cons (car sub) tail)))
                           ((null sub)
                            (return-from the-best-dotted-list-p (values nil l)))
                           (t 
                            (setf res sub)
                            nil))))
                 (let ((res2 (f l)))
                   (values res res2)))))
           (mapcar #'the-best-dotted-list-p
                   '(nil '(nil) '(1 2) '(1 . 2) '(1 2 3) '(1 2 . 3))))
         (perga-12-fun))



(defun perga-add-character-test ()
  (perga
    (:@ with-input-from-string (var "0"))
    (let a (read-from-string var))
    a))


(defun where-let-steps ()
  (let ((x 1))
    x))