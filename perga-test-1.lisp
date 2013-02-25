;;; -*- Encoding: utf-8; -*-
;;; perga-test-1 - port of proga-test
(in-package :perga-implementation)

(defmacro def-p-t (name x y)
  `(def-trivial-test::! ,name (macroexpand ,x) (macroexpand ,y)))



(def-p-t perga.1 '(perga 2) 2)

(def-p-t perga.2 '(perga foo nil) '(BLOCK FOO NIL))

(def-p-t perga.3 '(perga (let a b) c) '(LET ((A B)) C))

(def-p-t perga.4 '(perga b (:labels f (a) (+ a 1)) c)  ; diff to proga
         '(BLOCK b (LABELS ((F (A) (+ A 1))) C)))

(def-p-t perga.4a '(perga b (:labels (f (a) (+ a 1))) c)  ; diff to proga
         '(BLOCK b (LABELS ((F (A) (+ A 1))) C)))

(def-p-t perga.5 '(perga (flet f (a) (+ a 1)) c)
         '(FLET ((F (A) (+ A 1))) C))

(def-p-t perga.6 '(perga (let a 1 b 2) (flet f () (list a b)) (f))
         '(LET ((A 1) (B 2)) (FLET ((F () (LIST A B))) (F))))

(def-trivial-test::! perga.7 (macroexpand-1 
                  '(perga (destructuring-bind ((a &optional (b 'bee)) one two three) `((alpha) ,@(iota 3)))
                     (list a b three two one)))
         '(destructuring-bind ((a &optional (b 'bee)) one two three) `((alpha) ,@(iota 3))
            (list a b three two one)))

(def-trivial-test::! perga.8
                     (perga
                       (let a 1)
                       (flet foo () (+ a 1))
                       (macrolet bar () '(foo))
                       (bar))
                     '2)

;(def-trivial-test::! perga.9 так больше не делаем
;         (macroexpand-1 
;          '(perga (block foo) bar))
;         '(block foo bar))

(def-trivial-test::! perga.9a
                     (macroexpand-1
                      '(perga (block foo bar) baz))
                     '(progn (block foo bar) baz))

(def-trivial-test::! perga.10.dont-process
         (macroexpand-1
          '(perga (let (a b)) c))
         '(progn (let (a b)) c))

(def-trivial-test::! perga.11 ; не можем сделать, т.к. нужно смотреть вперёд
         (macroexpand-1
          '(perga (let ((a b)) c) d))
         '(progn (let ((a b)) c) d))


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
         (perga
           (flet the-best-dotted-list-p-1 (l)
             (perga 
              (let res nil)
              (:labels f (sub) ; diff to proga
                (cond
                 ((consp sub)
                  (let1 tail (f (cdr sub))
                    (cons (car sub) tail)))
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

(def-trivial-test::! perga.13
         (macroexpand-1 '(perga
                           (:& with-open-file foo "bar") ; diff to proga
                           (print "bar" foo)))
         `(with-open-file (foo "bar") (print "bar" foo)))

(def-trivial-test::! perga.13.a
         (macroexpand-1 '(perga
                           (with-open-file (foo "bar")
                             (print "bar" foo))))
         `(with-open-file (foo "bar") (print "bar" foo)))

(def-trivial-test::! perga.14.process-flet-body
         (perga (flet f (x) (let y x) y) (f 4))
         4)

(def-trivial-test::! perga.14.1.process-flet-body
         (perga (flet f (x) "Doc" (let y x) y) (f 4))
         4)


(def-trivial-test::! perga.15.empty-tail
                     (let1 x 1 
                       (perga (let y (incf x)) (setf y y)) 
                       x)
                     2
                     )

(def-trivial-test::! perga.16.when
         (perga (when t (let a 5) a)) 
         5)

(def-p-t perga.17.cond
         '(perga (cond (q (let a 6)) (t (let b 7) b)))
         '(cond (q (let ((a 6))))
                (t (let ((b 7)) b))))

; не работает, т.к. расширение содержит gensym-ы
;(def-p-t perga.18.case
;         '(perga (case 4 (1 (let a 6)) (t (let b 7) b)))
;         '(case 4 (1 (let ((a 6))))
;            (t (let ((b 7)) b))))



