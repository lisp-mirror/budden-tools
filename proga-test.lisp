﻿;;; -*- Encoding: utf-8; -*-
(in-package :proga-implementation)

(defmacro def-p-t (name x y)
  `(def-trivial-test::! ,name (macroexpand ,x) (macroexpand ,y)))



(def-p-t proga.1 '(proga 2) 2)

(def-p-t proga.2 '(proga foo nil) '(BLOCK FOO NIL))

(def-p-t proga.3 '(proga (let a b) c) '(LET ((A B)) C))

(def-p-t proga.4 '(proga b (labels f (a) (+ a 1)) c)  
         '(BLOCK b (LABELS ((F (A) (+ A 1))) C)))

(def-p-t proga.5 '(proga (flet f (a) (+ a 1)) c)
         '(FLET ((F (A) (+ A 1))) C))

(def-p-t proga.6 '(proga (let a 1 b 2) (flet f () (list a b)) (f))
         '(LET ((A 1) (B 2)) (FLET ((F () (LIST A B))) (F))))

(def-trivial-test::! proga.7 (macroexpand-1 
                  '(proga (destructuring-bind ((a &optional (b 'bee)) one two three) `((alpha) ,@(iota 3)))
                     (list a b three two one)))
         '(destructuring-bind ((a &optional (b 'bee)) one two three) `((alpha) ,@(iota 3))
            (list a b three two one)))

(def-trivial-test::! proga.8
                     (proga
                       (let a 1)
                       (flet foo () (+ a 1))
                       (macrolet bar () '(foo))
                       (bar))
                     '2)

;(def-trivial-test::! proga.9 так больше не делаем
;         (macroexpand-1 
;          '(proga (block foo) bar))
;         '(block foo bar))

(def-trivial-test::! proga.9a
                     (macroexpand-1
                      '(proga (block foo bar) baz))
                     '(progn (block foo bar) baz))

(def-trivial-test::! proga.10.dont-process
         (macroexpand-1
          '(proga (let (a b)) c))
         '(progn (let (a b)) c))

(def-trivial-test::! proga.11 ; не можем сделать, т.к. нужно смотреть вперёд
         (macroexpand-1
          '(proga (let ((a b)) c) d))
         '(progn (let ((a b)) c) d))


(def-trivial-test::! proga.12
         (proga
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
         (proga
           (flet the-best-dotted-list-p-1 (l)
             (proga 
              (let res nil)
              (labels f (sub)
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

(def-trivial-test::! proga.13
         (macroexpand-1 '(proga (with-open-file foo "bar") (print "bar" foo)))
         `(with-open-file (foo "bar") (print "bar" foo)))

(def-trivial-test::! proga.13.a
         (macroexpand-1 '(proga (with-open-file (foo "bar") (print "bar" foo))))
         `(with-open-file (foo "bar") (print "bar" foo)))

(def-trivial-test::! proga.14.process-flet-body
         (proga (flet f (x) (let y x) y) (f 4))
         4)

(def-trivial-test::! proga.14.1.process-flet-body
         (proga (flet f (x) "Doc" (let y x) y) (f 4))
         4)

(def-trivial-test::! proga.15.empty-tail
         (let1 x 1 
           (proga (let y (incf x))) 
           x)
         2)

(def-trivial-test::! proga.16.when
         (proga (when t (let a 5) a)) 
         5)

(def-p-t proga.17.cond
         '(proga (cond (q (let a 6)) (t (let b 7) b)))
         '(cond (q (let ((a 6))))
                (t (let ((b 7)) b))))

; не работает, т.к. расширение содержит gensym-ы
;(def-p-t proga.18.case
;         '(proga (case 4 (1 (let a 6)) (t (let b 7) b)))
;         '(case 4 (1 (let ((a 6))))
;            (t (let ((b 7)) b))))


#+see-packages
 (def-trivial-test::! proga.19.let-with-conc-type
          (proga 
            (let-with-conc-type x package (find-package :lisp))
            x.name
            )
          "COMMON-LISP"
          :test 'string=)


#+see-packages 
(def-trivial-test::! #:let-with-conc-type.1
                    (proga (let-with-conc-type x string "asdf") 
                      `(,(x.equal "asdf") ,(x.upcase) ,(x.equal x.upcase))
                      )
                    '(T "ASDF" T))
