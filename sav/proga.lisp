;;; -*- Encoding: utf-8; -*-
;;; proga macro 


(in-package :proga-implementation)


(defun proga-body-expander (body)
  (cond
   ((atom body) body)
   (t
    (let ((form (pop body)))
      (cond 
       ((atom form)
        `(,form ,@(proga-body-expander body)))
       (t 
        (let ((head (car form)))
          (flet ((dont-process () ; форму мы не поняли
                   `(,form ,@(proga-body-expander body)))
                 (open-up () ; всё, что за формой, нужно внести в скобки формы после формы
                   `((,@form ,@(proga-body-expander body))))
                 (wind-up (processed-form-body) ; тело преобразуется в список, к-рый становится вторым аргументом. 
                       ; Всё, что за формой - вносится внутрь скобок
                   `((,head ,processed-form-body ,@(proga-body-expander body))))
                 )
            (case head
              ((let let*)
               (cond
                ((atom (cadr form)) ; sugar
                 (pop form) 
                 (assert (evenp (length form)) 
                     () "In a proga, let/let* bindings should be a list of even length, but it was ~S" form)
                 (wind-up (splice-list form)))
                (t (dont-process))))
              ((flet labels macrolet symbol-macrolet)
               (cond
                ((atom (cadr form)) ; sugar
                 (pop form) 
                 (wind-up `(,form))         
                 )
                (t (dont-process))))
              ((block)
               (if (= (length form) 2) (open-up) (dont-process)))
              ((destructuring-bind let1 multiple-value-bind progv)
               (if (= (length form) 3) (open-up) (dont-process)))
              (otherwise 
               (assert (symbolp head) () "Funny car of form: ~S" head)
               (let ((transformer (get head 'proga-transformer)))
                 (if transformer
                     (multiple-value-bind
                         (process-p result-form)
                         (funcall transformer head (cdr form) (proga-body-expander body))
                       (if process-p result-form (dont-process)))
                   (dont-process))))
              )))))))))

(defun wind-up-tail-if-atom (head tail body)
       "Если второй элемент формы - символ, то cdr формы становится вторым аргументом, остальное вносится внутрь формы. Если второй элемент - константа, то это - ошибка"
       (assert (not (constantp head)) () "Constant is not allowed at second place in ~S" `(head ,@tail))
       (cond 
        ((symbolp head)
         (values t `((,head ,tail ,@body))))
        (t nil)))

(mapcar (lambda (sym) 
          (setf (get sym 'proga-transformer) 'wind-up-tail-if-atom))
        '(with-open-file))


 
(defun proga-expander (body)
  (cond 
   ((atom body) body)
   (t
    (let ((form (car body)))
      (typecase form
        (symbol (pop body) `(block ,form ,@(proga-body-expander body)))
        (t `(progn ,@(proga-body-expander body))))))))


(defmacro proga (&body body)
  (proga-expander body))

(deftest proga.1 (macroexpand-1 '(proga 2)) '(progn 2))

(deftest proga.2 (proga-expander '(foo nil)) '(BLOCK FOO NIL))

(deftest proga.3 (proga-expander '((let a b) c)) '(progn (LET ((A B)) C)))

(deftest proga.4 (proga-expander '(b (labels f (a) (+ a 1)) c)) 
         '(BLOCK b (LABELS ((F (A) (+ A 1))) C)))


(deftest proga.5 (proga-expander '((flet f (a) (+ a 1)) c))
         '(progn (FLET ((F (A) (+ A 1))) C)))

(deftest proga.6 (proga-expander '((let a 1 b 2) (flet f () (list a b)) (f)))
         '(progn (LET ((A 1) (B 2)) (FLET ((F () (LIST A B))) (F)))))

(deftest proga.7 (proga-expander '((destructuring-bind ((a &optional (b 'bee)) one two three) `((alpha) ,@(iota 3)))
                                   (list a b three two one)))
         '(progn (destructuring-bind ((a &optional (b 'bee)) one two three) `((alpha) ,@(iota 3))
                       (list a b three two one))))

(deftest proga.8
         (proga
           (let a 1)
           (flet foo () (+ a 1))
           (macrolet bar () '(foo))
           (bar))
         2)

(deftest proga.9
         (macroexpand-1 
          '(proga (block foo) bar))
         '(progn (block foo bar)))

(deftest proga.10 
         (macroexpand-1
          '(proga (let (a b)) c))
         '(progn (let (a b)) c))

(deftest proga.11 ; не можем сделать, т.к. нужно смотреть вперёд
         (macroexpand-1
          '(proga (let ((a b)) c) d))
         '(progn (let ((a b)) c) d))


(deftest proga.12
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

(deftest proga.13
         (proga-expander `((with-open-file foo "bar") (print "bar" foo)))
         `(progn (with-open-file (foo "bar") (print "bar" foo))))

