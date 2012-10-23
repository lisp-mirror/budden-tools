; -*- coding: windows-1251-dos; -*- 
; ��� ���������� ��� asdf ����� ������� (setf/let1 sb-impl::*default-external-format* :windows-1251)
;;; proga macro 


(in-package :proga-implementation)

(defun progify-body (body &key documentation)
  "�� ����� ����� ���� ����������� ���� defun, �� ��� ������, 
��������, (\"DOC\" (declare (special x)) (g x)). ����� �������� ���� ������������ proga. 
���������� ��������������� ����. Documentation - ������� ������� ����������"
  (multiple-value-bind 
      (forms decls doc)
      (ignore-errors (apply 'alexandria::parse-body body (dispatch-keyarg-simple documentation)))
    (cond
     ((typep decls 'error) body)
     ((or doc decls) `(,@(when doc `(,doc)) ,@decls ,@(proga-body-expander forms)))
     (t (proga-body-expander forms)))
    ))

(defun proga-body-expander (body) ; (clause &rest forms-after-clause)
  (cond
   ((null body) nil) ; forms-after-clause) `(,clause)) ; `(,clause))
   (t
    (let1 (clause . forms-after-clause) body
      (cond 
       ((atom clause)
        `(,clause ,@(proga-body-expander forms-after-clause)))
       (t 
        (let1 (head . tail) clause
          (flet ((dont-process () ; ����� �� �� ������, �������, ��� ��� ����������
                   `(,clause ,@(proga-body-expander forms-after-clause)))
                 (open-up () ; ��, ��� �� ������, ����� ������ � ������ ����� ����� �����. ��������, 
                             ; (block u) x y => (block u x y)
                   `((,@clause ,@(proga-body-expander forms-after-clause))))
                 (wind-up (processed-clause-body) ; ���� ������������� � ������, �-��� 
                                                ; ���������� ������ ����������. 
                                                ; ��, ��� �� ������ - �������� ������ ������
                                                ; ��������, (symbol-macrolet x y) a b => 
                                                ; (symbol-macrolet ((x y)) a b)
                   `((,head ,processed-clause-body ,@(proga-body-expander forms-after-clause))))
                 )
            (case head
              ((let let* #+nil :=)
               ; (when (eq head :=) (setf head 'let))
               (cond
                ((atom (cadr clause)) ; sugar
                 (assert (evenp (length tail)) 
                     () "In a proga, let/let* bindings should be a list of even length, but it was ~S" clause)
                 (wind-up (splice-list tail)))
                (t (dont-process))))
              ((symbol-macrolet smlet)
               (cond
                ((atom (cadr clause)) ; sugar
                 (let1 (name . binding-body) tail
                   (wind-up `((,name ,@(progify-body binding-body)))))
                 )
                (t (dont-process))))
              ((flet labels macrolet)
               (cond
                ((atom (cadr clause)) ; sugar
                 (let1 (fname fargs . args-decls-body) tail
                   (wind-up `((,fname ,fargs ,@(progify-body args-decls-body :documentation t)))))
                 )
                (t (dont-process))))
              ((block)
               (cond 
                ((= (length clause) 2) 
                 (break "������ ���� ������ (block name)") 
                 (open-up))
                (t
                 `((,head ,@(proga-body-expander tail)) 
                   ,@(proga-body-expander forms-after-clause)))))
              ((tagbody) ; tagbody is not opened up
               `((,head ,@(proga-body-expander tail)) 
                 ,@(proga-body-expander forms-after-clause)))
              ((destructuring-bind multiple-value-bind progv with-struct let1 pllet1)
               (if (= (length clause) 3) (open-up) (dont-process)))
              ((when unless dolist loop)
               `((,head ,@(proga-body-expander tail))
                 ,@(proga-body-expander forms-after-clause)
                 )
               )
              ((cond) 
               `((,head
                  ,@(iter 
                      (:for cond-clause in tail)
                      (let1 (cc-test . cc-exprs) cond-clause
                        (:collect `(,cc-test ,@(proga-body-expander cc-exprs)))
                        )))
                 ,@(proga-body-expander forms-after-clause)))
              ((case typecase etypecase ecase) 
               (let1 (keyform . case-clauses) tail
                   `((,head ,keyform
                            ,@(iter 
                                (:for case-clause in case-clauses)
                                (let1 (cc-test . cc-exprs) case-clause
                                  (:collect `(,cc-test ,@(proga-body-expander cc-exprs)))
                                  )))
                     ,@(proga-body-expander forms-after-clause))))
              (t
               (assert (symbolp head) () "Funny car of form: ~S" head)
               (let ((transformer (get head 'proga-transformer)))
                 (if transformer
                     (multiple-value-bind
                         (process-p result-form)
                         (funcall transformer head tail (proga-body-expander forms-after-clause))
                       (if process-p result-form (dont-process)))
                   (dont-process))))
              )))))))))


(defun open-up (head tail body) 
  "��, ��� �� ������, ����� ������ � ������ ����� ����� �����"
  (values t `((,head ,@tail ,@body))))

(defun wind-up (head tail body)
  "��, ��� ������������� clause, ������ � ������, � �� ���� ����� body"
  (values t `((,head ,tail ,@body))))

(defun open-up-if-3 (head tail body)
  "���� ����� `(,head ,@tail) ������� �� ��� ���������, �� ������ � ������ ����� ����� �����. ������ - mlvl-bind.
   (setf (get 'mlvl-bind 'proga-implementation::proga-transformer) 
      'proga-implementation::open-up-if-3
      ) 
   � ����� 
   (macroexpand-1 '(proga (mlvl-bind (a b) (values 1 2)) (list a b)))
   ==>
   (MLVL-BIND (A B) (VALUES 1 2) (LIST A B))
   "
  (when (= 2 (length tail))
    (open-up head tail body)))

(defun open-up-if-4 (head tail body)
  "���������� open-up-if-3"
  (when (= 3 (length tail))
    (open-up head tail body)))
    
(defun wind-up-tail-if-second-is-atom (head tail body)
  "Head - �������� �����, � �������� ���������� clause.
tail - �� ��������� � clause
body - ��� ������������ ����� ����� clause. 
���� ������ ������� clause - ������, �� tail ���������� ������ ����������, ��������� �������� ������ �����. ���� ������ ������� - ���������, �� ��� - ������. ���� �� ������ - �� �� �����������"
  (assert (not (constantp head)) () "Only symbol or list is allowed at second place in ~S" 
    `(,head ,tail))
  (typecase (car tail)
    (symbol
     (values t `((,head ,tail ,@body))))
    (cons
     nil)
    (t nil)))


(defun wind-up-tail-if-3 (head tail body)
  "���� ����� `(,head ,@tail) ������� �� ��� ���������, �� tail ����������� � ������, � ��������� �������� ������"
  (assert (not (constantp head)) () "Only symbol or list is allowed at second place in ~S" 
    `(,head ,tail))
  (cond 
   ((= 2 (length tail))
    (values t `((,head ,tail ,@body))))
   (t nil))
  )

(mapcar (lambda (sym) 
          (setf (get sym 'proga-transformer) 'wind-up-tail-if-second-is-atom))
        '(with-open-file
          with-input-from-string))

(setf (get 'with-conc-name 'proga-transformer) 'open-up)
(setf (get 'let-with-conc-type 'proga-transformer) 'open-up)


(defun proga-expander (body)
  (cond 
   ((atom body) body)
   (t
    (let1 (maybe-block-name . rest-of-exprs) body
      (let* ((it-is-a-block (and (symbolp maybe-block-name) rest-of-exprs))
             (real-body (if it-is-a-block rest-of-exprs body))
             (expanded-body (proga-body-expander real-body)))
        (cond
         (it-is-a-block `(block ,maybe-block-name ,@expanded-body))
         ((cdr expanded-body) ; ����� ������ ��������� - ��� progn
          `(progn ,@expanded-body))
         (t (first expanded-body))))))))


(defmacro proga (&body body)
  "proga is an extensible macro which helps to get rid of some unnecessary parens and nesting levels 
which are present in lisp syntax.
E.g. instead of writing 
 (let ((a 5))
   (flet ((f (x) x))
     (with-open-file (fi *file*)
       (f a))))

write just

 (proga
  (let a 5)
  (flet f (x) x)
  (with-open-file fi *file*)
  (f a))

So you have one code nesting level reduced from 4 to 1 and parens count reduced from 20 to 12. 
Currently proga is in need for redesign, so use with caution. let, flet forms will be kept, 
some other might change. Look at proga-transformer usage in proga.lisp file for examples of extending 
proga but beware this way of extending is likely to be broken in the near future.
"
  (proga-expander body))

