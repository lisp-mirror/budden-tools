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


|#



(in-package :perga-implementation)

#|(defun wind-up (body clause head tail forms-after-clause)
  "тело преобразуется в список, к-рый 
  становится вторым аргументом. 
  Всё, что за формой - вносится внутрь скобок
  Например, (symbol-macrolet x y) a b => 
  (symbol-macrolet ((x y)) a b)"
  (let* ((result
          `(,head ,tail ,.(perga-body-expander forms-after-clause))))
    (smash-cons clause result)
    (setf (cdr body) nil)
    body
    ))|#


(defun wind-up (body clause head tail forms-after-clause)
  "тело преобразуется в список, к-рый 
  становится вторым аргументом. 
  Всё, что за формой - вносится внутрь скобок
  Например, (symbol-macrolet x y) a b => 
  (symbol-macrolet ((x y)) a b)"
  (declare (ignore body))
  #-lispworks (declare (ignore clause))
  (let* ((result
          `(,head ,tail ,.(perga-body-expander forms-after-clause))))
    #+lispworks (set-source-location-substitution clause result)
    #+lispworks (put-source-cons-at-macroexpansion-result clause result)
    (list result)
    ))

(defun open-up
       (body clause head tail forms-after-clause)
  "всё, что за формой, нужно внести в скобки формы после формы. Например, 
  (block u) x y => (block u x y)"
  (declare (ignore body))
  #-lispworks (declare (ignore clause))
  (let ((result
         `((,head ,@tail ,@(perga-body-expander forms-after-clause)))))
    #+lispworks (set-source-location-substitution clause (car result))
    #+lispworks (put-source-cons-at-macroexpansion-result clause result car)
    (values
     result
     t)
    )
  )

(defun dont-process
       (body clause head tail forms-after-clause)
  "форму мы не поняли, считаем, что это вычисление" 
  (declare (ignore body head tail))
  (let* ((result `(,clause ,.(perga-body-expander forms-after-clause))))
    #+lispworks (set-source-location-substitution clause (car result))
  ;(smash-cons clause result)
  ;(setf (cdr body) nil)
  ;body
    result
    ))

(defun pergify-body (body &key documentation)
  "На входе имеем тело определения типа defun, но без головы, 
например, (\"DOC\" (declare (special x)) (g x)). Нужно окружить тело конструкцией perga. 
Возвращает преобразованное тело. Documentation - признак разбора докстринга"
  (multiple-value-bind 
      (forms decls doc)
      ;(nil ignore-errors
      (apply 'alexandria::parse-body body
             (budden-tools::dispatch-keyarg-simple documentation))
      ;)
    (cond
     ;((typep decls 'error) body)
     ((or doc decls) `(,@(when doc `(,doc)) ,@decls ,@(perga-body-expander forms)))
     (t (perga-body-expander forms)))
    ))


(defun perga-body-expander (body) ; (clause &rest forms-after-clause)
  (cond
   ((null body) nil) ; forms-after-clause) `(,clause)) ; `(,clause))
   (t
    (destructuring-bind (clause . forms-after-clause) body
      (cond 
       ((atom clause)
        `(,clause ,.(perga-body-expander forms-after-clause)))
       (t 
        (destructuring-bind (head . tail) clause
          (case head
            
            (t
               ;(assert (= 1 0) ())
             (assert (symbolp head) () "Funny car ~S of perga clause ~S:" head clause)
             (let ((transformer (get head 'perga-transformer)))
               (if transformer
                   (multiple-value-bind
                       (result-form process-p)
                       (funcall transformer body clause head
                                tail
                                forms-after-clause)
                     (if process-p result-form
                       (dont-process body clause head
                                     '#:invalid forms-after-clause
                                     )))
                 (dont-process body clause head
                               '#:invalid forms-after-clause
                               ))))
            ))))))))


#|(defun perga-expander (form)
      (let* ((body (cdr form))
             (form-table (my-wombat form))
             (expanded-body (perga-body-expander body))
             (result `(progn ,.expanded-body)))
        (let ((*print-circle* t))
          (budden-tools::show-expr `(,form-table ,result)))
        result
        )
      )|#


(defun perga-expander (form)
  (let* ((body (cdr form))
         ;(form-table (lw-macro-friendly-stepper::my-wombat form))
         (result nil))
    (setf result
          (cond 
           ((atom body) body)
           (t
            (destructuring-bind
                (maybe-block-name . rest-of-exprs) body
              (let* ((it-is-a-block
                      (and (symbolp maybe-block-name) rest-of-exprs))
                     (real-body (if it-is-a-block rest-of-exprs body))
                     (expanded-body (perga-body-expander real-body)))
                ;(show-expr expanded-body)
                (cond
                 (it-is-a-block
                  `(block ,maybe-block-name ,.expanded-body))
                 ((cdr expanded-body) ; более одного выражения - это progn
                  ;(break "проверить брекпойнты!")
                  `(progn ,.expanded-body))
                 (t (first expanded-body)
                    )))))))
    #|(let ((*print-circle* t))
        (budden-tools::show-expr `(,form-table ,result)))|#
    result
    ))


(defun symbol-macrolet-perga-transformer (body clause head tail forms-after-clause)
  (declare (ignorable head))
  (cond
   ((atom (second clause))
    (values (wind-up body clause 'symbol-macrolet
                     (list tail)
                     forms-after-clause)
            t
            ))
   ((eq head 'symbol-macrolet)
    (values (dont-process body clause head
                          '#:invalid forms-after-clause)
            t))
   (t (error "Perga syntax error in ~S" clause))
   ))


(defun process-local-function (function-def)
  "Progify body of a local function"
  (or (atom (first function-def)) (perga-syntax-error
                               "Syntax error in a local function definition: name must be an atom in ~S"
                               function-def))
  (or (listp (second function-def)) (perga-syntax-error
                                 "Syntax error in a local function definition: arguments must be a list in ~S"
                                 function-def))
  (destructuring-bind (fname fargs . args-decls-body) function-def
    `(,fname ,fargs ,.(pergify-body args-decls-body :documentation t))
    ))

(defun flet-perga-transformer-inner (body clause head tail forms-after-clause &key allow-one-only process-labels-bodies)
  (let ((processed-head
         (cond 
           ((member head '(flet labels macrolet)) head)
           ((eq head :labels)'labels) 
           (t "wrong call to flet-perga-transformer-inner"))))
    (cond
     ((atom (second clause))
      (values
       (wind-up body clause processed-head
                `(,(process-local-function tail))
                forms-after-clause)
       t))
     (allow-one-only
      (perga-syntax-error "syntax error in perga clause: (second clause) must be atom in ~S" clause))
     (process-labels-bodies ; several definitions. Process them all
      (assert (eq head :labels))
      (let* ((processed-definitions 
              (loop :for definition :in tail
                    :collect (process-local-function definition))))
        (values
         (wind-up body
                  clause
                  processed-head
                  processed-definitions
                  forms-after-clause)
         t)))
     ) ;cond
    ))



(def-perga-clause symbol-macrolet 'symbol-macrolet-perga-transformer)
; move to my package - budden
(def-perga-clause :symalet
                  'symbol-macrolet-perga-transformer)

(def-perga-clause flet
                  #'(lambda (body clause head tail forms-after-clause)
                      (flet-perga-transformer-inner body clause head tail forms-after-clause :allow-one-only t)))

(def-perga-clause macrolet
                  #'(lambda (body clause head tail forms-after-clause)
                      (flet-perga-transformer-inner body clause head tail forms-after-clause :allow-one-only t)))

(def-perga-clause block
                  #'(lambda (body clause head tail forms-after-clause)
                      (declare (ignore body))
                       #-lispworks (declare (ignore clause))
                      (let ((result `((,head ,(car tail) ,@(perga-body-expander (cdr tail))) ,@(perga-body-expander forms-after-clause))))
                        #+lispworks (set-source-location-substitution clause (car result))
                        #+lispworks (put-source-cons-at-macroexpansion-result clause result car)
                        (values
                         result
                         t))))

;(def-perga-clause block
;                  #'(lambda (&rest args)
;                      (error "perga block ~S" (first args))))

(def-perga-clause tagbody
                  #'(lambda (body clause head tail forms-after-clause)
                      (declare (ignore body ))
                       #-lispworks (declare (ignore clause))
                      (let ((result `((,head ,@(perga-body-expander tail)) 
                         ,@(perga-body-expander forms-after-clause))))
                        #+lispworks (set-source-location-substitution clause (car result))
                        #+lispworks (put-source-cons-at-macroexpansion-result clause result car)
                      (values
                       result
                       t)
                       )))

(def-perga-clause :labels
                  #'(lambda (body clause head tail forms-after-clause)
                           (flet-perga-transformer-inner body clause head tail forms-after-clause :allow-one-only nil :process-labels-bodies t)))

(defun let-perga-transformer (body clause head
                                   tail
                                   forms-after-clause)
  (when (atom (cadr clause)) ; sugar
    (assert (evenp (length tail)) 
        () "In a perga, let/let* bindings should be a list of even length, but it was ~S" clause)
    (values
     (wind-up body clause head
              (splice-list tail)
              forms-after-clause)
     t))
  )

(def-perga-clause let 'let-perga-transformer)
(def-perga-clause let* 'let-perga-transformer)


(defun open-up-if-3 (body clause &rest more-args
                                                   #|body clause head
                                   tail
                                   forms-after-clause|#)
  "Если форма `(,head ,@tail) состоит из трёх элементов, то внести в скобки формы после формы. Например, 
  (def-perga-clause with-something 'open-up-if-3)
   и далее 
   (macroexpand-1 '(perga (with-something a b) (list (a) b)))
   ==>
   "
  (when (= (length clause) 3)
    (apply 'open-up body clause more-args)
    ))

(def-perga-clause destructuring-bind 'open-up-if-3)


(def-perga-clause multiple-value-bind 'open-up-if-3)

(def-perga-clause progv 'open-up-if-3)
;(def-perga-clause let1 'destructuring-bind-perga-transformer)

;(def-perga-clause budden-tools::pllet1 'destructuring-bind-perga-transformer)

(defun when-perga-transformer (body clause head
                                   tail
                                   forms-after-clause)
  (declare (ignore body ))
   #-lispworks (declare (ignore clause))
 (let ((result `((,head ,@(perga-body-expander tail))
                  ,@(perga-body-expander forms-after-clause)
     )))
    #+lispworks (set-source-location-substitution clause (car result))
    #+lispworks (put-source-cons-at-macroexpansion-result clause result car)
    (values
     result
     t
     )))

(def-perga-clause when 'when-perga-transformer)
(def-perga-clause unless 'when-perga-transformer)
(def-perga-clause dolist 'when-perga-transformer)
(def-perga-clause loop 'when-perga-transformer)
(def-perga-clause dotimes 'when-perga-transformer)

(defun do-perga-transformer (body clause head
                                    tail
                                    forms-after-clause)
  "do пишется так же, как обычно, но тело обрабатывается. return-forms не обрабатаываются"
  #-lispworks (declare (ignore clause))
  (declare (ignore body))
  (destructuring-bind (bind-step-forms (end-test-form &rest result-forms) &body body-of-do)
      tail
    (let ((result `((,head ,bind-step-forms (,end-test-form ,@result-forms)
                           ,@(perga-body-expander body-of-do))
                    ,@(perga-body-expander forms-after-clause))
                  ))
      #+lispworks (set-source-location-substitution clause (car result))
      #+lispworks (put-source-cons-at-macroexpansion-result clause result car)
      (values
       result
       t
       ))))

(def-perga-clause do 'do-perga-transformer)
      

(defun cond-perga-transformer (body clause head
                                    tail
                                    forms-after-clause)
  (declare (ignore body))
   #-lispworks (declare (ignore clause))
 (let ((result
         `((,head
            ,@(mapcar 
               (lambda (cond-clause)
                 (destructuring-bind (cc-test . cc-exprs)
                     cond-clause
                   `(,cc-test ,@(perga-body-expander cc-exprs))))
               tail))
            ,@(perga-body-expander forms-after-clause))))
    #+lispworks (set-source-location-substitution clause (car result))
    #+lispworks (put-source-cons-at-macroexpansion-result clause result car)
    (values
     result
     t
     )))

(def-perga-clause cond 'cond-perga-transformer)

(defun case-perga-transformer (body clause head
                                    tail
                                    forms-after-clause)
  (declare (ignore body))
  #-lispworks (declare (ignore clause))
  (let (result)
    (destructuring-bind (keyform . case-clauses) tail
      (setf result
            `((,head ,keyform
                     ,@(mapcar
                        (lambda (case-clause)
                          (destructuring-bind (cc-test . cc-exprs) case-clause
                            `(,cc-test ,@(perga-body-expander cc-exprs))))
                        case-clauses)
                     )
              ,@(perga-body-expander forms-after-clause)))
      #+lispworks (set-source-location-substitution clause (car result))
      #+lispworks (put-source-cons-at-macroexpansion-result clause result car)
      (values
       result
       t))))

(def-perga-clause case 'case-perga-transformer)
(def-perga-clause typecase 'case-perga-transformer)
(def-perga-clause etypecase 'case-perga-transformer)
(def-perga-clause ecase 'case-perga-transformer)
(def-perga-clause alexandria:eswitch 'case-perga-transformer)
(def-perga-clause alexandria:switch 'case-perga-transformer)
(def-perga-clause alexandria:cswitch 'case-perga-transformer)


#|(defmacro perga (&whole form &body body)
  (declare (ignore body))
  (perga-expander form))

определена в vars-and-macros в таким определением.

  |#


(defun open-up-if-4 (body clause head tail forms-after-clause)
  "Для (with-the1 var type value)"
  (when (= 3 (length tail))
    (open-up body clause head tail forms-after-clause)
    ))
    
(defun wind-up-tail-if-second-is-atom (body clause head tail forms-after-clause)
  "Head - ключевое слово, с которого начинается clause.
tail - всё остальное в clause
forms-after-clause - уже обработанные формы после clause. 
Если второй элемент clause - символ, то tail становится вторым аргументом, остальное вносится внутрь формы. Если второй элемент - константа, то это - ошибка. Если он список - то не преобразуем. Например, можно было бы определить
  (def-perga-clause with-open-file 'wind-up-tail-if-second-is-atom)
  и тогда. 
  (perga
    (with-open-file var filename ...)
    (read var))"
  (declare (ignore body))
  #-lispworks (declare (ignore clause))
  (assert (not (constantp head)) () "Only symbol or list is allowed at second place in ~S" 
    `(,head ,tail))
  (let (result)
    (typecase (car tail)
      (symbol
       (setf result `((,head ,tail ,@(perga-body-expander forms-after-clause))))
       #+lispworks (set-source-location-substitution clause (car result))
       #+lispworks (put-source-cons-at-macroexpansion-result clause result car)
       (values result t))
      (t nil)
      )))

(defun colon-add-character-perga-transformer (body clause head tail forms-after-clause)
  "Раскрывает формы после clause и подставляет их в конец clause. Например,
  (perga
    (:@ with-input-from-string (var \"0\"))
    (let a (read-from-string var))
    a)
  ==>
  (with-open-file (var filename)
    (let ((a 1))
      a))"
  (declare (ignore body))
   #-lispworks (declare (ignore clause))
  (assert (eq head :@))
  (let (result)
    (setf result
          `((,@tail ,@(perga-body-expander forms-after-clause))))
    #+lispworks (set-source-location-substitution clause (car result))
    #+lispworks (put-source-cons-at-macroexpansion-result clause result car)
    (values result t)
  ))

(def-perga-clause :@ 'colon-add-character-perga-transformer)

(defun colon-ampersand-character-perga-transformer (body clause head tail forms-after-clause)
  "Оборачивает хвост clause списком, раскрывает формы после clause и подставляет их в конец clause. Например,
  (perga
    (:& with-input-from-string var \"0\")
    (let a (read-from-string var))
    a)
  ==>
  (with-open-file (var filename)
    (let ((a 1))
      a))"
  (declare (ignore body))
  #-lispworks (declare (ignore clause))
  (assert (eq head :&))
  (let (result)
    (setf result
          `((,(car tail) ,(cdr tail)
                         ,@(perga-body-expander forms-after-clause))))
    #+lispworks (set-source-location-substitution clause (car result))
    #+lispworks (put-source-cons-at-macroexpansion-result clause result car)
    (values result t)
  ))
  
(def-perga-clause :& 'colon-ampersand-character-perga-transformer)



(defun wind-up-tail-if-3 (body clause head tail forms-after-clause)
  "Если форма `(,head ,@tail) состоит из трёх элементов, то tail заключается в скобки, а остальное вносится внутрь"
  (declare (ignore body))
  #-lispworks (declare (ignore clause))
  (assert (not (constantp head)) () "Only symbol or list is allowed at second place in ~S" 
    `(,head ,tail))
  (let (result)
    (cond 
     ((= 2 (length tail))
      (setf result 
            `((,head ,tail ,@(perga-body-expander forms-after-clause))))
      #+lispworks (set-source-location-substitution clause (car result))
      #+lispworks (put-source-cons-at-macroexpansion-result clause result car)
      (values t result)
      )
     (t nil))
    )
  )

(def-perga-clause mlvl-bind
                  (lambda (body clause head
                                tail
                                forms-after-clause)
                    (declare (ignore head))
                    (open-up-if-3 body clause 'multiple-value-bind
                                  tail forms-after-clause)
                    ))


(def-perga-clause budden-tools:with-the1
                  #'(lambda (body clause head tail forms-after-clause)
                      ;(declare (ignorable body clause head tail))
                      (when forms-after-clause
                        (error "with-the1 in perga is forbidden, clause ~S" clause))
                      (dont-process
                       body clause head tail forms-after-clause)))

                      

(def-perga-clause :lett
                  #'(lambda (body clause
                                  head
                                  tail forms-after-clause)
                      (declare (ignore head))
                      (assert (= (length clause) 4) () ":lett perga clause ~S should have contained exactly four items" clause)
                      (multiple-value-bind (result processed)
                          (open-up-if-4
                           body clause
                           'budden-tools:with-the1
                           tail forms-after-clause)
                        (when processed
                          #+lispworks (set-source-location-substitution
                           clause (car result)))
                        (values result processed))))
                        


