;;; -*- Encoding: utf-8; -*-
;;; infrastructure for making lispworks stepper macro friendly
#|
Written by Denis Budyak, 
Email: (map 'string 'code-char '(98
 117 100 100 101 110 55 51 64 103 109 97 105 108 46 99 111 109))

The code is in public domain.

General idea is to reuse conses from original source at the places
where breakpoint can be set in macroexpanded code.

This practice is dangerous, as it is likely to change literal data. 
So we "smash" only when we are in lispworks-tools::stepize function, which
state we identify via advicing. 

As an example, patch to iterate is included, see last form and
separate lw-macro-friendly-stepper-iterate-patch.lisp
With that patch you hopefully can set breakpoints at the head 
of for and collect clauses. 

State of the code is pre-alpha. Feedback is greatly appreciated.
|#
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :lw-macro-friendly-stepper
              (:use :cl)
              (:export
               #:put-source-cons-at-macroexpansion-result
               #:*in-stepize*
               )))

(in-package :lw-macro-friendly-stepper)

(defun smash-cons (literal expanded)
  "Replaces car and cdr of literal by those of expanded"
  (setf (car literal) (car expanded)
        (cdr literal) (cdr expanded)))

(defmacro put-source-cons-at-macroexpansion-result (literal expanded accessor)
  #-russian
  "Anti-hygienic macro to smash literal data. 
  literal - name of variable in environment, which contains literal cons from original source
  expanded - name of variable in environment which contains result of macroexpansion or some part of that.
  accessor - symbol. Accessor to the place in macroexpanded code where breakpoint can be set.
  Acts only when *in-stepize* is true.
  Literal cons is filled with car and cdr of (accessor expanded) and is put into macroexpanded code at the place (accessor expanded) 
  "
  #+russian "
  Анти-гигиеничный макрос для уничтожения literal data.
  Действует только когда *in-stepize* - истина
  literal - имя переменной, содержащей cons из исходника, на кром хотим поставить брекпойнт.
  expanded - имя переменной, содержащей результат макрорасширения или его часть
  accessor - место в макрорасширеннном коде, где можно поставить точку останова.
  Cons из literal заполняется данными (car и cdr) из (accessor expanded) и помещается в макрорасширенный код на место (accessor expanded). После этого, на literal можно поставить точку останова.
   "
  (assert (symbolp literal))
  (assert (symbolp expanded))
  (assert (symbolp accessor))
  `(progn
     (when *in-stepize*
       (smash-cons ,literal (,accessor ,expanded))
       (setf (,accessor ,expanded) ,literal)
       )
     ))

(defun my-wombat (x)
  "Steal wombat-2 (for debugging)"
  (let ((COMPILER::*source-level-form-table*
         (make-hash-table :test 'eq)))
    (COMPILER::wombat-2 x)
    COMPILER::*source-level-form-table*))


(defun maptree (fun tree)  
   #+russian "Проходит рекурсивно по дереву. К каждому атому дерева применяет функцию fun.
    Получается такое дерево."
   #-russian "Walks tree, applying fun to any atom of it and collects results to the fresh isomorphic tree"
  (map 'list (lambda (x) (cond 
			  ((consp x) (maptree fun x)) 
			  (t (funcall fun x)))) tree))

(defun extract-stepper-context-from-fn (fn)
  "UNUSED. Extract stepper context from interpreted function"
  (maptree
   (lambda (x)
     (when (lispworks-tools::stepper-context-p x)
       (return-from extract-stepper-context-from-fn x)))
   (lispworks::function-lambda-expression fn)) nil)
  

(defun point-is-in-macro-call (point macro-name)
  "UNUSED. If point is in a subtree of macro-name, returns predecessor point"
  (let (parent)
    (setf parent (slot-value point 'lispworks-tools::parent))
    (cond
     ((null parent) nil)
     ((not (typep parent 'LISPWORKS-TOOLS::stepable-point)) nil)
     ((eq (slot-value parent 'lispworks-tools::operator) macro-name)
        parent
        )
     (t (point-is-in-macro-call parent macro-name)))))

(defun fix-parents-for-macro-in-context (context macro-name)
  "UNUSED. Updates parents for perga clauses so that one can set breakpoints at them"
  (let (points grand-macro)
    (setf points
          (slot-value context 'lispworks-tools::stepable-points))
    (dolist (point points)
      (setf grand-macro
            (point-is-in-macro-call point macro-name))
      (when grand-macro
        (setf (slot-value point 'lispworks-tools::parent)
              grand-macro)))
  ))



(defvar *in-stepize* nil "t when stepize function is on stack")

(lispworks::defadvice (lispworks-tools::stepize
                       bind-in-stepize-around-stepize
                       :around :documentation "binds *in-stepize* while calling stepize")
    (context form)
    (let ((*in-stepize* t))
      (lispworks:call-next-advice context form)))


(let* ((iterate-package-name (or (find-package :iterate-keywords) ; http://sourceforge.net/projects/iteratekeywords/
                                 (find-package :iterate))))
  (when iterate-package-name
    (let ((*package* iterate-package-name))
      (load (compile-file (merge-pathnames "lw-macro-friendly-stepper-iterate-patch.lisp"
                                           #.*compile-file-pathname*))))))

