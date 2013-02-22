;;; -*- Encoding: utf-8; -*-
;;; infrastructure for making lispworks stepper macro friendly
#|
General idea is to reuse conses from original source at the places
where breakpoint can be set in macroexpanded code.
This practice is dangerous, as it is likely to change literal data. 

|#
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :lispworks-macro-friendly-stepper
              (:use :cl)
              (:export
               #:smash-cons #:my-wombat #:extract-stepper-context-from-fn
               #:point-is-in-macro-call
               #:fix-parents-for-macro-in-context
               #:put-source-cons-at
               #:*in-stepize*
               )))

(in-package :lispworks-macro-friendly-stepper)

(defun smash-cons (literal expanded)
  (setf (car literal) (car expanded)
        (cdr literal) (cdr expanded)))

(defmacro put-source-cons-at (literal expanded accessor)
  "literal - имя переменной, содержащей cons из исходника, на кром хотим поставить брекпойнт.
  expanded - имя переменной, содержащей результат макрорасширения
  accessor - место в результате макрорасширения, куда надо подсунуть исходный конс"
  (assert (symbolp literal))
  (assert (symbolp expanded))
  (coerce accessor 'function)
  `(progn
     (smash-cons ,literal (,accessor ,expanded))
     (setf (,accessor ,expanded) ,literal)
     ))

(defun my-wombat (x)
  "Steal wombat-2"
  (let ((COMPILER::*source-level-form-table*
         (make-hash-table :test 'eq)))
    (COMPILER::wombat-2 x)
    COMPILER::*source-level-form-table*))


(defun extract-stepper-context-from-fn (fn)
  "Extract stepper context from interpreted function (unused)"
  (budden::maptree
   (lambda (x)
     (when (lispworks-tools::stepper-context-p x)
       (return-from extract-stepper-context-from-fn x)))
   (lispworks::function-lambda-expression fn)) nil)
  

(defun point-is-in-macro-call (point macro-name)
  "If point is in a subtree of macro-name, returns predecessor point (unused)"
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
  "Updates parents for perga clauses so that one can set breakpoints at them (unused)"
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
    (declare (ignore context form)) 
    (let ((*in-stepize* t))
      (lispworks:call-next-advice)))


(let* ((iterate-package-name #+budden :iterate-keywords ; budden uses patched iterate
                             #-budden :iterate
                             )
       ;(process-clause-symbol (find-symbol (string '#:process-clause)) iterate-package-name)
       (*package* (find-package iterate-package-name))
       )
  (eval
   (read-from-string 
   "(defun process-clause (clause)
      \"difference is that souce is smashed into result so that you can set lispworks stepper
    breakpoint at the clause\"
      ;; This should observe the invariant that the forms it returns are
      ;; already copied from the original code, hence nconc-able.  
      (let ((*clause* clause)
            (special-func (assoc (car clause) *special-clause-alist*))
            expanded
            result)
        (setf result
              (multiple-value-list
               (if special-func
                   (apply-clause-function (car clause) (cdr clause))
                 (let* ((ppclause (preprocess-clause clause))
                        (info (get-clause-info ppclause)))
                   (cond
                    (info
                     (arg-check ppclause info)
                     (let ((args (cons (keywordize (first ppclause))
                                       (cdr ppclause)))
                           (func (clause-info-function info)))
                       (if (macro-function func *env*)
                           (walk (macroexpand-1 (cons func args) *env*))
                         (apply-clause-function func args))))
                    (t
                     (clause-error \"No iterate function for this clause; do ~
  (~S) to see the existing clauses.\" 'display-iterate-clauses)))))))
        (setf expanded (car result)) ; extract body from clause code values list
        (when
            (and lispworks-macro-friendly-stepper:*in-stepize*
                 (consp (first expanded)))
          (lispworks-macro-friendly-stepper:put-source-cons-at *clause* expanded first))
        (values-list result)))
  
   )")))






