;;; -*- Encoding: utf-8; -*-
;;; this file intentionnaly contains no in-package form
;;; see lw-macro-friendly-stepper

;; this was commented out as something have changed in iterate itself
#| (defun process-clause (clause)
   "Redefined in lw-macro-friendly-stepper-iterate-patch.lisp. 
difference is that souce is smashed into result so that you can set lispworks stepper
    breakpoint at the clause"
      ;; This should observe the invariant that the forms it returns are
      ;; already copied from the original code, hence nconc-able.  
      (let ((*clause* clause)
            (special-func (assoc (car clause) *special-clause-alist*))
            expanded
            result
            )
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
                     (clause-error "No iterate function for this clause; do ~
  (~S) to see the existing clauses." 'display-iterate-clauses)))))))
        (setf expanded (car result)) ; extract body from clause code values list
        (when (consp (first expanded))
          (lw-macro-friendly-stepper:put-source-cons-at-macroexpansion-result
           *clause* expanded first))
        (values-list result)))


 (defun walk-expr (expr)
  "Redefined in lw-macro-friendly-stepper-iterate-patch.lisp. Smashes expr with result"
  (let* ((dont-smash (and (consp expr)
                          (starts-clause? (symbol-synonym (car expr)))
                          ; if we dont impose the restriction, some conses are
                          ; smashed twice, e.g in k.value.finding-maximizing.1
                          )))
    (multiple-value-bind (body decls init step final finalp)
        (walk expr)
      (augment *decls* decls)
      (augment *initial* init)
      (augment *step* step)
      (augment *final* final)
      (augment *finalp* finalp)
      (let ((result (prognify body)))
        (when (and (consp expr) (not dont-smash))
          #|(let ((*print-circle* t))
            (BUDDEN-TOOLS:show-expr `(,result ,expr)))|#
          (lw-macro-friendly-stepper:put-source-cons-at-macroexpansion-result
           expr
           result
           ))
        result)
      ))) |#



  
   


