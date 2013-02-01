;;; Budden, 2008-12-09
;;; Adding a clause to iterate to find the best element in an iteration using not simply
;;; minizing or maximizing, but using any predicate of two arguments. See also iterate-keywords:
;;; http://sourceforge.net/projects/iteratekeywords/
;;; (iter (:for (runner time) in '((ivan 10.0) (maria 9.9) (kolya 11.2)))
;;;	  (:finding runner :yielding-best-of time by #'>)) ==> MARIA

(in-package :iterate-keywords)

(defun return-find-extremum-code-with-test (expr m-expr var test)
  "Just the same as return-find-extremum-code, but accepts an arbitrary test"
  (setq expr (walk-expr expr))
  (setq m-expr (walk-expr m-expr))
  (let* ((function? (function-quoted? m-expr))
	 (temp-var (make-var-and-default-binding 'temp :using-type-of 
						 (if (not function?) m-expr)))
	 (temp-var-2 (if (and function? (not (duplicable? expr)))
			 (make-var-and-default-binding 'temp
						       :using-type-of expr)))
	 expr-var m-var)
    (cond
     ((null var)   
      ;; no var means return expr as a result
      (setq expr-var *result-var*)
      (setq m-var (genvar 'best)))
     ((var-spec? var)
      ;; a single var-spec means set expr to that var
      (setq expr-var var)
      (setq m-var (genvar 'best)))
     ((and (consp var) (= (length var) 2) (every #'var-spec? var))
      ;; a two-element list means set expr to 1st, m to 2nd
      (setq expr-var (first var))
      (setq m-var (second var)))
     (t
      (clause-error "The value for INTO, ~a, should be a variable specifier ~
  or a list of two variable specifiers." var)))
    (make-default-binding expr-var :using-type-of expr)
    (make-accum-var-default-binding m-var 'best :using-type-of m-expr)
    (setq expr-var (extract-var expr-var))
    (setq m-var (extract-var m-var))
    (let* ((expr-code (or temp-var-2 expr))
	   (esetq-code (if temp-var-2 `((setq ,temp-var-2 ,expr))))
	   (m-code (if function?
		       (make-funcall m-expr expr-code)
		       m-expr)))
      (return-code :body `(,.esetq-code
			   (setq ,temp-var ,m-code)
			   ,(if-1st-time 
			     `((setq ,m-var ,temp-var)
			       (setq ,expr-var ,expr-code))
			     `((cond
				((funcall ,test ,m-var ,temp-var)
				 (setq ,m-var ,temp-var)
				 (setq ,expr-var ,expr-code))
				(t ,expr-var)))))))))


(defclause (finding expr yielding-best-of extremize-expr by test &optional into variable)
  "Return value which minimizes expression"
  (return-find-extremum-code-with-test expr extremize-expr variable test))

