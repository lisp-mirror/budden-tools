; -*- system :budden-tools ; -*- 

(in-package :budden-tools)

(defun sbcl-policy-level (quality)
  "Сделано на базе sb-ext:describe-compiler-policy. Возвращает либо 
 уровень данного quality в compiler policy (например, 'debug), 
 либо nil, если такого quality нет"
  (let ((policy (sb-c::process-optimize-decl (cons 'optimize nil) sb-c::*policy*)))
    (sb-c::dovector (a-quality sb-c::**policy-primary-qualities**)
      (when (eq a-quality quality)
        (return-from sbcl-policy-level (sb-c::policy-quality policy quality))))
    nil))


