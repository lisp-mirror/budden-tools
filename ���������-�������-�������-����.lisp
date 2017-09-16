;;; -*- system: budden-tools; coding: utf-8; -*-

(in-package :SB-C)

#|
Если в файле написать
 (eval-when (:compile-toplevel)
   (setf sb-c::|*Запретить-неявное-сужение-типа*| t))
и (proclaim '(optimize (safety 2)))

то внутри этого файла нельзя будет неявно сузить тип. Компиляция следующей ф-ии даст предупреждение:

 (defun PUB (X)
  (declare (type NUMBER X))
  (flet ((FOO (Y) 
           (declare (type INTEGER Y))
           (+ Y 1)))
    (FOO X)))

|#

(defvar |*Запретить-неявное-сужение-типа*| nil
  "Если истина, то при safety 2 не позволяет сужать тип. Например, передача number в параметр, принимающий integer, вызовет warning. Связывается вокруг загрузки и компиляции любого файла в саму себя, поэтому её можно присваивать внутри eval-when внутри самого файла - эффект будет распространяться только на этот акт компиляции. Для справки: мы не обнаружили никаких отличий между (safety 3) и (safety 2), но safety 3 используется во многих местах, в т.ч. в чужом для нас коде и в самой среде, поэтому мы не можем глобально поменять поведение safety 3. А safety 2 вроде довольно редкая вещь, хотя в исходниках SBCL она тоже бывает")

(pushnew '|*Запретить-неявное-сужение-типа*|
         СВЯЗАТЬ-СПЕЦИАЛЬНЫЕ-ПЕРЕМЕННЫЕ-ВОКРУГ-LOAD-И-COMPILE-FILE:*СПИСОК-ПЕРЕМЕННЫХ-ДЛЯ-СВЯЗЫВАНИЯ-ВОКРУГ-LOAD-И-COMPILE-FILE*)

(defun named-function-some-cast-p (cast)
  "Без особого понимания выдернуто из ir1-optimize-cast"
  (let ((context (node-source-form cast))
        (detail (lvar-all-sources (cast-value cast))))
    (or (eq (car context) 'named-lambda) 
        (and (eq (car context) 'function) 
             (eq (caadr context) 'named-lambda)))))


(defun decorated-generate-type-checks (fn component)
  (declare (ignore fn))
  (collect ((casts))
    (do-blocks (block component)
      (when (and (block-type-check block)
                 (not (block-delete-p block)))
        ;; CAST-EXTERNALLY-CHECKABLE-P wants the backward pass
        (do-nodes-backwards (node nil block)
          (when (and (cast-p node)
                     (cast-type-check node))
            (cast-check-uses node)
            (cond ((cast-externally-checkable-p node)
                   (setf (cast-%type-check node) :external))
                  (t
                   ;; it is possible that NODE was marked :EXTERNAL by
                   ;; the previous pass
                   (setf (cast-%type-check node) t)
                   (casts node)))))
        (setf (block-type-check block) nil)))
    (dolist (cast (casts))
      ;(budden-tools:show-expr cast)
      (unless (bound-cast-p cast)
        (multiple-value-bind (check types) (cast-check-types cast)
          (ecase check
            (:simple
             (cond
              ((and (policy cast (= safety 2))
                    |*Запретить-неявное-сужение-типа*|
                    (not (named-function-some-cast-p cast)))
               (let* ((value (cast-value cast))
                      (atype (cast-asserted-type cast))
                      (value-type (lvar-derived-type value))
                      (context (node-source-form cast))
                      (detail (lvar-all-sources value)))
                 ;`(%compile-time-type-error 'dummy
                 ;                           ',(type-specifier atype)
                 ;                           ',(type-specifier value-type)
                 ;                           ',detail
                 ;                           ',(compile-time-type-error-context context)
                 ;                           ',(cast-context cast))
                 (let ((*compiler-error-context* cast))
                   (compiler-warn "Неявное сужение типа с проверкой во время выполнения запрещено в этой части кода:~%~S." cast)))))
             (convert-type-check cast types))
            (:too-hairy
             (let ((*compiler-error-context* cast))
               (when (policy cast (>= safety inhibit-warnings))
                 (compiler-notify
                  "type assertion too complex to check:~%~
                    ~/sb-impl:print-type/."
                  (coerce-to-values (cast-asserted-type cast)))))
             (setf (cast-type-to-check cast) *wild-type*)
             (setf (cast-%type-check cast) nil)))))))
  (values))


;(decorate-function:undecorate-function 'generate-type-checks)

(sb-int:encapsulate 'generate-type-checks 'Запретить-неявное-сужение-типа 'decorated-generate-type-checks)

