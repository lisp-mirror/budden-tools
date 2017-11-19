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
    (budden-tools:show-expr context)
    (or (eq (car context) 'named-lambda) 
        (and (eq (car context) 'function) 
             (eq (caadr context) 'named-lambda)))))









#|
 (defun in-tree (item tree)
  (cond
    ((eq item tree) t)
    ((consp tree) (or (in-tree item (car tree)) (in-tree item (cdr tree))))
    (t nil)))

 (defun decorated-ir1-optimize-cast (fn cast &optional do-not-optimize)
  (declare (ignore fn))
  (declare (type cast cast))
  (let ((value (cast-value cast))
        (atype (cast-asserted-type cast)))
    (unless (or do-not-optimize
                (not (may-delete-vestigial-exit cast)))
      (when (and (bound-cast-p cast)
                 (bound-cast-check cast)
                 (constant-lvar-p (bound-cast-bound cast)))
        (setf atype
              (specifier-type `(integer 0 (,(lvar-value (bound-cast-bound cast)))))
              (cast-asserted-type cast) atype
              (bound-cast-derived cast) t))
      (let ((lvar (node-lvar cast)))
        (when (and (or (not (bound-cast-p cast))
                       (bound-cast-derived cast))
                   (values-subtypep (lvar-derived-type value)
                                    (cast-asserted-type cast)))
          (when (function-designator-cast-p cast)
            (let ((*valid-fun-use-name* (function-designator-cast-caller cast))
                  (*lossage-fun* #'compiler-warn)
                  (*compiler-error-context* cast))
              (valid-callable-argument lvar
                                       (function-designator-cast-arg-count cast))))

          (delete-cast cast)
          (return-from decorated-ir1-optimize-cast t))

        (when (and (listp (lvar-uses value))
                   lvar)
          ;; Pathwise removing of CAST
          (let ((ctran (node-next cast))
                (dest (lvar-dest lvar))
                next-block)
            (collect ((merges))
              (do-uses (use value)
                (when (and (values-subtypep (node-derived-type use) atype)
                           (immediately-used-p value use))
                  (unless next-block
                    (when ctran (ensure-block-start ctran))
                    (setq next-block (first (block-succ (node-block cast))))
                    (ensure-block-start (node-prev cast))
                    (reoptimize-lvar lvar)
                    (setf (lvar-%derived-type value) nil))
                  (%delete-lvar-use use)
                  (add-lvar-use use lvar)
                  (unlink-blocks (node-block use) (node-block cast))
                  (link-blocks (node-block use) next-block)
                  (when (and (return-p dest)
                             (basic-combination-p use)
                             (eq (basic-combination-kind use) :local))
                    (merges use))))
              (dolist (use (merges))
                (merge-tail-sets use))))))

      (when (and (bound-cast-p cast)
                 (bound-cast-check cast)
                 (policy cast (= insert-array-bounds-checks 0)))
        (flush-combination (bound-cast-check cast))
        (setf (bound-cast-check cast) nil)))

    (let* ((value-type (lvar-derived-type value))
           (int (values-type-intersection value-type atype)))
      (derive-node-type cast int)
      (cond ((or
              (neq int *empty-type*)
              (eq value-type *empty-type*))
             (when (and (policy cast (= safety 2))
                        |*Запретить-неявное-сужение-типа*|)
               (let ((context (node-source-form cast))
                     (detail (lvar-all-sources (cast-value cast))))
                 (unless (or (eq (car context) 'named-lambda) 
                             (and (eq (car context) 'function) 
                                  (eq (caadr context) 'named-lambda))
                             ;(not (in-tree (car detail) context))
                             )
                   (filter-lvar
                     (cast-value cast)
                     ;; FIXME: Derived type.
                    (if (cast-silent-conflict cast)
                        (let ((dummy-sym (gensym)))
                          `(let ((,dummy-sym 'dummy))
                             ,(internal-type-error-call dummy-sym atype
                                                        (cast-context cast))
                             ,dummy-sym))
                        `(%compile-time-type-error 'dummy
                                                   ',(type-specifier atype)
                                                   ',(type-specifier value-type)
                                                   ',detail
                                                   ',(compile-time-type-error-context context)
                                                   ',(cast-context cast))))))))
            ;; No need to transform into an analog of
            ;; %COMPILE-TIME-TYPE-ERROR, %CHECK-BOUND will signal at
            ;; run-time and %CHECK-BOUND ir2-converter will signal at
            ;; compile-time if it survives further stages of ir1
            ;; optimization.
            ((bound-cast-p cast))
            (t
             ;; FIXME: Do it in one step.
             (let ((context (node-source-form cast))
                   (detail (lvar-all-sources (cast-value cast))))
               (unless (cast-silent-conflict cast)
                 (filter-lvar
                  value
                  (if (cast-single-value-p cast)
                      `(list 'dummy)
                      `(multiple-value-call #'list 'dummy))))
               (filter-lvar
                (cast-value cast)
                ;; FIXME: Derived type.
                (if (cast-silent-conflict cast)
                    (let ((dummy-sym (gensym)))
                     `(let ((,dummy-sym 'dummy))
                        ,(internal-type-error-call dummy-sym atype
                                                   (cast-context cast))
                        ,dummy-sym))
                    `(%compile-time-type-error 'dummy
                                               ',(type-specifier atype)
                                               ',(type-specifier value-type)
                                               ',detail
                                               ',(compile-time-type-error-context context)
                                               ',(cast-context cast)))))
             ;; KLUDGE: FILTER-LVAR does not work for non-returning
             ;; functions, so we declare the return type of
             ;; %COMPILE-TIME-TYPE-ERROR to be * and derive the real type
             ;; here.
             (setq value (cast-value cast))
             (derive-node-type (lvar-uses value) *empty-type*)
             (maybe-terminate-block (lvar-uses value) nil)
             ;; FIXME: Is it necessary?
             (aver (null (block-pred (node-block cast))))
             (delete-block-lazily (node-block cast))
             (return-from decorated-ir1-optimize-cast)))
      (when (eq (node-derived-type cast) *empty-type*)
        (maybe-terminate-block cast nil))

|#







#|(decorate-function:decorate-function
 'ir1-optimize-cast
 (lambda (fn cast &optional do-not-optimize)
   (budden-tools:show-expr `(ir1-optimize-cast
                             ;,cast
                             ,(cast-context cast)
                             ,(lvar-derived-type (cast-value cast))
                             ))
   (funcall fn cast do-not-optimize)
   ))|#

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
                    (not (named-function-some-cast-p cast))
                    )
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


(decorate-function:undecorate-function 'generate-type-checks :advice-name 'Запретить-неявное-сужение-типа)
;(sb-int:encapsulate 'generate-type-checks 'Запретить-неявное-сужение-типа 'decorated-generate-type-checks)

