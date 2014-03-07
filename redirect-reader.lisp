;;; -*- Encoding: utf-8; -*-

; (defmacro let1 (var val &body body) `(let ((,var ,val)) ,@body))

(in-package :budden-tools)
(setf *readtable* (copy-readtable nil))

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defun restore-rt () (setf *readtable* (copy-readtable nil)))
  (restore-rt)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Character table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fill-char-table)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Change a readtable ;;;;;;;;;;;;;

(defun packages-seen-p (readtable-designator)
  "Returns t, if enable-buddens-readtable-extensions was called on readtable"
  (gethash (ensure-readtable readtable-designator) *readtable-uses-sbcl-reader-budden-tools-lispworks*)
  )


(defun enable-buddens-readtable-extensions (readtable-designator-or-nil)
  "Alters readtable to enable budden's readtable extensions. If readtable is nil, 
alters a new copy of standard readtable. Returns readtable it has altered. If readtable
is already an altered readtable, simply returns it"
  (let (rt)
  (block nil
    ; (let readtable (ensure-readtable readtable-designator))
    (setf rt
      (etypecase readtable-designator-or-nil
        (null (copy-readtable nil))
        (t (ensure-readtable readtable-designator-or-nil))))
    (when (gethash rt *readtable-uses-sbcl-reader-budden-tools-lispworks*)
      (warn "buddens readtable extensions are already enabled on ~S" rt)
      (return rt))

    (set-macro-character #\( #'paren-reader-with-closing-paren-notification nil rt)

    (setf (gethash rt *readtable-uses-sbcl-reader-budden-tools-lispworks*) t)

    (set-dispatch-macro-character #\# #\: #'sbcl-reader-budden-tools-lispworks::sharp-colon rt)

    ;(setf good-readtable (copy-readtable rt))

    (iter 
      (:for i :from 0 to 255)
      (:for b := (elt *char-table* i))
      (:for c := (code-char i))
      (cond 
       ((consp b))
       (t
        (ecase b 
          ((:does-not-terminate-token ; :multiple-escape 
            ; :single-escape
            ; In a past, we could start tokens with \ with no problem. 
            ; with lispworks6, we can't do that anymore as it conforms to a standard in treating #\\ in string
            ; When #\\ is a macro char, it is not a single-escape anymore, so string reading becomes broken
            ; let's try to live without #\\ as a token starting character. 
            )
           ;?? (set-syntax-from-char c #\# rt good-readtable) ; will make it non-terminating macro character
           (set-macro-character c #'sbcl-reader-budden-tools-lispworks:read-token t rt))
          (:colon
           (set-macro-character c #'sbcl-reader-budden-tools-lispworks:read-token t rt))
          ((:dot :whitespace[2] :single-escape :multiple-escape nil))
          )
        )
       )
      )

    (iter
      (:for c in *def-symbol-reamacro-additional-name-starting-characters*)
      (set-macro-character c #'sbcl-reader-budden-tools-lispworks:read-token t rt))

    rt)
  ))

(defun reset-to-standard-readtable (readtable-designator)
  "Removes buddens readtable extensions from readtable and reset it to standard one"
  (let ((rt (ensure-readtable readtable-designator)))
    (setf (gethash rt *readtable-uses-sbcl-reader-budden-tools-lispworks*) nil)
    (setf (gethash rt *readtable-case-is-upcase-if-uniform*) nil)
    (copy-readtable nil rt)
    rt))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Change a readtable end ;;;;;;;;;;;;;

(decorate-function:portably-without-package-locks
; non-toplevel 
(defmethod print-object ((obj readtable) stream)
    "Print readtable with its name"
    (print-unreadable-object (obj stream :type t :identity t)
      (princ (ignore-errors (editor-hints.named-readtables::%readtable-name obj)) stream)
      (when (eq obj *readtable*) (princ '(:current) stream))))
)

(defun package-readtable (package-designator)
  "Use of the data is currently unknown. Maybe should be used to develop SLIME extension?"
  (cdr (assoc (package-name (find-package package-designator))
                 *readtable-alist* :test 'equalp)))

(defun (setf package-readtable) (readtable-designator package-designator)
  (pushnew 
   (cons 
    (package-name (find-package package-designator)) 
    (when readtable-designator (ensure-readtable readtable-designator))
    )
   *readtable-alist*
   :test 'equalp))


(defun rd (s) (with-my-readtable-0 (read-from-string s)))

(decorate-function::portably-without-package-locks

;non-toplevel
(defun keywordize (symbol-or-string) ; altering keywordize from iterate
  (careful-keywordize symbol-or-string)
  )
)


(defun my-print-symbol (o s)
  "see also print-symbol-string-with-advanced-readtable-case"
  (proga
    ;(let str (string o))
    ;(mlvl-bind (found status)
    ;    (symbol-is-in-package o *package* nil))
    ;(cond
    ; (found
    ;  (
    (let *print-escape* nil)
    (let *print-readably* nil)
    (let *readtable* *xrt*)
    (print o s)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Change printer if needed ;;;;;;;;;;;;;;;;;;;;;;;;
#+lispworks6 
(decorate-function::PORTABLY-WITHOUT-PACKAGE-LOCKS

; non-toplevel
(defmethod print-object :around ((o symbol) s)
    (cond
     (*print-readably*
      (let ((*readtable* *cached-default-readtable*)
            (system:*print-symbols-using-bars* t)
            (*print-case* :upcase))
        (call-next-method)))
     ((eq (readtable-case-advanced *readtable*) :upcase-if-uniform)
      (case (all-ascii-chars-in-same-case-p (string o))
        (:lowercase
         (let ((*print-case* :upcase)
               (*readtable* *cached-default-readtable*))
           (call-next-method)))
        (:uppercase
         (let ((*print-case* :downcase)
               (*readtable* *cached-default-readtable*))
           (call-next-method)))
        (t
         (let1 *readtable* *cached-preserve-case-readtable*
           (call-next-method))
         )
        ))
     (t
      (call-next-method)))))





#|    (let1 cases (all-ascii-chars-in-same-case-p (string o))
      (case cases
        (:uppercase 
         (let ((*print-case* :upcase)
               (*readtable* *cached-default-readtable*))
           (call-next-method)))
        ((:lowercase :ignore-case)
         (let ((*print-case* :downcase))
           (call-next-method)))
        (t 
         (call-next-method)
         ))))) |#

#|
    (let1 *readtable* *cached-default-readtable* 
    ; #+nil (if 
    ;                      (eq (symbol-package o) *keyword-package*) 
    ;                      *cached-default-readtable*
    ;                    (gethash *readtable* *my-readtable-to-good-readtable* *readtable*))
    ;(with-good-readtable-2 (:ensure-this-is-a-bad-one nil)
      (call-next-method)))) |#

#+lispworks6
(decorate-function::PORTABLY-WITHOUT-PACKAGE-LOCKS
  (defmethod print-object :around ((o structure-object) s)
    "Lispworks6 prints '|ASDF| as \\A\\S\\D\\F in our readtables. As a quick fix,
     we just set up 'good' readtable around printing"
    (let1 *readtable* *cached-default-readtable* 
      (call-next-method)
      )
    ))

