; -*- coding: windows-1251-dos; -*- 

; (defmacro let1 (var val &body body) `(let ((,var ,val)) ,@body))

(in-package :budden-tools)
(setf *readtable* (copy-readtable nil))

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defun restore-rt () (setf *readtable* (copy-readtable nil)))
  ;(print "-----------------1---------------------")
  (restore-rt)
  ;(print "-----------------2---------------------")
  (defun mess-rt () (setf *readtable* *my-readtable*))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Character table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fill-char-table)
;(print "-----------------3---------------------")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun redirect-read (s c)
;  (format *trace-output* "redirect-read~%")
  (careful-token-reader s c))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Change a readtable ;;;;;;;;;;;;;

(defun packages-seen-p (readtable-designator)
  "Returns t, if see-packages was called on readtable. If so, returns unmangled readtable"
  (gethash (ensure-readtable readtable-designator) *my-readtable-to-good-readtable*)
  )


(defun sharpdot-colon-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (let* (; (*package* (find-package :xlam-package))
         ; (*readtable* (gethash *readtable* *my-readtable-to-good-readtable*))
         (token (with-xlam-package (read stream)))
         (symbol (the* symbol token)))
    (assert (or (null symbol)
                (eq (symbol-package symbol) 
                    *xlam-package*)))
    (when symbol (unintern symbol *xlam-package*))
    symbol))

#+nil (defmethod print-object ((sym symbol) stream)
        (print "ch" stream)
        (if (%seen-package-list *package*)
            (call-next-method)
          (call-next-method))
        )


(defun make-safer-sharp-backslash-reader (rt)
  #+russian "Делает читалку, к-рая использует старую таблицу чтения"
  (let* 
      ((unmangled-rt (packages-seen-p rt))
       (current-sharp-backslash-reader
        (get-dispatch-macro-character #\# #\\ unmangled-rt)))
    (lambda (stream char numarg)
      (let1 *readtable* unmangled-rt
        (funcall current-sharp-backslash-reader stream char numarg)))))

(defun see-packages-on (readtable-designator-or-nil) 
  "Alters readtable to enable see-packages extensions. If readtable is nil, 
alters a new standard readtable. Returns readtable it has altered. If readtable
is already an altered readtable, simply returns it TODO: rename me"
  (proga nil
    ; (let readtable (ensure-readtable readtable-designator))
    (let rt
      (etypecase readtable-designator-or-nil
        (null (copy-readtable nil))
        (t (ensure-readtable readtable-designator-or-nil))))
    (smlet entry (gethash rt *my-readtable-to-good-readtable*))
    (when entry (return rt))
    ;; note this!!!
    (set-macro-character #\( #'paren-reader-with-closing-paren-notification nil rt)

    (let good-readtable (copy-readtable rt)) 

    (set-dispatch-macro-character #\# #\: #'sharpdot-colon-reader rt)

    (setf entry good-readtable)

    (iter:iter 
      (:for i :from 0 to 255)
      (:for b := (elt *char-table* i))
      (:for c := (code-char i))
      (cond 
       ((consp b))
       (t
        (ecase b 
          ((:does-not-terminate-token :multiple-escape 
            ; :single-escape
            ; In a past, we could start tokens with \ with no problem. 
            ; with lispworks6, we can't do that anymore as it conforms to a standard in treating #\\ in string
            ; When #\\ is a macro char, it is not a single-escape anymore, so string reading becomes broken
            ; let's try to live without #\\ as a token starting character. 
            )
           (set-syntax-from-char c #\# rt good-readtable) ; will make it non-terminating macro character
           (set-macro-character c #'careful-token-reader t rt))
          (:colon
           (set-macro-character c #'starting-colon-reader t rt))
          ((:dot :whitespace[2] :single-escape
            nil))
          )
        )
       )
      )

    (iter:iter
      (:for c in *def-symbol-reamacro-additional-name-starting-characters*)
      (set-macro-character c #'careful-token-reader t rt))

    (set-dispatch-macro-character 
     #\# #\\ 
     (make-safer-sharp-backslash-reader rt) rt)
    rt))




; (setf *readtable* (copy-readtable nil))
;(setf *my-readtable* readtable)
;(setf *good-readtable* (copy-readtable readtable))
  ;(print "-----------------3A---------------------")
(setf *my-readtable* (see-packages-on (copy-readtable nil)))
(setf *good-readtable* (gethash *my-readtable* *my-readtable-to-good-readtable*))
  ;(print "-----------------3B---------------------")

(defun make-colon-readtable (rt) 
  (proga
    (symbol-macrolet colon-readtable (gethash rt *my-readtable-to-colon-readtable*))
    (let c-rt colon-readtable)
    (unless c-rt
      (setf c-rt (copy-readtable (gethash rt *my-readtable-to-good-readtable*)))
      (set-syntax-from-char #\: #\  c-rt) ; we rely on the fact that #\  is a whitespace in rt
      (setf colon-readtable c-rt))
    c-rt
    ))

#+nil (defun going-to-read-up-to-colons (char rt)
  "Теперь двоеточие становится terminating macro char, а считанная буква становится обычной буквой.
Свяжите *reading-up-to-colons* с его собственным значением перед вызовом этой пары функциц"
  (show-expr :входим-в-going-to-read-up-to-colons)
  (setf *reading-up-to-colons* t)
  (set-syntax-from-char char char rt (gethash rt *my-readtable-to-good-readtable*))
  (set-syntax-from-char #\: #\  rt))


#+nil (defun done-reading-up-to-colons (char rt)
  (show-expr :входим-в-done-reading-up-to-colons)
  (when *reading-up-to-colons*
    (show-expr `(:restoring ,char))
    (set-macro-character char 'careful-token-reader t rt)
    (set-macro-character #\: 'starting-colon-reader t rt)
    (setf *reading-up-to-colons* nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Change a readtable end ;;;;;;;;;;;;;

(cl-user::portably-without-package-locks
  (defmethod print-object ((obj readtable) stream)
    "Print readtable with its name"
    (print-unreadable-object (obj stream :type t :identity t)
      (princ (ignore-errors (editor-hints.named-readtables::%readtable-name obj)) stream)
      (when (eq obj *readtable*) (princ '(:current) stream)))))

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

(cl-user::portably-without-package-locks

;non-toplevel
(defun keywordize (symbol-or-string) ; altering keywordize from iterate
  (careful-keywordize symbol-or-string)
  )
)

#|2012-08-27 (defpackage :tst (:use))
  ;(print "-----------------4---------------------")
(let ((*readtable* *my-readtable*)
      (*package* (find-package :tst)))
  (see-packages)
  #+nil (print (ignore-errors (read-from-string "#+ignore nil"))))
;(print "-----------------5---------------------")


|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Change printer if needed ;;;;;;;;;;;;;;;;;;;;;;;;
#+lispworks6 
(cl-user::PORTABLY-WITHOUT-PACKAGE-LOCKS
  (defmethod print-object :around ((o t) (s stream))
    "Lispworks6 prints '|ASDF| as \\A\\S\\D\\F in our readtables. As a quick fix,
     we just set up 'good' readtable around printing"
    (let1 *readtable* (gethash *readtable* BUDDEN-TOOLS::*MY-READTABLE-TO-GOOD-READTABLE* *readtable*)
      (call-next-method))))

; TODO: output ASDF as asdf when :upcase-if-uniform is set up. 

