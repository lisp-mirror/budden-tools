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
         (*readtable* (gethash *readtable* *my-readtable-to-good-readtable*))
         (token (with-xlam-package (read stream)))
         (symbol (the* symbol token)))
    (assert (eq (symbol-package symbol) 
                *xlam-package*))
    (unintern symbol *xlam-package*)
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
    (set-macro-character #\( #'paren-reader-with-symbol-readmacro nil rt)

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
          ((:does-not-terminate-token :multiple-escape :single-escape)
           (set-syntax-from-char c #\# rt good-readtable) ; will make it non-terminating macro character
           (set-macro-character c #'careful-token-reader t rt))
          (:colon
           (set-macro-character c #'starting-colon-reader t rt))
          ((:dot :whitespace[2] nil))
          )
        )
       ))
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

;;;;;;;;;;;;;;;;;;;;;;;;; Change a printer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+lispworks 
(defun decorated-output-symbol (undecorated-output-symbol obj)
  "Что надо ещё сделать? Работать с readtable-case"  
;  (when (symbolp obj) 
;    (trace-into-text-file (str+ "decorated-output-symbol: " (symbol-package obj)))
;    (trace-into-text-file (str+ "decorated-output-symbol: " (symbol-name obj))))
  (let1 id (new-show-package-system-vars-id)
    (unwind-protect
        (proga
          (show-package-system-vars "decorated-output-symbol: before" id)
          ;(trace-into-text-file (str+ "decorated-output-symbol: " (symbol-name obj)) id)
          ;(maybe-bind-package (or *real-package* *last-used-real-package*)
          (let ((*package* *package* #+nil (or *real-package* *package*)))
            (proga
              (flet print-as-usual () (progn 
                                        (show-package-system-vars "decorated-output-symbol: inside" id)
                                        (funcall undecorated-output-symbol obj)))
              (typecase obj
                (null (print-as-usual))
                (symbol
                 (proga if-symbol  
                   (let name (symbol-name obj))
                   (let pack (symbol-package obj))
                   (cond
                    ((member pack '(#.(find-package :keyword) nil))
                     (print-as-usual)
                     )
                    (t 
                     (iter
                       (:with sym-is-seen = nil)
                       (:for real-first-time-p :initially t :then nil)
                       (:for p in (cons *package* (package-seen-packages-list *package*)))
                       (:for (values p-sym storage-type) = (find-symbol name p))
                       (when (and p-sym (or real-first-time-p (eq storage-type :external)))
                         (when (eq p-sym obj) 
                           (setf sym-is-seen t))
                         (:count 1 into cnt))
                       (:finally
                        (cond
                         ((and sym-is-seen (= cnt 1)) ; если символ виден и нет конкурентов
                          (let1 *package* (if *print-normalize-seen-symbols*
                                              *package*
                                            pack
                                            ); печатаем без квалификатора
                            (print-as-usual)))
                         ((and sym-is-seen (> cnt 1)) ; символ виден, есть конкуренты, печатаем с квалификатором
                          (with-xlam-package-for-output
                            (print-as-usual)))
                         (t
                          (print-as-usual) ; по построению здесь будет квалификатор
                          ))))))))))))
      (show-package-system-vars "decorated-output-symbol: restored" id)
      )
    ))

#+(and :lispworks :really-see-packages)
(decorate-function 'io::output-symbol #'decorated-output-symbol)

#+(and (not :lispworks) :really-see-packages) (error "oy!")
;;;;;;;;;;;;;;;;;;;;;;; Change a printer end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cl-user::portably-without-package-locks
  (defun keywordize (symbol-or-string) ; altering keywordize from iterate
    (careful-keywordize symbol-or-string)
    #+nil (etypecase symbol-or-string
      (symbol (intern (symbol-name symbol-or-string) :keyword))
      (string (intern symbol-or-string :keyword)))))

(defpackage :tst (:use))
  ;(print "-----------------4---------------------")
(let ((*readtable* *my-readtable*)
      (*package* (find-package :tst)))
  (see-packages)
  #+nil (print (ignore-errors (read-from-string "#+ignore nil"))))
;(print "-----------------5---------------------")


