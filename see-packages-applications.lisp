(in-package :budden-tools)
(in-readtable nil) 

(setf (budden-tools:symbol-readmacro (intern "/WITH-PACKAGE/" :budden-tools))
      (lambda (stream symbol)
        (declare (ignore symbol))
        (it-is-a-car-symbol-readmacro)
        (let1 *package* (find-package (read stream))
          (read stream))))


(defpackage :package-for-read-symbol-name (:use))
(defparameter +package-for-read-symbol-name+ 
  (find-package :package-for-read-symbol-name))

(defun read-symbol-name (stream)
  "Читает символ, но возвращает только его имя. Если символ в потоке не имеет квалификатора, то такой символ вообще не будет создан"
  (let1 symbol-or-string (let1 *package* +package-for-read-symbol-name+
                 (read stream t))
    (etypecase symbol-or-string
      (string symbol-or-string)
      (symbol
       (assert (eq (symbol-package symbol-or-string) +package-for-read-symbol-name+))
       (unintern symbol-or-string +package-for-read-symbol-name+)
       (symbol-name symbol-or-string)))))



#+nil (defun -->-reader-internal (stream read-object object)
  "Если read-object=nil, то мы уже считали объект и читаем только то, что идёт после него"
  (let* ((object (if read-object (read stream t) object))
         (field-name (read-symbol-name stream))
         (args (read-delimited-list #\) stream t))) ; дерьмо вот здесь, и никак не решить. 
    (unread-char #\) stream) ; очень сомнительно.
    `(|-->| ,object ,field-name ,@args)))


(defun -->-reader-internal-2 (stream read-object object read-field-name field-name)
  "Если read-object=nil, то мы уже считали объект и читаем только то, что идёт после него"
  (let* ((object (if read-object (read stream t) object))
         (field-name (if read-field-name (read-symbol-name stream) field-name))
         (symbol (make-symbol (str+ "(--> " (if (string-designator-p object)
                                                (string object) "#<...>")
                                    " " field-name ")")))
         (args (make-symbol "args")))
    (eval `(defmacro ,symbol (&rest ,args) 
             `(|-->| ,',object ,',field-name ,@,args)))
    (eval `(define-symbol-macro ,symbol (|-->| ,object ,field-name)))
    symbol))
          
  
(defun -->-reader (stream symbol)
  (declare (ignore symbol))
  ; (it-is-a-car-symbol-readmacro)
  (-->-reader-internal-2 stream t nil t nil))

(setf (budden-tools:symbol-readmacro '|-->|) '-->-reader)

(defun convert-dot-to---> (stream symbol-name package)
  "превращает точку в -->"
  (let* ((p (position #\. symbol-name :from-end t)))
    (cond 
     ((null p) (values nil nil))
     ((= (+ p 1) (length symbol-name)) (values nil nil))
     (t ; (break "~A" symbol-name)
        (let ((beg (subseq symbol-name 0 p))
              (end (subseq symbol-name (+ p 1))))
          (values (let ((*package* package))
                    (funcall '-->-reader-internal-2 
                             (make-concatenated-stream (make-string-input-stream (str+ beg " ")) stream)
                             t nil 
                             nil end))
                  t))))))

; (setf (get-custom-token-parsers-for-package :budden) nil)
(pushnew 'convert-dot-to---> (get-custom-token-parsers-for-package :budden))


;; (/with-readtable-case/ :preserve '(foo bar))
;; Note that readtable case is evaluated at read-time 
(setf (budden-tools:symbol-readmacro (intern "/WITH-READTABLE-CASE/" :budden-tools))
      (lambda (stream symbol)
        (declare (ignore symbol))
        (it-is-a-car-symbol-readmacro)
        (let1 new-case (read stream)
          (print new-case)
          (pllet1 (readtable-case (packages-seen-p *readtable*)) new-case
            (pllet1 (readtable-case *readtable*) new-case
              (let1 colon-readtable (or (gethash *readtable* *my-readtable-to-colon-readtable*) *readtable*)
                (pllet1 (readtable-case colon-readtable) new-case
                  (read stream))))))))

                                     


(defparameter *essential-binding-checkers* 
  '(boundp fboundp) ;  ap5:rboundp - куда-то в другое мсто запихать
  "List of function names. Either function recieves one parameter, a symbol. If symbol is essential
and should be uninterned with caution, some of functions return true. E.g., if symbol denotes ap5 relation,
it is essential from ap5 viewpoint"
  )

(defun see-packages-check-bad-clashes (&key allow-exist (package *package*))
  "Проверям, что нет конфликтов между символами в текущем пакете и другими символами, которые мы видим с помощью see. Если конфликт обнаружен и такой символ не перечислен в разрешённых, вызываем cerror. TODO: проверять биндинги"
  (iter ; restart loop
    (iter ; finding clashes
      (:with allow-exist = (mapcar 'string allow-exist))
      (:for s in-package package)
      (:for essential = nil)
      (:for name = (symbol-name s))
      (multiple-value-bind (found here)
          (see-packages-find-symbol name package)
        (when (and (second found) 
                   here 
                   (not (member name allow-exist :test 'equal)))
          (iter
            (:for fname in *essential-binding-checkers*)
            (when (funcall fname s)
              (:collect fname :into ess))
            (:finally 
             (when ess
               (setf essential t)
               (cerror "Continue (no action will be taken)" 
                       "On bad clashing symbol ~S, function(s) ~S returned true. Uninterning it
may cause data loss. Other clashing symbols are ~S. You can unintern it manually" s ess (cdr found))
               )))
          (unless essential (:collect (cons s (cdr found)) :into bad-clashes))))
      (:finally
       (cond 
        (bad-clashes
         (let1 *print-readably* t
           (print `(mapcar 'unintern 
                           (mapcar 'car
                                   ',@bad-clashes))
                  *debug-io*))
         (cerror "Retry" "Some bad clashes found. Now you can execute code printed above to unintern them")
         )
        (t 
         (return-from see-packages-check-bad-clashes nil)))))))

