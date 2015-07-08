;;; -*- Encoding: utf-8; -*-
(in-package :budden-tools)
(setf *readtable* (copy-readtable nil))

(defun unread-char* (char stream) "Раньше возвращал новый stream, а теперь - просто unread-char"
  (progn 
    (unread-char char stream)
    stream
    )
  #+nil
  (make-concatenated-stream (make-string-input-stream (concatenate 'string (list char)))
                            stream))
  

(defun sharp-backslash-fallback (s c n) 
  "Fallback to standard readtable while reading character name"
  (let1 *readtable* *good-readtable*
    (funcall (get-dispatch-macro-character #\# #\\ *good-readtable*) s c n)))

; (defparameter *char-table* (make-array 256 :initial-element nil))

(defmacro symbol-readmacro (symbol) `(get ,symbol 'symbol-readmacro))

#+lispworks 
(dspec:define-form-parser def-symbol-readmacro (name &rest args)
  (declare (ignore def-symbol-readmacro args))
  name)

#+lispworks 
(defun def-symbol-readmacro-fun (symbol reader location)
  (setf (symbol-readmacro symbol) reader)
  (lispworks:record-definition symbol location)
  )

#+lispworks 
(defmacro def-symbol-readmacro (symbol reader &key documentation)
  (declare (ignore documentation))
  `(dspec:def ,symbol
     (def-symbol-readmacro-fun ',symbol ,reader (dspec:location))
     ))

#+example (def-symbol-readmacro |JUST-READ| 'read)

#-lispworks
(defmacro def-symbol-readmacro (symbol reader)
  `(setf (symbol-readmacro ',symbol) ,reader))


(defmacro with-good-readtable-2 ((&key (ensure-this-is-a-bad-one t)) &body body)
  "переданная readtable должна быть получена с помощью see-packages-on"
  (with-gensyms (good)
    `(proga
       (let ,good (gethash *readtable* *my-readtable-to-good-readtable*))
;       (print '(:good-readtable-is ,good))
       ,@(when ensure-this-is-a-bad-one
           `((assert ,good nil "with-good-readtable: ~A is not a hp-readtable" *readtable*)))
       (let *readtable* (or ,good *readtable*))
       ,@body
       )))

(defvar *token-starts-with-vertical-line* nil) ; don't need to be threadvar (?)


;;;; open-paren for symbol-readmacro
(defvar *reading-parens* nil "Если истина, то мы находимся внутри чтения скобок")  
(defvar *functions-to-call-when-paren-is-closing* nil
       "Здесь может быть функция от аргументов (считанный-список поток), к-рую мы вызовем на закрытии скобки")


; factored out (defvar *package-designator-starts-from-vertical-line* nil)  
; factored out (defvar *symbol-name-starts-from-vertical-line* nil)  
(defvar *inhibit-readmacro* nil "When true, readmacros are not processed and treated as normal symbols. It is used to find readmacro definition") 
(defvar *reading-parens-stream* nil) 
(defvar *fn-before-return-symbol-from-reader-hook*) 
(setf (documentation '*fn-before-return-symbol-from-reader-hook* 'variable)
      "Before the symbol is returned from sbcl-reader-budden-tools-lispworks::read-token, this function is called on symbol and a stream. Beware stream input is buffered, 
so use stream parameter only to identify a reader. Return value of hook function is then returned instead of the original symbol"
      )

(let ((default-open-paren-reader (get-macro-character #\( (copy-readtable nil))))
  (defun paren-reader-with-closing-paren-notification (stream char)
    "Если внутри readera кто-то заполнил ф-ями в *functions-to-call-when-paren-is-closing*,
то эти ф-и будут вызвана над результатом чтения (...) и потоком с первой по последнюю, преобразуя результат" 
    (let* ((position (extract-file-position stream))
           (*reading-parens* (cons position *reading-parens*))
           (*reading-parens-stream* stream)
           (*functions-to-call-when-paren-is-closing* nil)
           (result (funcall default-open-paren-reader stream char)))
      (ignored position)
      (dolist (f *functions-to-call-when-paren-is-closing*)
        (setf result (funcall f result stream)))
      result
      )))       


(defun push-function-to-call-when-paren-is-closing (f)
  (if *reading-parens* 
      (push f *functions-to-call-when-paren-is-closing*)
    (warn "Попытка назначить действие на чтение закрывающей скобки вне чтения скобок")))

(defun check-correct-use-of-a-car-symbol-readmacro (object)
  (when *reading-parens*
    (push-function-to-call-when-paren-is-closing
     (lambda (result stream)
       (unless (consp result)
         (simple-reader-error
          stream 
          "Something wrong with symbol readmacro: list reader on ~S returned atom ~S" stream result))
       (unless (eq object (car result))
         (simple-reader-error 
          stream                    
         "In ~S, symbol-readmacro should be at the first position in a list" result))
       (unless (null (cdr result))
         (simple-reader-error
          stream
          "car-symbol-readmacro should have read entire list in ~S" result))
       (car result))))
    object)

(defun it-is-a-car-symbol-readmacro (object-read)
  "Если определение symbol-readmacro-reader, то имеет место следующее:
Если readmacro находится внутри круглых скобок, то функция должна прочитать всё до 
закрывающей круглой скобки (не включая её) и вернуть одно значение. 
В противном случае, случится ошибка. 
Результатом чтения охватывающих данный symbol-readmacro скобок является не список, 
а значение, возвращённое symbol-readmacro-reader.
Если readmacro находится вне круглых скобок, то ничего особенного не происходит. 
"
  (check-correct-use-of-a-car-symbol-readmacro object-read))

;;; end of open-paren for symbol-readmacro


; (defun simple-reader-error (stream format-string &rest args )
;   (error "~A in stream ~A" (apply 'format nil format-string args) stream))


(eval-when (:load-toplevel) (print "6--------------------------------------"))

(defun char-type (c) (elt *char-table* (char-code c)))


;; sbcl has native local package nicknames support
(defun hp-alias-map (p)
  "Finds alias map for a package. p is a package designator" 
  (declare (ignorable p))
  #-sbcl
  (gethash 
   (the* not-null (apply-undecorated 'find-package (list p)))
   *per-package-alias-table*)
  #+sbcl
  (sb-ext::package-local-nicknames p)
  )

#-sbcl (defun (setf hp-alias-map) (new p)
  "Example: (setf (budden-tools:hp-alias-map :lgrep) '((:p . :meta-parse))). TODO: check structure"
  ;; This one should never be called if HP is not loaded.
  (declare (ignorable new p))
  (setf 
   (gethash 
    (the* not-null (apply-undecorated 'find-package (list p)))
    *per-package-alias-table*)
   new)
  )

#-sbcl (defun delete-hp-alias-map (p)
  (declare (ignorable p))
  (remhash (the* not-null (apply-undecorated 'find-package (list p)))
           *per-package-alias-table*))

;; redefining hp-find-package to know about qualified-package
;; note this was initially defined in hierarchial-packages with some conditionals
; 

(defun hp-find-package
    (name/package &optional (relative-to-package *package*) real-find-package-fn) 
    "For lispworks, find-package will be later decorated to this. For SBCL, they coexist"
    (declare (optimize speed))          ;this is critical code
    (let1 *package* *keyword-package* ; otherwise might crash on error messages
      (typecase name/package
        (package name/package)
        (t                                ;should be STRINGable
         ;; PN is package name, EPN is effective (aliased) name
         ;; if there is one
         (let* ((pn (string name/package))
                (map (hp-alias-map relative-to-package))
                (epn (and map (cdr (assoc pn map :test #'string=)))))
           ;; if there is an EPN, then do REAL-FIND-PACKAGE on it, 
           ;; otherwise use NAME/PACKAGE. not PN, in case it can do some
           ;; magic.  Otherwise look up a relative name.
           (if real-find-package-fn (funcall real-find-package-fn  (or epn name/package))
             (apply-undecorated 'find-package (list (or epn name/package))))
           )))))


; find-symbol
(defun see-packages-find-unqualified-symbol (name &optional package)
  "Returns: 
i) if symbol is not found, then nil,nil,nil
ii) if symbol is found once, then symbol,package,status
iii) if symbol is found more than once then first-symbol-found,list of packages,:ambigious
"
  (proga
    (let package (if package (find-package package) *package*))
    (iter
      (:with first-sym-found = nil)
      (:with first-status = nil)
      (:with first-package = nil)
      (:with seen-package-list = nil #|2012-08-27 (package-seen-packages-list package)|#)
      (:for real-first-time-p :initially t :then nil)
      (:for p :initially package :then (pop seen-package-list))
      (:while p)
      (:for (values p-sym storage-type) = (find-symbol name p))
      (when (and p-sym  ; символ 
                 (or (eq storage-type :external) ; должен быть внешним 
                     real-first-time-p  ; или мы смотрим в *package* и тогда он может быть внутренним тоже
                     ))
          ; если у нас несколько символов, то они могут совпадать. 
        (unless (eq p-sym first-sym-found) ; если не совпадают, то это сыграет более одного раза.
          (:count 1 :into cnt))
        (unless first-sym-found
          (setf first-sym-found p-sym first-package p first-status storage-type))
        (:collect p :into packs-found)
        )
      (:finally
       (return
        (case cnt
          (0 (values nil nil nil))
          (1 (values first-sym-found first-package first-status))
          (t (values first-sym-found packs-found :ambigious))))
       ))))


(defun see-packages-find-symbol (name &optional (default-package *package*))
  "Returns two values: 
1. List of (symbol . some-package-which-it-is-seen-in) for each symbol with the name
2. T is symbol name is found in default-package
"
  (iter
    (:with sym-found = nil)
    (:with found-in-package-itself = nil)
    (:for real-first-time-p :initially t :then nil)
    (:for p in (if (eq default-package (find-package :keyword)) 
                   default-package
                 (cons default-package nil #|2012-08-27 (package-seen-packages-list default-package)|#)))
    (:for (values p-sym storage-type) = (find-symbol name p))
    (when (and p-sym  ; символ 
               (or (eq storage-type :external) ; должен быть внешним 
                   real-first-time-p  ; или мы смотрим в *package* и тогда он может быть внутренним тоже
                   ))
          ; если у нас несколько символов, то они могут совпадать. 
      (when real-first-time-p (setf found-in-package-itself t))
      (unless (eq p-sym sym-found) ; если не совпадают, то это сыграет более одного раза.
        (setf sym-found p-sym)
        (:count 1 :into cnt)
        (:collect (cons p-sym p) :into syms-found))
      )
    (:finally
     (return (values syms-found found-in-package-itself)))))

   
(defun readtable-case-advanced (rt)
  (let1 rt (ensure-readtable rt)
    (cond
     ((gethash rt *readtable-case-is-upcase-if-uniform*)
      (assert (eq (readtable-case rt) :preserve))
      :upcase-if-uniform)
     (t 
      (readtable-case rt)))))

(defun set-readtable-case-advanced (rt rtcase)
  (proga
    (let rt (ensure-readtable rt))
    (let good-rt (or (packages-seen-p rt) (gethash rt *readtable-uses-sbcl-reader-budden-tools-lispworks*)))
    (case rtcase
      (:upcase-if-uniform
       (assert good-rt () "Readtable ~S must be mangled by see-packages or enable-buddens-readtable-extensions to be set to upcase-if-uniform" rt)
       (setf (readtable-case rt) :preserve
             (gethash rt *readtable-case-is-upcase-if-uniform*) t)
       (when (typep good-rt 'readtable)
         (setf (readtable-case good-rt) :preserve))
       )
      (t
       (setf (readtable-case rt) rtcase)
       (when (typep good-rt 'readtable)
         (setf (readtable-case good-rt) :preserve))))))

(defsetf readtable-case-advanced set-readtable-case-advanced)


(defun find-symbol-with-advanced-readtable-case (name p rt starts-with-vertical-line)
  "Если (readtable-case rt) = :upcase-if-unform, то символы, у к-рых все ascii буквы - 
   в нижнем регистре, ищутся только в верхнем регистре. Все остальные буквы ищутся как есть без преобразования регистра"
  (let ((p-sym nil) (storage-type nil))
    (case (readtable-case-advanced rt)
      (:upcase-if-uniform
       (let ((same-case-p (and (not starts-with-vertical-line) (all-ascii-chars-in-same-case-p name))))
         (ecase same-case-p
           (:lowercase
            (setf (values p-sym storage-type) (find-symbol (string-upcase-ascii name) p))
            ; убираем неоднозначность. Теперь всё, что введено в одинаковом регистре, апкейсится. 
            )
           ((:uppercase :ignore-case nil)
            (setf (values p-sym storage-type) (find-symbol name p))))
         (values p-sym storage-type same-case-p)))
      (t (setf (values p-sym storage-type) (find-symbol name p))
         (values p-sym storage-type nil)))
    ))

(defun budden-tools-find-symbol (name p)
  "Should be used instead of normal find-symbol with buddens readtable extensions. Transforms readtable case, and does not 'find' forbidden symbols.
FIXME shadow find-symbol? FIXME rename"
  (proga
    (let p (or p *package*))
    (let result (find-symbol-with-advanced-readtable-case name p *readtable* nil))
    (and 
     (not 
      (member result (package-metadata-forbidden-symbol-names (ensure-package-metadata p)) :test 'string=)
      )
     result
     )
    ))

(proclaim '(ftype (function (string package readtable symbol) symbol)
                  fix-symbol-name-for-advanced-readtable-case))
(defun fix-symbol-name-for-advanced-readtable-case (name package rt starts-with-vertical-line same-case-p)
  ;"Хотим заинтёрнить имя name в пакет package. Преобразуем его к верхнему регистру, если он - в пакет keyword"
  "Хотим заинтёрнить имя name в пакет package. Преобразуем его к верхнему регистру, если он набран в нижнем регистре без ||"
  (declare (ignore package))
  (cond
   ((and ;(eq package *keyword-package*) 
         (eq same-case-p :lowercase)
         (not starts-with-vertical-line)
         (eq (readtable-case-advanced rt) :upcase-if-uniform))
    (setf name (string-upcase-ascii name)))
   (t name)))

(defun intern-check-forbidden (name package stream qualified-p)
  "Looks if the name is forbidden. Prior to call of the function, name should be transformed according to readtable-case conventions. Internal function, do not use it in your code."
  (let ((m (gethash (keywordize-package-designator package) 
                    *per-package-metadata*)))
    (when m
      (let ((fs (package-metadata-forbidden-symbol-names m)))
        (when (find name fs :test 'string=)
          (simple-reader-error stream "Symbol name ~S is in forbidden-symbol-names of ~A" name package))))
      ;(break)
    (multiple-value-bind (symbol-found success) (find-symbol name package)
      (cond
       ((and qualified-p
             (not success)
             (or (not m)
                 (not (package-metadata-allow-qualified-intern m))))
        (simple-reader-error stream "qualified-intern is not allowed for ~A while trying to intern ~S" package name))
       ((and (not success)
             m
             (package-metadata-interning-is-forbidden m))
        (simple-reader-error stream "interning-is-forbidden for ~A while trying to intern ~S" package name))
       (success
        symbol-found)
       (t 
        (intern name package))))))

#|FIXME Delete me (defun check-symbol-forbidden (symbol package)
  "Check if the symbol's name is forbidden in the package. Prior to call of the function, name should be transformed according to readtable-case conventions. This is interal function, don't use it in your code"
  (when (member symbol (package-metadata-forbidden-symbol-names (ensure-package-metadata package)) :test 'string=) ;
    ; find with string= so that to intercept symbol with the wrong name even if someone evil have uninterned our 'true' forbidden symbol
    (error "Symbol ~S is forbidden in ~A" symbol package)
    )
  symbol 
  )|#

(defvar *show-package-system-vars-id* 0)

(defun new-show-package-system-vars-id () 
  #+nil (incf *show-package-system-vars-id*))

(defun trace-into-text-file (s)
  (declare (ignorable s))
  #+nil (with-open-file (oo (merge-pathnames "lisp.trace.txt" *default-pathname-defaults*) :direction :output :if-does-not-exist :create :if-exists :append)
          (write-string (str+ (the* string s) "
") oo)))




 
