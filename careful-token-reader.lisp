; -*- coding: windows-1251-dos; -*-

(in-package :budden-tools)
(setf *readtable* (copy-readtable nil))

(defun unread-char* (char stream) "���������� ����� stream"
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

#+nil (iter:iter ; ����� ������� � ��� ���������� �����? 
  (:for i :from 0 to 255) 
  (:for c := (code-char i))
  (setf (elt *char-table* i)
        (cond
         ((member c '(#\.)) :dot) ; symbol names starting from these would be misinterpreted
         ((eql c #\:) :colon)           ; ����� ���� ������� �������� ����
         ((eql c #\() :open-brace)      ; ����� ���� ������� �������� ����
         ((test-does-not-terminate-token c) :does-not-terminate-token)
         ((test-whitespace[2]p c) :whitespace[2])
         ((test-multiple-escape-p c) :multiple-escape)
         ((test-single-escape-p c) :single-escape))
   ))

(defmacro symbol-readmacro (symbol) `(get ,symbol 'symbol-readmacro))

#| FIXME!!!!! ���� ��� �� �� ����� ��������� �� ���, �������� ��� ������ � ������������ ���������� �� ��������� � ��������:
*readtable*
- keywords (����� �� ���������)
- uninterned ������� (����� �� ��������� ��� ���� ��� ����� ���� ���������, ��)


(defun reintern (stream symbol package-sym num-of-colons)
  "������� ������ � ����� �� ������ �������� *package* � *my-packages*
���� ��� �� ������, �� ���������� ��� ��� ����. ���� ��������, �� ��������.
package-sym ���������� ������� ������, � ������� �� ������� ���. num-of-colons
����������, ������� ��������� ��������� ��� ������� �� ����� ������"
  (unless (symbolp symbol) (return-from reintern symbol))
  (let1 res
      (let1 nm (symbol-name symbol)
        (cond
         (package-sym
          (let1 pack (find-package package-sym)
            (multiple-value-bind (target-sym storage-type)
                (find-symbol nm pack)
              (cond
               ((null target-sym)
                (cerror "Go on and intern it" "When reading from ~S, symbol ~A::~A does not exist at all" stream package-sym nm)
                (intern nm package-sym)
                )
               ((and (= num-of-colons 1)
                     (not (eq storage-type :external)))
                (simple-reader-error stream "~A is not an external symbol in ~A" nm package-sym))
               (t target-sym)))))
         (t ; ����� �� ������. ���� ������ �� ���� �������, �� ������� �� ������ ������ � �����. 
          (iter
            (:with sym-found = nil)
            (:for p in (if (eq *package* (find-package :keyword)) 
                           *package*
                         (cons *package* *my-packages*)))
            (:for (values p-sym storage-type) = (find-symbol nm p))
            (when (and p-sym  ; ������ 
                       (or (eq storage-type :external) ; ������ ���� ������� 
                           (:first-time-p)  ; ��� �� ������� � *package* � ����� �� ����� ���� ���������� ����
                           ))
          ; ���� � ��� ��������� ��������, �� ��� ����� ���������. 
              (unless (eq p-sym sym-found) ; ���� �� ���������, �� ��� ������� ����� ������ ����.
                (setf sym-found p-sym)
                (:count 1 :into cnt))
              (:collect p :into packs-found)
              )
            (:finally
             (return
              (case cnt
                (0 (intern nm *package*))
                (1 sym-found)
                (2 (simple-reader-error stream "symbol name ~A is ambigious between ~S" 
                                        nm packs-found)))))))
         ))
    (when (symbolp res)
      (let1 readmacro (symbol-readmacro res)
        (break)
        (when readmacro 
          (return-from reintern (funcall readmacro stream res)))))
    res
    )) |#



#| (defun collect-duplicates (list &rest key-args &key key test test-not)
  "Sub-optimal. Might sort, but does not..." 
  (declare (ignore key test test-not))          
  (iter 
    (:with known-values) ; don't forget to download iterate-keywords ;)
    (:with old-known-values) 
    (:for x in list)
    (setf old-known-values known-values known-values 
	  (apply 'adjoin x known-values key-args))
    (when (eq old-known-values known-values)
      (:collect x)))) |#

#| (defun interpret-compound-token (stream token)
  "��������� �����, ����������� �� read-token-with-colons � ������������ � char"
  (cond 
   ((null token) token) ;; ��������, #-
   (t
    (case (length token)
      (1 (reintern stream (car token) nil nil))
      (3 (let1 2nd  (second token)
           (or (and (consp 2nd)
                    (eq (car 2nd) 'colons)) 
               (simple-reader-error stream "Strange compound token ~S" token))
           (reintern stream (third token) (first token) (cdr 2nd))))
      (t (simple-reader-error stream "Strange compound token ~S" token)))))) |#


(defmacro with-good-readtable-2 ((&key (ensure-this-is-a-bad-one t)) &body body)
  "���������� readtable ������ ���� �������� � ������� see-packages-on"
  (with-gensyms (good)
    `(proga
       (let ,good (gethash *readtable* *my-readtable-to-good-readtable*))
;       (print '(:good-readtable-is ,good))
       ,@(when ensure-this-is-a-bad-one
           `((assert ,good nil "with-good-readtable: ~A is not a hp-readtable" *readtable*)))
       (let *readtable* (or ,good *readtable*))
       ,@body
       )))

(defun starting-colon-reader (stream char)
  (proga  
    ;(break)
    (let *token-starts-with-vertical-line* nil)
    (setf char (read-char stream))
    (let *package* *keyword-package*)
    (setf *have-colon* nil)
    (values (read-token-with-colons-1 stream char))
    ))
 

;;;; open-paren for symbol-readmacro
(defvar *reading-parens* nil "���� ������, �� �� ��������� ������ ������ ������")
(defvar *functions-to-call-when-paren-is-closing* nil
       "����� ����� ���� ������� �� ���������� (���������-������ �����), �-��� �� ������� �� �������� ������")

(let ((default-open-paren-reader (get-macro-character #\( (copy-readtable nil))))
  (defun paren-reader-with-closing-paren-notification (stream char)
    "���� ������ readera ���-�� �������� �-��� � *functions-to-call-when-paren-is-closing*,
�� ��� �-� ����� ������� ��� ����������� ������ (...) � ������� � ������ �� ���������, ���������� ���������" 
    (let* ((position (extract-file-position stream))
           (*reading-parens* (cons position *reading-parens*))
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
    (warn "������� ��������� �������� �� ������ ����������� ������ ��� ������ ������")))

(defun check-correct-use-of-a-car-symbol-readmacro (object)
  (when *reading-parens*
    (push-function-to-call-when-paren-is-closing
     (lambda (result stream)
       (assert (consp result) () 
         "Something wrong with symbol readmacro: list reader on ~S returned atom ~S" stream result)
       (assert (eq object (car result)) ()
         "In ~S, symbol-readmacro should be at the first position in a list" result)
       (assert (null (cdr result)) ()
         "car-symbol-readmacro should have read entire list in ~S" result)
       (car result))))
    object)

(defun it-is-a-car-symbol-readmacro (object-read)
  "���� ����������� symbol-readmacro-reader, �� ����� ����� ���������:
���� readmacro ��������� ������ ������� ������, �� ������� ������ ��������� �� �� 
����������� ������� ������ (�� ������� �) � ������� ���� ��������. 
� ��������� ������, �������� ������. 
����������� ������ ������������ ������ symbol-readmacro ������ �������� �� ������, 
� ��������, ������������ symbol-readmacro-reader.
���� readmacro ��������� ��� ������� ������, �� ������ ���������� �� ����������. 
"
  (check-correct-use-of-a-car-symbol-readmacro object-read))

; (defun it-is-a-half-car-symbol-readmacro (object-read))
;  "�� �� �����, �� ��� ���������� �����


;;; end of open-paren for symbol-readmacro


(defun simple-reader-error (stream format-string &rest args )
  (error "~A in stream ~A" (apply 'format nil format-string args) stream))


(eval-when (:load-toplevel) (print "6--------------------------------------"))
(set-syntax-from-char #\: #\  *colon-readtable* *colon-readtable*)

(defun char-type (c) (elt *char-table* (char-code c)))

(defun hp-alias-map (p &key (resolve-hp-alias t))
  "Finds alias map for a package. p is a package designator. 
When resolve-hp-alias is true, p may also be a package alias which is
resolved in the scope of *package*" 
  (declare (ignorable p))
  #+org.tfeb.hax.hierarchical-packages
  (gethash 
   (the* not-null (if resolve-hp-alias (hp-find-package p) (apply-undecorated 'find-package (list p))))
   *per-package-alias-table*)
  #-org.tfeb.hax.hierarchical-packages
  '())

(defun (setf hp-alias-map) (new p &key (resolve-hp-alias t))
  "Example: (setf (budden-tools:hp-alias-map :lgrep) '((:p . :meta-parse))). TODO: check structure"
  ;; This one should never be called if HP is not loaded.
  (declare (ignorable new p))
  #+org.tfeb.hax.hierarchical-packages
  (setf 
   (gethash 
    (the* not-null (if resolve-hp-alias (hp-find-package p) (find-package p)))
    *per-package-alias-table*)
   new)
  #-org.tfeb.hax.hierarchical-packages
  (error "No hierarchical packages, so aliases will not work"))

(defun delete-hp-alias-map (p &key (resolve-hp-alias t))
  (declare (ignorable p))
  #+org.tfeb.hax.hierarchical-packages
  (remhash (the* not-null (if resolve-hp-alias (hp-find-package p) (find-package p)))
           *per-package-alias-table*))



#|
� ��������, ���� ��� �������� - ���� ������ �������, ������� ������ ��� ����. ��� ������
��� ��������, ����������� ������, �� ����� ��� ��������, ����������� �����. ������
�������, ������������ ����� - ������ ��������� a.b.c ������ ������� ���� - ������ ���. 
������, custom-reader-for-package ������ ���������, ��� ��� ����� ������� ������� read, �������
������ ����� read ������ �����, ������� ����������� ��������
|#

(defun get-custom-reader-for-package (package-designator)
  "custom-reader, ���� �� �������� (� ������� setf), ����� �� �� ���������, ��� � read. ���������� ��� ������ �� ��������� ��������� ������, �.�., ����� custom-reader-for-package ������ ���������, ��� ��� ����� ������� ������� read, ������� ������ ����� read ������ �����, ������� ����������� ��������"
  (let1 pm (gethash (keywordize-package-designator package-designator) 
                    *per-package-metadata*)
    (and pm (package-metadata-custom-reader pm))))

(defun get-custom-token-parsers-for-package (package-designator)
  "custom-token-parsers, ���� ��������� (� ������� setf) - ��� ������ function designators (��� funcall), ������� ���������� ����� ������� ��� ������ �������. ��� �������� �� ����: �����, ������ � �����. ���������� ��� ��������. ������ �������� - ��������� ������. ������ - t, ���� ������ ������, ����� - nil"
  (let1 pm (gethash (keywordize-package-designator package-designator) 
                    *per-package-metadata*)
    (and pm (package-metadata-custom-token-parsers pm))))
  

(defsetf get-custom-reader-for-package (package-designator) (new-value)
  (with-gensyms (md)
    `(proga
       (check-type ,new-value (or null symbol function))
       (let ,md (ensure-package-metadata ,package-designator))
       (setf (package-metadata-custom-reader ,md) ,new-value))))

(defsetf get-custom-token-parsers-for-package (package-designator) (new-value)
  (with-gensyms (md)
    (once-only (new-value)
      `(proga
         (check-type ,new-value (or null cons))
         (loop :for x :in ,new-value :do (check-type x (or symbol function)))
         (let ,md (ensure-package-metadata ,package-designator))
         (setf (package-metadata-custom-token-parsers ,md) ,new-value)))))

;; redefining from tfeb... 
(defun relative-package-name-to-package (name &optional (relative-to-package *package*))
  ;; Given a package name, a string, do a relative package name lookup.
  ;;
  ;; It is intended that this function will be called from find-package.
  ;; In Allegro, find-package calls package-name-to-package, and the latter
  ;; function calls this function when it does not find the package.
  ;;
  ;; Because this function is called via the reader, we want it to be as
  ;; fast as possible.
  (declare (optimize speed)
	   ;#+sbcl
	   ;(type simple-base-string name)
	   ;#-sbcl
	   (type string name))
  (flet ((relative-to (package name)
	   (declare (type string name))
           (if (string= "" name)
              package
              (org.tfeb.hax.hierarchical-packages::real-find-package
	       (concatenate 'simple-string
			    (package-name package) "." name))))
         (find-non-dot (name)
	   ;#+sbcl
	   ;(declare (type simple-base-string name))
	   ;#-sbcl
	   (declare (type string name))
           (do* ((len (length name))
                 (i 0 (1+ i)))
               ((= i len) nil)
             (declare (fixnum len i))
             (when (char/= #\. (char name i)) (return i)))))
    (when (char= #\. (char name 0))
      (let* ((last-dot-position (or (find-non-dot name) (length name)))
             (n-dots last-dot-position)
             (name (subseq name last-dot-position)))
        (cond ((= 1 n-dots)
               ;; relative to current package
               (relative-to relative-to-package name))
              (t
               ;; relative to our (- n-dots 1)'th parent
               (let ((p relative-to-package)
                     tmp)
                 (dotimes (i (1- n-dots))
                   (when (not (setq tmp (org.tfeb.hax.hierarchical-packages::package-parent p)))
                     (error 'simple-hierarchical-package-error
                            :package p
                            :format-control "The parent of ~a does not exist."
                            :format-arguments (list p)))
                   (setq p tmp))
                 (relative-to p name))))))))

;; redefining hp-find-package to know about qualified-package
;; note this was initially defined in hierarchial-packages with some conditionals
; 
(defun hp-find-package
    (name/package &optional (relative-to-package *package*) real-find-package-fn) 
    (declare (optimize speed))          ;this is critical code
    (let1 *package* *keyword-package* ; otherwise might crash on error messages
    (typecase name/package
      (package name/package)
      (t                                ;should be STRINGable
       ;; PN is package name, EPN is effective (aliased) name
       ;; if there is one
       (let* ((pn (string name/package))
              (map (hp-alias-map relative-to-package :resolve-hp-alias nil))
              (epn (and map (cdr (assoc pn map :test #'string=)))))
         ;; if there is an EPN, then do REAL-FIND-PACKAGE on it, 
         ;; otherwise use NAME/PACKAGE. not PN, in case it can do some
         ;; magic.  Otherwise look up a relative name.
         (or (if real-find-package-fn (funcall real-find-package-fn  (or epn name/package))
                                      (org.tfeb.hax.hierarchical-packages::real-find-package (or epn name/package)))
             (budden-tools::relative-package-name-to-package (or epn pn) relative-to-package)))))))


(defmacro hp-in-package (name/package)
  (let1 pack (hp-find-package name/package)
    (assert pack () "(Relative/alias) Package ~A not found in package ~A" name/package *package*)
    `(progn
       (in-package ,(package-name pack))
       (setf *readtable* ,(swank::guess-buffer-readtable pack))
       (values *package* *readtable*))))


(defun hp-relative-package-name-p (name)
  "True if string designates a relative package name"
  (char= #\. (elt (string name) 0)))



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
      (:with seen-package-list = (package-seen-packages-list package))
      (:for real-first-time-p :initially t :then nil)
      (:for p :initially package :then (pop seen-package-list))
      (:while p)
      (:for (values p-sym storage-type) = (find-symbol name p))
      (when (and p-sym  ; ������ 
                 (or (eq storage-type :external) ; ������ ���� ������� 
                     real-first-time-p  ; ��� �� ������� � *package* � ����� �� ����� ���� ���������� ����
                     ))
          ; ���� � ��� ��������� ��������, �� ��� ����� ���������. 
        (unless (eq p-sym first-sym-found) ; ���� �� ���������, �� ��� ������� ����� ������ ����.
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
                 (cons default-package (package-seen-packages-list default-package))))
    (:for (values p-sym storage-type) = (find-symbol name p))
    (when (and p-sym  ; ������ 
               (or (eq storage-type :external) ; ������ ���� ������� 
                   real-first-time-p  ; ��� �� ������� � *package* � ����� �� ����� ���� ���������� ����
                   ))
          ; ���� � ��� ��������� ��������, �� ��� ����� ���������. 
      (when real-first-time-p (setf found-in-package-itself t))
      (unless (eq p-sym sym-found) ; ���� �� ���������, �� ��� ������� ����� ������ ����.
        (setf sym-found p-sym)
        (:count 1 :into cnt)
        (:collect (cons p-sym p) :into syms-found))
      )
    (:finally
     (return (values syms-found found-in-package-itself)))))

(defun all-chars-in-same-case-p (s)
  (let ((all-downs t)
        (all-ups t)
        (up #\a)
        (down #\a))
    (declare (symbol all-ups all-downs))
    (declare (character up down))
    (iter 
      (:while (or all-ups all-downs))
      (:for c :in-string s)
      (declare (character c))
      (when all-ups
        (setf up ; (#+russian char-upcase-cyr #-russian char-upcase c)
              (char-upcase c))
        (unless (char= up c)
          (setf all-ups nil)))
      (when all-downs 
        (setf down ; (#+russian char-downcase-cyr #-russian char-downcase c)
              (char-downcase c))
        (unless (char= down c)
          (setf all-downs nil)))
      )
    (cond
     ((and all-ups all-downs) :ignore-case)
     (all-ups :uppercase)
     (all-downs :lowercase)
     (t nil))))

  
#|(defun all-chars-in-same-case-p (s)
  "��� �� ������� �������� � ��������� ��������?
��������� ��������: 
:ingore-case - ��� ������� ����� ������� � ������ ���������
:uppercase - ��� � ������� ��������
:lowercase - ��� � ������
:capitalized - ������ � �������, ��������� - ���� � ������, ���� ��� ��������� ��� �������"
  (let ((all-downs t)
        (all-ups t)
        (capitalized t)
        (up #\a)
        (down #\a))
    (declare (symbol all-ups all-downs capitalized))
    (declare (character up down))
    (iter 
      (:while (or all-ups all-downs))
      (:for c :in-string s)
      (declare (character c))
      (when all-ups
        (setf up ; (#+russian char-upcase-cyr #-russian char-upcase c)
              (char-upcase-ascii c))
        (unless (char= up c)
          (setf all-ups nil)
          (when (iter:first-time-p)
            (setf capitalized nil))
          ))
      (when all-downs 
        (setf down ; (#+russian char-downcase-cyr #-russian char-downcase c)
              (char-downcase-ascii c))
        (unless (char= down c)
          (setf all-downs nil)))
      (when (and capitalized (not (iter:first-time-p)))
        (setf down (char-downcase-ascii c))
        (unless (char= down c)
          (setf capitalized nil)))
      )
    (cond
     ((and all-ups all-downs) :ignore-case)
     (all-downs :lowercase)
     (all-ups :uppercase)
     (capitalized :capitalised)
     (t nil))))|#



#+russian 
(trivial-deftest::! all-chars-in-same-case-p 
                    (list (budden-tools::all-chars-in-same-case-p "������")
                          (budden-tools::all-chars-in-same-case-p "������")
                          (budden-tools::all-chars-in-same-case-p "������")
                          (budden-tools::all-chars-in-same-case-p "aureki")
                          (budden-tools::all-chars-in-same-case-p "AUReki")
                          (budden-tools::all-chars-in-same-case-p "AUREKI")
                          )
                    (list :ignore-case :ignore-case :ignore-case
                          :lowercase nil :uppercase))
    
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
    (let good-rt (packages-seen-p rt))
    (case rtcase
      (:upcase-if-uniform
       (assert good-rt () "Readtable ~S must be mangled by see-packages to be set to upcase-if-uniform" rt)
       (setf (readtable-case rt) :preserve
             (readtable-case good-rt) :preserve
             (gethash rt *readtable-case-is-upcase-if-uniform*) t))
      (t
       (setf (readtable-case rt) rtcase)
       (when good-rt
         (setf (readtable-case good-rt) :preserve))))))

(defsetf readtable-case-advanced set-readtable-case-advanced)

(defun xlam-package-readtable-case (rt)
  (proga 
    (let rtcase (readtable-case-advanced rt))
    (case rtcase
      (:upcase-if-uniform :preserve)
      (t rtcase))))

; ���������: 1. ������� �� ����� ascii � ������ �������� ������ ������ � ������� ��������
; ��� ��������� - ������ "��� ����"
; 2. ��� keywords ������������� � �������� �������� � ������ ������
(defun find-symbol-with-advanced-readtable-case (name p rt starts-with-vertical-line)
  (let ((p-sym nil) (storage-type nil))
    (case (readtable-case-advanced rt)
      (:upcase-if-uniform
       (let ((same-case-p (and (not starts-with-vertical-line) (all-chars-in-same-case-p name))))
         (ecase same-case-p
           (:lowercase
            (setf (values p-sym storage-type) (find-symbol (string-upcase-ascii name) p))
            ; ������� ���������������. ������ ��, ��� ������� � ���������� ��������, ����������. 
            ; (when (and (not storage-type) 
            ;           (not (eq p *keyword-package*)) ; ��������� - ������ � ������� ��������
            ;           )
            ;  (setf (values p-sym storage-type) (find-symbol name p)))
            )
           ((:uppercase :ignore-case nil)
            (setf (values p-sym storage-type) (find-symbol name p))))
         (values p-sym storage-type same-case-p)))
      (t (setf (values p-sym storage-type) (find-symbol name p))
         (values p-sym storage-type nil)))
    ))

(defun budden-tools-find-symbol (name p)
  "Untested! Intended to be used instead of normal find-symbol. Transforms readtable case, and does not 'find' forbidden symbols.
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
  ;"����� ���������� ��� name � ����� package. ����������� ��� � �������� ��������, ���� �� - � ����� keyword"
  "����� ���������� ��� name � ����� package. ����������� ��� � �������� ��������, ���� �� ������ � ������ �������� ��� ||"
  (cond
   ((and ;(eq package *keyword-package*) 
         (eq same-case-p :lowercase)
         (not starts-with-vertical-line)
         (eq (readtable-case-advanced rt) :upcase-if-uniform))
    (setf name (string-upcase-ascii name)))
   (t name)))

(defvar +some-uninterned-symbol+ '#:some-uninterned-symbol)

(defun intern-check-forbidden (name package)
  "Looks if the name is forbidden. Prior to call of the function, name should be transformed according to readtable-case conventions. Internal function, do not use it in your code."
  (proga
    (let m (gethash (keywordize-package-designator package) 
                    *per-package-metadata*))
    (when m
      (let fs (package-metadata-forbidden-symbol-names m))
      (assert (null (find name fs :test 'string=)) () "Symbol name ~S is forbidden in ~A" name package))
    (intern name package)))

(defun check-symbol-forbidden (symbol package)
  "Check if the symbol's name is forbidden in the package. Prior to call of the function, name should be transformed according to readtable-case conventions. This is interal function, don't use it in your code"
  (when (member symbol (package-metadata-forbidden-symbol-names (ensure-package-metadata package)) :test 'string=) ;
    ; find with string= so that to intercept symbol with the wrong name even if someone evil have uninterned our 'true' forbidden symbol
    (error "Symbol ~S is forbidden in ~A" symbol package)
    )
  symbol 
  )

(defun reintern-1 (stream token default-package rt starts-with-vertical-line)  
  "��������� ���-��. ��������� ��� � �������, ������������ default-package (�� ������ ��� - *package*), *package-stack*, *colon-no-stack*. ����� �������� ������ ���� ��� ��� 
������� �������, �.�. ���������� readmacro"
  (proga function
    (typecase token
      (symbol 
       (proga 
         (let* name (symbol-name token) 
           qualified-package (when *have-colon* (car *package-stack*))
           qualified-colon-no (when *have-colon* (car *colon-no-stack*))
           package (or qualified-package default-package)
           package-kwd (keywordize-package-designator package)
           custom-token-parsers (unless starts-with-vertical-line (get-custom-token-parsers-for-package package-kwd))
           )
         (setf *have-colon* nil)
         (assert (or (null token) (eq (symbol-package token) *xlam-package*)))
         (unintern token *xlam-package*)
         (when stream ; stream ����� ���� nil ��� ������ �� decorated-get-symbol-from-point
           (dolist (parser custom-token-parsers)
             (multiple-value-bind (result parsed) (funcall parser stream name package)
               (when parsed
                 (return-from function (values result t))))))
         (let res 
           (cond ; unqualified symbol
            ((null qualified-package)
             (iter
               (:with sym-found = +some-uninterned-symbol+)
               (:for real-first-time-p :initially t :then nil)
               (:for p in (if (eq package *keyword-package*) 
                              (list package)
                            (cons package (package-seen-packages-list package))))
               (:for (values p-sym storage-type same-case-p) = (find-symbol-with-advanced-readtable-case name p rt *token-starts-with-vertical-line*))
               (when (and storage-type  ; ���� ����� ������
                          (or (eq storage-type :external) ; ������ ���� ������� 
                              real-first-time-p  ; ��� �� ������� � *package* � ����� �� ����� ���� ���������� ����
                              ))
          ; ���� � ��� ��������� ��������, �� ��� ����� ���������. 
                 (unless (eq p-sym sym-found) ; ���� �� ���������, �� ��� ������� ����� ������ ����.
                   (setf sym-found p-sym)
                   (:count 1 :into cnt))
                 (:collect p :into packs-found)
                 )
               (:finally
                (return
                 (case cnt
                   (0 (intern-check-forbidden (fix-symbol-name-for-advanced-readtable-case 
                                               name package rt starts-with-vertical-line same-case-p)
                                              package))
                   (1 (check-symbol-forbidden sym-found package))
                   (t (simple-reader-error stream "symbol name ~A is ambigious between ~S" 
                                           name packs-found)))))))
            (t ; qualified symbol
             (multiple-value-bind (sym status same-case-p)
                 (find-symbol-with-advanced-readtable-case name qualified-package rt starts-with-vertical-line)
               (setf name (fix-symbol-name-for-advanced-readtable-case name qualified-package rt starts-with-vertical-line same-case-p))
               (unless status ; symbol not found
                 (or (package-metadata-allow-qualified-intern (ensure-package-metadata qualified-package))
                     (let ((*readtable* (copy-readtable nil))) 
                       (cerror "Create symbol and use it" "Symbol ~A~A~A does not exist" 
                               (package-name qualified-package) 
                               (make-string qualified-colon-no :initial-element #\:)
                               name)))
                 (return-from function (intern-check-forbidden name qualified-package)))
               ; hence symbol is found 
               (check-symbol-forbidden sym qualified-package)
               (when (= qualified-colon-no 1) ; were looking for external symbol
                 (unless (eq status :external)
                   (cerror "Use symbol anyway" "Symbol ~S is not external in ~A" 
                           sym qualified-package)))
               sym))))
         (when (and (symbolp res) stream (not starts-with-vertical-line))
           (let1 readmacro (symbol-readmacro res)
             (when readmacro 
               ;(break)
               (return-from function (funcall readmacro stream res)))))
         res))
      (t token))))



(defun careful-token-reader (stream char) 
  (let1 *token-starts-with-vertical-line* nil
    (values (read-token-with-colons-1 stream char))
    ))

(defvar *show-package-system-vars-id* 0)

(defun new-show-package-system-vars-id () 
  #+nil (incf *show-package-system-vars-id*))

(defun new-show-package-system-vars-id-2 () 
  #+nil
  (incf *show-package-system-vars-id*))

(defun trace-into-text-file (s)
  (declare (ignorable s))
  #+nil (with-open-file (oo "c:/lisp.trace.txt" :direction :output :if-does-not-exist :create :if-exists :append)
    (write-string (str+ (the* string s) "
") oo)))

(defun show-package-system-vars (prefix id)
  (declare (ignorable prefix id))
  #+nil (with-open-file (oo "c:/lisp.trace.txt" :direction :output :if-does-not-exist :create :if-exists :append)
    (proga
      (macrolet d (var)
        `(format oo "~A ~A:~A=~A~%" prefix id (symbol-name ',var) 
                 (typecase ,var 
                   (package (package-name ,var))
                   (null "NIL")
                   (readtable (readtable-name ,var))
                   (symbol (symbol-name ,var))
                   (t "???"))))
      (terpri oo) 
;      (d *package*)
      (d *real-package*)
;      (d *last-used-real-package*)
;      (d *in-with-xlam-package*)
;      (d *readtable*)
      )))

(defun hp-find-package-with-advanced-readtable-case (string starts-with-vertical-line)
  (hp-find-package (if starts-with-vertical-line string (string-upcase string)) ; FIXME? 
                   ))

  
(defvar *package-designator-starts-from-vertical-line* nil)  
(defvar *symbol-name-starts-from-vertical-line* nil)  
(defvar *token-starts-with-vertical-line* nil)

(defun read-token-with-colons-1 (stream char)
  "������ ����� �� ���������. ��������, ������ � ���� ������� � �������� read"
  (proga function
    (setf *token-starts-with-vertical-line* (eql char #\|))
    (let the-package *package*)
    (let rt-to-restore *readtable*)
    (let *reading-up-to-colons* *reading-up-to-colons*) ; ��� thread-safety
    (let *have-colon* *have-colon*)
    (let *package-stack* *package-stack*)
    (let *colon-no-stack* *colon-no-stack*)
    (setf stream (unread-char* char stream))
    (when *read-suppress*
      (return-from function (with-good-readtable-2 () (read stream))))
;    (let *readtable* *colon-readtable*)
    (let result nil)
    (setf 
     result 
     (proga 
       (let tok (with-xlam-package-2 (make-colon-readtable rt-to-restore)
                  (read-preserving-whitespace stream nil nil)))
       (iter ; ������� ����� �� �����. �� ���������� ��� ������ ���� "������", �������
             ; ����� ���� ���� ��������, ���� package designator
             ; ��� �� ���? ���������, ��� ��� ���-��? 
         (:for cnt :from 0)
         (:for c :next (read-char stream nil nil))
         (:with local-have-colon = nil)
         (unless c ; ������ ���.  FIXME - ������ "cl-user:" ���� ���� ��������
           (return-from function (reintern-1 stream tok the-package rt-to-restore *token-starts-with-vertical-line*)))
         (case c
           (#\:
            (setf local-have-colon t)
            (when (and (> cnt 2) (not *read-suppress*)) ; ������� 3 ��������� - ����������� ������������ ������ + @?
              (simple-reader-error stream "To many colons in ~S" result))
               ; ���� ��� ���������� ����� ���������, �� ������ �� ������ - cnt ���������� � 
               ; ��������� ��������� ������
            )
           (t ; �� ���������
            (setf stream (unread-char* c stream))
            (cond 
             (local-have-colon ; �� ��������� ������ �� ���������
              (proga
                (let pack nil)
                (cond ((and (string= (string tok) "_") *package-stack*)
                       (setf pack (or (second *package-stack*) the-package))
                       (setf cnt 2) ; ���� ���� ���� ���� ���������, ���� �� �� ����� �������, ������� ����������
                       (pop *package-stack*)
                       (pop *colon-no-stack*)
                       (setf *have-colon* nil) ; ������ ���, ��� ��������� � ������ �����
                       )
                      (t ; ����� ������������ ������, � ���� ������������ - �� "_"
                       (unless (eq the-package *xlam-package*) 
                         (push the-package *package-stack*)
                         (push cnt *colon-no-stack*))
                       (setf pack (hp-find-package-with-advanced-readtable-case (string tok) *token-starts-with-vertical-line*))
                       (setf *have-colon* t)
                       (unless pack 
                         (loop 
                          (cerror "Retry" "No ~A package found" tok)))))
                (push pack *package-stack*)
                (push cnt *colon-no-stack*)
                (let custom-token-reader (get-custom-reader-for-package pack))
                (when custom-token-reader
                  (return-from function 
                    (funcall custom-token-reader stream t nil t)))
                (let id (new-show-package-system-vars-id))
                (show-package-system-vars "read-token-with-colons: before" id)
                ;;; ������ �� �� ����� ����������� ������� ������ - ��� ������� ������.
                ;;; � ��� ����� ���� ������� ������ �� ��. ��������� � �� ����� ������� 
                ;;; ������ ���������� symbol-readmacro
                ;;; �� ���� �����, �� �������� �������� �� �������� � see-packages. 
                #+nil (let *readtable* (swank::guess-buffer-readtable (package-name pack)))
                (return-from function (read stream t nil t))
                ))
             (t ; �� ��������� � �� ���� ���������
              ;
              (return-from function (reintern-1 stream tok the-package rt-to-restore *token-starts-with-vertical-line*))
              )
             )
            )
           ))
       ))))

           

(defun token-delimiterp (c) 
  (not (eq :does-not-terminate-token 
           (elt *char-table* (char-code c)))))

 
#|
�� ��� �� ��������
(defun def-symbol-readmacro-reader (stream symbol)
  (declare (ignore symbol))
  (it-is-a-car-symbol-readmacro)
  (let* ((symbol-to-define (with-good-readtable-2 (:ensure-this-is-a-bad-one nil) (read stream)))
         (rest (prog1 
                   (read-delimited-list #\) stream)
                 (unread-char #\) stream))
               ))
    (assert (symbolp symbol-to-define))
    `(do-def-symbol-readmacro ,symbol-to-define ,@rest) *DEBUG-IO*))

(setf (symbol-readmacro (intern "DEF-SYMBOL-READMACRO" :budden-tools))
      #'def-symbol-readmacro-reader)

|#