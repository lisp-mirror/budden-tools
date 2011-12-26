; -*- coding: windows-1251-dos; -*- 
; ��� ������ ���������� ����� ����� � sbcl ����� ������ (setf sb-impl::*default-external-format* :windows-1251) 

(in-package :budden-tools)

(defvar *keyword-package* (find-package :keyword))

(defun 1-to-list (x) 
  "Makes a list from atom, nil from nil and keeps list intact"
  (cond 
   ((consp x) x)
   (x (list x))
   (t nil)))
    
(defun list-to-1 (x)
  "If x is a list of one item, returns that item. Otherwise, returns x itself"
  (cond
   ((atom x) x)
   ((cdr x) x)
   (t (car x))))

(defun mapcarcar (list)
  (mapcar 'car list))

(defun mapcarcadr (list)
  (mapcar 'cadr list))

(defun symbol+ (&rest syms) "Concatenates symbols and intern concatenated symbol to current package"
  (let ((name (apply #'concatenate 'string (mapcar (lambda (x) (if x (string x) "")) syms))))
    (assert (not (equal name "")))
    (intern name)))

(defun ignored (&rest ignore) 
        (declare (ignore ignore)) nil) 

(defun show-hash (z) (maphash (lambda (x y) (print (list x y))) z))

(defun maptree (fun tree)  ; � d:/lisp/interp/ut.lsp ���������� subst-2
   #+russian "�������� ���������� �� ������. � ������� ����� ������ ��������� ������� fun.
    ���������� ����� ������."
   #-russian "Walks tree, applying fun to any atom of it and collects results to the fresh isomorphic tree"
  (map 'list (lambda (x) (cond 
			  ((consp x) (maptree fun x)) 
			  (t (funcall fun x)))) tree))

(defun rmsubseq (seq &rest args &key from-end start end count)
  #+russian "������� �� ������������������ ��������� ��������"
  #-russian "Removes subsequence from sequence"
  (declare (ignore from-end start end count))
  (apply #'remove nil seq :test (constantly t) args))

(defun subseq1 (seq start &optional end)
  "'Safe' version of subseq which allows for invalid range"
  (let1 l (length seq)
    (subseq seq (min l start) (when end (min l end)))))

(defun direct-sum (items errmsg &key (test #'eql))
  #+russian "���������, ��� ������ �������� �����������������. ���������� �� ����� ���� ������ ��������� �� ������, 
   ������� ��������� ��� ���������"
  #-russian "Checks that items are non-intersecting sets in terms of test. If they are, returns their sum. Otherwise, shows error message"
  (let1 res nil
    (dolist (item items)
      (or (null (intersection res item :test test))
          (error errmsg res item))
      (setf res (append res item)))
    res))



(defun sequence-last (sequence &optional (n 1)) 
  "Returns last n (default 1) elements of sequence as a new fresh sequence of the same type. Err if n>length(sequence)"
  (let1 start (- (length sequence) n)
    (subseq sequence start)))

(defun map-dir (fn pathname &key dir-options (file-test #'identity) (dir-test #'identity) subdirs) "Iterates over all files. :file-test and dir-test are pathname filters. Subdirs can be :recurse, :skip, :msp (fn is called for subdirs too) :map-and-recurse (fn is called and recursion occurs). Output format is ((pathname . (fn pathname)) ... (:subdir pathname ((pathname . (fn pathname)) ...))). directory structure should not be modified by fn except by deletion of pathname"
  (let1 result nil
    (dolist (item (apply #'directory pathname dir-options))
      (let1 subdir #+lispworks (lispworks:file-directory-p item)
        #-lispworks (error "put file-directory-p predicat to map-dir definition")
        (cond 
         (subdir 
          (when (funcall dir-test item)
            (case subdirs
              #+lispworks (setf item (pathname (namestring item))) ; bug: directory returns are incorrectly cached
                           ; so (directory (car (directory root-dir))) returns incorrect value
              (print (directory (pathname (namestring item))))
              (:recurse (push `(:subdir ,item ,@(funcall #'map-dir fn (pathname (namestring item))
                                                         :dir-options dir-options :file-test file-test :dir-test dir-test
                                                         :subdirs subdirs))
                              result))
              (:skip nil)
              (:map (push `(,item ,@(funcall fn item))
                          result))
              (:map-and-recurse
               (push `(,item ,@(funcall fn item))
                          result)
               (push `(:subdir ,item ,@(funcall #'map-dir fn (pathname (namestring item))
                                                         :dir-options dir-options :file-test file-test :dir-test dir-test
                                                         :subdirs subdirs))
                              result)
               )
              (t (error "wrong :subdirs keyword")))))
         (t ; is a file
          (when (funcall file-test item)
            (push `(,item ,@(funcall fn item))
                  result)))
         )))
    (nreverse result))) 

(defun replace-subseq (seq new-subseq &key (start 0) end (type 'list))
  #+russian "�������� ���������������������, ������������ start � end, ������
����������������������. ����� ����� �� ���������, ��� ���� ���������� ��������� ������ �����, ��� sequence.
�������� ����� �������� (concatenate). ����� ����������"
  #-russian "Replaces subsequence by other subsequence (maybe of another length. Uses concatenate and hence is slow"
  (concatenate type (subseq seq 0 start) new-subseq (when end (subseq seq end))))


(defun struct-to-alist (s) "��������� ������ �� ��������� � alist"
  #+lispworks (multiple-value-bind (names values) (structure:structure-names-and-values s)
                `((:type . ,(type-of s))
                   ,@(loop for x in names for y in values collect `(,x . ,y))))
  #-lispworks (error "struct-to-alist not defined for this lisp version")
  ) 
                    
          
(defun str+ (&rest args) (apply 'concatenate 'string (mapcar 'string args))) (export 'str+)
(defun str++ (&rest args) (format nil "~{~A~}" args)) (export 'str++) :budden-tools

(defun non-empty-string-p (x) 
  #+russian "���������� x, ���� x - �� nil � �� ������ ������"
  (cond ((null x) nil) ((equal x "") nil) (t x)))

(defun string-or (&rest args)
  "Returns first non empty-string of its args. Things are coerced to strings with (string)" 
  (iter (:for s in args)
    (when (non-empty-string-p s)
      (return-from string-or (string s)))
    )
  "")


(defun careful-keywordize (symbol-or-string) 
  "Keywordize, ��������� ��� advanced-readtable-case"
  (let1 symbol-name 
      (etypecase symbol-or-string
        (symbol (symbol-name symbol-or-string))
        (string symbol-or-string))
    (or (find-symbol symbol-name *keyword-package*) 
        (find-symbol-with-advanced-readtable-case symbol-name *keyword-package* *readtable* nil)
        (intern symbol-name *keyword-package*))))

(defmacro dispatch-keyarg-simple (keyarg)
  #+russian "��� �������� ������� ���������� � apply. �� ����� ����, ��� ����� ��������� default,
supplied-p � �.�."
  `(when ,keyarg `(,(careful-keywordize ',keyarg) ,,keyarg)))

(defmacro dispatch-keyargs-simple (&rest keyargs)
  #+russian "�� ��, ��� � dispatch-keyarg-simple, �� ��� ���������� ���������� �����"
  `(append ,@(iter (:for keyarg :in keyargs) 
               (:collecting `(dispatch-keyarg-simple ,keyarg)))))



(defun careful-add-suffix-to-a-symbol (symbol &rest suffixes)
  "������� �������� � ������� �������� � ��� ��� ���� (����������) ��������. ���� ����� ������ ��������� 
� ������ ������� symbol, �� ����������. ���� ��� - ��������. ���� ������ ��������� - ������ ���������"
  (let* ((ssuffixes (apply 'str+ suffixes))
         (package (symbol-package symbol))
         (sname (symbol-name symbol)))
    (cond
     ((null package)
      (make-symbol (str+ sname ssuffixes)))
     (t 
      (or
       (find-symbol (str+ sname (string-upcase ssuffixes)) package)
       (find-symbol (str+ sname (string-downcase ssuffixes)) package)
       (find-symbol (str+ sname (string-upcase ssuffixes)) *package*)
       (find-symbol (str+ sname (string-downcase ssuffixes)) *package*)
       (error "Unable to find symbol ~S with suffix ~S in packages ~S and ~S in either case" sname ssuffixes package *package*)
       )))))


(defmacro dispatch-keyarg-full (keyarg) 
  #+russian "� ������ supplied-p. ������������, ��� supplied-p ����� ��� keyarg-supplied-p"
  (let1 package (symbol-package keyarg)
    (assert (not (eq package *keyword-package*)))
    (let1 supplied-p-symbol (careful-add-suffix-to-a-symbol keyarg '-supplied-p)
      `(when ,supplied-p-symbol `(,(careful-keywordize ',keyarg) ,,keyarg)))))
; FIXME ������, ����������� careful-keywordize

(defmacro dispatch-keyargs-full (&rest keyargs)
  `(append ,@(iter (:for keyarg :in keyargs) 
               (:collecting `(dispatch-keyarg-full ,keyarg)))))  


(defun path-to-a-file (filename) "d:/foo/file.ext --> d:/foo/" 
  (let1 p (pathname filename)
    (make-pathname 
     :host (pathname-host p) 
     :directory (pathname-directory p))))

(defun quit-lisp ()
  (let ((quit-symbol (or (find-symbol "QUIT" "CL-USER")
                         (find-symbol "EXIT" "CL-USER"))))
    (if (fboundp quit-symbol)
        (funcall quit-symbol)
      (warn "QUIT Function doesn't seem to exist in this lisp."))))


(defun up-dir (pathname) "d:/foo/bar/ --> d:/foo; d:/foo/bar/file.ext --> d:/foo"
  (let1 p (pathname pathname)
    (make-pathname :host (pathname-host p) :directory (butlast (pathname-directory p)))))


#| (defun read-file-into-string-old (filename)
  (with-open-file (in filename :direction :input)
    (loop :with res = ""
          :for x = (read-line in nil nil) :unless x :do (return res)
          :do (setf res (concatenate 'string res (format nil "~%") x))))) |#


(defun read-file-into-string (filename)
  (with-open-file (in filename :direction :input)
    (with-output-to-string (result)
      (iter (:for line :in-file in :using #'read-line)
        (princ line result)))))


(defun save-string-to-file (string filename)
  (with-open-file (out filename :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format out "~A" string)))

(defun add-string-to-file (string filename)
  (with-open-file (out filename :direction :output :if-does-not-exist :create :if-exists :append)
    (format out "~A" string)))
  

  
(defun assert-unique-symbols (list) 
  #+russian "��� ������ ����� ��� ��������. ����������, ��� �� ��������. ���� ���, �� ����� ������, ������ ������ � ������������ ������"
  #-russian "Assert that there are no duplicates in list of strings or symbols, in terms of string<"
  (reduce (lambda (x y) (when (string-equal x y) 
                          (cerror "continue" "~A is not unique in ~A" x list)) y) 
          (sort list 'string<) :initial-value nil))          

(defun list-to-alist (x) "���������� '(:a 1 :b 2) � '((:a . 1) (:b . 2)). ��������. ��������� splice-list"
  (let1 res nil
    (loop
     (push (cons (pop x) (pop x)) res)
     (when (null x) (return-from list-to-alist (nreverse res))))))

(defun splice-list (list &key to-alist) 
  "converts (a b c d ...) into ((a b) (c d) ...), or, 
if to-alist is true, to ((a . b) (c . d) ...)"
  (iter
    (:while list)
    (:for a = (pop list))
    (:for b = (pop list))
    (:collect (if to-alist (cons a b) (list a b)))
    ))

(defun alist-to-list (x) "���������� alist � ������ ((a . b) (c . d)) -> (a b c d)"
  (loop for (key . value) in x append (list key value)))

(defun unsplice-list (list &key from-alist)
  "�������� � splice-list"
  (if from-alist
    (alist-to-list list)
    (iter (:for (a . b) in list)
      (assert b)
      (:appending `(,a ,@b)))))

(defun flat-assoc (thing list &key test key) "���������� assoc �� �������� ������ ���� (:a 1 :b 2), ��� ����� �� ��� a-������� ((:a . 1) (:b . 2))"
  (apply 'assoc thing (list-to-alist list) `(,@(if test `(:test ,test)) 
                                             ,@(if key `(:key ,key)))))
(unexport 'flat-assoc) ; deprecate it. 


(defun assoc-getf* (list thing &rest keyargs &key default key test test-not) 
  "Just like assoc, but operates on flat lists rather than on alists, (:a 1 :b 2) instead of ((:a . 1) (:b . 2)).
Returns sublist starting from key found."
  (declare (ignore key test test-not))
  (let1 keyargs-w/o-default (copy-list keyargs)
    (remf keyargs-w/o-default :default)
    (loop :for x = list ; don't loop! iterate!
          :then (cddr x)
          :unless x
          :do (return nil)
          :when (apply #'find thing x :end 1 keyargs-w/o-default) 
          :do (return x) 
          :finally (return default) 
          )))

(defun getf* (list thing &rest keyargs &key default key test test-not) 
  "Getf with extended arguments. Setf expander is defined too. It pushes new key and value to list when key is
not found (unless :key is specified, which is a error) and returns value"
  (declare (ignore key test test-not default))
  (cadr (apply 'assoc-getf* list thing keyargs)))


(define-setf-expander getf* (place the-key &environment env &rest keyargs &key key test test-not)
  (declare (ignore key test test-not))
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
    (declare (ignore newval))
    (let ((store (gensym)) (p (gensym)))
      (values (cons p dummies)
	      vals
	      `(,store)
	      `(let ((,p (assoc-getf* ,place ,the-key ,@keyargs))) 
		(cond (,p (setf (cadr ,p) ,store) ,p)
		      (t (push ,store ,place) 
			 (push ,the-key ,place)
			 ,p)))
	      setter
	      getter))))


(defun collect-duplicates (list &rest key-args &key key test test-not)
  #+russian "�������� ������ ��� ������. ���� �������, ����� �������� ��� sequence (������������ find-if-not)"
  #-russian "Collect only a single duplicate of all duplicated entries"
  (declare (ignore key test test-not))          
  (iter:iter 
    (:with known-values) (:with old-known-values)
    (:for x in list)
    (setf old-known-values known-values known-values 
	  (apply 'adjoin x known-values key-args))
    (when (eq old-known-values known-values)
      (:collect x))))


;; FIXME �������� proga-expander
(defmacro mlvl-bind (vars values-form &body body) `(multiple-value-bind ,vars ,values-form ,@body))
(defmacro mlvl-call (function &rest forms) `(multiple-value-call ,function ,@forms))
(defmacro mlvl-list (form) `(multiple-value-list ,form))
(defmacro mlvl-prog1 (form &rest forms) `(multiple-value-prog1 ,form ,@forms))
(defmacro mlvl-setq (variables form) `(multiple-value-setq ,variables ,form))
(defconstant mlvls-limit multiple-values-limit)


(defmacro dynamic-let1 (place-form value &body body)
  "Deprecated. See pllet1"  
  (cl-utilities::with-gensyms 
   (saved-value)
   `(let1 ,saved-value ,place-form
      (setf ,place-form ,value)
      (unwind-protect
          (progn ,@body)
        (setf ,place-form ,saved-value)))))

(defmacro pllet1 (place-form value &body body)
  #+russian "��������� place-form � ���������� ��� ��������.
����������� ��� value. ��������� body ��� progn, �� ������ 
��������� place-form � ���������� value, ������� �� ���������"
  (cl-utilities::with-gensyms 
   (saved-value)
   `(let1 ,saved-value ,place-form
      (setf ,place-form ,value)
      (unwind-protect
          (progn ,@body)
        (setf ,place-form ,saved-value)))))

(defmacro smlet (&rest args) 
  "Another name for symbol-macrolet"
  `(symbol-macrolet ,@args))

(defmacro the* (typespec value)
  "Asserts type and returns value. If type do not match, errs"
  `(progn
     (assert (typep ,value ',typespec))
     ,value))

(defun tree-weight (tree) #+russian "����� ���������� ������ � ������"
  #-russian "number of conses in a tree (or smth like this :)"
  (cond 
   ((atom tree) 0)
   (t (+ 1 (tree-weight (car tree)) (tree-weight (cdr tree))))))

(defmacro show-expr (expr &optional (stream '*trace-output*))
  "Shows expression and its value on the trace-output"
  `(format ,stream "~S=~S~%" ',expr ,expr))

(defmacro cl-user::eval-when* (situations &body body)
  "Use #.(eval-when* (:read ...) body)"
  (when (member :read situations) (eval `(progn ,@body)))
  `(eval-when ,(remove :read situations) ,@body))

(defun print-if (maybe-stream result)
  "If maybe-stream evaluates to non-nil, result is printed to it.
As a short-hand, #\s means *STANDARD-OUTPUT*, #\t - *TRACE-OUTPUT*"
  (cond
   ((streamp maybe-stream)
    (print result maybe-stream))
   ((eql maybe-stream #\s)
    (print result *standard-output*))
   ((eql maybe-stream #\t)
    (print result *trace-output*))
   ((null maybe-stream) result)
   (t (error "Wrong stream"))))

(defun dotted-name-to-list (name)
  "A.B.C -> (A B C)"
  (cond ((symbolp name) 
         (let1 name2 (symbol-name name)
           (mapcar (lambda (x) (intern x)) 
                   (cl-utilities:split-sequence name2 (lambda (x) (eql x #\.)) :start 0))))
        (t name)))

(defun dotted-p (l) (values (last l 0) (butlast l 0)))


(defun string-designator-p (x) (typep x 'string-designator))

(deftype not-null () '(not null))


#+russian 
(defparameter *cyrillic-symbols* '(#\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\�)
)

(defparameter *reversible-cyrillic-translit-table* (make-hash-table :test 'eql))

(iter 
  (:for (from . to) :in
   (list-to-alist 
    (split-sequence:split-sequence-if 
     'cl-ppcre::whitespacep
     "� a � b � v � g � d � e � jo � zh � z � i � jj � k � l � m � n � o � p � r � s � t � u 
� f � kh � c � ch � sh � shh � w � y � q � eh � ju � ja 
� A � B � V � G � D � E � JO � ZH � Z � I � JJ � K � L � M � N � O � P � R � S � T � U 
� F � KH � C � CH � SH � SHH � W � Y � Q � EH � YU � YA" :remove-empty-subseqs t)))
  (setf (gethash (elt from 0) *reversible-cyrillic-translit-table*) to))


;;; FIXME ������� �������� ��������������
(defun translit-reversibly (string-designator)
  "���� string-designator � ���������� ������������������� ������"
  (declare (optimize speed))
  (let* 
      ((s (typecase (the* string-designator string-designator)
            (string string-designator)
            (t (string string-designator))))
       (l (length s))
       (res (make-string (* 3 l))))
;    (declare (dynamic-extent res)) �� ��������. ������? 
    (iter 
      (:with i = 0)
      (:for c in-string s)
      (:for out = (gethash c *reversible-cyrillic-translit-table* c))
      (etypecase out
        (string
         (let1 outl (length out)
           (setf (subseq res i (incf i outl))
                 out)))
        (character
         (setf (elt res i) out)
         (incf i)))
      (:finally (return (subseq res 0 i))))))


#+russian 
(let* ((numchars (/ (length *cyrillic-symbols*) 2))
       (up (make-hash-table :test #'eql))
       (down (make-hash-table :test #'eql)))
  (check-type numchars integer)
  (iter 
    (:for i from 0 to (- numchars 1))
    (:for j from numchars to (- (* 2 numchars) 1))
    (:for lochar = (elt *cyrillic-symbols* i))
    (:for hichar = (elt *cyrillic-symbols* j))
    (setf (gethash lochar up) hichar)
    (setf (gethash hichar down) lochar))

; non-toplevel
(defun char-upcase-cyr (char) #+russian "��������� ������� �� ������"
    (or (gethash char up) (char-upcase char)))

; non-toplevel
(defun char-downcase-cyr (char) #+russian "�������� ������� ��-������"
    (or (gethash char down) (char-downcase char)))

; non-toplevel
(defun char-upcase-ascii (char) #+russian "��������� ������� ������ ��� ��������"
  (#+:|LISPWORKS4.4| char-upcase #-:|LISPWORKS4.4| error char))

; non-toplevel
(defun char-downcase-ascii (char) 
  (#+:|LISPWORKS4.4| char-downcase #-:|LISPWORKS4.4| error char))

; non-toplevel
(defun char-equal-cyr (c1 c2) #+russian "���������� ������� � ������ ���������"
    (or (char-equal c1 c2)
        (char-equal (char-upcase-cyr c1) (char-upcase-cyr c2))))

; non-toplevel
(defun string-upcase-cyr (s)
    (map 'string 'char-upcase-cyr s))

; non-toplevel
(defun string-upcase-ascii (s)
  (#+:|LISPWORKS4.4| string-upcase #-:|LISPWORKS4.4| error s))

; non-toplevel
(defun string-downcase-ascii (s)
  (#+:|LISPWORKS4.4| string-downcase #-:|LISPWORKS4.4| error s))

; non-toplevel
(defun string-downcase-cyr (s)
    (map 'string 'char-downcase-cyr s))

; non-toplevel
(defun string-equal-cyr (s1 s2)
    (let* ((s1 (string s1))
           (s2 (string s2)))
      (or (string-equal s1 s2)
          (every 'char-equal-cyr s1 s2))))

; non-toplevel
(defun textual-equal-cyr (s1 s2)
  "�� �� ����� ���������� equal-cyr, �.�. �� ����� ������ �� ���� ����� ������. �� ���� ���."
  (flet ((stringify (x)
           (typecase x
             (string x)
             (symbol (string x))
             (t (prin1-to-string x)))))
    (string-equal-cyr (stringify s1) (stringify s2))))

; non-toplevel
(defun cyrillic-char-p (x) 
  (and (characterp x) 
       #+(and nil lispworks win32) 
       ; ���� �� ����� ����� ��-�� �
       (let1 c (char-code x)
         (or 
          (<= #.(char-code #\�) c #.(char-code #\�))
          (eq x #\�)
          (eq x #\�)))
       #-(and nil lispworks win32)
       (or (gethash x up) (gethash x down))
       t
       ))

)


;; �������������� ������� �������� ��� ������-������ � open-pipe
#+(and russian lispworks (or windows win32))
(defmacro define-open-pipe-character-translators ()
  (let ((lisp->dos (make-hash-table :test #'eql :size 256))
        (dos->lisp (make-hash-table :test #'eql :size 256)))
    (iter 
      (:for c in (append *cyrillic-symbols* '(#\�)))
      (:for sou = 
       (with-output-to-string (ou) 
         (system:call-system-showing-output
          (format nil "cmd /c echo ~A" c) :prefix "" :show-cmd nil :output-stream ou)))
      (:for ou = (elt sou 0))
      (setf (gethash c lisp->dos) ou)
      (setf (gethash ou dos->lisp) c)
      )
    ; (iter (:for (a b) :in-hashtable lisp->dos) (print `(,a ,(char-code b))))
    `(progn
       (defun lisp-string-to-dos (s)
         (declare (optimize (speed 3) (safety 0) (debug 0)))
         (let1 s (copy-seq s)
           (iter 
             (:for i from 0)
             (:for c in-string s)
             (:for ou = (gethash c ,lisp->dos))
             (when ou (setf (aref s i) ou)))
           s))
       (defun dos-string-to-lisp (s)
         (declare (optimize (speed 3) (safety 0) (debug 0)))
         (let1 dest (make-string (length s) :element-type 'lispworks:simple-char)
           (iter 
             (:for i from 0)
             (:for c in-string s)
             (:for ou = (gethash c ,dos->lisp))
             (setf (aref dest i) (or ou c)))
           dest))
       (compile 'lisp-string-to-dos)
       (compile 'dos-string-to-lisp)
       )))


(defmacro define-open-pipe-character-translators-by-sample (sample x-to-lisp lisp-to-x)
  "�������� �� ��������� ������������� ������, �������� ����� ��������� (concatenate 'string (append bu::*cyrillic-symbols* '(#\�))). 
   ��������� �-� x-to-lisp, ����� ��������� ������ �� ����� ��������� � lisp-to-x, ����� �������� � ��������"
  (let ((lisp->x (make-hash-table :test #'eql :size 256))
        (x->lisp (make-hash-table :test #'eql :size 256))
        (character-list (append *cyrillic-symbols* '(#\�))))
    (assert (= (length sample) (length (concatenate 'string character-list))))
    (assert (= (length sample) (length (remove-duplicates (concatenate 'list sample) :test '= :key 'char-code))) () "������� ������ ��������� ��� ��������� �����")
    (iter 
      (:for c :in character-list)
      (:for ou :in-sequence sample)
      (setf (gethash c lisp->x) ou)
      (setf (gethash ou x->lisp) c)
      )
    ; (iter (:for (a b) :in-hashtable lisp->dos) (print `(,a ,(char-code b))))
    `(progn
       (defun ,lisp-to-x (s)
         (declare (optimize (speed 3) (safety 0) (debug 0)))
         (let1 s (copy-seq s)
           (iter 
             (:for i from 0)
             (:for c in-string s)
             (:for ou = (gethash c ,lisp->x))
             (when ou (setf (aref s i) ou)))
           s))
       (defun ,x-to-lisp (s)
         (declare (optimize (speed 3) (safety 0) (debug 0)))
         (let1 dest (make-string (length s) :element-type 'lispworks:simple-char)
           (iter 
             (:for i from 0)
             (:for c in-string s)
             (:for ou = (gethash c ,x->lisp))
             (setf (aref dest i) (or ou c)))
           dest))
       (compile ',lisp-to-x)
       (compile ',x-to-lisp)
       )))
             
       
(defmacro _f (op place &rest args)
  "Macro from @cite{(Graham 1993)}.  Turns the operator @arg{op} into a
modifying form, e.g. @code{(_f + a b) @equiv{} (incf a b)}."
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro __f (op arg1 place &rest args)
  "��������� ���"
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,arg1 ,access ,@args)))
       ,set)))

(defmacro ensure-gethash-2 (key hash-table &optional default)
  "Like alexandria:ensure-gethash, but evaluates default only when key is not found"
  (once-only (key hash-table) 
    (with-gensyms (value ok) 
      `(multiple-value-bind (,value ,ok) (gethash ,key ,hash-table)
         (if ,ok
             (values ,value ,ok)
           (values (setf (gethash ,key ,hash-table) ,default) nil))))))


(defmacro symbol-macroletf-helper (store-vars writer-form reader-form)
  (declare (ignore store-vars writer-form))
  reader-form)

(define-setf-expander symbol-macroletf-helper (store-vars writer-form
                                                          reader-form)
  (values '() '() store-vars writer-form reader-form))

(defmacro symbol-macroletf ((&rest bindings) &body body
                            &environment environment)
  "Like SYMBOL-MACROLET but evaluate subforms just once up front."
  (loop with (vars vals store-vars writer-form reader-form)
        for (symbol place) in bindings
        do (setf (values vars vals store-vars writer-form reader-form)
                 (get-setf-expansion place environment))
        nconc (mapcar #'list vars vals)
          into let*-bindings
        collect `(,symbol (symbol-macroletf-helper ,store-vars ,writer-form
                                                   ,reader-form))
          into symbol-macrolet-bindings
        finally (return `(let* (,@let*-bindings)
                           (symbol-macrolet (,@symbol-macrolet-bindings)
                             ,@body)))))

(defun princ-to-string-delimited-list (delimiter list)
  "�������� ������ � ������������ ����� ������� ����� ����������"
  (with-output-to-string (s)
    (let1 firsttime t
      (dolist (x list) 
        (if firsttime (setf firsttime nil) (princ delimiter s))
        (princ x s)
        )))) :budden-tools
