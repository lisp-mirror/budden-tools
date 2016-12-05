;;; -*- Encoding: utf-8; -*-

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

(defun maptree (fun tree)  ; в d:/lisp/interp/ut.lsp называлась subst-2
   #+russian "Проходит рекурсивно по дереву. К каждому атому дерева применяет функцию fun.
    Получается такое дерево."
   #-russian "Walks tree, applying fun to any atom of it and collects results to the fresh isomorphic tree"
  (map 'list (lambda (x) (cond 
			  ((consp x) (maptree fun x)) 
			  (t (funcall fun x)))) tree))

;(defun copy-tree-with-structures (tree)
;  (

(defun rmsubseq (seq &rest args &key from-end start end count)
  #+russian "Удаляет из последовательности указанные элементы"
  #-russian "Removes subsequence from sequence"
  (declare (ignore from-end start end count))
  (apply #'remove nil seq :test (constantly t) args))

(defun subseq1 (seq start &optional end)
  "'Safe' version of subseq which allows for invalid range"
  (let1 l (length seq)
    (subseq seq (min l start) (when end (min l end)))))

(defun direct-sum (items errmsg &key (test #'eql))
  #+russian "Проверяет, что списки являются непересекающимися. Возвращает их сумму либо выдает сообщение об ошибке, 
   которое принимает два параметра"
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

(defun map-dir (fn pathname &key dir-options (file-test #'identity) (dir-test #'identity) subdirs) "Iterates over all files. List of files is acquired with cl-fad:list-directory and dir-options. :file-test and dir-test are pathname filters. Subdirs can be :recurse, :skip, :msp (fn is called for subdirs too) :map-and-recurse (fn is called and recursion occurs). Output format is ((pathname . (fn pathname)) ... (:subdir pathname ((pathname . (fn pathname)) ...))). directory structure should not be modified by fn except by deletion of pathname"
  (let1 result nil
    (dolist (item (apply #'cl-fad:list-directory pathname dir-options))
      (let1 subdir (cl-fad:directory-pathname-p item)
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


; (map-dir #'map-dir-example-fn "/s2/fc/" :subdirs :recurse)
#| An example of using map-dir       
 (defun sum-filesizes-grouped-by-extension (directory)
  (let (listing ; list of types and sizes
        (extension-to-total-size (make-hash-table :test 'equal))
        result ; list of results
        )
    (labels
        ((file-size (file) (with-open-file (stream file :element-type '(unsigned-byte 8))
                             (file-length stream)))
         (store-type-and-size (file)
           (push
            (list
             :path file
             :type (pathname-type (pathname file))
             :size (file-size file)
             )
            listing))
         (dir-filter (dir)
           (let ((dirname (car (last (pathname-directory dir)))))
             (not (member dirname '(".git") :test 'string=))
             ))
         )
      (map-dir #'store-type-and-size directory
               :dir-options '(:follow-symlinks nil)
               :dir-test #'dir-filter
               :subdirs :recurse)
      )
    (dolist (plist listing)
      (let (type size entry)
        (setf type (getf plist :type))
        (setf size (getf plist :size))
        (setf entry (gethash type extension-to-total-size (list :count 0 :size 0)))
        (incf (getf entry :count))
        (incf (getf entry :size) size)
        (setf (gethash type extension-to-total-size) entry)))
    
    (maphash
     (lambda (key val)
       (push `(,key ,val) result))
     extension-to-total-size
     )
    (_f sort result '>
        :key (lambda (item)
               (let ((entry (second item)))
                 (getf entry :size))))
    (dolist (x result)
      (destructuring-bind (key entry) x
        (format t "~%~A;~D;~D" key (getf entry :count) (getf entry :size))
        ))
    (values)
    ))
|#


(defun replace-subseq (seq new-subseq &key (start 0) end (type 'list))
  #+russian "Заменяет подпоследовательность, определяемую start и end, другой
подпоследовательностью. Длины могут не совпадать, при этом получается результат другой длины, чем sequence.
Работает очень медленно (concatenate). Нужно переделать"
  #-russian "Replaces subsequence by other subsequence (maybe of another length. Uses concatenate and hence is slow"
  (concatenate type (subseq seq 0 start) new-subseq (when end (subseq seq end))))


(defun struct-to-alist (s) "сохраняет тип и данные из структуры в alist"
  (assert (typep (class-of s) 'structure-class))
  #+lispworks (multiple-value-bind (names values) (structure:structure-names-and-values s)
                `((:type . ,(type-of s))
                   ,@(loop for x in names for y in values collect `(,x . ,y))))
  #+(and closer-mop (not lispworks))
  (let* ((slots (closer-mop:class-slots (class-of s)))
         (names (mapcar 'closer-mop:slot-definition-name slots)))
    `((:type . ,(type-of s))
      ,@(loop :for name :in names
              :for value = (slot-value s name)
              :collect `(,name . ,value))))
  #-(or lispworks closer-mop) (error "struct-to-alist not defined for this lisp version")
  )


(def-trivial-test::! struct-to-alist.1
                     (let (x)
                       (defstruct struct-to-alist.test.struct a b c)
                       (setf x (make-struct-to-alist.test.struct :a 5))
                       (struct-to-alist x)
                       )
                     `((:type . struct-to-alist.test.struct)
                       (a . 5)
                       (b)
                       (c)))
                     

(defun mandatory-slot (slot-name)
  (error "Обязательное поле ~S в defstruct не инициализировано" slot-name)) 

(defun copy-tree-of-structures (tree)
  ;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((copy-structure-and-its-slots (s)
           (let ((data (cdr (struct-to-alist s)))
                 (copy (copy-structure s)))
             (iter 
               (:for (name . value) :in data)
               (setf (slot-value copy name) (copy-tree-of-structures value)))
             copy)))
    (typecase tree
     (null tree)
     (cons
      (iter (:for x in tree)
        (:collect (copy-tree-of-structures x))))
     (structure-object
      (copy-structure-and-its-slots tree))
     (t tree))))
                    
          
(defun str+ (&rest args) (apply 'concatenate 'string (mapcar 'string args)))
(defun str++ (&rest args) (let ((*print-circle* nil)) (format nil "~{~A~}" args)))

(defun non-empty-string-p (x) 
  #+russian "Возвращает x, если x - не nil и не пустая строка"
  (cond ((null x) nil) ((equal x "") nil) (t x)))

(defun string-or (&rest args)
  "Returns first non empty-string of its args. Things are coerced to strings with (string)" 
  (iter (:for s in args)
    (when (non-empty-string-p s)
      (return-from string-or (string s)))
    )
  "")


(defun careful-keywordize (symbol-or-string) 
  "Keywordize, пригодный для advanced-readtable-case"
  (let1 symbol-name 
      (etypecase symbol-or-string
        (symbol (symbol-name symbol-or-string))
        (string symbol-or-string))
    (or (find-symbol symbol-name *keyword-package*) 
        (find-symbol-with-advanced-readtable-case symbol-name *keyword-package* *readtable* nil)
        (intern symbol-name *keyword-package*))))

(defmacro dispatch-keyarg-simple (keyarg)
  #+russian "Для передачи похожих аргументов в apply. Подразумевается, что вызывающая сторона не даёт
ключам значения по умолчанию. Если параметр равен nil, он не передаётся, в этом случае значение по умолчанию
будет взято из вызываемой функции"
  `(when ,keyarg `(,(careful-keywordize ',keyarg) ,,keyarg)))

(defmacro dispatch-keyargs-simple (&rest keyargs)
  #+russian "То же, что и dispatch-keyarg-simple, но для нескольких аргументов сразу"
  `(append ,@(iter (:for keyarg :in keyargs) 
               (:collecting `(dispatch-keyarg-simple ,keyarg)))))



(defun careful-add-suffix-to-a-symbol (symbol &rest suffixes)
  "пробует добавить к символу суффиксы в том или ином (одинаковом) регистре. Если такой символ находится 
в пакете символа symbol, то возвращает. Если нет - ругается. Если символ бездомный - просто добавляет"
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
  #+russian "Пусть есть вызываемая функция с &key параметром, и вызывающай с таким же &key параметром.
Мы хотим передать параметры так, чтобы умолчание было взято от вызываемой функции. Нужно в вызывающей функции задать параметр в виде
&key (param-name nil param-name-supplied-p), а в вызваемой - в виде &key (param-name default [supplied-p])" 
  (let1 package (symbol-package keyarg)
    (assert (not (eq package *keyword-package*)))
    (let1 supplied-p-symbol (careful-add-suffix-to-a-symbol keyarg '-supplied-p)
      `(when ,supplied-p-symbol `(,(careful-keywordize ',keyarg) ,,keyarg)))))
; FIXME похоже, понадобится careful-keywordize

(defmacro dispatch-keyargs-full (&rest keyargs)
  `(append ,@(iter (:for keyarg :in keyargs) 
               (:collecting `(dispatch-keyarg-full ,keyarg)))))  

(defmacro pass-keyarg (keyarg)
  "Имеется вызывающая функция и вызываемая, у них одинаковый &key arg. Хотим передать из вызывающей в вызываемую. Можно написать так:
 (apply 'вызываемая (pass-keyarg arg)), имя аргумента пишется в виде символа, а не в виде keyword-а"
  ``(,(careful-keywordize ',keyarg) ,,keyarg)
  )

(defmacro pass-keyargs (&rest keyargs)
  `(append ,@(iter (:for keyarg :in keyargs) 
               (:collecting `(pass-keyarg ,keyarg)))))


(defun path-to-a-file (filename) "d:/foo/file.ext --> d:/foo/"
  (make-pathname :defaults (pathname filename) :name nil :type nil))

(defun name-and-type-of-a-file (filename) "d:/foo/file.ext --> file.ext"
  (make-pathname :defaults (pathname filename) :host nil :device nil :directory nil))


(defun subdir-p (subdir super-dir) "Истина, если subdir является поддиректорией super-dir. В этом случае возвращает путь от super-dir до subdir. Работает только для абсолютных имён"
  (let*
      ((pp (pathname subdir))
       (psd (cl-fad:pathname-as-directory (pathname super-dir)))
       (ppns (namestring pp))
       (psdns (namestring psd))
       )
    (assert (cl-fad:pathname-absolute-p pp))
    (assert (cl-fad:pathname-absolute-p psd))
    (multiple-value-bind
        (success suffix)
        (alexandria:starts-with-subseq psdns ppns :test 'char= :return-suffix t)
      (cond
       (success (copy-seq suffix))
       (t nil)))))
  

(defun pathname-relative-to-super-dir (pathname super-dir) "d:/dir/subdir/file.ext, d:/dir -> subdir/file.ext or error"
  (let*
      ((pp (pathname pathname))
       (psd (cl-fad:pathname-as-directory (pathname super-dir)))
       (ppns (namestring pp))
       (psdns (namestring psd))
       )
    (assert (cl-fad:pathname-absolute-p pp))
    (assert (cl-fad:pathname-absolute-p psd))
    (multiple-value-bind
        (success suffix)
        (alexandria:starts-with-subseq psdns ppns :test 'char= :return-suffix t)
      (cond
       (success (copy-seq suffix))
       (t (error "pathname-relative-to-super-dir: ~S is not a super-dir of ~S" super-dir pathname))))))


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


(defun read-file-into-string (filename #+(or lispworks sbcl) &key #+(or lispworks sbcl) (external-format :default))
  (with-open-file (in filename :direction :input #+(or lispworks sbcl) :external-format #+(or lispworks sbcl) external-format)
    (with-output-to-string (result)
      (iter (:for line :in-stream in :using #'read-line)
        (princ line result)
        (terpri result)))))


(defun save-string-to-file (string filename &key (external-format #-(and lispworks6 mswindows russian) :default 
                                                                  #+(and lispworks6 mswindows russian) '(win32:code-page :id 1251)))
  (with-open-file (out filename :direction :output :if-does-not-exist :create :if-exists :supersede :external-format external-format)
    (format out "~A" string)))

(defun add-string-to-file (string filename)
  (with-open-file (out filename :direction :output :if-does-not-exist :create :if-exists :append)
    (format out "~A" string)))
  

  
(defun assert-unique-symbols (list) 
  #+russian "Дан список строк или символов. Убежадемся, что он уникален. Если нет, то выдаём ошибку, выводя список и неуникальынй символ"
  #-russian "Assert that there are no duplicates in list of strings or symbols, in terms of string<"
  (reduce (lambda (x y) (when (string-equal x y) 
                          (cerror "continue" "~A is not unique in ~A" x list)) y) 
          (sort list 'string<) :initial-value nil))          

(defun list-to-alist (x) "Превращает '(:a 1 :b 2) в '((:a . 1) (:b . 2)). Устарела. Используй splice-list"
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


(defun plist-names (list)
  (iter 
    (:while list)
    (:for name = (pop list))
    (pop list) ;value
    (:collect name)))

(defun plist-values (list)
  (iter
    (:while list)
    (pop list) ; name
    (:for value = (pop list))
    (:collect value)))
       

(defun alist-to-list (x) "Превращает alist в список ((a . b) (c . d)) -> (a b c d)"
  (loop for (key . value) in x append (list key value)))

(defun swap-pairs-in-plist (list)
  (iter
    (:while list)
    (:for name = (pop list))
    (:for value = (pop list))
    (:collect value)
    (:collect name)))
  

(defun unsplice-list (list &key from-alist)
  "Обратный к splice-list"
  (if from-alist
    (alist-to-list list)
    (iter (:for (a . b) in list)
      (assert b)
      (:appending `(,a ,@b)))))

(defun flat-assoc (thing list &key test key) "Возвращает assoc из плоского списка типа (:a 1 :b 2), как будто он был a-списком ((:a . 1) (:b . 2))"
  (apply 'assoc thing (list-to-alist list) `(,@(if test `(:test ,test)) 
                                             ,@(if key `(:key ,key)))))
(unexport 'flat-assoc) ; deprecate it. 


(defun assoc-getf* (list thing &rest keyargs &key key test test-not mandatory) 
  "Just like assoc, but operates on flat lists rather than on alists, (:a 1 :b 2) instead of ((:a . 1) (:b . 2)). Returns sublist starting from key found. If not found and mandtory is true, errs. If not found and mandatory is null, returns nil"
  (declare (ignore key test test-not))
  (let ((keyargs-copy (copy-list keyargs)))
    (remf keyargs-copy :mandatory)
    (do ((x list (cddr x)))
        ((null x)
         (cond
          (mandatory
           (error "In a list ~S, can't find ~S" list thing))
          (t
           nil)))
      (when (apply #'find thing x :end 1 keyargs-copy)
        (return x))
      )))

 
(def-trivial-test::! assoc-getf*.work
                     (assoc-getf* '(1 2 3 4) 1)
                     '(1 2 3 4))

(def-trivial-test::! assoc-getf*.test-key
                     (assoc-getf* '(("a") "b" ("c") "d") "C"
                                  :test 'string-equal
                                  :key 'car)
                     '(("c") "d"))

(def-trivial-test::! assoc-getf*.default
                     (assoc-getf* '(1 2 3 4) 5)
                     nil)

(def-trivial-test::! assoc-getf*.check
                     (typep 
                      (nth-value
                       1
                       (ignore-errors 
                         (assoc-getf* '(1 2 3 4) 5 :mandatory t)))
                      'error)
                     t)

(defun getf* (list thing &rest keyargs &key default key test test-not mandatory) 
  "Getf with extended arguments. If mandatory is true and key is not found, error is signalled. Setf expander is defined too"
  (declare (ignore key test test-not mandatory)) ; they're used via keyargs indeed
  (let*
      ((keyargs-copy (copy-list keyargs))
       place)
    (remf keyargs-copy :default)
    (setf place (apply #'assoc-getf* list thing keyargs-copy))
    (cond
     (place
      (cadr place))
     (t
      default))))

(define-setf-expander getf* (place the-key &environment env &rest keyargs &key key test test-not mandatory)
  "It pushes new key and value to list when key is not found (unless :key is specified, which is a error) and returns value. Mandatory means that the-key must already exist in the list stored in place."
  (declare (ignore key test test-not mandatory))
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
    (declare (ignore newval))
    (let ((store (gensym)) (p (gensym)))
      (values (cons p dummies)
	      vals
	      `(,store)
	      `(let* ((,p (assoc-getf* ,place ,the-key ,@keyargs))) 
		(cond (,p (setf (cadr ,p) ,store) ,p)
		      (t (push ,store ,place) 
			 (push ,the-key ,place)
			 ,p))
                ,store)
	      setter
	      getter))))

(def-trivial-test::! getf*.work
                     (getf* '(("a") "b" ("c") "d") "C"
                            :test 'string-equal
                            :key 'car
                            :mandatory t)
                     "d")

(def-trivial-test::! getf*.default
                     (getf* '(1 2 3 4) 5 :default 6)
                     6)

(def-trivial-test::! setf-getf*.1
                     (let ((l '(1 2 3 4)))
                       (setf (getf* l 1 :mandatory t) 5)
                       (setf (getf* l 0) 6)
                       l)
                     '(0 6 1 5 3 4))

(def-trivial-test::! setf-getf*.check
                     (block b
                       (handler-bind
                           ((simple-error #'(lambda (x) (declare (ignore x))
                                              (return-from b 234))))
                         (let ((l '(1 2 3 4)))
                           (setf (getf* l 5 :mandatory t) 6)
                           nil)))
                     234)


(defun collect-duplicates (list &rest key-args &key key test test-not)
  #+russian "Работает только для списка. Надо сделать, чтобы работало для sequence (использовать find-if-not). См. также kons:budden-tools-collect-duplicates"
  #-russian "Collect only a single duplicate of all duplicated entries"
  (declare (ignore key test test-not))
  (let ((all-duplicates-maybe-several-times
         (iter 
          (:with known-values) (:with old-known-values)
          (:for x in list)
          (setf old-known-values known-values)
          (setf known-values 
                (apply 'adjoin x known-values key-args))
          (when (eq old-known-values known-values)
            (:collect x)))))
    (apply 'remove-duplicates all-duplicates-maybe-several-times key-args)))

;; FIXME добавить proga-expander
(defmacro mlvl-bind (vars values-form &body body) `(multiple-value-bind ,vars ,values-form ,@body))
(defmacro mlvl-call (function &rest forms) `(multiple-value-call ,function ,@forms))
(defmacro mlvl-list (form) `(multiple-value-list ,form))
(defmacro mlvl-prog1 (form &rest forms) `(multiple-value-prog1 ,form ,@forms))
(defmacro mlvl-setq (variables form) `(multiple-value-setq ,variables ,form))
(defconstant mlvls-limit multiple-values-limit)

(setf (get 'mlvl-bind 'proga-implementation::proga-transformer) 
      'proga-implementation::open-up-if-3
      )

         



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
  #+russian "вычисляем place-form и запоминаем его значение.
Присваиваем ему value. Выполянем body как progn, на выходе 
вычисляем place-form и записываем value, которое мы запомнили"
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

#|(defmacro the* (typespec value)
  "Combines check-type and the"
  (let ((the-symbol (gensym (format nil "~A" value))))
    `(let ((,the-symbol ,value))
       (assert (typep ,the-symbol ',typespec))
       (the ,typespec ,the-symbol))))|#
        

(defmacro the* (typespec value)
  "Combines check-type and the"
  #-sbcl
  (once-only (value)
    `(progn
       (assert (typep ,value ',typespec)) 
       (the ,typespec ,value)))
  ;; в SBCL не нужно проверять тип, см. "Declarations as assertions"
  #+sbcl
  `(the ,typespec ,value)
  )

(defun implies (a b)
  (or b (not a)))

(defun exclusive-or (a b) (or (and (not a) b) (and a (not b))))

(defun tree-weight (tree) #+russian "Общее количество консов в дереве"
  #-russian "number of conses in a tree (or smth like this :)"
  (cond 
   ((atom tree) 0)
   (t (+ 1 (tree-weight (car tree)) (tree-weight (cdr tree))))))

(defmacro show-expr (expr &optional (stream '*trace-output*))
  "Shows expression and its value on the trace-output"
  (let1 e1 expr
    (once-only (e1)
      `(progn 
         (format ,stream "~%~S = ~S" ',expr ,e1)
         ,e1))))
     

(defmacro show-exprt (expr)
  `(show-expr ,expr t))

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


      
       
(defmacro _f (op place &rest args &environment env)
  "Macro from @cite{(Graham 1993)}.  Turns the operator @arg{op} into a
modifying form, e.g. @code{(_f + a b) @equiv{} (incf a b)}. See also __f. Modifed accoring to Matt to take care of env"
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro npushback (place item)
  "Add item to the end of list. Beware the macro affects all references to the list in 'place'"
  `(_f nconc ,place (list ,item)))

(defmacro nenqueue (place item)
  "Add item to the end of list. Beware the macro affects all references to the list in 'place'"
  `(_f nconc ,place (list ,item)))

;; see also atomic-queue.lisp


(defmacro __f (op arg1 place &rest args &environment env)
  "Call (op arg1 place . args) and store the result to place. Place access is calculated once only. See also _f"
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place env)
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
  "Печатает список с разделителем между каждыми двумя элементами"
  (with-output-to-string (s)
    (let1 firsttime t
      (dolist (x list) 
        (if firsttime (setf firsttime nil) (princ delimiter s))
        (princ x s)
        ))))



#+lispworks (dspec:define-dspec-alias defparameter-always (x) `(defparameter ,x))

(defmacro defparameter-always (name initial-value &optional (documentation nil docp))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defparameter ,name ,initial-value ,@(when docp `(,documentation)))))


(defun symbol-is-in-package (symbol package external-only)
  "Возвращает два значения: 1. t, если данный символ доступен в данном пакете. Если external-only, то возвращает t, только если он внешний в данном пакете
   2. статус из find-symbol, если символ доступен"
  (multiple-value-bind (other-symbol status)
                       (find-symbol (symbol-name symbol) package)
    (cond
     ((null symbol)
      (cond ((null other-symbol) (values t status))
            (t nil)))
     ((null other-symbol) nil)
     ((not (eq symbol other-symbol)) nil)
     ((eq status :EXTERNAL) (values t status))
     ((not external-only) (values t status))
     (t nil))))

; нам это нужно очень рано из-за defun-to-file
(defmacro symbol-readmacro (symbol) `(get ,symbol 'symbol-readmacro))

(defvar *escape-symbol-readmacros* t
  "Скрывать символы, которые symbol-readmacro при печати, чтобы они читались как символы")


(defun |Написать-экспорт-для-структуры| (type)
  (let* ((pack (symbol-package type))
         (pack-name (package-name pack))
         (struct-name (string type)))
    (with-output-to-string (ou)
      (macrolet ((doit (filter-expr)
                   `(do-symbols (x pack)
                      (let ((x-name (string x)))
                        (when (and (eq (symbol-package x) pack) ,filter-expr)
                          (format ou "~%~A:~A" pack-name x-name))))))
        (format ou "~%;;~A" struct-name)
        (doit (eq x type))
        (doit (string= x-name (str+ "MAKE-" struct-name)))
        (doit (string= x-name (str+ struct-name "-P")))
        (doit (string= x-name (str+ "COPY-" struct-name)))
        (format ou "~%")
        (doit (and
               (alexandria:starts-with-subseq (str+ struct-name "-") x-name)
               (not (string= x-name (str+ struct-name "-P")))))
        (format ou "~%")
        (format ou "~%")))))

        

(defun write-exports-for-defstruct (type)
  (|Написать-экспорт-для-структуры| type))
