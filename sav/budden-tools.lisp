;;; -*- Encoding: utf-8; -*-
; -*- coding: windows-1251-dos; -*- 
; при ручной компиляции этого файла в sbcl нужно делать (setf sb-impl::*default-external-format* :windows-1251) 

(in-package :budden-tools)

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

(defun rmsubseq (seq &rest args &key from-end start end count)
  #+russian "Удаляет из последовательности указанные элементы"
  #-russian "Removes subsequence from sequence"
  (declare (ignore from-end start end count))
  (apply #'remove nil seq :test (constantly t) args))

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

(defun search-and-replace-seq (type seq subseq newseq &key all (test #'equalp))
  (let1 num-matches 0
    (loop 
     (let1 found (search subseq seq :test test)
       (when found 
         (setf seq (concatenate type 
                                (subseq seq 0 found)
                                newseq
                                (subseq seq (+ found (length subseq)) (length seq))))
         (incf num-matches))
       (when (or (not found) (not all))
         (return))))
    (values seq num-matches)))

(defun replace-subseq (seq new-subseq &key (start 0) end (type 'list))
  #+russian "Заменяет подпоследовательность, определяемую start и end, другой
подпоследовательностью. Длины могут не совпадать, при этом получается результат другой длины, чем sequence.
Работает очень медленно (concatenate). Нужно переделать"
  #-russian "Replaces subsequence by other subsequence (maybe of another length. Uses concatenate and hence is slow"
  (concatenate type (subseq seq 0 start) new-subseq (when end (subseq seq end))))



(defun struct-to-alist (s) "сохраняет данные из структуры в alist"
  #+lispworks (multiple-value-bind (names values) (structure:structure-names-and-values s)
                `((:type . ,(type-of s))
                   ,@(loop for x in names for y in values collect `(,x . ,y))))
  #-lispworks (error "struct-to-alist not defined for this lisp version")
  ) 
                    
          
(defun str+ (&rest args) (apply 'concatenate 'string (mapcar 'string args))) (export 'str+)

(defun non-empty-string-p (x) 
  #+russian "Возвращает x, если x - не nil и не пустая строка"
  (cond ((null x) nil) ((equal x "") nil) (t x)))

(defmacro dispatch-keyarg-simple (keyarg)
  #+russian "Для передачи похожих аргументов в apply. На самом деле, тут нужно учитывать default,
supplied-p и т.п."
  `(when ,keyarg `(,(keywordize ',keyarg) ,,keyarg)))


(defun path-to-a-file (filename) "d:/foo/file.ext --> d:/foo/" 
  (let1 p (pathname filename)
    (make-pathname :host (pathname-host p) :directory (pathname-directory p))))

(defun up-dir (pathname) "d:/foo/bar/ --> d:/foo; d:/foo/bar/file.ext --> d:/foo"
  (let1 p (pathname pathname)
    (make-pathname :host (pathname-host p) :directory (butlast (pathname-directory p)))))


(defun read-file-into-string (filename)
  (with-open-file (in filename :direction :input)
    (loop :with res = ""
          :for x = (read-line in nil nil) :unless x :do (return res)
          :do (setf res (concatenate 'string res (format nil "~%") x)))))

(defun save-string-to-file (string filename)
  (with-open-file (out filename :direction :output :if-does-not-exist :create :if-exists :supersede)
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
  (do ((a (pop list) (pop list)) 
       (b (pop list) (pop list))
       res) 
      (nil) 
    (push (if to-alist (cons a b) (list a b)) res)
    (when (null list) (return-from splice-list (reverse res)))
    ))

(defun alist-to-list (x) "Превращает alist в список ((a . b) (c . d)) -> (a b c d)"
  (loop for (key . value) in x append (list key value)))

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
  #+russian "Работает только для списка. Надо сделать, чтобы работало для sequence (использовать find-if-not)"
  #-russian "Collect only a single duplicate of all duplicated entries"
  (declare (ignore key test test-not))          
  (iter:iter 
    (:with known-values) (:with old-known-values)
    (:for x in list)
    (setf old-known-values known-values known-values 
	  (apply 'adjoin x known-values key-args))
    (when (eq old-known-values known-values)
      (:collect x))))



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

(defun tree-weight (tree) #+russian "Общее количество консов в дереве"
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
                   (cllib:split-seq name2 (lambda (x) (eql x #\.)) :start 0))))
        (t name)))

(defun dotted-p (l) (values (last l 0) (butlast l 0)))


(defun keywordize (symbol-or-string)
  (etypecase symbol-or-string
    (symbol (intern (symbol-name symbol-or-string) :keyword))
    (string (intern symbol-or-string :keyword))))

#+russian 
(defparameter *cyrillic-symbols* '(#\а #\б #\в #\г #\д #\е #\ё #\ж #\з #\и #\й #\к #\л #\м #\н #\о #\п #\р #\с #\т #\у #\ф #\х #\ц #\ч #\ш #\щ #\ъ #\ы #\ь #\э #\ю #\я #\А #\Б #\В #\Г #\Д #\Е #\Ё #\Ж #\З #\И #\Й #\К #\Л #\М #\Н #\О #\П #\Р #\С #\Т #\У #\Ф #\Х #\Ц #\Ч #\Ш #\Щ #\Ъ #\Ы #\Ь #\Э #\Ю #\Я)
)

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
  (defun char-upcase-cyr (char) #+russian "поднимает регистр по Русски"
    (or (gethash char up) (char-upcase char)))
  (defun char-downcase-cyr (char) #+russian "опускает регистр по-русски"
    (or (gethash char down) (char-downcase char)))
  (defun char-equal-cyr (c1 c2) #+russian "сравнивает символы с учётом кириллицы"
    (or (char-equal c1 c2)
        (char-equal (char-upcase-cyr c1) (char-upcase-cyr c2))))
  (defun string-upcase-cyr (s)
    (map 'string 'char-upcase-cyr s))
  (defun string-downcase-cyr (s)
    (map 'string 'char-downcase-cyr s))
  (defun string-equal-cyr (s1 s2)
    (let* ((s1 (string s1))
           (s2 (string s2)))
      (or (string-equal s1 s2)
          (every 'char-equal-cyr s1 s2))))
  )

