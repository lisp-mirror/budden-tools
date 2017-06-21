;;; -*- Encoding: utf-8; system :see-packages -*-
; patching lispworks tools for def-symbol-readmacro reader extensions

(in-package :budden-tools)
(in-readtable nil)

(defvar *editors-real-package* nil) 


#|(mapcar 'undecorate-function
        '(find-package find-symbol editor::complete-symbol 
                       editor::buffer-package-to-use editor::pathetic-parse-symbol
                       editor::symbol-string-at-point editor::complete-symbol-1
                       editor::complete-symbol-command
                       editor::indent-selection-or-complete-symbol-command
                       string-capitalize
                       editor::intern-symbol-from-string))|#
                       

;;; ВНИМАНИЕ! Это изменение необходимо для нормальной работы иерархических пакетов, 
;;; т.к. мы не можем назначить наш ридмакрос на #\.
#-sbcl
(defun decorated-find-package (fn name)
  (budden-tools::hp-find-package (if (stringp name) (string-upcase name) name)
                                 *package* fn))

#-sbcl
(decorate-function 'find-package #'decorated-find-package)
 

#|BUDDEN 100 > editor::pathetic-parse-symbol "budden::cons" *package*
#<PACKAGE BUDDEN>
"CONS"
NIL
8
|#

;(decorate-function 'editor::i-find-package-name-for-point 'decorated-i-find-package-name-for-point)


(defun decorated-buffer-package-to-use (fn &rest args)
  (let1 res (apply fn args)
    (cond ; здесь задаём по умолчанию пакет budden вместо cl-user для Help и Background Output
     ((not (eq res #.(find-package :common-lisp-user))) res)
     (t
      (let* 
          ((first-arg (first budden-tools::args))
           (buffer-name
            (typecase first-arg
              (editor::i-point 
               (slot-value (slot-value first-arg 'editor::buffer) 'editor::%name))
              (editor::buffer
               (slot-value first-arg 'editor::%name))
              (t ""))))
        (cond 
         ((or (alexandria.0.dev::STARTS-WITH-SUBSEQ "Background Output" buffer-name)
              (alexandria.0.dev::STARTS-WITH-SUBSEQ "Help" buffer-name)
              (warn "Странный пакет ~S в decorated-buffer-package-to-use" res))
          (or (find-package :budden) res))
         (t res)))
      )
     )
    ))


;2012-12-19(decorate-function 'editor::buffer-package-to-use #'decorated-buffer-package-to-use)

(defun decorated-pathetic-parse-symbol (fn symbol default-package &optional errorp)
;  (print "decorated-pathetic-parse-symbol IN")
  (let1 id (new-show-package-system-vars-id)
    ;(show-package-system-vars "decorated-pathetic-parse-symbol:before" id)
    (trace-into-text-file (str++ "decorated-pathetic-parse-symbol:default-package " id " "
                                 (package-name default-package)))
;    (let1 defaul*package* default-package ; (or *last-used-real-package* default-package)
    ;(print `("decorated-p-p-s" ,symbol))
    (let1 *use-decorated-package-system-fns* t
      (multiple-value-prog1 
          (funcall fn 
                   symbol 
                   default-package 
                   errorp)
       ; (print "decorated-pathetic-parse-symbol OUT")
        ))))

; budden-tools::see-packages-find-unqualified-symbol "S1" :tst

(decorate-function 'editor::pathetic-parse-symbol #'decorated-pathetic-parse-symbol)

       
(defun decorated-symbol-string-at-point (fn point)
  (let1 *use-decorated-package-system-fns* t 
    (multiple-value-bind (string package)
        (funcall fn point)
      ; (SHOW-EXPR `(symbol-string-at-point returned ,string ,package))
      (values
       string
       package 
       ))))

(decorate-function 'editor::symbol-string-at-point #'decorated-symbol-string-at-point)

(defun extract-symbol-string-from-point-with-range (pnt)
  (perga-implementation:perga function
    (let string (editor::i-read-symbol-from-point pnt t nil t))
    (let offset (length (editor::i-read-symbol-from-point pnt t t t)))
    (let beg (editor::copy-point pnt :kind :temporary))
    (editor:character-offset beg (- offset))
    (let end (editor::copy-point beg :kine :temporary))
    (editor:character-offset end (length string))
    (values string beg end)))

    ;(show-expr (multiple-value-list (editor::i-read-symbol-from-point (editor:current-point) T NIL T)))
    ;(show-expr (multiple-value-list (editor:get-symbol-from-point (editor:current-point) :create-new nil)))
    ;(let symbol (editor:get-symbol-from-point (editor:current-point) :create-new nil))
    ;(unless symbol 
    ;  (return-from function nil))

(defun do-fix-case-of-symbol-at-point (pnt)
  "To be called from editor command only"
  (perga-implementation:perga
    (mlvl-bind (string beg end) (extract-symbol-string-from-point-with-range pnt))
    ;(show-expr `(,string ,beg ,end))
    (editor:delete-between-points beg end)
    (editor:insert-string beg (print-symbol-string-with-advanced-readtable-case string))
    ))

(editor::defcommand "Fix Case of Symbol at Point" (p) "" ""
  (declare (ignorable p))
  (do-fix-case-of-symbol-at-point (editor:current-point)))

(editor::defcommand "Complete Symbol With Budden Tools"
     (p) "Complete Symbol With Local Package Nicknames and advanced readtable-case"
         "Complete Symbol With Local Package Nicknames and advanced readtable-case"
  (declare (ignorable p))
  ;; получаем исходный текст, который нужно завершить
  (let* ((str (editor::symbol-string-at-point (editor:current-point)))
         (str-len (length str))
         (package (editor::buffer-package-to-use (editor::current-point)))
         (res (do-complete-symbol-with-budden-tools
               str
               package
               #'editor:editor-error
               (lambda (show-list) (editor::call-scrollable-menu show-list nil)))))
    (when res
      (editor:delete-previous-character-command str-len)
      (editor:insert-string (editor:current-point) res))))

(defun print-symbol-string-with-advanced-readtable-case (string &key (readtable *readtable*) (package *package*))
  "see also print-symbol-string-with-advanced-readtable-case-2"
  (let1 str string
    (when
        (and 
         str
         (eq (readtable-case-advanced readtable) :upcase-if-uniform)
         (multiple-value-bind (pckg name-only-str xlam1 xlam2)
             (editor::pathetic-parse-symbol str package)
           (declare (ignore pckg xlam1 xlam2))
           (all-ascii-chars-in-same-case-p (sequence-last str (length name-only-str)))
           )
         )
      (setf str (string-downcase-ascii str))
      )
    str))

(defun decorated-create-print-function-for-symbols (fn &key (package *package*) case)
  (declare (ignorable case fn))
  (lambda (symbol) (print-symbol-string-with-advanced-readtable-case (string symbol) :package package)))


(defvar *in-complete-symbol-command* nil)

; string-capitalize
(defun decorated-string-capitalize (fn string &rest keyargs)
  (if ; editor::*editor-state* ; опыты показали, что эта переменная - истина внутри окна редактора и нет - иначе
      ; но она в данном случае не годится, т.к. она истина ещё где-то и от этого начинаются глюки.
      *in-complete-symbol-command*
      string
    (apply fn string keyargs)))

(decorate-function 'string-capitalize #'decorated-string-capitalize)


(defun decorated-intern-symbol-from-string (fn string &optional default-package)
  (declare (ignore fn))
  (perga-implementation:perga function 
    (let res 
      (let ((*package* (or default-package *package*)))
        (read-from-string string nil "")))
    (when (symbolp res)
      (find-symbol (symbol-name res) (symbol-package res))
      )))


(decorate-function 'editor::intern-symbol-from-string #'decorated-intern-symbol-from-string)



(editor:defcommand "RT Restore" 
     (p) ""
     "" 
  (declare (ignorable p))
  (setf *readtable* (copy-readtable nil)))


(defun list-all-packages-with-nicknames ()
  (let (result)
    (dolist (p (list-all-packages))
      (push p result)
      (dolist (n (package-nicknames p))
        (push (cons n p) result))
      )
    (nreverse result)
    ))

(defun package-string-or-symbol-to-string (x)
  (etypecase x
    (package (package-name x))
    (string x)
    (symbol (string x))))

(editor::defcommand "Complete Package Name"
     (p) "Complete package at point" "Complete package at point"
  (declare (ignorable p))
  (perga-implementation:perga all
    (let package (editor::buffer-package-to-use (editor:current-point)))
    (flet last-elt (sequence) 
      (let len (length sequence))
      (if (= len 0) nil
        (elt sequence (- (length sequence) 1))))
    (flet may-string-complete-string (completion partial-name all-ascii-chars-in-same-case-p)
      ;(break)
      (alexandria.0.dev:starts-with-subseq 
       partial-name
       completion
       :test (if all-ascii-chars-in-same-case-p #'char-equal #'char=)))
    (let partial-name (editor::symbol-string-at-point (editor:current-point)))
    ; process some characters in a special way, as symbol-string-at-point treats listener prompt as a symbol string
    (when (member (last-elt partial-name) '(#\  #\() :test 'char=)
      (setf partial-name ""))
    (let starts-with-colon (alexandria.0.dev:starts-with-subseq ":" partial-name :test 'char=))
    (when starts-with-colon
      (setf partial-name (subseq partial-name 1)))
    (let partial-name-length (length partial-name))
    (mlvl-bind (titles prefixes)
        (iter 
          (:for p in (append (gethash package *per-package-alias-table*)
                             (list-all-packages-with-nicknames)))
          (:for title = (typecase p 
                          (cons 
                           (str++ (car p) '= (package-string-or-symbol-to-string (cdr p))))
                          (package (package-name p))))
          (:for prefix = (typecase p
                           (cons (string (car p)))
                           (package (package-name p))))
          (when (may-string-complete-string prefix partial-name t)
            (:collect title :into titles)
            (:collect prefix :into prefixes))
          (:finally 
           (return (values titles prefixes)))))
    (let choice
      (cond ((= (length titles) 0) 
             (editor:message "No package names to complete ~A" partial-name)
             nil)
            ((and (= (length titles) 1)
                  (string= (first titles) (first prefixes)))
             (first titles))
            (t
             (capi:prompt-with-list
              titles
              "Complete package"))))
    (unless choice 
      (return-from all nil))
    (let pos (position choice titles :test 'equalp))
    (let prefix (elt prefixes pos))
    (when (> partial-name-length 0)
      (editor:delete-previous-character-command partial-name-length))
    (editor:insert-string (editor:current-point) 
                          (string-downcase 
                           (str++ prefix (if starts-with-colon "" ":"))))))
     
; (editor::bind-key "Complete Package Name" "control-meta-j")


#| 

bu

Что тестировать?
1. Completion символа без квалификатора пакета
2. То же, с квалификатором
3. То же, с локально-псевдонимным квалификатором
3. То же, без квалификатора, с окном продолжения
4. То же, с квалификатором и окном продолжения. 
5. Поиск определения
6. Показ аргументов

На данный момент completion не показывает символы, котоырые мы 
видим с помощью "see" 
|#

