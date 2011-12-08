(in-package :budden-tools)
(in-readtable nil)

(defvar *editors-real-package* nil) 

(defvar *use-decorated-package-system-fns* nil)

;;; ВНИМАНИЕ! Это изменение необходимо для нормальной работы иерархических пакетов, 
;;; т.к. мы не можем назначить наш ридмакрос на #\. 
(defun decorated-find-package (fn name)
  (budden-tools::hp-find-package (if (stringp name) (string-upcase name) name)
                       (if *use-decorated-package-system-fns*
                           (minimal-fix-xlam-package *package* :stack :find-package)
                         *package*) fn))

(decorate-function 'find-package #'decorated-find-package)

(defun decorated-find-symbol 
       (fn string &optional (package nil package-supplied-p))
  (cond
   (*use-decorated-package-system-fns*
    (let1 *use-decorated-package-system-fns* nil
      (unless package 
        (setf package (minimal-fix-xlam-package *package*)))
      (funcall fn string package)))
   (package-supplied-p
    (funcall fn string package))
   (t 
    (funcall fn string))))

(decorate-function 'find-symbol #'decorated-find-symbol)


(defun minimal-fix-xlam-package (pack &key stack)
  "Stack is passed for trace purposes only"
  (declare (ignore stack))
  (cond
   ((null pack) pack)
   (*editors-real-package* *editors-real-package*)
   ((eq pack *xlam-package*)
    (or *real-package* *last-used-real-package* 
        (progn 
          (trace-into-text-file "minimal-fix-xlam-package :ouch!") 
          nil)
        *keyword-package*))
   (t pack)))

#+nil (defun decorated-complete-symbol
       (fn symbol &key predicate symbols default-package return-common-string)
  (declare (ignorable predicate symbols return-common-string))
  (proga 
    (let *use-decorated-package-system-fns* t)
;    (let id (new-show-package-system-vars-id))
    (let *editors-real-package* default-package)
    (trace-into-text-file (str++ "decorated-complete-symbol:" id ":default-package was:" (if default-package (package-name default-package) "NIL")))
    (apply fn symbol `(,@(dispatch-keyarg-simple predicate)
                       ,@(dispatch-keyarg-simple symbols)
                       ,@(dispatch-keyarg-simple default-package)
                       ,@(dispatch-keyarg-simple return-common-string)))
    )
  )

(defun symbol-is-in-package (symbol package external-only)
  "Возвращает t, если данный символ доступен в данном пакете. Если external-only, то возвращает t, только если он внешний в данном пакете"
  (proga
    (multiple-value-bind (other-symbol status) (find-symbol (symbol-name symbol) package))
    (cond
     ((null symbol)
      (cond ((null other-symbol) t)
            (t nil)))
     ((null other-symbol) nil)
     ((not (eq symbol other-symbol)) nil)
     ((eq status :EXTERNAL) t)
     ((not external-only) t)
     (t nil))))

(defun may-symbol-complete-symbol (symbol default-package partial-name external-only all-chars-in-same-case-p)
  (proga
    (cond
     ((not (symbol-is-in-package symbol default-package external-only))
           nil)
     (t 
      (alexandria.0.dev:starts-with-subseq 
       partial-name 
       (symbol-name symbol)
       :test (if all-chars-in-same-case-p #'char-equal #'char=)))
     )))
  

#|BUDDEN 100 > editor::pathetic-parse-symbol "budden::cons" *package*
#<PACKAGE BUDDEN>
"CONS"
NIL
8
|#

(defun my-complete-symbol (partial-name &key predicate symbols default-package return-common-string)
  "Нужно это написать, т.к. lw не понимает регистра и ищет только символы в верхнем регистре"
  (proga
    (when (or predicate symbols)
      (break "пришли неведомые аргументы predicate,symbols"))
    (unless default-package
      (break "нет default-package"))
    (unless return-common-string
      ;(break "not return-common-string")
      )
    (multiple-value-bind (pckg sym-str external-only prefix-length)
        (editor::pathetic-parse-symbol partial-name default-package))
    (let partial-name-length (length partial-name))
    (when prefix-length (setf partial-name sym-str))
    (let all-chars-in-same-case-p (all-chars-in-same-case-p partial-name))
    ; тогда ищём всё, что подходит. Но только в default-package
    (let list
      (iter:iter
        (:for sym :in-package pckg)
        (when (may-symbol-complete-symbol sym pckg partial-name external-only all-chars-in-same-case-p)
          (:collect sym))))
    (cond
     (list 
      (values list partial-name-length (string (first list)) (symbol-package (first list))))
     (t 
      (values nil 0 nil nil)))
    ))
    
  


(defun decorated-complete-symbol 
       (fn partial-name &key predicate symbols default-package return-common-string)
  (declare (ignorable predicate symbols return-common-string))
  (setf fn 'my-complete-symbol)
  (proga function
    (let *use-decorated-package-system-fns* t)
;    (let id (new-show-package-system-vars-id))
    (when (hp-relative-package-name-p partial-name)
      (let1 pos (position #\: partial-name :test 'char=)
        (when pos
          (let*
              ((pos1 (if (and (< (1+ pos) (length partial-name))
                              (eql (elt partial-name (1+ pos)) #\:))
                         (+ 2 pos)
                       (+ 1 pos)))
               (new-default-package (budden-tools::hp-find-package (subseq partial-name 0 pos) default-package)))
            (return-from function
              (multiple-value-bind (symbols length some-symbol some-package)
                  (apply 'decorated-complete-symbol               
                         fn 
                         (str+ (package-name new-default-package)
                               ":"
                               (subseq partial-name pos1))
                         `(,@(dispatch-keyarg-simple predicate)
                           ,@(dispatch-keyarg-simple symbols)
                           :default-package ,new-default-package
                           ,@(dispatch-keyarg-simple return-common-string)))
                (ignored length)
                (values symbols pos1 some-symbol some-package)))))))
      (iter:iter 
      (:with sp = (package-seen-packages-list default-package))
      (:with (rlist rlength rstring rpackage) = nil)
      (:for p :initially default-package :then (pop sp))
      (:while p)
      (:for (values list length string package) = 
       (let1 *editors-real-package* p
         (apply fn partial-name `(,@(dispatch-keyarg-simple predicate)
                            ,@(dispatch-keyarg-simple symbols)
                            :default-package ,p
                            ,@(dispatch-keyarg-simple return-common-string)))))
      (cond
       ((and list rlist)
        (alexandria:appendf rlist list))
       ((and list (not rlist))
        (setf rlist list rlength length rstring string rpackage package))
       )
      (:finally 
       ;(break)
       (return-from function (values (sort rlist 'editor::symbol-string-<)
                                     rlength rstring rpackage))))))

(decorate-function 'editor::complete-symbol
                   #'decorated-complete-symbol)



(defun decorated-buffer-package-to-use (fn &rest args)
  (let1 res (minimal-fix-xlam-package (apply fn args))
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
              (warn "Странный пакет в decorated-buffer-package-to-use"))
          (or (find-package :budden) res))
         (t res)))
      )
     )
    ))


(decorate-function 'editor::buffer-package-to-use #'decorated-buffer-package-to-use)

(defun decorated-pathetic-parse-symbol (fn symbol default-package &optional errorp)
;  (print "decorated-pathetic-parse-symbol IN")
  (let1 id (new-show-package-system-vars-id)
    (show-package-system-vars "decorated-pathetic-parse-symbol:before" id)
    (trace-into-text-file (str++ "decorated-pathetic-parse-symbol:default-package " id " "
                                 (package-name default-package)))
;    (let1 defaul*package* default-package ; (or *last-used-real-package* default-package)
    ;(print `("decorated-p-p-s" ,symbol))
    (let1 *use-decorated-package-system-fns* t
      (multiple-value-prog1 
          (funcall fn 
                   symbol 
                   (minimal-fix-xlam-package default-package :stack :parse-symbol)
                   errorp)
       ; (print "decorated-pathetic-parse-symbol OUT")
        ))))

; budden-tools::see-packages-find-unqualified-symbol "S1" :tst

(decorate-function 'editor::pathetic-parse-symbol
                   #'decorated-pathetic-parse-symbol)

       
(defun decorated-symbol-string-at-point (fn point)
  (let1 *use-decorated-package-system-fns* t 
    (multiple-value-bind (string package)
        (funcall fn point)
      (values
       string
       (minimal-fix-xlam-package package)))))

(decorate-function 'editor::symbol-string-at-point #'decorated-symbol-string-at-point)


#|(defun decorated-intern-symbol-from-string (fn string default-package)
    2 EDITOR::INTERN-SYMBOL-FROM-STRING > ...
      >> STRING                  : "казя-базя"
      >> EDITOR::DEFAULT-PACKAGE : #<PACKAGE TST>
    2 EDITOR::INTERN-SYMBOL-FROM-STRING < ...
      << VALUE-0 : казя-базя
      << VALUE-1 : NIL

|#
(defun decorated-parse-symbol (fn string &key package)
  (let1 *use-decorated-package-system-fns* t
    (multiple-value-bind
        (out-package name found-p prefix-length)
        (apply fn string (dispatch-keyarg-simple package))
      (cond
       (prefix-length ; символ с квалификатором не трогаем
        (values out-package name found-p prefix-length))
       (t 
        (multiple-value-bind
            (symbol packages status)
            (see-packages-find-unqualified-symbol name package)
          ; (print status *trace-output*)
          (case status
            (:ambigious
             (values (first packages) (symbol-name symbol) t nil))
            (:external
             (values packages (symbol-name symbol) t nil))
            (t
             (values out-package name found-p prefix-length)))))))))

(decorate-function 'editor:parse-symbol #'decorated-parse-symbol)

(defun decorated-complete-symbol-1 (fn string &key 
                                       (package nil package-supplied-p)
                                       (print-function nil print-function-supplied-p)
                                       (predicate nil predicate-supplied-p)
                                       (print-case nil print-case-supplied-p))
  (multiple-value-bind (str len complete)
      (apply fn string (dispatch-keyargs-full package print-function predicate print-case))
    ;(break)
    (when
        (and 
         str
         (member complete '(:complete :complete-but-not-unique :not-unique))
         (eq (readtable-case-advanced *readtable*) :upcase-if-uniform)
         (multiple-value-bind (pckg name-only-str xlam1 xlam2)
             (editor::pathetic-parse-symbol str package)
           (declare (ignore pckg xlam1 xlam2))
           ; (print `("returned from p-p-s to d-c-s-1" ,name-only-str))
           (all-chars-in-same-case-p (sequence-last str (length name-only-str)))
           )
    ;     (not (every 'upper-case-p string)) ; если набирали в верхнем регистре, и останемся в верхнем
         )
      ; (print "ura!")
      (setf str (string-downcase str))
      )
    ; (print `(,str ,len ,complete))
    (values str len complete)
    )  ; FIXME - отключить кириллицу в нашем мухляже с RT - кириллицы нет в CL и пусть для неё будет всё preserve
  )
    

(decorate-function 'editor::complete-symbol-1 #'decorated-complete-symbol-1)

(defvar *in-complete-symbol-command* nil)
(defun decorated-complete-symbol-command (fn &rest args)
  (proga
    (let *in-complete-symbol-command* t)
    (apply fn args)))

(decorate-function 'editor::complete-symbol-command #'decorated-complete-symbol-command)

; string-capitalize
(defun decorated-string-capitalize (fn string &rest keyargs)
  (if ; editor::*editor-state* ; опыты показали, что эта переменная - истина внутри окна редактора и нет - иначе
      ; но она в данном случае не годится, т.к. она истина ещё где-то и от этого начинаются глюки.
      *in-complete-symbol-command*
      string
    (apply fn string keyargs)))

(decorate-function 'string-capitalize #'decorated-string-capitalize)

; (undecorate-function 'editor::complete-symbol-1) 

; editor:prompt-for-symbol editor:find-source-command
; editor::complete-symbol-1
; editor::get-symbol-from-point
; editor::complete-symbol


(defun decorated-intern-symbol-from-string (fn string &optional default-package)
  (declare (ignore fn))
  (proga function 
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

(editor:defcommand "RT New" 
     (p) ""
     "" 
  (declare (ignorable p))
  (setf *readtable* *my-readtable*))


#| 

budden-tools:se

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

