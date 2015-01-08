;;; -*- Encoding: utf-8; system :see-packages -*-
; patching lispworks tools for def-symbol-readmacro reader extensions

(in-package :budden-tools)
(in-readtable nil)

(defvar *editors-real-package* nil) 

(defvar *use-decorated-package-system-fns* nil)

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
(defun decorated-find-package (fn name)
  (budden-tools::hp-find-package (if (stringp name) (string-upcase name) name)
                                 *package* fn))

(decorate-function 'find-package #'decorated-find-package)

(defun decorated-find-symbol 
       (fn string &optional (package nil package-supplied-p))
  (cond
   (package-supplied-p
    (funcall fn string package))
   (t 
    (funcall fn string))))

;2012-12-19 (decorate-function 'find-symbol #'decorated-find-symbol)

(defun symbol-is-in-package (symbol package external-only)
  "Возвращает два значения: 1. t, если данный символ доступен в данном пакете. Если external-only, то возвращает t, только если он внешний в данном пакете
   2. статус из find-symbol, если символ доступен"
  (perga-implementation:perga
    (:@ mlvl-bind (other-symbol status) (find-symbol (symbol-name symbol) package))
    (cond
     ((null symbol)
      (cond ((null other-symbol) (values t status))
            (t nil)))
     ((null other-symbol) nil)
     ((not (eq symbol other-symbol)) nil)
     ((eq status :EXTERNAL) (values t status))
     ((not external-only) (values t status))
     (t nil))))

(defun may-symbol-complete-symbol (symbol default-package partial-name external-only all-chars-in-same-case-p)
  (perga-implementation:perga
    (cond
     ((not (symbol-is-in-package symbol default-package external-only))
           nil)
     (t 
      (alexandria.0.dev:starts-with-subseq 
       partial-name
       (typecase symbol (symbol (symbol-name symbol)) (t (string symbol)))
       :test (if all-chars-in-same-case-p #'char-equal #'char=)))
     )))


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


#|
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
    (let all-chars-in-same-case-p (all-ascii-chars-in-same-case-p partial-name)) 
    ; тогда ищём всё, что подходит. Но только в default-package 
    ;(break) 
    (let list 
      (iter 
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
       (fn partial-name &key abbreviated predicate symbols default-package return-common-string) 
  (declare (ignore abbreviated)) 
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
    (mlvl-bind (rlist rlength rstring rpackage)  
        (let1 *editors-real-package* default-package 
          (apply fn partial-name (dispatch-keyargs-simple predicate symbols default-package return-common-string)))) 
    (return-from function  
      (values  
       (sort rlist 'editor::symbol-string-<) 
       rlength rstring rpackage))))
|#

(defun do-complete-symbol-with-budden-tools (str)
  "Функция, позволяющая сделать завершение символа.
Str - входная строка, для которой необходимо завершение.

На выходе строка с завершенным символом или nil, если пользователь
отказался от выбора (в случае неоднозначности).

Функция учитывает наличие local-nickname пакетов.

Таким образом, на вход может подаваться следующая информация:

1) Часть имени без указания пакета: symb
2) Часть имени внешнего символа, с указанием префикса пакета: package:symb
3) Часть имени внутреннего символа с указанием префикса пакета: package::symb
4,5) Повтор пп. 2 и 3, где в качестве префикса указывается local-nickname пакет.
6) Пакет указан, но в имени пакета есть ошибка, т.е. нельзя найти пакет по префиксу.
   Действуем по п.1, исключив префикс.
"  
  (let* ((partial-name str)
         ;; Позиция первого двоеточия
         (colon-pos (position #\: 
                              partial-name))
         ;; Позиция второго двоеточия
         (2colon-pos (when colon-pos
                       (position #\: 
                                 partial-name 
                                 :start (1+ colon-pos))))
         ;; Текст до двоеточий, может отсутствовать
         (prefix (and colon-pos 
                      (> colon-pos 0)
                      (subseq partial-name 0 colon-pos)))

         (casified-prefix (and prefix
                               (string (read-from-string (concatenate 'string "#:" prefix)))))

         ;; Текст после двоеточий, может отсутствовать
         (suffix (if colon-pos
                     (subseq partial-name 
                             (1+ 
                              (max (if (null colon-pos)
                                       0 colon-pos)
                                   (if (null 2colon-pos)
                                       0 2colon-pos))))
                   partial-name))

         ;; Завершаемый символ внутренний?
         (external? (and colon-pos (not 2colon-pos)))

         ;; Пакет, в котором находится редактор (есть всегда)
         (editor-package (editor::buffer-package-to-use
                          (editor::current-point)))

         ;; Имя пакета, в котором находится редактор (есть всегда)
         ;(editor-package-name (when editor-package
         ;                       (package-name editor-package)))

         ;; Пакет, полученный из префикса
         (found-package (cond
                         (casified-prefix
                          (or 
                           (budden-tools:hp-find-package casified-prefix editor-package)
                           (editor:editor-error "Package or local-nickname ~S not found" casified-prefix)))
                         ((and
                           (null casified-prefix)
                           (eql colon-pos 0)
                           (not 2colon-pos))
                          *keyword-package*)))

         ;; Имя пакета из префикса, если удалось найти.
         (found-package-name (when found-package
                               (package-name found-package)))

         ;; Пакет является 
         (fake-package? (and found-package-name 
                             prefix
                             (not (string-equal 
                                   found-package-name 
                                   prefix))))
         (camel-case-suffix?
          (not (def-merge-packages::all-ascii-chars-in-same-case-p suffix))))

    (let ((raw-list ())
          (list-of-completes ())
          (show-list ())
          (ext (if found-package external? nil))
          (pkg (if found-package 
                   found-package 
                 editor-package)))

      (setf raw-list
            (remove-duplicates 
            ; (identity ; append
              (iter
                (:for sym 
                 :in-package pkg
                 :external-only nil)
                (:for storage = (nth-value 1 (find-symbol (string sym) pkg)))
                (when (and ext (not (eq storage :external)))
                  (:next-iteration))
                (:for name = (subseq (prin1-to-string (make-symbol (symbol-name sym))) 2))
                (:for pkg2 = (symbol-package sym))
                (when pkg2 ; can be inherited uninterned symbol so it has no home package
                  (:collect 
                   (list name (package-name pkg2) storage sym))))
              #|(if (not ext)
                  (iter
                    (:for sym 
                     :in-package pkg
                     :external-only nil)
                    (:collect 
                     (let* ((name (subseq (prin1-to-string (make-symbol (symbol-name sym))) 2))
                            (pkg (symbol-package sym)))
                       (list name (package-name pkg) :internal sym)))))|# 
             :from-end t :key #'first :test #'string=))
              

      (labels ((casify-name (nm) 
                            ; если пользователь набрал имя в верхнем регистре, а имя может быть прочитано в обоих, 
                            ; приводим продолженный символ к верхнему регистру
                 (if 
                     (and suffix
                          (eq (all-ascii-chars-in-same-case-p suffix) :uppercase)
                          (all-ascii-chars-in-same-case-p nm)
                          (not (find #\| nm :test 'char=))
                          (not (find #\\ nm :test 'char=))
                          (member (readtable-case-advanced *readtable*) '(:upcase :upcase-if-uniform)))
                     (string-upcase-ascii nm)
                   nm))
               (do-format-item (x rem-prefix home-package)
                 (let* ((name (first x)) ; имя для печати
                        (p-name (second x))
                        (status (third x))
                        (symbol (fourth x))
                        (colons "")
                        (package-name "")
                        (do-delete-prefix
                         (cond
                          ((null prefix)
                           t)
                          ((eq rem-prefix :prompt)
                           (yes-or-no-p "Символ ~S доступен в текущем пакете. Убрать префикс?" symbol))
                          (t rem-prefix))))
                
                   (setf package-name
                         (if (and (string-equal p-name 
                                                found-package-name)
                                  fake-package?)
                             prefix
                           p-name))
                   (setf colons
                         (cond
                          ((and (string-equal p-name 
                                              found-package-name)
                                (eq :external status))
                           ":")
                           ((and (string-equal p-name
                                              (if (symbol-package symbol)
                                                  (package-name (symbol-package symbol))
                                                ""))
                                (eq :external status))
                            ":")
                           (t                         
                           "::")))
                   (format nil "~A~A~A" 
                           (if do-delete-prefix "" (if home-package (if fake-package? prefix home-package) package-name))
                           (cond
                            ((eq (symbol-package symbol) *keyword-package*)
                             ":")
                            (do-delete-prefix "")
                            (home-package (if (eq status :external) ":" "::"))
                            (t colons))
                           (casify-name name))))
               (format-item (x)
                 (do-format-item x nil nil)))

        (setf list-of-completes
              (sort
               (remove-if #'null
                          (mapcar #'(lambda (x)
                                      (if (alexandria:starts-with-subseq 
                                           suffix
                                           (first x)
                                           :test (if camel-case-suffix?
                                                     #'char= 
                                                   #'char-equal))
                                          x nil))
                                  raw-list))
               #'string-lessp :key #'first))

        (setf show-list 
              (mapcar #'format-item list-of-completes))

        (let* ((s (if (= 1 (length show-list)) 
                      (first show-list) 
                    (editor::call-scrollable-menu show-list nil)))
               (pos (position s show-list :test #'string=))
               (delete-prefix? nil)
               (src (when pos (nth pos list-of-completes)))
               ;(name (first src)) ; имя для печати
               ;(p-name (second src))
               ;(status (third src))
               (symbol (fourth src))
              )
          (when (and
                 ;; указан префикс
                 prefix 
                 ;; символ доступен в текущем пакете 
                 (symbol-is-in-package symbol editor-package nil)
                 )
            (setf delete-prefix? :prompt))
          (when src
            (do-format-item src delete-prefix? found-package-name)))))))


(editor::defcommand "Complete Symbol With Budden Tools"
     (p) "Complete Symbol With Local Package Nicknames and advanced readtable-case"
         "Complete Symbol With Local Package Nicknames and advanced readtable-case"
  (declare (ignorable p))
  ;; получаем исходный текст, который нужно завершить
  (let* ((str (editor::symbol-string-at-point (editor:current-point)))
         (str-len (length str))
         (res (do-complete-symbol-with-budden-tools str)))
    (when res
      (editor:delete-previous-character-command str-len)
      (editor:insert-string (editor:current-point) res))))




#|(defun decorated-intern-symbol-from-string (fn string default-package)
    2 EDITOR::INTERN-SYMBOL-FROM-STRING > ...
      >> STRING                  : "казя-базя"
      >> EDITOR::DEFAULT-PACKAGE : #<PACKAGE TST>
    2 EDITOR::INTERN-SYMBOL-FROM-STRING < ...
      << VALUE-0 : казя-базя
      << VALUE-1 : NIL

|#
#|(defun decorated-parse-symbol (fn string &key package)
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
             (values out-package name found-p prefix-length)))))))))|#

;(undecorate-function 'editor:parse-symbol #'decorated-parse-symbol)

(defun print-symbol-string-with-advanced-readtable-case (string &key (readtable *readtable*) (package *package*))
  "see also print-symbol-string-with-advanced-readtable-case-2"
  (let1 str string
    (when
        (and 
         str
       ;(member complete '(:complete :complete-but-not-unique :not-unique))
         (eq (readtable-case-advanced readtable) :upcase-if-uniform)
         (multiple-value-bind (pckg name-only-str xlam1 xlam2)
             (editor::pathetic-parse-symbol str package)
           (declare (ignore pckg xlam1 xlam2))
           ;(show-expr `("returned from p-p-s to d-c-s-1" ,name-only-str))
           (all-ascii-chars-in-same-case-p (sequence-last str (length name-only-str)))
           )
         ;(eq (all-ascii-chars-in-same-case-p string) :lowercase) ; если набирали в верхнем или смешанном регистре, и останемся в верхнем
         )
      ; (print "ura!")
      (setf str (string-downcase-ascii str))
      )
    str))

#|(defun print-symbol-with-advanced-readtable-case (symbol &key (readtable *readtable*) (package *package*))
  (proga
    (let name (symbol-name symbol)
      (cond
       ((and 
         (eq (readtable-case-advanced readtable) :upcase-if-uniform)
         (all-ascii-chars-in-same-case-p name)
         )
        (setf name (string-downcase-ascii str))
        )))|#
    

(defun decorated-create-print-function-for-symbols (fn &key (package *package*) case)
  (declare (ignorable case fn))
  (lambda (symbol) (print-symbol-string-with-advanced-readtable-case (string symbol) :package package)))


#|(defun decorated-complete-symbol-1 (fn string &key 
                                       (package nil package-supplied-p)
                                       (print-function nil print-function-supplied-p)
                                       (predicate nil predicate-supplied-p)
                                       (print-case nil print-case-supplied-p)
                                       (abbreviated nil abbreviated-supplied-p))
  (multiple-value-bind (str len complete)
      (apply fn string (dispatch-keyargs-full package print-function predicate print-case abbreviated))
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
           (all-ascii-chars-in-same-case-p (sequence-last str (length name-only-str)))
           )
         (eq (all-ascii-chars-in-same-case-p string) :lowercase) ; если набирали в верхнем или смешанном регистре, и останемся в верхнем
         )
      ; (print "ura!")
      (setf str (string-downcase str))
      )
    ; (print `(,str ,len ,complete))
    (values str len complete)
    )  ; FIXME - отключить кириллицу в нашем мухляже с RT - кириллицы нет в CL и пусть для неё будет всё preserve
  )
    

(decorate-function 'editor::complete-symbol-1 #'decorated-complete-symbol-1)|#

(defvar *in-complete-symbol-command* nil)

;(decorate-function 'editor:complete-symbol-command #'decorated-complete-symbol-command)
;(decorate-function 'editor::indent-or-complete-symbol-command #'decorated-complete-symbol-command)
;(decorate-function 'editor::indent-selection-or-complete-symbol-command #'decorated-complete-symbol-command)


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
     
(editor::bind-key "Complete Package Name" "control-meta-j")


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

