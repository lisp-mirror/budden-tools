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
                       

;;; ��������! ��� ��������� ���������� ��� ���������� ������ ������������� �������, 
;;; �.�. �� �� ����� ��������� ��� ��������� �� #\. 
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

(defun symbol-is-in-package (symbol package external-only)
  "���������� t, ���� ������ ������ �������� � ������ ������. ���� external-only, �� ���������� t, ������ ���� �� ������� � ������ ������"
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
       (typecase symbol (symbol (symbol-name symbol)) (t (string symbol)))
       :test (if all-chars-in-same-case-p #'char-equal #'char=)))
     )))


#|BUDDEN 100 > editor::pathetic-parse-symbol "budden::cons" *package*
#<PACKAGE BUDDEN>
"CONS"
NIL
8
|#

(defun my-complete-symbol (partial-name &key predicate symbols default-package return-common-string)
  "����� ��� ��������, �.�. lw �� �������� �������� � ���� ������ ������� � ������� ��������"
  (proga
    (when (or predicate symbols)
      (break "������ ��������� ��������� predicate,symbols"))
    (unless default-package
      (break "��� default-package"))
    (unless return-common-string
      ;(break "not return-common-string")
      )
    (multiple-value-bind (pckg sym-str external-only prefix-length)
        (editor::pathetic-parse-symbol partial-name default-package))
    (let partial-name-length (length partial-name))
    (when prefix-length (setf partial-name sym-str))
    (let all-chars-in-same-case-p (all-ascii-chars-in-same-case-p partial-name))
    ; ����� ���� ��, ��� ��������. �� ������ � default-package
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

(decorate-function 'editor::complete-symbol
                   #'decorated-complete-symbol)



(defun decorated-buffer-package-to-use (fn &rest args)
  (let1 res (minimal-fix-xlam-package (apply fn args))
    (cond ; ����� ����� �� ��������� ����� budden ������ cl-user ��� Help � Background Output
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
              (warn "�������� ����� � decorated-buffer-package-to-use"))
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
      (SHOW-EXPR `(symbol-string-at-point returned ,string ,package))
      (values
       string
       (minimal-fix-xlam-package package)))))

(decorate-function 'editor::symbol-string-at-point #'decorated-symbol-string-at-point)

(defun extract-symbol-string-from-point-with-range (pnt)
  (proga function
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
  (proga
    (mlvl-bind (string beg end) (extract-symbol-string-from-point-with-range pnt))
    (show-expr `(,string ,beg ,end))
    (editor:delete-between-points beg end)
    (editor:insert-string beg (print-symbol-string-with-advanced-readtable-case string))
    ))

(editor::defcommand "Fix Case of Symbol at Point" (p) "" ""
  (declare (ignorable p))
  (do-fix-case-of-symbol-at-point (editor:current-point)))


#|(defun decorated-intern-symbol-from-string (fn string default-package)
    2 EDITOR::INTERN-SYMBOL-FROM-STRING > ...
      >> STRING                  : "����-����"
      >> EDITOR::DEFAULT-PACKAGE : #<PACKAGE TST>
    2 EDITOR::INTERN-SYMBOL-FROM-STRING < ...
      << VALUE-0 : ����-����
      << VALUE-1 : NIL

|#
#|(defun decorated-parse-symbol (fn string &key package)
  (let1 *use-decorated-package-system-fns* t
    (multiple-value-bind
        (out-package name found-p prefix-length)
        (apply fn string (dispatch-keyarg-simple package))
      (cond
       (prefix-length ; ������ � �������������� �� �������
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
         ;(eq (all-ascii-chars-in-same-case-p string) :lowercase) ; ���� �������� � ������� ��� ��������� ��������, � ��������� � �������
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


(defun decorated-complete-symbol-1 (fn string &key 
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
         (eq (all-ascii-chars-in-same-case-p string) :lowercase) ; ���� �������� � ������� ��� ��������� ��������, � ��������� � �������
         )
      ; (print "ura!")
      (setf str (string-downcase str))
      )
    ; (print `(,str ,len ,complete))
    (values str len complete)
    )  ; FIXME - ��������� ��������� � ����� ������� � RT - ��������� ��� � CL � ����� ��� �� ����� �� preserve
  )
    

(decorate-function 'editor::complete-symbol-1 #'decorated-complete-symbol-1)

(defvar *in-complete-symbol-command* nil)
(defun decorated-do-complete-symbol (fn p abbreviated)
  (proga
    (let *in-complete-symbol-command* t)
    (funcall fn p abbreviated)
    (do-fix-case-of-symbol-at-point (editor::current-point))
    ))
  
;(editor::bind-key "Fix Case of Symbol at Point" "f12")

(decorate-function 'editor::DO-COMPLETE-SYMBOL 'DECORATED-DO-COMPLETE-SYMBOL)

;(decorate-function 'editor:complete-symbol-command #'decorated-complete-symbol-command)
;(decorate-function 'editor::indent-or-complete-symbol-command #'decorated-complete-symbol-command)
;(decorate-function 'editor::indent-selection-or-complete-symbol-command #'decorated-complete-symbol-command)


; string-capitalize
(defun decorated-string-capitalize (fn string &rest keyargs)
  (if ; editor::*editor-state* ; ����� ��������, ��� ��� ���������� - ������ ������ ���� ��������� � ��� - �����
      ; �� ��� � ������ ������ �� �������, �.�. ��� ������ ��� ���-�� � �� ����� ���������� �����.
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

(editor::defcommand "Complete Package Name"
     (p) "Complete package at point" "Complete package at point"
  (declare (ignorable p))
  (proga all
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
                             (list-all-packages)))
          (:for title = (typecase p 
                          (cons 
                           (str++ (car p) '= (cdr p)))
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
            ((= (length titles) 1) (first titles))
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

��� �����������?
1. Completion ������� ��� ������������� ������
2. �� ��, � ��������������
3. �� ��, � ��������-������������ ��������������
3. �� ��, ��� �������������, � ����� �����������
4. �� ��, � �������������� � ����� �����������. 
5. ����� �����������
6. ����� ����������

�� ������ ������ completion �� ���������� �������, �������� �� 
����� � ������� "see" 
|#

