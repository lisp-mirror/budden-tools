(in-package :budden-tools)
(in-readtable nil)

(defvar *editors-real-package* nil) 

(defvar *use-decorated-package-system-fns* nil)
(defparameter *keyword-package* (apply-undecorated 'find-package '(:keyword)))

;;; ВНИМАНИЕ! Это изменение необходимо для нормальной работы иерархических пакетов, 
;;; т.к. мы не можем назначить наш ридмакрос на #\. 
(defun decorated-find-package (fn name)
  (bu::hp-find-package name 
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


(defun decorated-complete-symbol
       (fn symbol &key predicate symbols default-package return-common-string)
  (declare (ignorable predicate symbols return-common-string))
  (proga function
    (let *use-decorated-package-system-fns* t)
;    (let id (new-show-package-system-vars-id))
    (when (hp-relative-package-name-p symbol)
      (let1 pos (position #\: symbol :test 'char=)
        (when pos
          (let*
              ((pos1 (if (and (< (1+ pos) (length symbol))
                              (eql (elt symbol (1+ pos)) #\:))
                         (+ 2 pos)
                       (+ 1 pos)))
               (new-default-package (bu::hp-find-package (subseq symbol 0 pos) default-package)))
            (return-from function
              (multiple-value-bind (symbols length some-symbol some-package)
                  (apply 'decorated-complete-symbol               
                         fn 
                         (str+ (package-name new-default-package)
                               ":"
                               (subseq symbol pos1))
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
         (apply fn symbol `(,@(dispatch-keyarg-simple predicate)
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
       (return-from function (values (sort rlist 'editor::symbol-string-<)
                                     rlength rstring rpackage))))))


(decorate-function 'editor::complete-symbol
                   #'decorated-complete-symbol)

(defun decorated-buffer-package-to-use (fn &rest args)
  (let* ((res (apply fn args)))
    (minimal-fix-xlam-package res)))


(decorate-function 'editor::buffer-package-to-use #'decorated-buffer-package-to-use)

#+nil (defun decorated-pathetic-parse-symbol (fn symbol default-package &optional errorp)
  (let1 id (new-show-package-system-vars-id)
    (show-package-system-vars "decorated-pathetic-parse-symbol:before" id)
    (trace-into-text-file (str++ "decorated-pathetic-parse-symbol:default-package " id " "
                                 (package-name default-package)))
    (let1 *package* default-package ; (or *last-used-real-package* default-package)
      (funcall fn symbol default-package errorp))))


(defun decorated-pathetic-parse-symbol (fn symbol default-package &optional errorp)
  (let1 id (new-show-package-system-vars-id)
    (show-package-system-vars "decorated-pathetic-parse-symbol:before" id)
    (trace-into-text-file (str++ "decorated-pathetic-parse-symbol:default-package " id " "
                                 (package-name default-package)))
;    (let1 defaul*package* default-package ; (or *last-used-real-package* default-package)
    (let1 *use-decorated-package-system-fns* t
      (funcall fn 
               symbol 
               (minimal-fix-xlam-package default-package :stack :parse-symbol)
               errorp))))

; bu::see-packages-find-unqualified-symbol "S1" :tst

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

; (decorate-function 'editor::complete-symbol-1 #'decorated-complete-symbol-1)
; (undecorate-function 'editor::complete-symbol-1)

; editor:prompt-for-symbol editor:find-source-command
; editor::complete-symbol-1
; editor::get-symbol-from-point
; editor::complete-symbol

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

bu:se

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

