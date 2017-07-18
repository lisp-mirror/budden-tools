; -*- coding: utf-8; system :EDITOR-BUDDEN-TOOLS ;  -*- 
;;; Утилиты для работы с редактором. 
(in-package :editor-budden-tools)
(asdf::of-system :editor-budden-tools)




;lw(defvar *aux-editor*)
;lw(editor::set-vector-value
;lw (slot-value editor::*default-syntax-table* 'editor::table) '(#\[ #\{) 2)
;lw(editor::set-vector-value 
;lw (slot-value editor::*default-syntax-table* 'editor::table) '(#\] #\}) 3)

(defmacro do-in-editor-command (buffer &body body)
  "Выполняет body внутри команды. Задействованы переменные *aux-exitor* и *my-command-result*. Создает block nil
  "
  (declare (ignore buffer body))
  #+sbcl (break "not implemented")
  )
  
(defun return-expr-points (buffer expr)
  "Находит выражение, начиная от текущей позиции в буфере и возвращает список из точки начала и точки конца.
   Двигает текущую позицию в конец. Если не находит, то возвращает null"
  (declare (ignore buffer expr))
  (break "not implemented"))
 
(defun replace-string-in-file (filename from to &key (test #'eql) (external-format nil external-format-supplied-p) all)
  "Заменяет строку в файле. all - заменить все вхождения. Пишет в исходный файл. "
  (proga 
    (let chars (apply 'read-file-into-string filename (dispatch-keyargs-full external-format)))
    (let new-string (search-and-replace-seq 'string chars from to :test test :all all))
    (apply 'write-string-into-file new-string filename :if-exists :supersede (dispatch-keyargs-full external-format))))
    
    
(defun replace-once (filename from to)
  "Заменяет from на to один раз, с текущей позиции                                   . Функция устарела - нужно пользоваться 
replace-string-in-file filename from to :close-file nil :times 1 :from-the-start nil . 
Возможное проблемное место - replace-string-in-file не двигает курсор вперед         . "
  (declare (special *aux-editor*))
  (declare (ignore filename from to))
  (error "deprecated, us replace-string-in-file"))

(define-constant +whitespace+ 
  (concatenate 'string '(#\Space #\Newline #\Tab #\Linefeed #\Return #\Page))
  :test 'string=
  :documentation "Whitespace characters.")

; для SBCL есть соотв. команда EMACS
#+(and sbcl (not clcon))
(defmethod goto-xy (pathname row col)
  (swank:eval-in-emacs `(goto-xy ,(namestring pathname) ,row ,col)))

;; в SBCL оно, видимо, будет позднее
   
(defvar *in-find-source* nil "in lispworks, it is bound to t in find-source-command by defadvice")

(defun fix-offset-with-no-of-newlines (offset no-of-newlines)
  (declare (ignore no-of-newlines))
  (- offset #+nil no-of-newlines 1))


(defmethod GOTO-OFFSET (PATHNAME OFFSET &KEY KILL-BUFFER set-foreground-window subtract-no-of-newlines)
  "Offset может быть:
  - структурой row-col-offset
  - смещением в буквах
  - file-position. В этом случае нужно передать аргумент subtract-no-of-newlines

  См. также editor::count-lines, editor::what-line-command
  
  Данная команда не сработает при отсутствии редактора. При kill-buffer опасно, т.к. закрывает файл без изменений. 
   См. также modest-goto-offset"
  #+sbcl (declare (ignore set-foreground-window kill-buffer))
  (perga
    #+lispworks (let ed (get-some-editor))
    #+lispworks (unless ed 
      (warn "goto-offset: Нет окна редактора! Не могу показать место ~S в файле ~S" offset pathname)
      (return-from goto-offset nil)
      )

    (when (and (typep offset 'integer)
               subtract-no-of-newlines)
      (setf offset (fix-offset-2 pathname offset)))

    (:lett str string "")

   ; integer здесь - это почти рудимент, но боюсь трогать!
    (:lett no-of-newlines integer
     (etypecase offset
       (row-col-offset 0)
       (integer
        (setf str (subseq1 (read-file-into-string pathname) 0 offset))
        (count #\Newline str))))
   
    (:lett line-count integer
     (etypecase offset
       (row-col-offset (- (row-col-offset-row offset) 1))
       (integer no-of-newlines)))
    (:lett char-count integer
     (etypecase offset
       (row-col-offset (- (row-col-offset-col offset) 1))
       (integer
        (let ((newline-position (position #\Newline str :from-end t)))
          (- (length str) (if newline-position (+ newline-position 1) 0))))))

    (goto-xy pathname line-count char-count)
  
    #+lispworks (#+lispworks6 capi:execute-with-interface 
                  #+lispworks4 capi:apply-in-pane-process
                  ED
                  (LAMBDA ()
                    (when kill-buffer (editor::kill-buffer-no-confirm nil (FIND-FILE-BUFFER PATHNAME #'ignored)))
                    (let1 buf (find-file-buffer pathname #+nil #'ignored)
                      (goto-buffer-2 buf t))
                    (capi:call-editor ed "Beginning Of Buffer" )
                                  ;(ignore-errors 
                    (next-line-command line-count)
                    (FORWARD-CHARACTER-COMMAND char-count);)
                    (ignore-errors (GOTO-BUFFER-2 (FIND-FILE-BUFFER PATHNAME) T :set-foreground-window set-foreground-window))
                    ))
    #+lispworks6 
    (ignore-errors
      (CAPI:set-pane-focus (slot-value ed 'LISPWORKS-TOOLS::editor-panes-pane)))
    
    ))

(defun budden-tools::edit-stream-position (stream &optional position subtract-no-of-newlines) 
  ;(editor-budden-tools::do-in-editor-command (budden-tools::extract-source-filename-from-stream stream)
  (perga ; ignore-errors
    (let filename (budden-tools:extract-source-filename-from-stream stream))
    (let position (or position (budden-tools:input-stream-position-in-chars stream)))
      ;(print buf)
      ;(print position)
    (goto-offset filename position :kill-buffer nil :subtract-no-of-newlines subtract-no-of-newlines)
    )
  )

#+nil (defun editor::WORD-DELIMITER-ATTRIBUTE-P (char)
  (case char ; вроде не работает? 
    ((#\  #\Tab #\Newline #\( #\) #\[ #\] #\{ #\} #\; #\^ #\" #\' #\, #\. #\+ #\= #\:)
     t)
    (t nil)))
     

(defun char-can-be-in-symbol (c)
  "Проверяем символы в общем смысле слова, не в смысле лиспа. Только тире позволяем быть. Также разрешаем двоеточие"
  (not (find c '(#\  #\newline #\, #\^ #\' #\" #\` #\tab #\; #\( #\) #\@
                               ; попробуем ловить символы АА:|Бб| #\|
                               #\# #\\ #\>))))

(defun process-potential-symbol (x package)
  "returns either (symbol t), or (values nil nil) if there is no symbol"
  (perga
    (let found-package
      (sbcl-reader-budden-tools-lispworks:potential-symbol-package x))
    (let maybe-symbol-name
      (sbcl-reader-budden-tools-lispworks:potential-symbol-casified-name x))
    (mlvl-bind (maybe-symbol storage)
        (find-symbol maybe-symbol-name found-package))
    (cond
     (storage
      (values maybe-symbol t))
     (*in-find-source*
      (values
       (try-to-choose-the-best-matching-symbol-in-find-source maybe-symbol-name package)
       t)
      )
     (t
      (values nil nil)))))

    
;  (setf max-non-alphanumeric 100)
;  (mlvl-bind (value error)
;      (apply fn point (dispatch-keyargs-simple previous max-length max-non-alphanumeric create-new))
;    (if error nil value)))

; #+lispworks6 (budden-tools::decorate-function 'editor::get-symbol-from-point 'decorated-get-symbol-from-point)

#|(decorate-function:portably-without-package-locks
; non-toplevel
(editor::defcommand "Function Argument List" (p) "Переопределил, т.к. глючило. Также покажет структуру"
     (declare (ignore p))
  (let1 sym (editor::get-symbol-from-point (editor::current-point) :previous nil :create-new nil)
    (cond
     ((fboundp sym)
      (editor::describe-function-lambda-list sym))
     ((typep (ignore-errors (find-class sym nil)) 'structure-class)
      (editor::message
       (let ((class (find-class sym))
             (*print-pretty* nil)
             (*package* (editor::buffer-package-to-use (editor:current-buffer))))
         (prin1-to-string 
          `(defstruct ,sym ,@(STRUCTURE:structure-class-slot-names class))))))
     ) ; cond
    )))|#

(defun select-symbol-from-list (symbols package message)
  "Returns two values: symbol returned and (t if choice accepted, nil if it is rejected)"
  #+(and (not lispworks) swank)
  (let* (line
         num
         (text
           (let ((*package* package)
                 (n 1))
             (with-output-to-string (out)
               (dolist (sym symbols)
                 (format out "~%~A = ~S" n sym)
                 (incf n))
               ))))
    (format t "(zero or just RETURN to abort)~A~A~%?" message text)
                                        
      ; FIXME check the ability to do this call
    (swank:eval-in-emacs `(progn (slime-repl) nil))
    (setf line (let ((*read-eval* nil))
                 (read-line *standard-input*)))
    (setf num (parse-integer line :junk-allowed t))
    (cond
      ((or (= num 0) (null num))
       (values nil nil))
      (num
       (values (elt symbols (- num 1)) t)))) 
  #+lispworks (let* ((items
          (let ((*package* package))
            (mapcar 'prin1-to-string symbols))))
    (multiple-value-bind (choice success)
        (capi:prompt-with-list
         items
         message)
      (cond
       (success
        (values (elt symbols (position choice items :test 'string=)) t))
       (t
        (values nil nil))))))


#+(and sbcl swank)
(defun editor-error (format-string &rest format-args)
  (let ((message (apply #'format nil format-string format-args)))
    (swank:eval-in-emacs `(message ,message))
    (apply #'error format-string format-args)))
    

(defun try-to-choose-the-best-matching-symbol-in-find-source (casified-string package)
  "Returns symbol or errs"
  (perga
    (let all-matching (find-all-symbols casified-string))
    (cond
     (all-matching
      (mlvl-bind (symbol success)
          (select-symbol-from-list
           all-matching package
           (format nil "No symbol ~S in ~A. Choose one:"
                   casified-string (package-name package))))
      (cond
       (success symbol)
       (t (editor-error "User canceled symbol selection"))
       )
      )
     (t
      (editor-error "No symbol named ~S in image" casified-string) 
      ))))

(defun is-element-new-in-hash (x hash)
  "If x is in hash already, returns nil. Otherwise returns t and adds x to a hash"
  (cond
   ((gethash x hash)
    nil)
   (t
    (setf (gethash x hash) t)
    )))


       
  
