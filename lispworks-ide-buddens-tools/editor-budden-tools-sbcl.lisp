; -*- Encoding: utf-8; system :EDITOR-BUDDEN-TOOLS ;  -*- 
;;; Утилиты для работы с редактором. 
(in-package :editor-budden-tools)
(asdf::of-system :editor-budden-tools)




;lw(defvar *aux-editor*)
;lw(editor::set-vector-value
;lw (slot-value editor::*default-syntax-table* 'editor::table) '(#\[ #\{) 2)
;lw(editor::set-vector-value 
;lw (slot-value editor::*default-syntax-table* 'editor::table) '(#\] #\}) 3)

;lw(defun get-some-editor ()) 
  
#|lw(defun set-aux-editor () 
  (declare (special *aux-editor*))
  (setf *aux-editor* (get-some-editor)
        ))


 (eval-when (:load-toplevel) 
  (unless (set-aux-editor)
    (warn "Нет редактора в editor-budden-tools::set-aux-editor")))

  
 (defparameter *my-command-result* nil)
 (defparameter *my-command-fn* nil)


 (defun current-buffer-window () 
  "Окон может быть несколько, возвращает одно из"
  (car (slot-value (editor:current-buffer) 'editor::windows)))|#

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

#|
unit RusClipboard;

interface

uses Clipbrd;

type
  TRusClipboard = class(TClipboard)
   private
    procedure SetCodePage(const CodePage: longint);
   public
    procedure Open; override;
    procedure Close; override;
  end;

implementation

uses Windows;

{ TRusClipboard }

procedure TRusClipboard.Close;
begin
  SetCodePage($0419);
  inherited;
end;

procedure TRusClipboard.Open;
begin
  inherited;
  SetCodePage($0419);
end;

procedure TRusClipboard.SetCodePage(const CodePage: longint);
var Data: THandle;
    DataPtr: Pointer;
begin
  // Назначить кодовую страницу для буфера обмена
  Data:= GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, 4);
  try
    DataPtr := GlobalLock(Data);
    try
      Move(CodePage, DataPtr^, 4);
      SetClipboardData(CF_LOCALE, Data);
     finally
      GlobalUnlock(Data);
    end;
   except
    GlobalFree(Data);
  end;
end;

var FClipboard: TClipboard;
    OldClipboard: TClipboard;

initialization
  // Установить клипборд
  FClipboard:= TRusClipboard.Create;
  OldClipboard:= SetClipboard(FClipboard);
  if OldClipboard <> nil then
    OldClipboard.Free;

end.



// место исходника - editor-budden-tools.lisp
program putclip;
// версия с поддержкой Unicode. Другую версию см. в сорсах lispworks, но она почему-то хуже работает в XP (в Lispworks тоже не на всех машинах). Видимо, был какой-то ключик в реестре или ещё какая-то тонкость, но она утрачена.
{$APPTYPE CONSOLE}
uses
  SysUtils,classes,clipbrd,windows,rusclipboard;


procedure SetClipboardText(const S: string);
 var
   wsz: PWideChar;
   dwSize: Cardinal;
   hData: Cardinal;
 begin
   if (Win32Platform = VER_PLATFORM_WIN32_NT) then begin

     dwSize := (Length(S)+1) * SizeOf(WideChar);
     hData := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, dwSize);
     wsz := PWideChar(GlobalLock(hData));
     StringToWideChar(S, wsz, Length(S)+1);
     GlobalUnlock(hData);
     Clipboard.SetAsHandle(CF_UNICODETEXT, hData);
   end else
     Clipboard.AsText := S;
 end;

var 
    l:TStringList;
    s:string;
begin
l:=TStringList.Create;
if pos('?',paramstr(1))<>0 then
  begin
  writeln('Puts text to Windows clipboard.');
  writeln('Usage: ');
  writeln('putclip');
  writeln('simply reads stdin');
  writeln;
  writeln('putclip -');
  writeln('reads command line');
  writeln;
  writeln('putclip file_name');
  writeln('reads file');
  writeln;
  end
else if paramstr(1)='-' then
  begin
  s:=CmdLine;
  SetClipboardText(s);
  end
else if paramstr(1)<>'' then
  begin
  l.LoadFromFile(paramstr(1));
  SetClipboardText(l.text);
  end
else
  begin
  while not system.eof do
    begin
    system.readln(s);
    l.add(s);
    end;
  SetClipboardText(l.text);
  end;
end.

|#

#+lispworks (defun text-to-clipboard (str &aux path-to-tempfile) "saves str to clipboard"
  #+clcon (error "You would like to rewrite it with tcl")
  (setf path-to-tempfile (merge-pathnames "temp/putclip.temp" cl-user::*lisp-root*))
  (with-open-file (x path-to-tempfile :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :cp1251)
    (format x "~A" str))
  (let ((sb-impl::*default-external-format* :utf-8))
    ; если здесь лисп завис, значит надо было запускать его не с помощью M-x slime, а запустить отдельно sbcl в консоли, затем (asdf:load-system :swank), (swank:create-server) и из емакса сделать M-x slime-connect

    (sb-ext:run-program "putclip.exe"
                       (list (sb-ext::native-namestring path-to-tempfile))
                       :output nil
                       :error nil
                       :search t
                       :wait t
                       )
    ; а нижеследующий кусок может пригодиться для
    ; других команд, когда нужен ввод-вывод. Его можем
    ; делать только через батник
    #|(sb-ext:run-program "cmd.exe"
                        (list
                         "/c"
                         "start"
                         "/wait"
                         "putclip"
                         (sb-ext::native-namestring path-to-tempfile))
                        :search t
                        :input nil :output nil :error nil
                        :wait t
                        ; :external-format :utf-8
                        :directory cl-user::*lisp-root*)|#
    )
  (finish-output *standard-output*) ; волшебство, без которого SLIME
  ; перестаёт нормально работать при таком вызове
  )

(defun get-clipboard-text ()
  #+(and lispworks win32)
  (CAPI-WIN32-LIB::GET-CLIPBOARD-TEXT)
  #-(and lispworks win32)
  (error "Don't know how to get clipboard text"))


(define-constant +whitespace+ 
  (concatenate 'string '(#\Space #\Newline #\Tab #\Linefeed #\Return #\Page))
  :test 'string=
  :documentation "Whitespace characters.")

;; for sbcl it is a emacs function paste-filename
#+lispworks (editor:defcommand "Paste Filename" (p) "Pastes and unixized filename from clipboard" "Pastes and unixized filename from clipboard"  
  (declare (ignore p))
  #+ignore (editor:backward-kill-form-command p)
  (insert-string (current-point) 
                 (format nil "~S" 
                         (substitute 
                          #\/ #\\ 
                          (string-trim +whitespace+ (get-clipboard-text))))))

; в sbcl есть ф-я EMACS insert-string-from-clipboard
#+lispworks (editor:defcommand "Insert string from clipboard" (p) "Pastes string from a clipboard as a lisp string"
     (declare (ignore p))
  (insert-string (current-point)
                 (format nil "~S"
                         (string-trim +whitespace+ (capi-win32-lib::get-clipboard-text)))))

; в sbcl - ф-я EMACS buffer-pathname-to-clipboard
#+lispworks (defcommand "Buffer Pathname To Clipboard" (p)
     "Копирует имя файла (namestring) в буфер обмена"
     (declare (ignorable p))
     (proga 
       (let pathname (buffer-pathname (current-buffer)))
       #+nil (capi-win32-lib::set-clipboard-text (namestring pathname)) ; это не работает
       (text-to-clipboard (namestring pathname))))

(defun window-handle-by-buffer (buffer)
  "Возвращает handle первого окна данного буфера"
  #-lispworks (declare (ignore buffer))
  #-lispworks (break "not implemented")
  #+lispworks
  (progn
  (assert (typep buffer 'editor::buffer))
  (let1 window (car (editor:buffer-windows buffer))
    (let1 pane (slot-value window 'editor::text-pane)
      (let1 representation (slot-value pane 'capi-internals:representation)
        (slot-value representation 'win32:hwnd))))))

#+lispworks (defun goto-buffer-2 (buffer in-same-window &key (set-foreground-window t))
  "Не только отображает буфер, но и показывет окно"
  (goto-buffer buffer in-same-window)
  (when set-foreground-window 
    (win32:set-foreground-window (window-handle-by-buffer buffer)))
  )

#+lispworks (defun goto-or-create-xy (pathname row col &key kill-buffer (set-foreground-window t))
  (capi:find-interface 'lispworks-tools:editor)
  (goto-xy pathname row col :kill-buffer kill-buffer :set-foreground-window set-foreground-window))

; для SBCL есть соотв. команда EMACS
#+(and sbcl (not clcon))
(defmethod goto-xy (pathname row col)
  (swank:eval-in-emacs `(goto-xy ,(namestring pathname) ,row ,col)))

#+(and sbcl clcon)
(defgeneric goto-xy (pathname row col))
   
#+lispworks (defun GOTO-XY (PATHNAME ROW COL &KEY KILL-BUFFER (set-foreground-window t))
  "Не сработает при отсутствии редактора. При kill-buffer опасно, т.к. закрывает файл без изменений"
  (perga
    (let ED (get-some-editor))
    (unless ed (error "Не могу показать исходника - нет окна редактора!"))
    (
     #+lispworks6 capi:execute-with-interface 
                  #+lispworks4 capi:apply-in-pane-process
                  ed
                  (LAMBDA ()
                    (perga
                    (ignore-errors
                      (editor::switch-to-or-from-typeout-command 0))
                    (let buf (editor::find-file-in-buffer-list PATHNAME #'ignored))
                    (when (and buf kill-buffer)
                      (editor::kill-buffer-no-confirm buf))
                    (unless buf
                      (setf buf (find-file-buffer pathname #'ignored)))
                    (goto-buffer-2 buf t :set-foreground-window set-foreground-window))
                    (capi:call-editor ed "Beginning Of Buffer" )
                    (when (> row 1) (ignore-errors (NEXT-LINE-COMMAND (- ROW 1))))
                    (when (> col 1) 
                      (ignore-errors
                        (FORWARD-CHARACTER-COMMAND (- COL 1))))
                    (ignore-errors (GOTO-BUFFER-2 (FIND-FILE-BUFFER PATHNAME) T :set-foreground-window set-foreground-window))
                    ))
    ))

(defun real-point-offset (point) 
  "offset of the point is not an offset indeed. Trying to calculate real offset relative to buffer start"
  #+lispworks
  (let ((result 
         (+ (point-offset point) (slot-value (editor::point-bigline point) 'editor::start-char))))
    (unless (= result (editor::find-point-offset (mark-buffer point) point))
      (cerror "продолжить"
       "нашёл функцию в редакторе, к-рая делает то же самое, что и real-point-offset. Надеялся, что они одинаковы")
      )
    result)
  #-lispworks
  (declare (ignore point))
  #-lispworks
  (error "not implemented in this lisp"))

#|(defcommand "hg" (p) "Calls hg on current file"
     (declare (ignorable p))
  (proga 
    (let pathname (buffer-pathname (current-buffer)))
    (let file (namestring pathname))
    ;(let path (path-to-a-file pathname))
    (let subcommand (prompt-for-symbol nil :prompt "Hg command" :default :commit))
    (save-file-command nil)
    (editor::SWITCH-TO-OR-FROM-TYPEOUT-COMMAND nil)
    (system:call-system-showing-output
     (format nil "hg ~A ~A" 
             (string-downcase (string subcommand))
             (str+ (string-downcase (subseq file 0 1)) (subseq file 1)))
     :wait t :show-cmd t 
     :current-directory (user-homedir-pathname) ; бага в hg: не работает для подкаталогов репозитория, если находишься в них, а не в корне
     )
    ))|#


#|(defcommand "hgtk" (p) "Calls hgtk on current file"
     (declare (ignorable p))
  (proga 
    (let pathname (buffer-pathname (current-buffer)))
    (let file (namestring pathname))
    ;(let path (path-to-a-file pathname))
    (let subcommand (prompt-for-symbol nil :prompt "Hg command" :default :commit))
    (save-file-command nil)
    (system:call-system-showing-output
     (format nil "hgtk ~A ~A" 
             (string-downcase (string subcommand))
             (str+ (string-downcase (subseq file 0 1)) (subseq file 1)))
     :current-directory (user-homedir-pathname)
     )
    ))|#


#|(defcommand "hg-commit-buffers-directory" (p) "calls hg commit on current buffer's directory"
     (declare (ignorable p))
  (proga
    (let path (path-to-a-file (buffer-pathname (current-buffer))))
    (system:call-system-showing-output 
     (format nil "hgtk commit")
     :current-directory path)))|#

#+lispworks (defun get-current-buffer-text-and-offset ()
  "Возвращает два значения - полный текст из текущего окна и смещение точки"
  (proga function
    (let pnt (current-point))
    (let offset (real-point-offset pnt))
    (let text (capi:editor-pane-text (editor:window-text-pane (editor:current-window))))
    (values text offset)
    ))

; в EMACS - команда find-file-at-point
#+lispworks (defun get-filename-at-point-and-prompt ()
  "Подсказка для открытия файла, имя к-рого находится в текущей точке" 
  (perga function
    (let location (namestring (editor::buffer-default-directory (current-buffer))))
    (mlvl-bind (text offset) (get-current-buffer-text-and-offset))
    ; получить текст из текущего окна
    
    (let scanner (cl-ppcre:create-scanner 'PPCRE-SHORTCUTS::filename-with-path))
    (let forward-part
      (cl-ppcre:do-matches (o1 o2 scanner text nil :start offset) ; ( scanner (subseq1 text offset))
        (return
         (if (= o1 offset)
             (subseq text o1 o2)
           nil))))
    (let reversed-text (reverse text))
    (let reversed-offset (- (length reversed-text) offset))
    (let backward-part
      (cl-ppcre:do-matches (o1 o2 scanner reversed-text nil :start reversed-offset) ; ( scanner (subseq1 text offset))
        (return
         (if (= o1 reversed-offset)
             (reverse (subseq reversed-text o1 o2))
           nil))))
    (let matched-filename
      (str++ (or backward-part "")
             (or forward-part "")))
    (let result (prompt-for-file :default-string (str++ location matched-filename) :default "fh"))
    (return-from function result)
    ))

(defvar *in-find-source* nil "in lispworks, it is bound to t in find-source-command by defadvice")

#+lispworks
(lw:defadvice (find-source-command bind-in-find-source :around) (&rest args)
  (let ((*in-find-source* t))
    (apply #'lw:call-next-advice args)))

; no need for that in lispworks with asdf-tools
#+lispworks
(defcommand "Find current package definition" (p) "Goto current p"
     (declare (ignorable p))
  (declare (ignorable p))
  (editor:find-source-for-dspec-command
   nil
   (list 'defpackage
         (package-name (editor::buffer-package-to-use (editor:current-buffer))))))

; no need for that in lispworks with asdf-tools
#+lispworks
(defcommand "Find current system definition" (p) "Goto current system from attribute line"
     (declare (ignorable p))
  (declare (ignorable p))
  (perga
    (let buf (current-buffer))
    (let system (buffer-value buf 'system))
    (cond
     ((null system)
      (editor:message "no system is defined in attribute line (e.g. ; -*- system :mysys ; -*- )"))
     (t
      (let system-sym
        (let ((*read-eval* nil))
          (budden-tools::careful-keywordize system)))
      (editor:find-source-for-dspec-command
       nil
       (list 'asdf:defsystem
             system-sym))))))


#+lispworks (defcommand "Grep symbol" (p) "Finds symbol with grep"
     (declare (ignorable p))
  (let1 sym (prompt-for-symbol nil :prompt "What?")
    (setf lispworks:*grep-command-format* (str+ "~a ~a \"" sym "\" c:/project/sales_budden/*.pas c:/lisp/sw/fb2/wrapper.lisp"))
    (grep-command nil)))

; (bind-key "Grep symbol" "f4")
             
                      


(defun fix-offset-with-no-of-newlines (offset no-of-newlines)
  (declare (ignore no-of-newlines))
  (- offset #+nil no-of-newlines 1))


(DEFUN GOTO-OFFSET (PATHNAME OFFSET &KEY KILL-BUFFER (set-foreground-window t) subtract-no-of-newlines)
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

#+lispworks
(defun modest-goto-offset (point offset)
  "Находимся в команде. Есть точка в буфере. Переместить её на смещение p, где смещение имеется в виду
  по буквам. t, если успех, nil - если слишком далеко. См. также goto-offset, editor::goto-point-command"
  (let ((result nil))
    (buffer-start point)
    (setf result (editor::limited-character-offset point offset))
    (real-point-offset point) ; ради побочного эффекта - проверяем, что всё ок с вычислением
    result
    )
  )
  


(defun budden-tools::edit-stream-position (stream &optional position subtract-no-of-newlines) 
  ;(editor-budden-tools::do-in-editor-command (budden-tools::extract-source-filename-from-stream stream)
  (perga ; ignore-errors
    (let filename (budden-tools::extract-source-filename-from-stream stream))
    (let position (or position (budden-tools::extract-file-position stream)))
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
     

#+lispworks
(defun char-at-point (p)
  (perga
    (let pp (copy-point p :temporary))
    (character-offset pp 1)
    (elt (points-to-string p pp) 0)))

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

#|(defun get-symbol-from-point (point &key (previous t)
                                    white-space-only 
                                    (max-length 100)
                                    (max-non-alphanumeric 15)
                                    (create-new nil ; budden
                                                ; budden t
                                                ))
  (with-point-locked
      (point :for-modification nil :errorp nil)
    (when-let (symbol-name
               (editor::i-read-symbol-from-point  point t nil 
                                          (if previous 
                                              nil
                                            (if white-space-only  :whitepace t))))
      (unless (or (> (length symbol-name) max-length)
                  (> (count-if-not
                      'lw:unicode-alphanumericp ; budden
                      ; budden 'alphanumericp
                      symbol-name) max-non-alphanumeric))
        (let ((package (editor::buffer-package-to-use point)))
          (multiple-value-bind (found-symbol status)
              (editor::find-symbol-from-string symbol-name package)
            (cond
             (status
              (values found-symbol status))
             (create-new 
              (editor::intern-symbol-from-string symbol-name package))
             (t ;
              (let ((all-matching (find-all-symbols symbol-name)))
                (cond
                 (all-matching
                  (first all-matching) ; не всегда можно вызывать меню. 
                         ;(select-symbol-from-list all-matching package (format nil "No symbol ~S here. Choose one:" "CLIENT"))
                  )
                 (t
                  nil 
                  )))))))))))|#


#|(defun my-get-defun-start-and-end-points (point start end)
  "Не тестировано, положил для коллекции. Changed clone of get-defun-start-and-end-points. If it fails to find end of defun, returns 
end of buffer. Returns 0 if it found neither start nor end, 1 if it found start, 2 if it found both"
  (move-point start point)
  (line-start start)
  (editor::top-level-offset start -1)
  (when (not (editor::start-defun-p start))
    (unless (editor::top-level-offset start 1)
      (return-from my-get-defun-start-and-end-points 0)))
  (move-point end start)
  (if (form-offset end 1)
      2
    1))|#


#+lispworks (defparameter EDITOR-BUDDEN-TOOLS:*ide-code-snippets* 
 "
  Здесь будем хранить полезные знания о функциях среды
  IDE CODE SNIPPETS
  DSPEC::find-dc 'package - находит класс dspec пакета
  для редактора: Разные code snippets для IDE
  'editor::i-find-pattern - фактически это funcall
  EDITOR::%i-search-find-pattern - есть исходник
  LISPWORKS-TOOLS::inspect-an-object - открыть инспектор для объекта
  ")


#+lispworks (defmacro editor-do-registers ((name value &optional sorted) &rest body)
  "Re-birth of editor::do-registers"
  (if sorted
      (let ((sorted-regs (gensym))
	    (reg (gensym)))
	`(let ((,sorted-regs nil))
	   (declare (list ,sorted-regs))
	   (maphash #'(lambda (,name ,value)
			(push (cons ,name ,value) ,sorted-regs))
		    editor::*registers*)
	   (setf ,sorted-regs (sort ,sorted-regs #'char-lessp :key #'car))
	   (dolist (,reg ,sorted-regs)
	     (let ((,name (car ,reg))
		   (,value (cdr ,reg)))
	       ,@body))))
      `(maphash #'(lambda (,name ,value)
		    ,@body)
		editor::*registers*)))



;; stolen from 'portable hemlock' project which magically turned out to be
;; compatible with lispworks' editor
#+lispworks (defmacro editor::define-file-option (name lambda-list &body body)
  "Define-File-Option Name (Buffer Value) {Form}*
   Defines a new file option to be user in the -*- line at the top of a file.
   The body is evaluated with Buffer bound to the buffer the file has been read
   into and Value to the string argument to the option."
  (let ((name (string-downcase name)))
    `(setf (cdr (or (assoc ,name editor::*mode-option-handlers*  :test #'string=)
		    (car (push (cons ,name nil) editor::*mode-option-handlers*))))
	   #'(lambda ,lambda-list ,@body))))


#+lispworks (editor::define-file-option "System" (buffer value)
  (perga
    (mlvl-bind (p name) (starts-with-subseq ":" value :return-suffix t))
    (:lett name2 string (if p name value))
    (editor::def-ed-var 'system
                        :buffer buffer
                        name2 "This is the system of this buffer")
    (update-buffer-modelines buffer)))

  #|
  (set-buffer-current-package buffer
   (let ((form (ignore-errors (read-from-string value nil nil))))
     (if (and form
              (or (typep form 'symbol) (typep form 'list)))
         (let* ((pname (if (symbolp form)
                           form
                         (car form)))
	        (package (find-package-without-lod pname)))
	   (if (packagep package)
	       package
	     (if (listp form)
		 (make-package-from-attribute-form form)
	       (unless *running-operation*
		 (message "Could not find package ~A" pname)
		 nil))))
       (progn
         (message "Invalid package specification \"~A\" on attribute line" value)
         nil))))|# 



#+lispworks (defadvice (editor::default-modeline-function-function print-system-and-readtable :around) (window)
  (let* ((res (multiple-value-list (lispworks:call-next-advice window)))
         (buffer (window-buffer window))
         (system
          (cond
           (buffer (buffer-value buffer 'system))
           (t nil)))
         (system-str
          (if system (str+ "     s " system) "")))
    (setf (nth 3 res)
          (str++ "p " (nth 3 res)
                 system-str))
    (apply 'values res)))
 
      

#+lispworks (defmacro crossref-command-nachinka (query-fn)
  (with-gensyms (sym)
    `(let ((,sym (editor::get-symbol-from-point (editor::current-point) :previous nil :create-new nil)))
       (when ,sym 
         (editor::with-output-to-help-window-1 
          #'(lambda (stream)
              (let ((*package* (find-package :cl-user)))
                (format stream "~%~S ~S :~%" ',query-fn ,sym)
                (print (,query-fn ,sym) stream))))
         ))))
  
#+lispworks (editor:defcommand "Who References" (p) "" "" 
  (declare (ignore p))
  (crossref-command-nachinka hcl:who-references))


#+lispworks (editor:defcommand "References Who" (p) "" "" 
  (declare (ignore p))
  (crossref-command-nachinka hcl:references-who)
  )

#+lispworks (editor:defcommand "Who binds" (p) "" "" 
  (declare (ignore p))
  (crossref-command-nachinka hcl:who-binds)
  )

#+lispworks (editor:defcommand "Binds who" (p) "" "" 
  (declare (ignore p))
  (crossref-command-nachinka hcl:binds-who)
  )

#+lispworks (editor:defcommand "Sets who" (p) "" "" 
  (declare (ignore p))
  (crossref-command-nachinka hcl:sets-who)
  )

#+lispworks (editor:defcommand "Who sets" (p) "" "" 
  (declare (ignore p))
  (crossref-command-nachinka hcl:who-sets)
  )



#| 

Хотим экспортировать все ф-ии, определённые в файле. Как поступим? 

 (LISPWORKS:defadvice (dspec:record-definition steal-definition-name :around) (dspec &rest args)

   (format t "~%~A:~A" *package* dspec)
   (apply #'LISPWORKS:call-next-advice dspec args))

 затем компилируем файл, затем
   
   (lispworks:remove-advice 'dspec:record-definition 'steal-definition-name)

|#

(defun is-element-new-in-hash (x hash)
  "If x is in hash already, returns nil. Otherwise returns t and adds x to a hash"
  (cond
   ((gethash x hash)
    nil)
   (t
    (setf (gethash x hash) t)
    )))


#+lispworks (defun maybe-print-name-of-dspec (dspec stream package &key exported-already-hash)
  (cond
   ((and (hash-table-p exported-already-hash)
        (not (is-element-new-in-hash dspec exported-already-hash)))
   ; don't print as we only want to print every dspec once
    )
   (t
    (typecase dspec
      (symbol
     ; (unless (member dspec '(defun defvar defparameter defstruct defclass nil))
       (when (eq (symbol-package dspec) package)
         (let ((*package* (find-package :keyword)))
           (format stream "~% ~S" dspec))))
      (cons
       (apply #'maybe-print-name-of-dspec (car dspec) stream package (dispatch-keyargs-simple exported-already-hash))
       (apply #'maybe-print-name-of-dspec (cdr dspec) stream package (dispatch-keyargs-simple exported-already-hash))
       )))))
     


#+lispworks (defun steal-definitions-from-file-for-export (filename)
  (perga
    (:lett advice-name symbol (gensym "steal-definition-name"))
    (:lett exported-already hash-table (make-hash-table :test 'equal))
    (:lett exports string
     (with-output-to-string (ou)
       (eval 
        `(lw:defadvice (DSPEC:record-definition ,advice-name :after) (dspec &rest args)
           (maybe-print-name-of-dspec
            dspec ,ou *package*
            :exported-already-hash ,exported-already))
           )
       (unwind-protect
           (load (compile-file filename))
         (lw:remove-advice 'dspec:record-definition advice-name))))
    (format t "; generated by (budden-tools::steal-definitions-from-file-for-export ~S)~%" (pathname filename))
    `(:export ,exports)))
        
  
