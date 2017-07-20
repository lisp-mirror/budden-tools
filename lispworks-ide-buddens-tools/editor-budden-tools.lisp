; -*- coding: utf-8; system :EDITOR-BUDDEN-TOOLS ;  -*- 
(in-package :editor-budden-tools)
(asdf::of-system :editor-budden-tools)

;;; Утилиты для работы с редактором (lispworks). Для clcon см. editor-budden-tools-sbcl.lisp 

#+new-projects-structure 
(defun this-system-dir ()
  (component-pathname (component-system (load-time-value *current-component*))))

(defvar *aux-editor* #+ignore (capi:contain
              (make-instance 'capi:editor-pane
                             :text "abc")))
(editor::set-vector-value
 (slot-value editor::*default-syntax-table* 'editor::table) '(#\[ #\{) 2)
(editor::set-vector-value 
 (slot-value editor::*default-syntax-table* 'editor::table) '(#\] #\}) 3)

#|(defun get-some-editor-0 () 
  (loop for x in capi-win32-lib::*top-level-windows* 
              for elt = (slot-value x 'win32::element) 
              when (starts-with-subseq "Editor " (slot-value elt 'capi::title))
              do (return (slot-value elt #+lispworks4 'capi:editor-pane #+lispworks6 'lispworks-tools::definition-name-pane))))|#

#-lispworks6.1
(defun get-some-editor () 
  (loop for x in capi-win32-lib::*top-level-windows* 
              for elt = (slot-value x 'win32::element) 
              when (starts-with-subseq "Editor " (slot-value elt 'capi::title))
              do (return #+lispworks4 (slot-value elt 'capi:editor-pane) #+lispworks6 elt)))

#+lispworks6.1
(defun get-some-editor ()
  "Ensures editor, activates and returns it"
  (or (CAPI:locate-interface 'lispworks-tools:editor)
      (CAPI:find-interface 'lispworks-tools:editor :display-state :iconic))
  #|(let* ((interfaces (slot-value (car capi-win32-lib::*screens*) 'capi::interfaces)))
    (assert interfaces () "the-listener: unable to find interfaces")
    (let ((editor
           (find 'lispworks-tools:editor interfaces :key 'type-of)))
      editor))|#
  )
  
(defun set-aux-editor () 
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
  (car (slot-value (editor:current-buffer) 'editor::windows)))

(defmacro do-in-editor-command (buffer &body body)
  "Выполняет body внутри команды. Задействованы переменные *aux-exitor* и *my-command-result*. 
   Создает block nil
  "
  (declare (special *aux-editor*))
  (declare (special *my-command-result*))
  (with-gensyms () ; (buffer2 fn)
    `(proga
       (set-aux-editor)
       (setf *my-command-fn* 
             (lambda () 
               (use-buffer ,buffer
                 (setf *my-command-result* (block nil (progn ,@body))))))
       ; не thread-safe
       (eval `(defcommand "come on" (p) "asdf" "Служебная команда из do-in-editor-command"
                (funcall *my-command-fn*)))
       (#+lispworks6 capi:execute-with-interface 
                     #+lispworks4 capi:apply-in-pane-process
                     *aux-editor* 'capi:call-editor *aux-editor* "come on")
       *my-command-result*)))
       
                
(defun return-expr-points (buffer expr)
  "Находит выражение, начиная от текущей позиции в буфере и возвращает список из точки начала и точки конца.
   Двигает текущую позицию в конец. Если не находит, то возвращает null"
  (declare (special *aux-editor*))
  (eval
   `(defcommand "come on" (p) "asdf" "Служебная команда" 
      (declare (special *my-command-result*))
      (setf *my-command-result* nil)
      (use-buffer ,buffer
        (let ((pnt (copy-point (current-point)))
              (search-string ,expr))
          (editor:forward-search-command nil search-string pnt)
          (cond
           ((editor:point< (current-point) pnt) ; something found
            (let1 endpnt (copy-point pnt)
              (editor:backward-search-command nil search-string pnt) ; search start
              (move-point (current-point) endpnt) ; move to end
              (setf *my-command-result* `(,pnt ,endpnt))))
           (t (setf *my-command-result* nil)))))))
;  (capi:apply-in-pane-process *aux-editor* 'capi:call-editor *aux-editor* "come on")
  '(capi:call-editor *aux-editor* "come on")
  *my-command-result*)

; (defun return-regexp-points (buffer expr) такая функция почему-то не работает.
; при поиске по рег. выражению курсор не всегда встает в конец выражения. 
; отсюда мораль - нет возможности работать в лиспворкс с регулярными выражениями. Полдня убил!

#|(defun write-between-points (buffer pnt1 pnt2 str)
  "Заменить текст между точками на str"
  (declare (special *aux-editor*))
  (eval
   `(defcommand "come on" (p) "asdf" "Служебная команда" 
      (declare (special *my-command-result*))
      (setf *my-command-result* nil)
      (use-buffer ,buffer
        (move-point (current-point) ,pnt1) ; make region
        (set-current-mark ,pnt2)
        (delete-region-command nil)
        (insert-string (current-point) ,str))))
   (capi:call-editor *aux-editor* "come on")
  *my-command-result*)|#


(defun save-and-close-buffer (filename)
  "Должен быть открыт файл filename. Его сохраняет и закрывает"
  (let1 buf (find-file-buffer (pathname filename))
    (assert buf)
    (format *debug-io* "saving ~A~%" filename) 
    (save-file-command nil buf)
    (editor::kill-buffer-no-confirm buf)))

 
(defun replace-string-in-file (filename from to &key (test #'eql))
  "Редактор нормально не работает. Поэтому, нужно пользоваться регекспами, как в creatab2deftbl.lisp"
  (proga 
    (let chars (read-file-into-string filename))
    (let new-string (search-and-replace-seq 'string chars from to :test test))
    (write-string-into-file new-string filename :if-exists :supersede)))
    
    
(defun replace-once (filename from to)
  "Заменяет from на to один раз, с текущей позиции. Функция устарела - нужно пользоваться 
replace-string-in-file filename from to :close-file nil :times 1 :from-the-start nil.
Возможное проблемное место - replace-string-in-file не двигает курсор вперед."
  (declare (special *aux-editor*))
  (eval
   `(defcommand "come on" (p) "asdf" "..." 
      (declare (special *my-command-result*))
      (setf *my-command-result* nil)
      (use-buffer (find-file-buffer (pathname ,filename))
        (let ((pnt (copy-point (current-point)))
              (search-string ,from))
          (editor:forward-search-command nil search-string pnt)
          (when (editor:point< (current-point) pnt) ; something found
            (let1 endpnt (copy-point pnt)
              (editor:backward-search-command nil search-string pnt) ; move pnt to start?
              (move-point (current-point) pnt) ; make region
              (set-current-mark endpnt)
              (delete-region-command nil)
              (insert-string (current-point) ,to)
              (forward-character-command nil) ; ??? do we need it? where is current point?
              (setf *my-command-result* t)))))))
  (capi:call-editor *aux-editor* "come on")
  *my-command-result*)


#+ignore (defun replace-string-in-file (filename from to &key (from-the-start t) (close-file t) (times nil))
  "Заменяет все вхождения. Сохраняет (если нужно) и закрывает файл. Устарело. Пользоваться регэкспами, creatab2deftbl.lisp"
  (let ((buf (editor:find-file-buffer filename)))
    (editor:use-buffer buf 
      (when from-the-start (editor:beginning-of-buffer-command))
      (editor:replace-string-command times from to)
      (when (buffer-modified buf)
        (save-file-command nil buf))
      (when close-file (kill-buffer-command nil buf)))))


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
// версия с поддержкой OEM
{$APPTYPE CONSOLE}
uses
  SysUtils,classes,clipbrd,windows,rusclipboard;

(* function stringToOEM(s:string):string; 
begin // http://delphiworld.narod.ru/base/rus_console.html
Setlength(result,length(s)); 
if not CharToOem(PChar(s),PChar(result)) then halt(177); 
end;  *)

var c:TClipboard;
    l:TStringList;
    s:string;
begin
c:=Clipboard;
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
  c.AsText:=copy(s,length(paramstr(0))+4,length(s));
  end
else if paramstr(1)<>'' then
  begin
  l.LoadFromFile(paramstr(1));
  c.AsText:=l.text;
  end
else
  begin
  while not system.eof do
    begin
    system.readln(s);
    l.add(s);
    end;
  c.AsText:=l.text;
  end;
end.
|#

(defun text-to-clipboard (str &aux path-to-tempfile) "saves str to clipboard"
  (setf path-to-tempfile (merge-pathnames "temp/putclip.temp" 
                                          #+new-projects-structure (this-system-dir)
                                          #-new-projects-structure cl-user::*lisp-root*))
  (with-open-file (x path-to-tempfile :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format x "~A" str))
  (system:call-system-showing-output
   #+ignore system:call-system-showing-output 
   ; for XP 
    (str+ "cmd /c putclip.exe " (namestring path-to-tempfile))
    #+for-win-98 "putclip.exe l:/temp/putclip.temp" 
    :show-cmd nil :wait t))





#+ignore (bind-key "input russian" "Meta-`") ; для ввода русских букв надо нажать alt, и не отпуская его, дважды нажать shift

(defconstant +whitespace+ 
  (concatenate 'string '(#\Space #\Newline #\Tab #\Linefeed #\Return #\Page))
  "Whitespace characters.")

(defcommand "Copy current line" (p) "" "" 
  (declare (ignore p))
  (save-excursion
    (proga
      (beginning-of-line-command nil)
      (let1 p1 (copy-point (current-point)))
      (end-of-line-command nil)
      (setf *my-command-result* 
            (points-to-string p1 (current-point))))))

;; for sbcl it is a emacs function paste-filename
(editor:defcommand "Paste Filename" (p) "Pastes and unixized filename from clipboard" "Pastes and unixized filename from clipboard"  
  (declare (ignore p))
  #+ignore (editor:backward-kill-form-command p)
  (insert-string (current-point) 
                 (format nil "~S" 
                         (substitute 
                          #\/ #\\ 
                          (string-trim +whitespace+ (CAPI-WIN32-LIB::GET-CLIPBOARD-TEXT))))))

(editor:defcommand "Paste Windows Filename" (p) "Pastes and unixized filename from clipboard" "Pastes and unixized filename from clipboard"  
  (declare (ignore p))
  #+ignore (editor:backward-kill-form-command p)
  (insert-string (current-point) 
                 (format nil "~S" 
                         (string-trim +whitespace+ (CAPI-WIN32-LIB::GET-CLIPBOARD-TEXT)))))

; в sbcl есть ф-я EMACS insert-string-from-clipboard
(editor:defcommand "Insert string from clipboard" (p) "Pastes string from a clipboard as a lisp string"
     (declare (ignore p))
  (insert-string (current-point)
                 (format nil "~S"
                         (string-trim +whitespace+ (capi-win32-lib::get-clipboard-text)))))

  
; в sbcl - ф-я EMACS buffer-pathname-to-clipboard
(defcommand "Buffer Pathname To Clipboard" (p)
     "Копирует имя файла (namestring) в буфер обмена"
     (declare (ignorable p))
     (proga 
       (let pathname (buffer-pathname (current-buffer)))
       #+nil (capi-win32-lib::set-clipboard-text (namestring pathname)) ; это не работает
       (text-to-clipboard (namestring pathname))))

(defun window-handle-by-buffer (buffer)
  "Возвращает handle первого окна данного буфера"
  (assert (typep buffer 'editor::buffer))
  (let1 window (car (editor:buffer-windows buffer))
    (let1 pane (slot-value window 'editor::text-pane)
      (let1 representation (slot-value pane 'capi-internals:representation)
        (slot-value representation 'win32:hwnd)))))

(defun goto-buffer-2 (buffer in-same-window &key (set-foreground-window t))
  "Не только отображает буфер, но и показывет окно"
  (goto-buffer buffer in-same-window)
  (when set-foreground-window 
    (win32:set-foreground-window (window-handle-by-buffer buffer)))
  )

(defun goto-or-create-xy (pathname row col &key kill-buffer (set-foreground-window t))
  (capi:find-interface 'lispworks-tools:editor)
  (goto-xy pathname row col :kill-buffer kill-buffer :set-foreground-window set-foreground-window))

;; В EMACS есть аналогичная goto-xy
(DEFUN GOTO-XY-EXTENDED (PATHNAME ROW COL &KEY KILL-BUFFER (set-foreground-window t))
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

(defmethod goto-xy ((pathname t) (row t) (col t))
  (goto-xy-extended pathname row col))

#+lispworks
(defmethod real-point-offset ((point t)) 
  "offset of the point is not an offset indeed. Trying to calculate real offset relative to buffer start. Похоже, что в буквах"
  (let ((result 
         (+ (point-offset point) (slot-value (editor::point-bigline point) 'editor::start-char))))
    (unless (= result (editor::find-point-offset (point-buffer point) point))
      (cerror "продолжить"
       "нашёл функцию в редакторе, к-рая делает то же самое, что и real-point-offset. Надеялся, что они одинаковы")
      )
    result))

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

(defun get-current-buffer-text-and-offset ()
  "Возвращает два значения - полный текст из текущего окна и смещение точки"
  (proga function
    (let pnt (current-point))
    (let offset (real-point-offset pnt))
    (let text (capi:editor-pane-text (editor:window-text-pane (editor:current-window))))
    (values text offset)
    ))

(defparameter *last-identifier-sought* nil) 

(defun do-identifier-search (skip-one-char)
  (proga
    (let pnt (current-point))
    (let scanner (cl-ppcre:create-scanner 'ppcre-shortcuts:lisp-identifier))
    (mlvl-bind (text offset) (get-current-buffer-text-and-offset))
    (when skip-one-char (incf offset))
    (cl-ppcre:do-matches (o1 o2 scanner text nil :start offset) ; ( scanner (subseq1 text offset))
      (proga
        (when (string-equal (subseq text o1 o2) *last-identifier-sought*)
          (editor::push-buffer-point pnt)
          (beginning-of-buffer-command nil)
          (forward-character-command o1 #+nil (+ o1 offset))
          (let target-point (copy-point (current-point)))
          (forward-character-command (- o2 o1))
      ;(editor::set-isearch-highlight target-point (current-point))
          (set-current-mark (current-point))
          (move-point (current-point) target-point)
      ; (highlight-active-region (editor:current-window))
          (editor::set-highlight-buffer-region t)
          (return-from do-identifier-search t) ; from do-matches
          )))
    (editor:message "Identifier not found")
    ))

(defcommand "Identifier search" (p) "Finds lisp identifier"
     (declare (ignorable p))
  (setf *last-identifier-sought* (prompt-for-symbol nil :prompt "Find what?") #+nil (editor::prompt-for-target-string "Find what? "))
  (do-identifier-search nil)
  )

(defcommand "Continue identifier search" (p) "Continues identifier search"
     (declare (ignorable p))
  (do-identifier-search t))


; в EMACS - команда find-file-at-point
(defun get-filename-at-point-and-prompt ()
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
    (show-expr backward-part)
    (let matched-filename
      (str++ (or backward-part "")
             (or forward-part "")))
    (let result (prompt-for-file :default-string (str++ location matched-filename) :default "fh"))
    (return-from function result)
    ))

(defcommand "WFind file at point" (p) "Читает с текущей точки нечто похожее на идентификатор лиспа и предлагает открыть такой файл" "" 
  (declare (ignorable p))
  (proga
    (let pathname (get-filename-at-point-and-prompt))
    (let buf (find-file-buffer pathname #'ignored))
    (goto-buffer-2 buf t)
    )
  )


(defvar *in-find-source* nil "is bound to t in find-source-command by defadvice")

(lw:defadvice (find-source-command bind-in-find-source :around) (&rest args)
  (let ((*in-find-source* t))
    (apply #'lw:call-next-advice args)))


#| Go Back работает лучше. 

 (defvar *find-definition-locations-stack* nil)

 (defcommand "Push location and Find Source" (p &rest more-args) "Remembers current location and calls \"Find Source\". Use \"Pop location from find source\" to go back. Do not call the command, call decorated \"Find source\" command instead"
  (let* ((file-name (buffer-pathname (current-buffer)))
         (pnt (current-point))
         (offset (real-point-offset pnt)))
    (when file-name
      (push `(,file-name ,offset) *find-definition-locations-stack*))
    (DECORATE-FUNCTION:apply-undecorated 'find-source-command (cons p more-args))))

 (DECORATE-FUNCTION:decorate-function
 'find-source-command
 (lambda (fn &rest args)
   (declare (ignore fn))
   (apply 'push-location-and-find-source-command args)))


 (defcommand "Push location and Continue Tags Search" (p &rest more-args) "Remembers current location and calls \"Find Source\". Use \"Pop location from find source\" to go back. Do not call the command, call decorated \"Find source\" command instead"
     (declare (ignorable p))
  (let* ((file-name (buffer-pathname (current-buffer)))
         (pnt (current-point))
         (offset (real-point-offset pnt)))
    (when file-name
      (push `(,file-name ,offset) *find-definition-locations-stack*))
    (DECORATE-FUNCTION:apply-undecorated 'continue-tags-search-command (cons p more-args))))

 (DECORATE-FUNCTION:decorate-function
 'continue-tags-search-command
 (lambda (fn &rest args)
   (declare (ignore fn))
   (apply 'push-location-and-continue-tags-search-command args)))



 (defcommand "Pop location from find source" (p)
     (declare (ignore p))
     (let* ((cur-file-and-offset (pop *find-definition-locations-stack*))
            (file-name (first cur-file-and-offset))
            (where (second cur-file-and-offset))
            )
       (cond
        (cur-file-and-offset
         (goto-buffer (find-file-buffer file-name) t)
         (beginning-of-buffer-command nil)
         (EDITOR::move-buffer-point-to-offset (editor::current-buffer) where))
        (t
         (EDITOR::editor-beep))
       )))

|#

(defcommand "My find source" (p) "Finds source with budden-tools machinery. See srcpl"
     (declare (ignorable p))
  (perga function
    (let file (buffer-pathname (current-buffer)))
    (unless file
      (editor:message "No file is associated with the point")
      (return-from function nil))
    (let pnt (current-point))
    (let offset (real-point-offset pnt))
            ;(new-point (character-offset (copy-point pnt) 10))
            ;(text (when new-point (points-to-string pnt new-point)))
    (let targets (budden-tools::l/find-sources-in-file file offset :strict t))
       ;(break)
       ;(print `(,file ,text ,offset ,targets))
    (when targets
      (let* ((target (car targets))
             (target-file (first target)) ; FIXME Записывать позиции единообразно, например, с помощью olm. 
             (offset (second target)))
        (goto-offset target-file offset :kill-buffer nil)
        ))))


(defcommand "Find current package definition" (p) "Goto current p"
     (declare (ignorable p))
  (declare (ignorable p))
  (editor:find-source-for-dspec-command
   nil
   (list 'defpackage
         (package-name (editor::buffer-package-to-use (editor:current-buffer))))))


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


(defcommand "Grep symbol" (p) "Finds symbol with grep"
     (declare (ignorable p))
  (let1 sym (prompt-for-symbol nil :prompt "What?")
    (setf lispworks:*grep-command-format* (str+ "~a ~a \"" sym "\" c:/project/sales_budden/*.pas c:/lisp/sw/fb2/wrapper.lisp"))
    (grep-command nil)))

; (bind-key "Grep symbol" "f4")
             
                      


(defun fix-offset-with-no-of-newlines (offset no-of-newlines)
  (declare (ignore no-of-newlines))
  (- offset #+nil no-of-newlines 1))


(defun fix-offset-2 (pathname offset)
  "Имеется числовой offset, к-рый вернул file-position. Давайте попробуем превратить его в 
  row-col-offset. См. также BUDDEN-TOOLS::input-stream-position-in-chars"
  (with-open-file (stream pathname)
    (let ((map (budden-tools::ensure-file-position-to-char-position-for-stream stream)))
       (budden-tools::file-position-and-map-to-char-position offset map)))
  #|(perga
    (when (typep offset 'row-col-offset)
      (warn "fix-offset-2: attempted to fix row-col-offset")
      (return-from fix-offset-2 offset))
    (:lett row integer 1)
    (:lett col integer 0)
    (:lett b-offset integer 0) ; = f-offset
    (perga 
      (:@ with-open-file (in pathname :direction :input))
      (loop
       (:lett char (or character null) (read-char in nil nil))
       (when (>= (file-position in) offset)
         (return-from fix-offset-2
           (make-row-col-offset :row row :col col
                                :b-offset b-offset
                                :f-offset offset)))
       (etypecase char
         (null
          (warn "fix-offset-2: approached EOF")
          (return-from fix-offset-2 0))
         (character
          (case char 
            (#\Newline
             (incf row)
             (incf b-offset)
             (setf col 0))
            (#\Return
             (warn "fix-offset-2: got Return character!"))
            (t
             (incf b-offset)
             (incf col))))))))|#)

      

(DEFUN GOTO-OFFSET (PATHNAME OFFSET &KEY KILL-BUFFER (set-foreground-window t) subtract-no-of-newlines)
  "Offset может быть:
  - структурой row-col-offset
  - смещением в буквах
  - file-position. В этом случае нужно передать аргумент subtract-no-of-newlines

  См. также editor::count-lines, editor::what-line-command
  
  Данная команда не сработает при отсутствии редактора. При kill-buffer опасно, т.к. закрывает файл без изменений. 
   См. также modest-goto-offset"
  (perga
    (let ed (get-some-editor))
    (unless ed 
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

  
    (#+lispworks6 capi:execute-with-interface 
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
     

(defun char-at-point (p)
  (perga
    (let pp (copy-point p :temporary))
    (character-offset pp 1)
    (elt (points-to-string p pp) 0)))

(defun char-can-be-in-symbol (c)
  "Проверяем символы в общем смысле слова, не в смысле лиспа. Только тире позволяем быть"
  (not (find c '(#\  #\newline #\, #\^ #\' #\" #\` #\tab #\; #\( #\) #\@ #\| #\# #\\ #\>))))

(defun process-potential-symbol (x package)
  (perga
    (let found-package
      (sbcl-reader-budden-tools-lispworks:potential-symbol-package x))
    (let maybe-symbol-name
      (sbcl-reader-budden-tools-lispworks:potential-symbol-casified-name x))
    (mlvl-bind (maybe-symbol storage)
        (find-symbol maybe-symbol-name found-package))
    (cond
     (storage maybe-symbol)
     (*in-find-source*
      (try-to-choose-the-best-matching-symbol-in-find-source maybe-symbol-name package)
      )
     (t
      (values nil nil)))))

       
(lw:defadvice (editor::get-symbol-from-point get-symbol-from-point-around-advice :around)
    (POINT &REST keyargs &KEY (PREVIOUS T) (MAX-LENGTH 100) (MAX-NON-ALPHANUMERIC 15) (CREATE-NEW nil))
  "editor::get-symbol-from-point используется в редакторе, когда хотим забрать символ из текущей точки буфера. Что мы делаем в адвайсе?
  Полностью подменяем команду. Читаем символ с помощью ридера, переключённого в спец. режим. 
  Если с пакетом неясность, ищем все символы с таким именем и предлагаем пользователю выбор.   
  Имеем возможность не создавать символ при попытке забрать его из буфера - это аккуратная политика. 
  Выражение a^b рассматриваем как два символа a и b "
  (declare (ignore fn previous max-non-alphanumeric))
  (perga function
    (unless (BUDDEN-TOOLS::packages-seen-p *readtable*)
      (unless (member :create-new (splice-list keyargs) :key 'car)
        (setf keyargs (append keyargs (list :create-new t))))
      (return-from function (apply #'lw:call-next-advice point keyargs)))
    (let p1 (copy-point point :temporary))
    (let buf-beg (buffers-start (point-buffer point)))
    (let buf-end (buffers-end (point-buffer point)))
    (let rest-length (or max-length -1))
    (let symbol-beginning nil)
    ;(let symbol-end nil) 
    (let v-in-symbol nil) ; истина, когда внутри символа
    (let cur-in-symbol nil) ; истина, когда текущий char относится к символу
    ; looking for a symbol at or before point
    (do () ((not (and (> rest-length 0)
                  (point> p1 buf-beg))) nil) 
      (unless (point= p1 buf-end)
        (setf cur-in-symbol (char-can-be-in-symbol (char-at-point p1))))
      (when cur-in-symbol
        (unless v-in-symbol
           ;(setf symbol-end (copy-point p1 :temporary))
          (setf v-in-symbol t)))
      (when v-in-symbol
        (unless cur-in-symbol
          (character-offset p1 1)
          (setf symbol-beginning (copy-point p1 :temporary))
          (return nil)))
      (character-offset p1 -1)
      (incf rest-length -1)
      )
    ; find the end of the symbol
    (when symbol-beginning
      (let lookup-end (copy-point symbol-beginning :temporary))
      (let lookup-end-count max-length)
      (do () ((not (and (point< lookup-end buf-end)
                        (or 
                         (null max-length) 
                         (> lookup-end-count 0)))) nil)
        (character-offset lookup-end 1)
        (incf lookup-end-count -1))
      (let ss (points-to-string symbol-beginning lookup-end))
      ;2012-12-19 (let *package* (editor::buffer-package-to-use (point-buffer point)))
      ;(let pack *package*)
      ;(show-expr *package*)
      ;(let budden-tools::*inhibit-readmacro* t)
      (let package (editor::buffer-package-to-use (point-buffer point)))
      (cond
       (create-new
        (ignore-errors
          (let ((budden-tools::*inhibit-readmacro* t)
                (*package* package)
                )
            (read-from-string ss))))
       (t 
        (mlvl-bind (maybe-potential-symbol maybe-error)
            (ignore-errors
              ;(cond
               ;((BUDDEN-TOOLS::packages-seen-p *readtable*)
                (let ((sbcl-reader-budden-tools-lispworks:*return-package-and-symbol-name-from-read* t)
                      (*package* package)
                      (budden-tools::*inhibit-readmacro* t))
                  (read-from-string ss))
                ; )
               #|(t
                (mlvl-bind (a-package a-name)
                    (editor::pathetic-parse-symbol ss package)
                  (sbcl-reader-budden-tools-lispworks::make-potential-symbol :package a-package :casified-name a-name))
                ))|# 
                ))
        (cond
         ((typep maybe-error 'error) ; maybe-symbol can be nil, and read returns position. 
                                     ; so we check if there is a error
          (values nil nil) ; no symbol
          )
         ((sbcl-reader-budden-tools-lispworks:potential-symbol-p maybe-potential-symbol)
          (process-potential-symbol maybe-potential-symbol package)
          )
           
         ;((not (symbolp maybe-symbol))
         ; (editor-error "~S is not a symbol name" maybe-symbol))
         ((and (consp maybe-potential-symbol)
               (eq (car maybe-potential-symbol) 'budden-tools:|^|)
               (SBCL-READER-BUDDEN-TOOLS-LISPWORKS:potential-symbol-p (second maybe-potential-symbol)))
          (process-potential-symbol (second maybe-potential-symbol) package))

         (t ; in some modes we should not err
          (values nil nil)
          )
         )
        )
       ))))
    
;  (setf max-non-alphanumeric 100)
;  (mlvl-bind (value error)
;      (apply fn point (dispatch-keyargs-simple previous max-length max-non-alphanumeric create-new))
;    (if error nil value)))

; #+lispworks6 (budden-tools::decorate-function 'editor::get-symbol-from-point 'decorated-get-symbol-from-point)

(decorate-function:portably-without-package-locks
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
    )))


(defun select-symbol-from-list (symbols package message)
  "Returns two values: symbol returned and (t if choice accepted, nil if it is rejected)"
  (let* ((items
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


(defun try-to-choose-the-best-matching-symbol-in-find-source (casified-string package)
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


(defun my-get-defun-start-and-end-points (point start end)
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
    1))


(defparameter EDITOR-BUDDEN-TOOLS:*ide-code-snippets* 
 "
  Здесь будем хранить полезные знания о функциях среды
  IDE CODE SNIPPETS
  DSPEC::find-dc 'package - находит класс dspec пакета
  для редактора: Разные code snippets для IDE
  'editor::i-find-pattern - фактически это funcall
  EDITOR::%i-search-find-pattern - есть исходник
  LISPWORKS-TOOLS::inspect-an-object - открыть инспектор для объекта
  ")


(defmacro editor-do-registers ((name value &optional sorted) &rest body)
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




(editor::defcommand "Goto Selected Register" (p)
     "Не работает - does not work"
     (declare (ignore p))
     (declare (special editor::*registers*))
     (let (items choice register-names choice-index)
       (editor-do-registers
        (name val :sorted)
        (push 
         (with-output-to-string (*standard-output*)
          (editor::print-pretty-character name *standard-output*)
          (write-string ":  ")
          (etypecase val
            (editor::point
             (let ((buff (point-buffer val)))
               (if (editor::good-point-p val)
                   (format t "Line ~S, col ~S in buffer ~A~%"
                           (count-lines  (editor::buffer-%start buff) val)
                           (point-column val)
                           (buffer-name buff)
                           )
                 (format t "Deleted from buffer ~A ~%" (buffer-name buff)))))
            (cons
             (let* ((str (editor::buffer-string-string (car val)))
                    (nl (position #\newline str :test #'char=))
                    (len (length str))
                    (buff (cdr val)))
               (format t "Text~@[ from buffer ~A~]~%   ~A~:[~;...~]~%"
                       (if buff (buffer-name buff))
                       (subseq str 0 (if nl (min 61 len nl) (min 61 len)))
                       (> len 60))))))
         items)
        (push name register-names))
       (setf items (nreverse items))
       (setf choice
             (CAPI:prompt-with-list items "Select a register"))
       (when choice
         (setf choice-index (position choice items :test 'string=))
         (jump-to-register-command nil (elt register-names choice-index))
        )))


;; stolen from 'portable hemlock' project which magically turned out to be
;; compatible with lispworks' editor
(defmacro editor::define-file-option (name lambda-list &body body)
  "Define-File-Option Name (Buffer Value) {Form}*
   Defines a new file option to be user in the -*- line at the top of a file.
   The body is evaluated with Buffer bound to the buffer the file has been read
   into and Value to the string argument to the option."
  (let ((name (string-downcase name)))
    `(setf (cdr (or (assoc ,name editor::*mode-option-handlers*  :test #'string=)
		    (car (push (cons ,name nil) editor::*mode-option-handlers*))))
	   #'(lambda ,lambda-list ,@body))))


(editor::define-file-option "System" (buffer value)
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



(defadvice (editor::default-modeline-function-function print-system-and-readtable :around) (window)
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
 
      

(defmacro crossref-command-nachinka (query-fn)
  (with-gensyms (sym)
    `(let ((,sym (editor::get-symbol-from-point (editor::current-point) :previous nil :create-new nil)))
       (when ,sym 
         (editor::with-output-to-help-window-1 
          #'(lambda (stream)
              (let ((*package* (find-package :cl-user)))
                (format stream "~%~S ~S :~%" ',query-fn ,sym)
                (print (,query-fn ,sym) stream))))
         ))))
  
(editor:defcommand "Who References" (p) "" "" 
  (declare (ignore p))
  (crossref-command-nachinka hcl:who-references))


(editor:defcommand "References Who" (p) "" "" 
  (declare (ignore p))
  (crossref-command-nachinka hcl:references-who)
  )

(editor:defcommand "Who binds" (p) "" "" 
  (declare (ignore p))
  (crossref-command-nachinka hcl:who-binds)
  )

(editor:defcommand "Binds who" (p) "" "" 
  (declare (ignore p))
  (crossref-command-nachinka hcl:binds-who)
  )

(editor:defcommand "Sets who" (p) "" "" 
  (declare (ignore p))
  (crossref-command-nachinka hcl:sets-who)
  )

(editor:defcommand "Who sets" (p) "" "" 
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


(defun maybe-print-name-of-dspec (dspec stream package &key exported-already-hash)
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
     


(defun steal-definitions-from-file-for-export (filename)
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
        
  
