;;; -*- Encoding: (win32:code-page :id 866 :eof-style :crlf);  -*-
; Пример файла в кодировке 866. 
; См. также editor::PROCESS-FILE-OPTIONS , system::file-encoding-option, s
; Lispworks user guide and reference manual/22.6.3 guessing external formats and file streams
; and look for "attribute line" in Lispworks docs.

; преобразование Русских букв для чтения-записи в open-pipe - см. russian-budden-tools:dos-string-to-lisp-for-pipe


; Если в файл нельзя вставить эту строку, в Lispworks можно указать :external-format '(win32:code-page :id 866 :eof-style :crlf)
; при открытии файла, например, в with-open-file или в open. 

 

