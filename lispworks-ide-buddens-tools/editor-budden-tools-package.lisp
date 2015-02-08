
(asdf::of-system :editor-budden-tools)

(def-merge-packages::! :ppcre-shortcuts (:use :cl-ppcre :lisp :iterate-keywords)
  ; see "ppcre-shortcuts.lisp"
  ; see also obsolete cl-ppcre-idioms.lisp
  (:always t)
  (:export 
   "PPCRE-SHORTCUTS:SAFE-LISP-IDENTIFIER
    PPCRE-SHORTCUTS:LISP-IDENTIFIER ; deprecated
    PPCRE-SHORTCUTS:CONSTITUENT-CHAR
    PPCRE-SHORTCUTS:@IDENTIFIER
    PPCRE-SHORTCUTS:@NON-NEGATIVE-DECIMAL 
    PPCRE-SHORTCUTS:@REG-IDENTIFIER 
    PPCRE-SHORTCUTS:@WHITESPACE 
    PPCRE-SHORTCUTS:@MAYBE-EVERYTHING
    PPCRE-SHORTCUTS:@SQL-SHORT-COMMENT
    PPCRE-SHORTCUTS:@REG-WHITESPACE 
    PPCRE-SHORTCUTS:FILENAME-WITH-PATH
    ") 
  )

(def-merge-packages::!
 :EDITOR-BUDDEN-TOOLS
 (:use :cl :editor :budden-tools :alexandria :ppcre-shortcuts :iterate-keywords)
 (:shadowing-import-from :budden-tools #:READ-FILE-INTO-STRING)
 (:shadowing-import-from :lispworks #:defadvice)
 (:import-from :perga-implementation perga-implementation:perga)
 (:import-from :asdf #:component-pathname #:component-system #:*current-component*)
 (:export 
  "
  EDITOR-BUDDEN-TOOLS:GOTO-BUFFER-2 ; перейти к буферу и вывести окно вперёд
  EDITOR-BUDDEN-TOOLS:GOTO-XY ; перейти на заданную строку и колонку файла
  EDITOR-BUDDEN-TOOLS:GOTO-OFFSET ; перейти на заданную позицию файла #\newline считается за один символ!
  EDITOR-BUDDEN-TOOLS:REPLACE-STRING-IN-FILE 
  EDITOR-BUDDEN-TOOLS:COPY-CURRENT-LINE-COMMAND
  EDITOR-BUDDEN-TOOLS:*MY-COMMAND-RESULT*
  EDITOR-BUDDEN-TOOLS:TEXT-TO-CLIPBOARD
  EDITOR-BUDDEN-TOOLS:GET-THE-LISTENER-INTERFACE ; возвращает интерфейс по текущему процессу
  EDITOR-BUDDEN-TOOLS:BUDDENS-LISTENER-MESSAGE ; мы умеем вставлять свою подсказку в строку состояния листенера
  EDITOR-BUDDEN-TOOLS:DECORATED-PACKAGE-PROMPT ; и подменять подсказку листенера
  EDITOR-BUDDEN-TOOLS:CLEAR-LEXEM-POSITION-CACHE
  EDITOR-BUDDEN-TOOLS:*sc-begin-fb-code-face*
  EDITOR-BUDDEN-TOOLS:*sc-end-fb-code-face*
  EDITOR-BUDDEN-TOOLS:*sc-firebird-macro-name-face*
  EDITOR-BUDDEN-TOOLS:MPF-FACE ; имя свойства
  EDITOR-BUDDEN-TOOLS:GET-SOME-EDITOR
  EDITOR-BUDDEN-TOOLS:*IDE-CODE-SNIPPETS* ; полезные кусочки кода
  "

  ; fixme - это должно работать и в нижнем регистре
  ))


