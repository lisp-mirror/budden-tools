; -*- Encoding: utf-8 ; -*- 
;; помощник для поиска ошибок чтения. Пример простого расширения отладчика
(asdf::of-system :editor-budden-tools)

(def-merge-packages::!
 :error-browser-for-paren-reader
 (:always t)
 (:use :cl :budden-tools :iterk
  :editor-budden-tools
  :editor 
  )
 (:import-from :perga-implementation perga-implementation:perga)
 (:export
  "error-browser-for-paren-reader:READ-ERROR-BROWSER
   error-browser-for-paren-reader:READ-ERROR-BROWSER-AGAIN
  error-browser-for-paren-reader:CALL-ERROR-BROWSER-FROM-DEBUGGER"
  )
 )

(in-package :error-browser-for-paren-reader)
(in-readtable :buddens-readtable-a)

(defstruct error-browser-context 
  Stream
  Filename
  Locations
  Selection)

(defparameter *error-browser-context* nil)


(defvar *face-blue*
  (make-face 'sc-face-blue
             :foreground :blue
             :if-exists :overwrite))

(defvar *face-green*
  (make-face 'sc-face-green
             :foreground :green
             :if-exists :overwrite))

(defvar *face-yellow*
  (make-face 'sc-fase-yellow
             :foreground :yellow
             :if-exists :overwrite))


(defvar *face-red*
  (make-face 'sc-face-red
             :foreground :red
             :if-exists :overwrite))

(defun level-to-face (level)
  (case level
    (0 *face-red*)
    (t (ecase (mod level 3)
          (1 *face-yellow*)
          (2 *face-green*)
          (0 *face-blue*)
          ))
    ))


(defun point-at-offset (buffer offset)
  "Возвращает точку в буфере по смещению в файле. FIXME трогает буфер, шмыркает окнами. Переписать"
  (proga
    (let filename (buffer-pathname buffer))
    (goto-offset filename offset :set-foreground-window t)
    (copy-point (buffer-point buffer) :temporary t)
    )
  )


(defun colorize-region-for-error-browser (buffer locations)
  "Ошибка здесь в том, что fbody вызывается только при компиляции. По идее, ридер должен 
вызываться при каждом редактировании, но пока не будем этого делать"
  (proga
    (iter (:for l in (cons 1 (reverse locations)))
      (:for i :downfrom (+ 0 (length locations)))
      (show-expr `(,l ,i))
      (:for p = (point-at-offset buffer l))
      (:for p0 :previous p)
      (unless (:first-time-p)
        (editor::font-lock-unfontify-region p0 p)
        (editor::font-lock-apply-highlight p0 p (level-to-face i)))
      )))

(defun read-error-browser-goto-selection ()
  (perga function
    (:lett ebc error-browser-context *error-browser-context*)
    (unless (<= 0 
                (slot-value ebc 'Selection) 
                (- (length (slot-value ebc 'Locations)) 1))
      (format t "should be ~A<=YourChoice<~A" 0 (length (slot-value ebc 'Locations)))
      (return-from function nil))
    (let filename (slot-value ebc 'Filename))
    (get-some-editor)
    (let buffer (find-file-buffer filename))
    (editor-budden-tools::do-in-editor-command buffer
      (colorize-region-for-error-browser buffer (slot-value ebc 'Locations)))
    (editor-budden-tools:goto-offset filename
                                     (nth (slot-value ebc 'Selection) (slot-value ebc 'Locations)))
    ))


(defun smart-resolve-export-conflict (&optional ignore p)
  "При конфликте во время экспорта убирает все символы, кроме экспортируемого"
  (declare (ignore ignore p))
  (perga-implementation:perga
    (let c DBG::*debug-condition*)
    (typecase c
      (CONDITIONS:exported-symbol-conflict
       (dolist (symbol (slot-value c 'CONDITIONS::symbol-list))
         (let symbol-name (symbol-name symbol))
         (dolist (p (slot-value c 'CONDITIONS::package-names))
           (mlvl-bind (conflicting-symbol storage)
               (find-symbol symbol-name p)
             (case storage
               (nil (warn "Strange: symbol-name ~S not found in ~S" symbol-name p))
               (:internal
                (warn "Uninterning ~S from ~S" symbol-name p)
                (unintern conflicting-symbol p)
                )
               (:inherited
                (warn "~S is inherited in ~S. Shadowing-import ~S" symbol-name p symbol)
                (shadowing-import symbol p)
                )
               (:external
                (warn "~S is exported from ~S. Re-exproting ~S" symbol-name p symbol)
                (export symbol p)
                )
               (t
                (warn "strange storage ~S of ~S in ~S" storage symbol-name p)))))))
      (t (warn "~S is not a conditions:exported-symbol-conflict" c)))))

(push 
 '(:smart-resolve-export-conflict smart-resolve-export-conflict "Smart resolve export conflict") 
 dbg::*default-debugger-commands*)

; для SBCL - sb-debug::*debug-commands* - там же есть !def-debug-command 

(defun call-error-browser-from-debugger (&optional user-typed-command argument)
  (declare (ignore user-typed-command))
  (proga
    (cond
     ((or (null argument) (numberp argument))
      (read-error-browser (or argument 0)))
     ((string-equal argument "?")
      (read-error-browser-again))
     ((eq argument :help)
      (princ "Просмотровщик ошибок чтения
  :e 0 - показать место ошибки (default)
  :e 1 - показать самую последнюю открытую скобку          
  :e 2 - показать предпоследнюю
  ... и т.п. 
  :e ? - меню для выбора скобки с указанием позиции в файле
  :e :help - данная справка"
      )))))


#+lispworks6.1
(defvar *lispworks-system-load-text-stream-stream*)

#+lispworks6.1
(lw:defadvice (SYSTEM::load-text-stream bind-lispworks-system-load-text-stream-stream :around) (stream &rest more-args)
  (let ((*lispworks-system-load-text-stream-stream* stream))
    (apply #'lw:call-next-advice stream more-args)))


(defun read-error-browser (&optional p)
  (PROGN; ignore-errors
    (perga function
           (let opens budden-tools::*reading-parens*)
      (let stream (or budden-tools::*reading-parens-stream*
                      (and (boundp '*lispworks-system-load-text-stream-stream*)
                           *lispworks-system-load-text-stream-stream*)
                      ))
      (let cur-pathname (lw:current-pathname))
      (unless stream
        (cond 
         ((and (typep DBG::*debug-condition* 'CONDITIONS:simple-reader-error)
           (typep (slot-value dbg::*debug-condition* 'stream)
                  '(or 
                    editor::editor-region-stream
                    stream::ef-file-stream
                    stream::file-stream))
           (return-from function
             (edit-stream-position (slot-value dbg::*debug-condition* 'stream) nil nil))))
         ((and (pathnamep cur-pathname)
               (not (LISPWORKS:file-directory-p cur-pathname))
               (not (member (pathname-type cur-pathname)
                            '("ofasl") :test 'string-equal))) 
          (format t "~%read-error-browser : don't know where is error, but (lw:current-pathname) is sane. Trying to open ~S"
                 cur-pathname)
          (goto-offset cur-pathname 0)
          (return-from function (values))
          )
         (t 
          (format t "~%read-error-browser : I see no parens, no stream is available from condition object. Try to find a stream in a call stack and call (BUDDEN-TOOLS:edit-stream-position that-stream) from that frame or watch at (lw:current-pathname) = ~S" cur-pathname)
          (return-from function nil)
          )))
      (let close nil)
      (with-output-to-string (*standard-output*) 
        (setf close (budden-tools::extract-file-position stream)))
      (let all-positions 
        (append (list close) opens))
      (let filename (budden-tools::extract-source-filename-from-stream stream))
      #|(when (typep stream '(or stream::ef-file-stream stream::file-stream))
        (setf all-positions
              (mapcar (lambda (x) 
                        (EDITOR-BUDDEN-TOOLS::fix-offset-2 filename x))
                      all-positions)))|#
      (unless all-positions
        (setf *error-browser-context* nil)
        (princ "No positions to browse")
        (return-from read-error-browser (values)))
      (setf *error-browser-context* (make-error-browser-context
                                     :Stream stream
                                     :Filename filename
                                     :Locations all-positions
                                     :Selection (or p (- (length all-positions) 1))))
      (read-error-browser-goto-selection)
      (values))))

(defcommand "Read error browser" (p) "Показывает ошибки чтения - вызывать, когда чтение упало по ошибке"
     "Требуются механизмы из budden-tools"
  (declare (ignorable p))
  (editor:insert-string (current-point) "(error-browser-for-paren-reader:read-error-browser)")
  (editor:execute-or-insert-newline-or-yank-from-previous-prompt-command nil)
  )
  ;(capi:apply-in-pane-process
  ; (editor::CURRENT-EDITOR-PANE)
  ; 'read-error-browser p)
 


;(defcommand "Read error browser again" (p) 
(defun read-error-browser-again (&optional p)
  (proga all
    (unless *error-browser-context*
      (message "No positions to browse")
      (return-from all nil))
    (with-the1 ebc error-browser-context *error-browser-context*)
    (let choice 
      (unless p
        (capi:prompt-with-list
         (slot-value ebc 'Locations)
         (format nil "select position in ~S" (slot-value ebc 'Filename))
         :selected-item (nth (slot-value ebc 'Selection) (slot-value ebc 'Locations)))))
    (unless (or p choice)
      (return-from all nil))
    (setf (slot-value ebc 'Selection) 
          (cond
           (choice
            (position choice (slot-value ebc 'Locations)))
           (p 
            (min (- (length (slot-value ebc 'Locations)) 1)
                 (max 0
                      (+ p (slot-value ebc 'Selection)))))))
    (read-error-browser-goto-selection)))


(push 
 '(:E call-error-browser-from-debugger "Read error browser") 
 dbg::*default-debugger-commands*)
          
