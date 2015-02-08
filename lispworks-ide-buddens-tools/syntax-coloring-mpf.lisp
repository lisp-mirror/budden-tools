;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/editor:syntax-coloring:syntax-coloring.lisp,v 1.3.8.1 2009/08/21 21:12:58 davef Exp $" -*-

;; Copyright (c) 1987--2010 LispWorks Ltd. All rights reserved.
;; Взято из примеров редактора и внесены минимальные изменения для использования
;; 

(in-package :editor-budden-tools)
(asdf::of-system :editor-budden-tools)

(defparameter *mpf-background* :lightgoldenrodyellow)

(defparameter *sc-string-face*
  (make-face 'sc-string-face
             :foreground :pink4
             :background *mpf-background*
             :if-exists :overwrite))

(defparameter *sc-comment-face*
  (make-face 'sc-comment-face
             :background *mpf-background*
             :foreground :blueviolet
             :if-exists :overwrite))


(defparameter *sc-keyword-face*
  (make-face 'sc-keyword-face
             :foreground :purple
             :background *mpf-background*
             :if-exists :overwrite))

(defparameter *sc-fb-object-name-face*
  (make-face 'sc-fb-object-name-face
             :foreground :brown4
             :background *mpf-background*
             :if-exists :overwrite))

(defparameter *sc-variable-name-face*
  (make-face 'sc-variable-name-face
             :foreground :green
             :background *mpf-background*
             :underline-p t
             :if-exists :overwrite))

(defparameter *sc-type-face*
  (make-face 'sc-type-face
             :foreground :pink2
             :background *mpf-background*
             :if-exists :overwrite))

(defparameter *sc-builtin-face*
  (make-face 'sc-builtin-face
             :foreground :red
             :background *mpf-background*
             :if-exists :overwrite))


(defparameter *sc-other-face*
  (make-face 'sc-other-face
             :foreground :black
             :background *mpf-background*
             :if-exists :overwrite))

(defparameter *sc-begin-fb-code-face*
  (make-face 'sc-begin-fb-code-face
             :foreground :black
             :background :cyan
             :if-exists :overwrite))


(defparameter *sc-end-fb-code-face*
  (make-face 'sc-end-fb-code-face
             :foreground :black
             :background :orange
             :if-exists :overwrite))

(defparameter *sc-firebird-macro-name-face*
  (make-face 'sc-firebird-macro-name-face
             :foreground :green3
             :background *mpf-background*
             :if-exists :overwrite))
             


(defun point-forward (point)
  (character-offset point 1))
(defun point-backwards (point)
  (character-offset point -1))

(defun point-character (point)
  (character-at point 0))

(defun is-symbol-character (x)
  (not (or (get-macro-character x)
           (lispworks::whitespace-char-p x))))




#| этот код позволяет подменить стандартную раскраску на свою 
(editor::%set-variable-value 'EDITOR::FONT-LOCK-FONTIFY-SYNTACTICALLY-REGION-FUNCTION
                             :mode
                             "Common-Lisp"
                             'region-set-custom-face)
; editor::lisp-font-lock-fontify-syntactically-region - умолчание
|#

