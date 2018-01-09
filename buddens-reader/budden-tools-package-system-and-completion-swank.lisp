;;; -*- coding: utf-8; system :see-packages ;  -*-
;;; Some SWANK symbols are decorated here. This code may be sbcl-specific. А также пытаемся декорировать find-package
 
(in-package :budden-tools)
(in-readtable nil)

; disable stepping
(declaim (optimize (debug 3) (compilation-speed 3) (safety 3)))

(defun decorated-swank--tokenize-symbol-thoroughly (fn string)
  "Use sbcl-reader-budden-tools-lispworks machinery if appropriate"
  (cond
    ((packages-seen-p *readtable*)
     (perga-implementation:perga
       (let buddens-reader-extensions:*return-package-and-symbol-name-from-read* t)
       (let ps (ignore-errors (read-from-string string)))
       (cond
         ((null ps)
          nil)
         ((sbcl-reader-budden-tools-lispworks::potential-symbol-p ps)
          ;(budden-tools:show-expr (sbcl-reader-budden-tools-lispworks:potential-symbol-qualified ps))
          (values
           (sbcl-reader-budden-tools-lispworks:potential-symbol-casified-name ps)
           (package-name (sbcl-reader-budden-tools-lispworks::potential-symbol-package ps))
           (/= 1 (sbcl-reader-budden-tools-lispworks:potential-symbol-qualified ps)))
          )
         (t (funcall fn string)))))
    (t
     (funcall fn string))))

(cl-advice:define-advice swank::tokenize-symbol-thoroughly #'decorated-swank--tokenize-symbol-thoroughly)

;;; FIXME see also oduvanchik-regexp-synonyms
(defun decorated-swank--completion-output-case-converter (fn input &optional with-escaping-p)
  "Return a function to convert strings for the completion output.
INPUT is used to guess the preferred case."
  (cond
    ((and (packages-seen-p *readtable*)
          (eq (readtable-case-advanced *readtable*) :upcase-if-uniform))
     (cond
       ((or with-escaping-p
            (and (plusp (length input))
                 (eq (all-ascii-chars-in-same-case-p input) :uppercase)))
        #'identity)
       (t #'string-downcase-ascii)))
    (t
     (funcall fn input with-escaping-p)
     )))

(cl-advice:define-advice swank::completion-output-case-converter
                                     #'decorated-swank--completion-output-case-converter)

(defun decorated-swank--symbol-completion-set (fn name package-name package internal-p matchp)
  (perga-implementation:perga
   (let completions nil)
   (flet set-completions (x) (setf completions x) "")
   (cond
    ((packages-seen-p *readtable*)
     (let single-completion
       (do-complete-symbol-with-budden-tools
        name package 'error #'set-completions :yes-or-no-p-fn (constantly nil) :internal internal-p))
     (cond
      (single-completion
       (list single-completion))
      (t completions)))
    (t
     (funcall fn name package-name package internal-p matchp)))))


(cl-advice:define-advice swank::symbol-completion-set #'decorated-swank--symbol-completion-set)

(defvar *call-original-find-package* nil)

#+(and sbcl (not careful-token-reader-via-native-package-local-nicknames))
(cl-advice:PORTABLY-WITHOUT-PACKAGE-LOCKS
 (defun decorated-find-package (fn name)
   (if *call-original-find-package*
       (funcall fn name)
       (budden-tools::hp-find-package (if (stringp name) (string-upcase name) name)
                                      *package* fn))))

#+(and sbcl (not careful-token-reader-via-native-package-local-nicknames))
(cl-advice:PORTABLY-WITHOUT-PACKAGE-LOCKS
 (define-advice find-package #'decorated-find-package))



#|(defun decorated-swank--all-completions (fn prefix package)
  (perga-implementation:perga
    (let completions nil)
    (flet set-completions (x) (setf completions x))
  (cond
    ((packages-seen-p *readtable*)
     (break "test me")
     (do-complete-symbol-with-budden-tools prefix package 'error #'set-completions))
    (t
     (funcall fn prefix package)))))|#


#|           (strings (loop for sym in syms
                          for str = (unparse-symbol sym)
                          when (prefix-match-p name str) ; remove |Foo|
                          collect str)))
      (swank::format-completion-set strings intern pname)))) |#

(defun note-package-and-readtable-change-in-line (string old-package old-readtable old-rt-specified-p)
  "Принимает ранее известные значения пакета, таблицы чтения и признак, что таблица чтения была задана. Возвращает (values new-package new-readtable new-rt-specified-p)"
  (let* ((new-package old-package)
         (new-readtable old-readtable)
         (new-rt-specified-p old-rt-specified-p)
         (new-pkg-name
          (oduresy:find-package-change-in-string string)))
    (multiple-value-bind (readtable-change-found new-rt-name)
                         (oduresy:find-readtable-change-in-string string)
      (setf new-package (if new-pkg-name (find-package new-pkg-name) old-package))
      (cond
       (readtable-change-found
        (setf new-rt-specified-p t)
        ; problem here for non-existent, but specified readtable
        (setf new-readtable (find-readtable new-rt-name))
        )
       (new-pkg-name
        (let ((rt-by-pkg (cdr (assoc new-pkg-name
                                     swank::*readtable-alist*
                                     :test 'string=))))
          (cond
           (old-rt-specified-p
           ; do nothing for rt when package changed
            )
           (rt-by-pkg
            (setf new-readtable rt-by-pkg)))))))
    (values new-package new-readtable new-rt-specified-p)))

(defun decorated-swank-source-path-parser--guess-reader-state (fn stream)
  "Take readtable from source where possible. See also oduvanchik-internals::recompute-line-tag-inner-1"
  (declare (ignore fn))
  (let* ((point (file-position stream))
         (pkg *package*)
         (rt *readtable*)
         (rt-specified-p nil))
    (file-position stream 0)
    (loop for line = (read-line stream nil nil) do
      (when (not line) (return))
      (multiple-value-setq
          (pkg rt rt-specified-p)
        (note-package-and-readtable-change-in-line line pkg rt rt-specified-p)))
    (file-position stream point)
    (values rt pkg)))

(cl-advice:define-advice swank/source-path-parser::guess-reader-state
                                     #'decorated-swank-source-path-parser--guess-reader-state)




(defun decorated-swank-source-path-parser--make-source-recording-readtable (fn readtable source-map)
  (declare (type readtable readtable) (type hash-table source-map))
  (declare (ignore fn))
  "Return a source position recording copy of READTABLE.
The source locations are stored in SOURCE-MAP."
  (flet ((install-special-sharpdot-reader (rt)
	   (let ((fun (ignore-errors
			(get-dispatch-macro-character #\# #\. rt))))
	     (when fun
	       (let ((wrapper (swank/source-path-parser::make-sharpdot-reader fun)))
		 (set-dispatch-macro-character #\# #\. wrapper rt)))))
	 (install-wrappers (rt)
	   (dolist (char (append (loop for code from 0 to 127 collect (code-char code))
                                 RUSSIAN-BUDDEN-TOOLS:*CYRILLIC-CHARACTERS*))
             (multiple-value-bind (fun nt) (get-macro-character char rt)
                #+SBCL
               (when ; follow the logic of sb-impl::%read-preserving-whitespace
                   (and (null fun)
                        (not (sb-impl::whitespace[2]p char readtable)))
                 (setq fun #'sb-impl::read-token
                       nt t))
               (when fun
                 (let ((wrapper (swank/source-path-parser::make-source-recorder fun source-map)))
                   (set-macro-character char wrapper nt rt)))))))
    (let ((rt (copy-readtable readtable)))
      (when (packages-seen-p readtable)
        (BUDDEN-TOOLS:ENABLE-BUDDENS-READTABLE-EXTENSIONS rt)
        (setf (readtable-case-advanced rt) (readtable-case-advanced readtable)))
      (install-special-sharpdot-reader rt)
      (install-wrappers rt)
      rt)))


(define-advice swank/source-path-parser::make-source-recording-readtable
                                     'decorated-swank-source-path-parser--make-source-recording-readtable)
