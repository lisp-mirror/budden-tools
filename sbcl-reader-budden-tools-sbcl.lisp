;;;; Ридер из SBCL-1.3.4, подразумевается, что будет запускаться только из SBCL

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.


(named-readtables:in-readtable nil)

; (in-package :sbcl-reader-budden-tools-sbcl)
(in-package :sb-impl)



;brt
(defstruct potential-symbol package casified-name (qualified 0 :type (integer 0 2)))

;brt
(defvar *return-package-and-symbol-name-from-read* nil
  "Side branch of read. If this var is t, potential-symbol structures are returned instead of new symbols. Nothing is interned. Existing symbols may be returned as potential symbols are just as symbols")

;;;; character classes (direct copy from SBCL reader)

;;; Return the character class for CHAR.
;;;
;;; FIXME: why aren't these ATT-getting forms using GET-CAT-ENTRY?
;;; Because we've cached the readtable tables?
(defmacro char-class (char attarray atthash)
  `(let ((att (if (typep (truly-the character ,char) 'base-char)
                  (aref ,attarray (char-code ,char))
                  (gethash ,char ,atthash +char-attr-constituent+))))
     (declare (fixnum att))
     (cond
       ((<= att +char-attr-terminating-macro+) +char-attr-delimiter+)
       ((< att +char-attr-constituent+) att)
       (t (setf att (get-constituent-trait ,char))
          (if (= att +char-attr-invalid+)
              (simple-reader-error stream "invalid constituent")
              att)))))

;;; Return the character class for CHAR, which might be part of a
;;; rational number.
(defmacro char-class2 (char attarray atthash)
  `(let ((att (if (typep (truly-the character ,char) 'base-char)
                  (aref ,attarray (char-code ,char))
                  (gethash ,char ,atthash +char-attr-constituent+))))
     (declare (fixnum att))
     (cond
       ((<= att +char-attr-terminating-macro+) +char-attr-delimiter+)
       ((< att +char-attr-constituent+) att)
       (t (setf att (get-constituent-trait ,char))
          (cond
            ((digit-char-p ,char *read-base*) +char-attr-constituent-digit+)
            ((= att +char-attr-constituent-digit+) +char-attr-constituent+)
            ((= att +char-attr-invalid+)
             (simple-reader-error stream "invalid constituent"))
            (t att))))))

;;; Return the character class for a char which might be part of a
;;; rational or floating number. (Assume that it is a digit if it
;;; could be.)
(defmacro char-class3 (char attarray atthash)
  `(let ((att (if (typep (truly-the character ,char) 'base-char)
                  (aref ,attarray (char-code ,char))
                  (gethash ,char ,atthash +char-attr-constituent+))))
     (declare (fixnum att))
     (cond
       ((<= att +char-attr-terminating-macro+) +char-attr-delimiter+)
       ((< att +char-attr-constituent+) att)
       (t (setf att (get-constituent-trait ,char))
          (when possibly-rational
            (setq possibly-rational
                  (or (digit-char-p ,char *read-base*)
                      (= att +char-attr-constituent-slash+))))
          (when possibly-float
            (setq possibly-float
                  (or (digit-char-p ,char 10)
                      (= att +char-attr-constituent-dot+))))
          (cond
            ((digit-char-p ,char (max *read-base* 10))
             (if (digit-char-p ,char *read-base*)
                 (if (= att +char-attr-constituent-expt+)
                     +char-attr-constituent-digit-or-expt+
                     +char-attr-constituent-digit+)
                 +char-attr-constituent-decimal-digit+))
            ((= att +char-attr-invalid+)
             (simple-reader-error stream "invalid constituent"))
            (t att))))))


#| ;;; FIXME: for these two macro chars, if STREAM is a FORM-TRACKING-STREAM,
;;; every cons cell should generate a notification so that the readtable
;;; manipulation in SB-COVER can be eliminated in favor of a stream observer.
;;; It is cheap to add events- it won't increase consing in the compiler
;;; because it the extra events can simply be ignored.
 (macrolet
    ((with-list-reader ((streamvar delimiter) &body body)
       `(let* ((thelist (list nil))
               (listtail thelist)
               (collectp (if *read-suppress* 0 -1)))
          (declare (dynamic-extent thelist))
          (loop (let ((firstchar (flush-whitespace ,streamvar)))
                  (when (eq firstchar ,delimiter)
                    (return (cdr thelist)))
                  ,@body))))
     (read-list-item (streamvar)
       `(multiple-value-bind (winp obj)
            (read-maybe-nothing ,streamvar firstchar)
          ;; allow for a character macro return to return nothing
          (unless (zerop (logand winp collectp))
            (setq listtail
                  (cdr (rplacd (truly-the cons listtail) (list obj))))))))

  ;;; The character macro handler for left paren
  (defun read-list (stream ignore)
    (declare (ignore ignore))
    (with-list-reader (stream #\))
      (when (eq firstchar #\.)
        (let ((nextchar (read-char stream t)))
          (cond ((token-delimiterp nextchar)
                 (cond ((eq listtail thelist)
                        (unless (zerop collectp)
                          (simple-reader-error
                           stream "Nothing appears before . in list.")))
                       ((whitespace[2]p nextchar)
                        (setq nextchar (flush-whitespace stream))))
                 (rplacd (truly-the cons listtail)
                         (read-after-dot stream nextchar collectp))
                 ;; Check for improper ". ,@" or ". ,." now rather than
                 ;; in the #\` reader. The resulting QUASIQUOTE macro might
                 ;; never be exapanded, but nonetheless could be erroneous.
                 (unless (zerop (logand *backquote-depth* collectp))
                   (let ((lastcdr (cdr (last listtail))))
                     (when (and (comma-p lastcdr) (comma-splicing-p lastcdr))
                       (simple-reader-error
                        stream "~S contains a splicing comma after a dot"
                        (cdr thelist)))))
                 (return (cdr thelist)))
                    ;; Put back NEXTCHAR so that we can read it normally.
                (t (unread-char nextchar stream)))))
      ;; Next thing is not an isolated dot.
      (read-list-item stream)))

  ;;; (This is a COMMON-LISP exported symbol.)
  (defun read-delimited-list (endchar &optional
                                      (input-stream *standard-input*)
                                      recursive-p)
  #!+sb-doc
  "Read Lisp values from INPUT-STREAM until the next character after a
   value's representation is ENDCHAR, and return the objects as a list."
    (declare (explicit-check))
    (check-for-recursive-read input-stream recursive-p 'read-delimited-list)
    (flet ((%read-delimited-list ()
             (with-list-reader (input-stream endchar)
               (read-list-item input-stream))))
      (if recursive-p
          (%read-delimited-list)
          (with-read-buffer () (%read-delimited-list)))))) ; end MACROLET

 (defun read-after-dot (stream firstchar collectp)
  ;; FIRSTCHAR is non-whitespace!
  (let ((lastobj ()))
    (do ((char firstchar (flush-whitespace stream)))
        ((eq char #\))
         (if (zerop collectp)
             (return-from read-after-dot nil)
             (simple-reader-error stream "Nothing appears after . in list.")))
      ;; See whether there's something there.
      (multiple-value-bind (winp obj) (read-maybe-nothing stream char)
        (unless (zerop winp) (return (setq lastobj obj)))))
    ;; At least one thing appears after the dot.
    ;; Check for more than one thing following dot.
    (loop
     (let ((char (flush-whitespace stream)))
       (cond ((eq char #\)) (return lastobj)) ;success!
             ;; Try reading virtual whitespace.
             ((not (zerop (logand (read-maybe-nothing stream char)
                                  (truly-the fixnum collectp))))
              (simple-reader-error
               stream "More than one object follows . in list.")))))))

;;; Read from the stream up to the next delimiter. Leave the resulting
;;; token in *READ-BUFFER*, and return three values:
;;; -- a TOKEN-BUF
;;; -- whether any escape character was seen (even if no character is escaped)
;;; -- whether a package delimiter character was seen
;;; Normalizes the input to NFKC before returning
 (defun internal-read-extended-token (stream firstchar escape-firstchar
                                     &aux (read-buffer *read-buffer*))
  (reset-read-buffer read-buffer)
  (when escape-firstchar
    (ouch-read-buffer-escaped firstchar read-buffer)
    (setq firstchar (read-char stream nil +EOF+)))
  (do ((char firstchar (read-char stream nil +EOF+))
       (seen-multiple-escapes nil)
       (rt *readtable*)
       (colon nil))
      ((cond ((eq char +EOF+) t)
             ((token-delimiterp char rt)
              (unread-char char stream)
              t)
             (t nil))
       (progn
         (multiple-value-setq (read-buffer colon)
           (normalize-read-buffer read-buffer colon))
         (values read-buffer
                 (or (plusp (fill-pointer (token-buf-escapes read-buffer)))
                     seen-multiple-escapes)
                 colon)))
    (cond ((single-escape-p char rt)
           ;; It can't be a number, even if it's 1\23.
           ;; Read next char here, so it won't be casified.
           (let ((nextchar (read-char stream nil +EOF+)))
             (if (eq nextchar +EOF+)
                 (reader-eof-error stream "after escape character")
                 (ouch-read-buffer-escaped nextchar read-buffer))))
          ((multiple-escape-p char rt)
           (setq seen-multiple-escapes t)
           ;; Read to next multiple-escape, escaping single chars
           ;; along the way.
           (loop
             (let ((ch (read-char stream nil +EOF+)))
               (cond
                ((eq ch +EOF+)
                 (reader-eof-error stream "inside extended token"))
                ((multiple-escape-p ch rt) (return))
                ((single-escape-p ch rt)
                 (let ((nextchar (read-char stream nil +EOF+)))
                   (if (eq nextchar +EOF+)
                       (reader-eof-error stream "after escape character")
                       (ouch-read-buffer-escaped nextchar read-buffer))))
                (t
                 (ouch-read-buffer-escaped ch read-buffer))))))
          (t
           (when (and (not colon) ; easiest test first
                      (constituentp char rt)
                      (eql (get-constituent-trait char)
                           +char-attr-package-delimiter+))
             (setq colon t))
           (ouch-read-buffer char read-buffer)))))

;;; Normalize TOKEN-BUF to NFKC, returning a new TOKEN-BUF and the
;;; COLON value
 (defun normalize-read-buffer (token-buf &optional colon)
  (unless (readtable-normalization *readtable*)
    (return-from normalize-read-buffer (values token-buf colon)))
  (when (token-buf-only-base-chars token-buf)
    (return-from normalize-read-buffer (values token-buf colon)))
  (let ((current-buffer (copy-token-buf-string token-buf))
        (old-escapes (copy-seq (token-buf-escapes token-buf)))
        (str-to-normalize (make-string (token-buf-fill-ptr token-buf)))
        (normalize-ptr 0) (escapes-ptr 0))
    (reset-read-buffer token-buf)
    (macrolet ((clear-str-to-normalize ()
               `(progn
                  (loop for char across (sb!unicode:normalize-string
                                         (subseq str-to-normalize 0 normalize-ptr)
                                         :nfkc) do
                       (ouch-read-buffer char token-buf))
                  (setf normalize-ptr 0)))
               (push-to-normalize (ch)
                 (let ((ch-gen (gensym)))
                   `(let ((,ch-gen ,ch))
                      (setf (char str-to-normalize normalize-ptr) ,ch-gen)
                      (incf normalize-ptr)))))
      (loop for c across current-buffer
         for i from 0
         do
           (if (and (< escapes-ptr (length old-escapes))
                    (eql i (aref old-escapes escapes-ptr)))
               (progn
                 (clear-str-to-normalize)
                 (ouch-read-buffer-escaped c token-buf)
                 (incf escapes-ptr))
               (push-to-normalize c)))
      (clear-str-to-normalize)
      (values token-buf colon))))
|#
;;; Modify the read buffer according to READTABLE-CASE, ignoring
;;; ESCAPES. ESCAPES is a vector of the escaped indices.
(defun casify-read-buffer (token-buf)
  (let ((case (readtable-case *readtable*))
        (escapes (token-buf-escapes token-buf))
        (acase (budden-tools::readtable-case-advanced *readtable*))
        )
    ;brt если в имени нет #\\, #\| и все латинские буквы в нём - в в одинаковом регистре, приводим к верхнему
    (when (and (zerop (length escapes)) (eq acase :upcase-if-uniform))
      (setf case :preserve)
      (when (budden-tools::all-ascii-chars-in-same-case-p (copy-token-buf-string token-buf))
        (let ((buffer (token-buf-string token-buf)))
          (dotimes (i (token-buf-fill-ptr token-buf))
            (setf (schar buffer i) (budden-tools::char-upcase-ascii (schar buffer i)))
            )))
      )
    (cond
     ((and (zerop (length escapes)) (eq case :upcase))
      (let ((buffer (token-buf-string token-buf)))
        (dotimes (i (token-buf-fill-ptr token-buf))
          (declare (optimize (sb-c::insert-array-bounds-checks 0)))
          (setf (schar buffer i) (char-upcase (schar buffer i))))))
     ((eq case :preserve))
     (t
      (macrolet ((skip-esc (&body body)
                   `(do ((i (1- (token-buf-fill-ptr token-buf)) (1- i))
                         (buffer (token-buf-string token-buf))
                         (esc (if (zerop (fill-pointer escapes))
                                  -1 (vector-pop escapes))))
                        ((minusp i))
                      (declare (fixnum i)
                               (optimize (sb-c::insert-array-bounds-checks 0)))
                      (if (< esc i)
                          (let ((ch (schar buffer i)))
                            ,@body)
                          (progn
                            (aver (= esc i))
                            (setq esc (if (zerop (fill-pointer escapes))
                                          -1 (vector-pop escapes))))))))
        (flet ((lower-em ()
                 (skip-esc (setf (schar buffer i) (char-downcase ch))))
               (raise-em ()
                 (skip-esc (setf (schar buffer i) (char-upcase ch)))))
          (ecase case
            (:upcase (raise-em))
            (:downcase (lower-em))
            (:invert
             (let ((all-upper t)
                   (all-lower t)
                   (fillptr (fill-pointer escapes)))
               (skip-esc
                 (when (both-case-p ch)
                   (if (upper-case-p ch)
                       (setq all-lower nil)
                       (setq all-upper nil))))
               (setf (fill-pointer escapes) fillptr)
               (cond (all-lower (raise-em))
                     (all-upper (lower-em))))))))))))

(defun reader-find-package-common (package-designator stream with-read-buffer)
  (if (%instancep package-designator)
      package-designator
      (let ((package (budden-tools:hp-find-package
                      package-designator
                      (or *reader-package* (sane-package)))))
        (cond (package
               ;; Release the token-buf that was used for the designator
               (when with-read-buffer
                 (release-token-buf (shiftf (token-buf-next *read-buffer*) nil)))
               package)
              (t
               (error 'simple-reader-package-error
                      :package package-designator
                      :stream stream
                      :format-control "package or local-package nickname ~S not found from package ~S. Note we ignore SBCL's local-package nicknames, but obey those from def-merge-packages::! or defpackage-l2::!"
                      :format-arguments (list package-designator *reader-package*)))))))

(defun reader-find-package (package-designator stream)
  (reader-find-package-common package-designator stream t))

(defun read-token-2 (stream firstchar)
  "Default readmacro function. Handles numbers, symbols, and SBCL's
extended <package-name>::<form-in-package> syntax."
  ;; Check explicitly whether FIRSTCHAR has an entry for
  ;; NON-TERMINATING in CHARACTER-ATTRIBUTE-TABLE and
  ;; READ-DOT-NUMBER-SYMBOL in CMT. Report an error if these are
  ;; violated. (If we called this, we want something that is a
  ;; legitimate token!) Read in the longest possible string satisfying
  ;; the Backus-Naur form for "unqualified-token". Leave the result in
  ;; the *READ-BUFFER*. Return next char after token (last char read).

  (when *read-suppress*
    (internal-read-extended-token stream firstchar nil)
    (return-from read-token-2 nil))
  (let* ((rt *readtable*)
         (attribute-array (character-attribute-array rt))
         (attribute-hash-table (character-attribute-hash-table rt))
         (buf *read-buffer*)
         (package-designator nil)
         (colons 0)
        (colons-in-symbol 0)
         (possibly-rational t)
         (seen-digit-or-expt nil)
         (possibly-float t)
         (was-possibly-float nil)
         ;(escapes ())
         (seen-multiple-escapes nil))
    (declare (token-buf buf))
    (reset-read-buffer buf)
    (macrolet ((getchar-or-else (what)
                 `(when (eq (setq char (read-char stream nil +EOF+)) +EOF+)
                    ,what)))
     (prog ((char firstchar))
      (case (char-class3 char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-sign+ (go SIGN))
        (#.+char-attr-constituent-digit+ (go LEFTDIGIT))
        (#.+char-attr-constituent-digit-or-expt+
         (setq seen-digit-or-expt t)
         (go LEFTDIGIT))
        (#.+char-attr-constituent-decimal-digit+ (go LEFTDECIMALDIGIT))
        (#.+char-attr-constituent-dot+ (go FRONTDOT))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-invalid+ (simple-reader-error stream
                                                    "invalid constituent"))
        ;; can't have eof, whitespace, or terminating macro as first char!
        (t (go SYMBOL)))
     SIGN ; saw "sign"
      (ouch-read-buffer char buf)
      (getchar-or-else (go RETURN-SYMBOL))
      (setq possibly-rational t
            possibly-float t)
      (case (char-class3 char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-digit+ (go LEFTDIGIT))
        (#.+char-attr-constituent-digit-or-expt+
         (setq seen-digit-or-expt t)
         (go LEFTDIGIT))
        (#.+char-attr-constituent-decimal-digit+ (go LEFTDECIMALDIGIT))
        (#.+char-attr-constituent-dot+ (go SIGNDOT))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (t (go SYMBOL)))
     LEFTDIGIT ; saw "[sign] {digit}+"
      (ouch-read-buffer char buf)
      (getchar-or-else (return (make-integer)))
      (setq was-possibly-float possibly-float)
      (case (char-class3 char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-digit+ (go LEFTDIGIT))
        (#.+char-attr-constituent-decimal-digit+ (if possibly-float
                                                     (go LEFTDECIMALDIGIT)
                                                     (go SYMBOL)))
        (#.+char-attr-constituent-dot+ (if possibly-float
                                           (go MIDDLEDOT)
                                           (go SYMBOL)))
        (#.+char-attr-constituent-digit-or-expt+
         (if (or seen-digit-or-expt (not was-possibly-float))
             (progn (setq seen-digit-or-expt t) (go LEFTDIGIT))
             (progn (setq seen-digit-or-expt t) (go LEFTDIGIT-OR-EXPT))))
        (#.+char-attr-constituent-expt+
         (if was-possibly-float
             (go EXPONENT)
             (go SYMBOL)))
        (#.+char-attr-constituent-slash+ (if possibly-rational
                                             (go RATIO)
                                             (go SYMBOL)))
        (#.+char-attr-delimiter+ (unread-char char stream)
                                 (return (make-integer)))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     LEFTDIGIT-OR-EXPT
      (ouch-read-buffer char buf)
      (getchar-or-else (return (make-integer)))
      (case (char-class3 char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-digit+ (go LEFTDIGIT))
        (#.+char-attr-constituent-decimal-digit+ (bug "impossible!"))
        (#.+char-attr-constituent-dot+ (go SYMBOL))
        (#.+char-attr-constituent-digit-or-expt+ (go LEFTDIGIT))
        (#.+char-attr-constituent-expt+ (go SYMBOL))
        (#.+char-attr-constituent-sign+ (go EXPTSIGN))
        (#.+char-attr-constituent-slash+ (if possibly-rational
                                             (go RATIO)
                                             (go SYMBOL)))
        (#.+char-attr-delimiter+ (unread-char char stream)
                                 (return (make-integer)))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     LEFTDECIMALDIGIT ; saw "[sign] {decimal-digit}+"
      (aver possibly-float)
      (ouch-read-buffer char buf)
      (getchar-or-else (go RETURN-SYMBOL))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-digit+ (go LEFTDECIMALDIGIT))
        (#.+char-attr-constituent-dot+ (go MIDDLEDOT))
        (#.+char-attr-constituent-expt+ (go EXPONENT))
        (#.+char-attr-constituent-slash+ (aver (not possibly-rational))
                                         (go SYMBOL))
        (#.+char-attr-delimiter+ (unread-char char stream)
                                 (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     MIDDLEDOT ; saw "[sign] {digit}+ dot"
      (ouch-read-buffer char buf)
      (getchar-or-else (return (make-integer 10)))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-digit+ (go RIGHTDIGIT))
        (#.+char-attr-constituent-expt+ (go EXPONENT))
        (#.+char-attr-delimiter+
         (unread-char char stream)
         (return (make-integer 10)))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     RIGHTDIGIT ; saw "[sign] {decimal-digit}* dot {digit}+"
      (ouch-read-buffer char buf)
      (getchar-or-else (return (make-float stream)))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-digit+ (go RIGHTDIGIT))
        (#.+char-attr-constituent-expt+ (go EXPONENT))
        (#.+char-attr-delimiter+
         (unread-char char stream)
         (return (make-float stream)))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     SIGNDOT ; saw "[sign] dot"
      (ouch-read-buffer char buf)
      (getchar-or-else (go RETURN-SYMBOL))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-digit+ (go RIGHTDIGIT))
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (t (go SYMBOL)))
     FRONTDOT ; saw "dot"
      (ouch-read-buffer char buf)
      (getchar-or-else (simple-reader-error stream "dot context error"))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-digit+ (go RIGHTDIGIT))
        (#.+char-attr-constituent-dot+ (go DOTS))
        (#.+char-attr-delimiter+  (simple-reader-error stream
                                                       "dot context error"))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     EXPONENT
      (ouch-read-buffer char buf)
      (getchar-or-else (go RETURN-SYMBOL))
      (setq possibly-float t)
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-sign+ (go EXPTSIGN))
        (#.+char-attr-constituent-digit+ (go EXPTDIGIT))
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     EXPTSIGN ; got to EXPONENT, and saw a sign character
      (ouch-read-buffer char buf)
      (getchar-or-else (go RETURN-SYMBOL))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-digit+ (go EXPTDIGIT))
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     EXPTDIGIT ; got to EXPONENT, saw "[sign] {digit}+"
      (ouch-read-buffer char buf)
      (getchar-or-else (return (make-float stream)))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-digit+ (go EXPTDIGIT))
        (#.+char-attr-delimiter+
         (unread-char char stream)
         (return (make-float stream)))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     RATIO ; saw "[sign] {digit}+ slash"
      (ouch-read-buffer char buf)
      (getchar-or-else (go RETURN-SYMBOL))
      (case (char-class2 char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-digit+ (go RATIODIGIT))
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     RATIODIGIT ; saw "[sign] {digit}+ slash {digit}+"
      (ouch-read-buffer char buf)
      (getchar-or-else (return (make-ratio stream)))
      (case (char-class2 char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-digit+ (go RATIODIGIT))
        (#.+char-attr-delimiter+
         (unread-char char stream)
         (return (make-ratio stream)))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     DOTS ; saw "dot {dot}+"
      (ouch-read-buffer char buf)
      (getchar-or-else (simple-reader-error stream "too many dots"))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-dot+ (go DOTS))
        (#.+char-attr-delimiter+
         (unread-char char stream)
         (simple-reader-error stream "too many dots"))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     SYMBOL ; not a dot, dots, or number
      (let ((stream (in-synonym-of stream)))
        (macrolet
           ((scan (read-a-char &optional finish)
             `(prog ()
               SYMBOL-LOOP
               (ouch-read-buffer char buf)
               (setq char ,read-a-char)
               (when (eq char +EOF+) (go RETURN-SYMBOL))
               (case (char-class char attribute-array attribute-hash-table)
                 (#.+char-attr-single-escape+ ,finish (go SINGLE-ESCAPE))
                 (#.+char-attr-delimiter+ ,finish
                                          (unread-char char stream)
                                          (go RETURN-SYMBOL))
                 (#.+char-attr-multiple-escape+ ,finish (go MULT-ESCAPE))
                 (#.+char-attr-package-delimiter+ ,finish (go COLON))
                 (t (go SYMBOL-LOOP))))))
        (if (ansi-stream-p stream)
            (prepare-for-fast-read-char stream
              (scan (fast-read-char nil +EOF+) (done-with-fast-read-char)))
            ;; CLOS stream
            (scan (read-char stream nil +EOF+)))))
     SINGLE-ESCAPE ; saw a single-escape
      ;; Don't put the escape character in the read buffer.
      ;; READ-NEXT CHAR, put in buffer (no case conversion).
      (let ((nextchar (read-char stream nil +EOF+)))
        (when (eq nextchar +EOF+)
          (reader-eof-error stream "after single-escape character"))
        (ouch-read-buffer-escaped nextchar buf))
      (getchar-or-else (go RETURN-SYMBOL))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
      MULT-ESCAPE
      (setq seen-multiple-escapes t)
      ;; sometimes we pass eof-error=nil but check. here we just let it err.
      ;; should pick one style and stick with it.
      (do ((char (read-char stream t) (read-char stream t)))
          ((multiple-escape-p char rt))
        (if (single-escape-p char rt) (setq char (read-char stream t)))
        (ouch-read-buffer-escaped char buf))
      (getchar-or-else (go RETURN-SYMBOL))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
      COLON

      (unless (zerop colons)
        (simple-reader-error
         stream "too many colons in ~S" (copy-token-buf-string buf)))
      (setf buf (normalize-read-buffer buf))
      (casify-read-buffer buf)
      (setq colons 1)
      (setq package-designator
            (if (or (plusp (token-buf-fill-ptr *read-buffer*))
                    seen-multiple-escapes)
                (prog1 (sized-token-buf-string buf)
                  (let ((new (acquire-token-buf)))
                    (setf (token-buf-next new) buf ; new points to old
                          buf new *read-buffer* new)))
                *keyword-package*))
      (reset-read-buffer buf)
      ; brt - если назначен специальный ридер для этого пакета, используем его
      (let ((found (reader-find-package-common package-designator stream nil)))
        (unless found
          (simple-reader-error stream "package or local-package nickname ~S not found. Note we ignore SBCL's local-package nicknames, but obey those from def-merge-packages::! or defpackage-l2::!" package-designator
                 ))
        (let ((custom-token-reader (budden-tools::get-custom-reader-for-package found)))
          (when custom-token-reader
            (return
             (funcall custom-token-reader stream t nil t)))))  
      (getchar-or-else (reader-eof-error stream "after reading a colon"))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-delimiter+
         (unread-char char stream)
         (simple-reader-error stream
                              "illegal terminating character after a colon: ~S"
                              char))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go INTERN))
        (t (go SYMBOL)))
      INTERN
      (setq colons 2)
      (getchar-or-else (reader-eof-error stream "after reading a colon"))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-delimiter+
         (unread-char char stream)
         (if package-designator
             (let* ((*reader-package*
                     (reader-find-package package-designator stream)))
               (return (read stream t nil t)))
             (simple-reader-error stream
                                  "illegal terminating character after a double-colon: ~S"
                                  char)))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+
         (simple-reader-error stream
                              "too many colons after ~S name"
                              package-designator))
        (t (go SYMBOL)))
      RETURN-SYMBOL
      (setf buf (normalize-read-buffer buf))
      (casify-read-buffer buf)
      (let ((pkg (if package-designator
                     (reader-find-package package-designator stream)
                     (or *reader-package* (sane-package)))))
                     
        ;brt
        (let ((custom-token-parsers (budden-tools::get-custom-token-parsers-for-package pkg)))
          (dolist (parser custom-token-parsers)
            (multiple-value-bind (result parsed) (funcall parser stream (copy-token-buf-string buf) pkg)
              (when parsed
                (return-from read-token-2 (values result t))))))

        (when *return-package-and-symbol-name-from-read*
          (return-from read-token-2 (make-potential-symbol :package pkg :casified-name (copy-token-buf-string buf) :qualified colons)))
                     
        (let ((symbol
               (block nil
                 (cond
                  ((= colons 2)
                   (return (budden-tools::intern-check-forbidden (copy-token-buf-string buf) pkg stream t)))
                  ((eq pkg *keyword-package*)
                   (return (%intern (token-buf-string buf) (token-buf-fill-ptr buf) pkg t)))
                  ((zerop colons)
                   (return (budden-tools::intern-check-forbidden (copy-token-buf-string buf) pkg stream nil)))
                  (t 
                   (multiple-value-bind (symbol accessibility)
                       (%find-symbol (token-buf-string buf) (token-buf-fill-ptr buf) pkg)
                     (when (eq accessibility :external) (return symbol))
                     (let ((name (copy-token-buf-string buf)))
                       (with-simple-restart (continue "Use symbol anyway.")
                        (error 'simple-reader-package-error
                         :package pkg
                         :stream stream
                         :format-arguments (list name (package-name pkg))
                         :format-control
                         (if accessibility
                             "The symbol ~S is not external in the ~A package."
                             "Symbol ~S not found in the ~A package.")))
                       (return (intern name pkg)))))))))
          (when (not seen-multiple-escapes)
          (let ((readmacro (budden-tools::symbol-readmacro symbol)))
            (when (and readmacro (not budden-tools::*inhibit-readmacro*))
               ;(break)
              (return (funcall readmacro stream symbol)))))
          (when (boundp 'budden-tools::*fn-before-return-symbol-from-reader-hook*)
            (setf symbol (funcall budden-tools::*fn-before-return-symbol-from-reader-hook* symbol stream)))
          (return symbol)
          )
        ))))
  )

(defvar *read-token-d* nil)

(defun read-token-d (fn stream firstchar)
  (if t ; *read-token-d*
      (read-token-2 stream firstchar)
      (funcall fn stream firstchar)))

(budden-tools::decorate-function 'read-token #'read-token-d)


#|(defun sharp-colon (stream sub-char numarg)
  "Сильно устарело и не работает,т.к. token - это буфер, а не строка"
  (declare (ignore sub-char numarg))
  (multiple-value-bind (token escapep colon) (read-extended-token stream)
    (declare (simple-string token) (ignore escapep))
    (cond
     (*read-suppress* nil)
     (colon
      (simple-reader-error
       stream "The symbol following #: contains a package marker: ~S" token))
     (t
      (make-symbol token)))))|#

(def-merge-packages::! :sbcl-reader-budden-tools-sbcl
 (:nicknames :sbcl-reader-budden-tools-lispworks :sbcl-reader-budden-tools)
 (:always t)
 (:use :cl 
  :budden-tools
  :sb-impl
  )
 (:shadow
   ; #:read-token
   #:constituentp
   )
 (:import-from :sb-impl
   #:*return-package-and-symbol-name-from-read*
   #:read-token ; это не обычная ф-я чтения, она требует буферов
   #:potential-symbol
   #:make-potential-symbol
   #:potential-symbol-casified-name
   #:potential-symbol-package
   #:potential-symbol-qualified
   #:potential-symbol-p
   #:sharp-colon
   )
 (:export 
   #:*return-package-and-symbol-name-from-read*
   #:read-token
   #:potential-symbol
   #:make-potential-symbol
   #:potential-symbol-casified-name
   #:potential-symbol-package
   #:potential-symbol-qualified
   #:potential-symbol-p
   #:sharp-colon
   #:constituentp ; отключено для SBCL
   ; #:read-preserving-whitespace-2
 )) 


(defmacro sbcl-reader-budden-tools-sbcl::test-attribute (char whichclass rt)
  `(= (the fixnum (get-cat-entry ,char ,rt)) ,whichclass))

(defmacro sbcl-reader-budden-tools-sbcl::constituentp (char &optional (rt '*readtable*))
  `(test-attribute ,char +char-attr-constituent+ ,rt))
