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

;;; Modify the read buffer according to READTABLE-CASE, ignoring
;;; ESCAPES. ESCAPES is a vector of the escaped indices.
(defun casify-read-buffer (token-buf)
  (let ((case (readtable-case *readtable*))
        (escapes (token-buf-escapes token-buf))
        ;brt
        (acase (budden-tools::readtable-case-advanced *readtable*))
        )
    (cond
    ;brt если в имени нет #\\, #\| и все латинские буквы в нём - в в одинаковом регистре, приводим к верхнему
     ((and (zerop (length escapes)) (eq acase :upcase-if-uniform))
      ; (setf case :preserve)
      (when (budden-tools::all-ascii-chars-in-same-case-p (copy-token-buf-string token-buf))
        (let ((buffer (token-buf-string token-buf)))
          (dotimes (i (token-buf-fill-ptr token-buf))
            (setf (schar buffer i) (budden-tools::char-upcase-ascii (schar buffer i)))
            ))))
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
              ;; is that a too general place for this? 
              ((swank::guess-package package-designator)
               (swank::guess-package package-designator))
              (t
               (error 'simple-reader-package-error
                      :package package-designator
                      :stream stream
                      :format-control "package or local-package nickname ~S not found from package ~S. Note we ignore SBCL's local-package nicknames, but obey those from def-merge-packages::! or defpackage-l2::!"
                      :format-arguments (list package-designator *reader-package*)))))))

(defun reader-find-package (package-designator stream)
  (reader-find-package-common package-designator stream t))

(defun read-token (stream firstchar)
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
    (return-from read-token nil))
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
      (let ((stream (in-stream-from-designator stream)))
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
                 )))  
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
                (return-from read-token (values result t))))))

        (when buddens-reader-extensions:*return-package-and-symbol-name-from-read*
          (return-from read-token (buddens-reader-extensions:make-potential-symbol :package pkg :casified-name (copy-token-buf-string buf) :qualified colons)))
                     
        (let ((symbol
               (block nil
                 (cond
                  ((= colons 2)
                   (return (budden-tools::intern-check-forbidden (copy-token-buf-string buf) pkg stream t)))
                  ((eq pkg *keyword-package*)
                   (return (%intern (token-buf-string buf) (token-buf-fill-ptr buf) pkg
                           'character))); (token-elt-type (token-buf-only-base-chars buf)))))
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
          (when (boundp 'budden-tools:*fn-before-return-symbol-from-reader-hook*)
            (setf symbol (funcall budden-tools:*fn-before-return-symbol-from-reader-hook* symbol stream)))
          (return symbol)
          )
        ))))
  )

(defmacro buddens-reader-extensions::test-attribute (char whichclass rt)
  `(= (the fixnum (get-cat-entry ,char ,rt)) ,whichclass))

(defmacro buddens-reader-extensions:constituentp (char &optional (rt '*readtable*))
  `(test-attribute ,char +char-attr-constituent+ ,rt))

