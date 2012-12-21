;;;; READ and friends

;;;; This software is ported from SBCL to Lispworks by Budden. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; ;b is commented out by budden
;;; ;brt - buddens-readtable

(in-package :sbcl-reader-budden-tools-lispworks)


;;;; miscellaneous global variables

;;; ANSI: "the floating-point format that is to be used when reading a
;;; floating-point number that has no exponent marker or that has e or
;;; E for an exponent marker"

;b(defvar *read-default-float-format* 'single-float)

(declaim (type (member short-float single-float double-float long-float)
               *read-default-float-format*))

;b(defvar *readtable*)
;b(declaim (type readtable *readtable*))
#-no-sb-doc
;b(setf (fdocumentation '*readtable* 'variable)
;b       "Variable bound to current readtable.")

;;; a standard Lisp readtable. This is for recovery from broken
;;; read-tables (and for WITH-STANDARD-IO-SYNTAX), and should not
;;; normally be user-visible.
(defvar *standard-readtable* (copy-readtable nil))

;b(defvar *old-package* nil
;b  #-no-sb-doc
;b  "the value of *PACKAGE* at the start of the last read, or NIL")

;;; In case we get an error trying to parse a symbol, we want to rebind the
;;; above stuff so it's cool.

;;; FIXME: These forward declarations should be moved somewhere earlier,
;;; or discarded.
;b(declaim (special *package* *keyword-package* *read-base*))

;;;; reader errors

; ����������������, ����� lispworks ���� �� SBCL
(defun reader-eof-error (stream context)
  (#+lispworks6 system::reader-eof-error-function stream context)
  #+nil (error 'reader-eof-error
         :stream stream
         :context context))

;;; If The Gods didn't intend for us to use multiple namespaces, why
;;; did They specify them?
(defun simple-reader-error (stream control &rest args)
  (apply 'alexandria.0.dev:simple-reader-error stream control args);b
  #+nil(error 'simple-reader-error ;b
         :stream stream
         :format-control control
         :format-arguments args))

;;;; macros and functions for character tables

;b;; FIXME: could be SB!XC:DEFMACRO inside EVAL-WHEN (COMPILE EVAL)
;b(defmacro get-cat-entry (char rt)
;b  ;; KLUDGE: Only give this side-effect-free args.
;b  ;; FIXME: should probably become inline function
;b  `(if (typep ,char 'base-char)
;b       (elt (character-attribute-array ,rt) (char-code ,char))
;b       (gethash ,char (character-attribute-hash-table ,rt)
;b        +char-attr-constituent+)))

;b(defun set-cat-entry (char newvalue &optional (rt *readtable*))
;b  (if (typep char 'base-char)
;b      (setf (elt (character-attribute-array rt) (char-code char)) newvalue)
;b      ;; FIXME: could REMHASH if we're setting to
;b      ;; +CHAR-ATTR-CONSTITUENT+
;b      (setf (gethash char (character-attribute-hash-table rt)) newvalue)))

;;; the value actually stored in the character macro table. As per
;;; ANSI #'GET-MACRO-CHARACTER and #'SET-MACRO-CHARACTER, this can
;;; be either a function or NIL.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro sb!xc-defmacro (&rest x) `(defmacro ,@x)))

(eval-when (:compile-toplevel :execute)
  (sb!xc-defmacro get-raw-cmt-entry (char readtable)
    `(if (typep ,char 'base-char)
         (svref (character-macro-array ,readtable) (char-code ,char))
         ;; Note: DEFAULT here is NIL, not #'UNDEFINED-MACRO-CHAR, so
         ;; that everything above the base-char range is a non-macro
         ;; constituent by default.
         (gethash ,char (character-macro-hash-table ,readtable) nil))))

;;; the value represented by whatever is stored in the character macro
;;; table. As per ANSI #'GET-MACRO-CHARACTER and #'SET-MACRO-CHARACTER,
;;; a function value represents itself, and a NIL value represents the
;;; default behavior.
;b(defun get-coerced-cmt-entry (char readtable)
;b  (the function
;b    (or (get-raw-cmt-entry char readtable)
;b        #'read-token)))
(defun get-coerced-cmt-entry (char readtable)
  (the function
       (or (get-macro-character char readtable)
           #'read-token)))

(defun undefined-macro-char (stream char)
  (unless *read-suppress*
    (simple-reader-error stream "undefined read-macro character ~S" char)))

;;; The character attribute table is a CHAR-CODE-LIMIT vector of integers.


#+lispworks6 
(defun extended-char-table-get-value (tbl char)
  (let ((code (char-code char)))
    (if (<= code 255)
        (svref (system::extended-char-table-vector tbl) code)
      (gethash char (system::extended-char-table-htable tbl)
               (system::extended-char-table-default tbl)))))

#+lispworks6
(defun get-cat-entry (char tbl)
  (extended-char-table-get-value (system::readtable-attributes tbl) char))


(defmacro test-attribute (char whichclass rt)
  `(= (the fixnum (get-cat-entry ,char ,rt)) ,whichclass))

;;; predicates for testing character attributes

;;; the [1] and [2] here refer to ANSI glossary entries for
;;; "whitespace".
;b(declaim (inline whitespace[1]p whitespace[2]p))
(defun whitespace[1]p (char)
  (test-attribute char +char-attr-whitespace+ *standard-readtable*))
(defun whitespace[2]p (char &optional (rt *readtable*))
  (test-attribute char +char-attr-whitespace+ rt))

(defmacro constituentp (char &optional (rt '*readtable*))
  `(test-attribute ,char +char-attr-constituent+ ,rt))

(defmacro terminating-macrop (char &optional (rt '*readtable*))
  `(test-attribute ,char +char-attr-terminating-macro+ ,rt))

(defmacro single-escape-p (char &optional (rt '*readtable*))
  `(test-attribute ,char +char-attr-single-escape+ ,rt))

(defmacro multiple-escape-p (char &optional (rt '*readtable*))
  `(test-attribute ,char +char-attr-multiple-escape+ ,rt))

(defmacro token-delimiterp (char &optional (rt '*readtable*))
  ;; depends on actual attribute numbering above.
  `(<= (get-cat-entry ,char ,rt) +char-attr-terminating-macro+))

;;;; constituent traits (see ANSI 2.1.4.2)

;;; There are a number of "secondary" attributes which are constant
;;; properties of characters (as long as they are constituents).

(defvar *constituent-trait-table*)
;(declaim (type attribute-table *constituent-trait-table*))

(defmacro aver (&rest args) `(assert ,@args))

(defun !set-constituent-trait (char trait)
  (aver (typep char 'base-char))
  (setf (elt *constituent-trait-table* (char-code char))
        trait))

(defun !cold-init-constituent-trait-table ()
  (setq *constituent-trait-table*
        (make-array 256 :element-type '(unsigned-byte 8)
                    :initial-element +char-attr-constituent+))
  (!set-constituent-trait #\: +char-attr-package-delimiter+)
  (!set-constituent-trait #\. +char-attr-constituent-dot+)
  (!set-constituent-trait #\+ +char-attr-constituent-sign+)
  (!set-constituent-trait #\- +char-attr-constituent-sign+)
  (!set-constituent-trait #\/ +char-attr-constituent-slash+)
  (do ((i (char-code #\0) (1+ i)))
      ((> i (char-code #\9)))
    (!set-constituent-trait (code-char i) +char-attr-constituent-digit+))
  (!set-constituent-trait #\E +char-attr-constituent-expt+)
  (!set-constituent-trait #\F +char-attr-constituent-expt+)
  (!set-constituent-trait #\D +char-attr-constituent-expt+)
  (!set-constituent-trait #\S +char-attr-constituent-expt+)
  (!set-constituent-trait #\L +char-attr-constituent-expt+)
  (!set-constituent-trait #\e +char-attr-constituent-expt+)
  (!set-constituent-trait #\f +char-attr-constituent-expt+)
  (!set-constituent-trait #\d +char-attr-constituent-expt+)
  (!set-constituent-trait #\s +char-attr-constituent-expt+)
  (!set-constituent-trait #\l +char-attr-constituent-expt+)
  (!set-constituent-trait #\Space +char-attr-invalid+)
  (!set-constituent-trait #\Newline +char-attr-invalid+)
  (dolist (c (list (char-code #\Backspace) (char-code #\Tab) (char-code #\formfeed)
                   (char-code #\return) (char-code #\rubout)))
    (!set-constituent-trait (code-char c) +char-attr-invalid+)))

(defmacro get-constituent-trait (char)
  `(if (typep ,char 'base-char)
       (elt *constituent-trait-table* (char-code ,char))
       +char-attr-constituent+))

;;;; definitions to support internal programming conventions

(defparameter *eof-object* (make-symbol "EOF-OBJECT"));b from early-extensions.lisp

(defmacro eofp (char)
  `(eq ,char *eof-object*))

(defun flush-whitespace (stream)
  ;; This flushes whitespace chars, returning the last char it read (a
  ;; non-white one). It always gets an error on end-of-file.
  (let (;(stream (in-synonym-of stream))
        )
    (do ((char (read-char stream nil :eof) (read-char stream nil :eof)))
        ((or (eq char :eof)
             (/= (get-cat-entry char *readtable*)
                 +char-attr-whitespace+)
         (if (eq char :eof)
             (error 'end-of-file :stream stream)
           char))))))

;;;; temporary initialization hack



;;;; implementation of the read buffer

(defvar *read-buffer*)
(defvar *read-buffer-length*)
;;; FIXME: Is it really helpful to have *READ-BUFFER-LENGTH* be a
;;; separate variable instead of just calculating it on the fly as
;;; (LENGTH *READ-BUFFER*)?

(defvar *inch-ptr*)
(defvar *ouch-ptr*)

(declaim (type fixnum *read-buffer-length* *inch-ptr* *ouch-ptr*))
;(declaim (type (simple-array character (*)) *read-buffer*))

(defmacro reset-read-buffer ()
  ;; Turn *READ-BUFFER* into an empty read buffer.
  `(progn
     ;; *OUCH-PTR* always points to next char to write.
     (setq *ouch-ptr* 0)
     ;; *INCH-PTR* always points to next char to read.
     (setq *inch-ptr* 0)))

;;; FIXME I removed "THE FIXNUM"'s from OUCH-READ-BUFFER and
;;; OUCH-UNREAD-BUFFER, check to make sure that Python really is smart
;;; enough to make good code without them. And while I'm at it,
;;; converting them from macros to inline functions might be good,
;;; too.

(defmacro ouch-read-buffer (char)
  `(progn
     ;; When buffer overflow
     (when (>= *ouch-ptr* *read-buffer-length*)
       ;; Size should be doubled.
       (grow-read-buffer))
     (setf (elt (the simple-string *read-buffer*) *ouch-ptr*) ,char)
     (setq *ouch-ptr* (1+ *ouch-ptr*))))

;;; macro to move *ouch-ptr* back one.
(defmacro ouch-unread-buffer ()
  '(when (> *ouch-ptr* *inch-ptr*)
     (setq *ouch-ptr* (1- (the fixnum *ouch-ptr*)))))

(defun grow-read-buffer ()
  (let* ((rbl (length *read-buffer*))
         (new-length (* 2 rbl))
         (new-buffer (make-string new-length)))
    (setq *read-buffer* (replace new-buffer *read-buffer*))
    (setq *read-buffer-length* new-length)))

(defun inchpeek-read-buffer ()
  (if (>= (the fixnum *inch-ptr*) (the fixnum *ouch-ptr*))
      *eof-object*
      (elt *read-buffer* *inch-ptr*)))

(defun inch-read-buffer ()
  (if (>= *inch-ptr* *ouch-ptr*)
      *eof-object*
      (prog1
          (elt *read-buffer* *inch-ptr*)
        (incf *inch-ptr*))))

(defmacro unread-buffer ()
  `(decf *inch-ptr*))

(defun read-unwind-read-buffer ()
  ;; Keep contents, but make next (INCH..) return first character.
  (setq *inch-ptr* 0))

(defun read-buffer-to-string ()
  (subseq *read-buffer* 0 *ouch-ptr*))

(defmacro with-reader ((&optional recursive-p) &body body)
  #-no-sb-doc
  "If RECURSIVE-P is NIL, bind *READER-BUFFER* and its subservient
variables to allow for nested and thread safe reading."
  `(if ,recursive-p
       (progn ,@body)
       (let* ((*read-buffer* (make-string 128))
              (*read-buffer-length* 128)
              (*ouch-ptr* 0)
              (*inch-ptr* 0))
         ,@body)))

;;;; READ-PRESERVING-WHITESPACE, READ-DELIMITED-LIST, and READ

;;; an alist for #=, used to keep track of objects with labels assigned that
;;; have been completely read. Each entry is (integer-tag gensym-tag value).
;;;
;;; KLUDGE: Should this really be an alist? It seems as though users
;;; could reasonably expect N log N performance for large datasets.
;;; On the other hand, it's probably very very seldom a problem in practice.
;;; On the third hand, it might be just as easy to use a hash table
;;; as an alist, so maybe we should. -- WHN 19991202
(defvar *sharp-equal-alist* ())

(declaim (special *standard-input*))

;;; READ-PRESERVING-WHITESPACE behaves just like READ, only it makes
;;; sure to leave terminating whitespace in the stream. (This is a
;;; COMMON-LISP exported symbol.)
(defun read-preserving-whitespace2 (&optional (stream *standard-input*)
                                             (eof-error-p t)
                                             (eof-value nil)
                                             (recursivep nil))
  #-no-sb-doc
  "Read from STREAM and return the value read, preserving any whitespace
   that followed the object."
  (if recursivep
      ;; a loop for repeating when a macro returns nothing
      (loop
       (let ((char (read-char stream eof-error-p *eof-object*)))
         (cond ((eofp char) (return eof-value))
               ((whitespace[2]p char))
               (t
                (let* ((macrofun (get-coerced-cmt-entry char *readtable*))
                       (result (multiple-value-list
                                (funcall macrofun stream char))))
                  ;; Repeat if macro returned nothing.
                  (when result
                    (return (unless *read-suppress* (car result)))))))))
      (with-reader ()
        (let ((*sharp-equal-alist* nil))
          (read-preserving-whitespace2 stream eof-error-p eof-value t)))))

;;; Return NIL or a list with one thing, depending.
;;;
;;; for functions that want comments to return so that they can look
;;; past them. We assume CHAR is not whitespace.
(defun read-maybe-nothing (stream char)
  (let ((retval (multiple-value-list
                 (funcall (get-coerced-cmt-entry char *readtable*)
                          stream
                          char))))
    (if retval (rplacd retval nil))))

(defun read2 (&optional (stream *standard-input*)
                       (eof-error-p t)
                       (eof-value ())
                       (recursivep ()))
  #-no-sb-doc
  "Read the next Lisp value from STREAM, and return it."
  (let ((result (read-preserving-whitespace2 stream
                                            eof-error-p
                                            eof-value
                                            recursivep)))
    ;; This function generally discards trailing whitespace. If you
    ;; don't want to discard trailing whitespace, call
    ;; CL:READ-PRESERVING-WHITESPACE instead.
    (unless (or (eql result eof-value) recursivep)
      (let ((next-char (read-char stream nil nil)))
        (unless (or (null next-char)
                    (whitespace[2]p next-char))
          (unread-char next-char stream))))
    result))

;;; (This is a COMMON-LISP exported symbol.)
(defun read-delimited-list2 (endchar &optional
                                    (input-stream *standard-input*)
                                    recursive-p)
  #-no-sb-doc
  "Read Lisp values from INPUT-STREAM until the next character after a
   value's representation is ENDCHAR, and return the objects as a list."
  (with-reader (recursive-p)
    (do ((char (flush-whitespace input-stream)
               (flush-whitespace input-stream))
         (retlist ()))
        ((char= char endchar) (unless *read-suppress* (nreverse retlist)))
      (setq retlist (nconc (read-maybe-nothing input-stream char) retlist)))))

;;;; basic readmacro definitions
;;;;
;;;; Some large, hairy subsets of readmacro definitions (backquotes
;;;; and sharp macros) are not here, but in their own source files.

(defun read-quote (stream ignore)
  (declare (ignore ignore))
  (list 'quote (read stream t nil t)))

(defun read-comment (stream ignore)
  (declare (ignore ignore))
  (handler-bind
      (#|(character-decoding-error
        #'(lambda (decoding-error)
            (declare (ignorable decoding-error))
            (style-warn
             'sb!kernel::character-decoding-error-in-macro-char-comment
             :position (file-position stream) :stream stream)
            (invoke-restart 'attempt-resync)))|#)
    (let (#|(stream (in-synonym-of stream))|#)
      (do ((char (read-char stream nil :eof) (read-char stream nil :eof)))
          ((or (eq char :eof) (char= char #\newline))))))
  ;; Don't return anything.
  (values))

(defun read-list (stream ignore)
  (declare (ignore ignore))
  (let* ((thelist (list nil))
         (listtail thelist))
    (do ((firstchar (flush-whitespace stream) (flush-whitespace stream)))
        ((char= firstchar #\) ) (cdr thelist))
      (when (char= firstchar #\.)
            (let ((nextchar (read-char stream t)))
              (cond ((token-delimiterp nextchar)
                     (cond ((eq listtail thelist)
                            (unless *read-suppress*
                              (simple-reader-error
                               stream
                               "Nothing appears before . in list.")))
                           ((whitespace[2]p nextchar)
                            (setq nextchar (flush-whitespace stream))))
                     (rplacd listtail
                             ;; Return list containing last thing.
                             (car (read-after-dot stream nextchar)))
                     (return (cdr thelist)))
                    ;; Put back NEXTCHAR so that we can read it normally.
                    (t (unread-char nextchar stream)))))
      ;; Next thing is not an isolated dot.
      (let ((listobj (read-maybe-nothing stream firstchar)))
        ;; allows the possibility that a comment was read
        (when listobj
              (rplacd listtail listobj)
              (setq listtail listobj))))))

(defun read-after-dot (stream firstchar)
  ;; FIRSTCHAR is non-whitespace!
  (let ((lastobj ()))
    (do ((char firstchar (flush-whitespace stream)))
        ((char= char #\) )
         (if *read-suppress*
             (return-from read-after-dot nil)
             (simple-reader-error stream "Nothing appears after . in list.")))
      ;; See whether there's something there.
      (setq lastobj (read-maybe-nothing stream char))
      (when lastobj (return t)))
    ;; At least one thing appears after the dot.
    ;; Check for more than one thing following dot.
    (do ((lastchar (flush-whitespace stream)
                   (flush-whitespace stream)))
        ((char= lastchar #\) ) lastobj) ;success!
      ;; Try reading virtual whitespace.
      (if (and (read-maybe-nothing stream lastchar)
               (not *read-suppress*))
          (simple-reader-error stream
                               "More than one object follows . in list.")))))

(defun read-string (stream closech)
  ;; This accumulates chars until it sees same char that invoked it.
  ;; For a very long string, this could end up bloating the read buffer.
  (reset-read-buffer)
  (let (#|(stream (in-synonym-of stream))|#)
    (do ((char (read-char stream nil :eof) (read-char stream nil :eof)))
        ((or (eq char :eof) (char= char closech))
         (if (eq char :eof)
             (error 'end-of-file :stream stream)))
      (when (single-escape-p char)
        (setq char (read-char stream nil :eof))
        (if (eq char :eof)
            (error 'end-of-file :stream stream)))
      (ouch-read-buffer char)))
  (read-buffer-to-string))

(defun read-right-paren (stream ignore)
  (declare (ignore ignore))
  (simple-reader-error stream "unmatched close parenthesis"))

;;; Read from the stream up to the next delimiter. Leave the resulting
;;; token in *READ-BUFFER*, and return two values:
;;; -- a list of the escaped character positions, and
;;; -- The position of the first package delimiter (or NIL).
(defun internal-read-extended-token (stream firstchar escape-firstchar)
  (reset-read-buffer)
  (let ((escapes '()))
    (when escape-firstchar
      (push *ouch-ptr* escapes)
      (ouch-read-buffer firstchar)
      (setq firstchar (read-char stream nil *eof-object*)))
  (do ((char firstchar (read-char stream nil *eof-object*))
       (colon nil))
      ((cond ((eofp char) t)
             ((token-delimiterp char)
              (unread-char char stream)
              t)
             (t nil))
       (values escapes colon))
    (cond ((single-escape-p char)
           ;; It can't be a number, even if it's 1\23.
           ;; Read next char here, so it won't be casified.
           (push *ouch-ptr* escapes)
           (let ((nextchar (read-char stream nil *eof-object*)))
             (if (eofp nextchar)
                 (reader-eof-error stream "after escape character")
                 (ouch-read-buffer nextchar))))
          ((multiple-escape-p char)
           ;; Read to next multiple-escape, escaping single chars
           ;; along the way.
           (loop
             (let ((ch (read-char stream nil *eof-object*)))
               (cond
                ((eofp ch)
                 (reader-eof-error stream "inside extended token"))
                ((multiple-escape-p ch) (return))
                ((single-escape-p ch)
                 (let ((nextchar (read-char stream nil *eof-object*)))
                   (cond ((eofp nextchar)
                          (reader-eof-error stream "after escape character"))
                         (t
                          (push *ouch-ptr* escapes)
                          (ouch-read-buffer nextchar)))))
                (t
                 (push *ouch-ptr* escapes)
                 (ouch-read-buffer ch))))))
          (t
           (when (and (constituentp char)
                      (eql (get-constituent-trait char)
                           +char-attr-package-delimiter+)
                      (not colon))
             (setq colon *ouch-ptr*))
           (ouch-read-buffer char))))))

;;;; character classes

;;; Return the character class for CHAR.
;;;
;;; FIXME: why aren't these ATT-getting forms using GET-CAT-ENTRY?
;;; Because we've cached the readtable tables?
(defmacro char-class (char rt)
  `(let ((att (get-cat-entry ,char ,rt)))
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
(defmacro char-class2 (char rt)
  `(let ((att (get-cat-entry ,char ,rt)))
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
(defmacro char-class3 (char rt)
  `(let ((att (get-cat-entry ,char ,rt)))
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

;;;; token fetching

;b(defvar *read-suppress* nil
;b  #-no-sb-doc
;b  "Suppress most interpreting in the reader when T.")

;b(defvar *read-base* 10
;b  #-no-sb-doc
;b  "the radix that Lisp reads numbers in")
;b(declaim (type (integer 2 36) *read-base*))

;;; Modify the read buffer according to READTABLE-CASE, ignoring
;;; ESCAPES. ESCAPES is a list of the escaped indices, in reverse
;;; order.
(defun casify-read-buffer (escapes)
  (let ((case (readtable-case *readtable*))
        (acase (budden-tools::readtable-case-advanced *readtable*))
        )
    ;brt ���� � ����� ��� #\\, #\| � ��� ��������� ����� � �� - � � ���������� ��������, �������� � ��������
    (when (and (null escapes) (eq acase :upcase-if-uniform))
      (setf case :preserve)
      (when (budden-tools::all-ascii-chars-in-same-case-p (subseq *read-buffer* 0 *ouch-ptr*))
        (let ((buffer *read-buffer*))
          (dotimes (i *ouch-ptr*)
            (setf (schar buffer i) (budden-tools::char-upcase-ascii (schar buffer i)))
            )))
      )
    (cond
     ((and (null escapes) (eq case :upcase))
      ;; Pull the special variable access out of the loop.
      (let ((buffer *read-buffer*))
        (dotimes (i *ouch-ptr*)
          ;(declare (optimize (sb!c::insert-array-bounds-checks 0)))
          (setf (schar buffer i) (char-upcase (schar buffer i))))))
     ((eq case :preserve))
     (t
      (macrolet ((skip-esc (&body body)
                   `(do ((i (1- *ouch-ptr*) (1- i))
                         (buffer *read-buffer*)
                         (escapes escapes))
                        ((minusp i))
                      (declare (fixnum i)
                               ;(optimize (sb!c::insert-array-bounds-checks 0))
                               )
                      (when (or (null escapes)
                                (let ((esc (first escapes)))
                                  (declare (fixnum esc))
                                  (cond ((< esc i) t)
                                        (t
                                         (aver (= esc i))
                                         (pop escapes)
                                         nil))))
                        (let ((ch (schar buffer i)))
                          ,@body)))))
        (flet ((lower-em ()
                 (skip-esc (setf (schar buffer i) (char-downcase ch))))
               (raise-em ()
                 (skip-esc (setf (schar buffer i) (char-upcase ch)))))
          (ecase case
            (:upcase (raise-em))
            (:downcase (lower-em))
            (:invert
             (let ((all-upper t)
                   (all-lower t))
               (skip-esc
                 (when (both-case-p ch)
                   (if (upper-case-p ch)
                       (setq all-lower nil)
                       (setq all-upper nil))))
               (cond (all-lower (raise-em))
                     (all-upper (lower-em))))))))))))

(defun sane-package ()
  (let ((maybe-package *package*))
    (cond ((and (packagep maybe-package)
                ;; For good measure, we also catch the problem of
                ;; *PACKAGE* being bound to a deleted package.
                ;; Technically, this is not undefined behavior in itself,
                ;; but it will immediately lead to undefined to behavior,
                ;; since almost any operation on a deleted package is
                ;; undefined.
                (package-name maybe-package))
           maybe-package)
          (t
           ;; We're in the undefined behavior zone. First, munge the
           ;; system back into a defined state.
           (let ((really-package (find-package :cl-user)))
             (setf *package* really-package)
             ;; Then complain.
             (warn "*package* can't be a ~A; it has been reset to ~S"
                   (if (packagep maybe-package)
                       "deleted package"
                     (type-of maybe-package))
                   really-package)
             )))))


;;; If the symbol named by the first LENGTH characters of NAME doesn't exist,
;;; then create it, special-casing the keyword package.
(defun intern* (name length package)
  (intern (subseq name 0 length) package))

;;; Check internal and external symbols, then scan down the list
;;; of hashtables for inherited symbols.
(defun find-symbol* (string length package)
  (find-symbol (subseq string 0 length) package)) 

(defun read-token (stream firstchar)
  #-no-sb-doc
  "This function is just an fsm that recognizes numbers and symbols."
  ;; Check explicitly whether FIRSTCHAR has an entry for
  ;; NON-TERMINATING in CHARACTER-ATTRIBUTE-TABLE and
  ;; READ-DOT-NUMBER-SYMBOL in CMT. Report an error if these are
  ;; violated. (If we called this, we want something that is a
  ;; legitimate token!) Read in the longest possible string satisfying
  ;; the Backus-Naur form for "unqualified-token". Leave the result in
  ;; the *READ-BUFFER*. Return next char after token (last char read).
  (with-reader (); brt we need this as we are unable to get access to read,read-delimited-list, etc.definitions
  (when *read-suppress*
    (internal-read-extended-token stream firstchar nil)
    (return-from read-token nil))
  (let (;(attribute-array (character-attribute-array *readtable*))
        ;(attribute-hash-table (character-attribute-hash-table *readtable*))
        (package-designator nil)
        (colons 0)
        (possibly-rational t)
        (seen-digit-or-expt nil)
        (possibly-float t)
        (was-possibly-float nil)
        (escapes ())
        (seen-multiple-escapes nil))
    (reset-read-buffer)
    (prog ((char firstchar))
      (case (char-class3 char *readtable*)
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
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (setq possibly-rational t
            possibly-float t)
      (case (char-class3 char *readtable*)
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
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-integer)))
      (setq was-possibly-float possibly-float)
      (case (char-class3 char *readtable*)
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
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-integer)))
      (case (char-class3 char *readtable*)
        (#.+char-attr-constituent-digit+ (go LEFTDIGIT))
        (#.+char-attr-constituent-decimal-digit+ (error "impossible!"))
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
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char *readtable*)
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
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (let ((*read-base* 10))
                             (make-integer))))
      (case (char-class char *readtable*)
        (#.+char-attr-constituent-digit+ (go RIGHTDIGIT))
        (#.+char-attr-constituent-expt+ (go EXPONENT))
        (#.+char-attr-delimiter+
         (unread-char char stream)
         (return (let ((*read-base* 10))
                   (make-integer))))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     RIGHTDIGIT ; saw "[sign] {decimal-digit}* dot {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-float stream)))
      (case (char-class char *readtable*)
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
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char *readtable*)
        (#.+char-attr-constituent-digit+ (go RIGHTDIGIT))
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (t (go SYMBOL)))
     FRONTDOT ; saw "dot"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (simple-reader-error stream "dot context error"))
      (case (char-class char *readtable*)
        (#.+char-attr-constituent-digit+ (go RIGHTDIGIT))
        (#.+char-attr-constituent-dot+ (go DOTS))
        (#.+char-attr-delimiter+  (simple-reader-error stream
                                                       "dot context error"))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     EXPONENT
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (setq possibly-float t)
      (case (char-class char *readtable*)
        (#.+char-attr-constituent-sign+ (go EXPTSIGN))
        (#.+char-attr-constituent-digit+ (go EXPTDIGIT))
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     EXPTSIGN ; got to EXPONENT, and saw a sign character
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char *readtable*)
        (#.+char-attr-constituent-digit+ (go EXPTDIGIT))
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     EXPTDIGIT ; got to EXPONENT, saw "[sign] {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-float stream)))
      (case (char-class char *readtable*)
        (#.+char-attr-constituent-digit+ (go EXPTDIGIT))
        (#.+char-attr-delimiter+
         (unread-char char stream)
         (return (make-float stream)))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     RATIO ; saw "[sign] {digit}+ slash"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class2 char *readtable*)
        (#.+char-attr-constituent-digit+ (go RATIODIGIT))
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     RATIODIGIT ; saw "[sign] {digit}+ slash {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-ratio stream)))
      (case (char-class2 char *readtable*)
        (#.+char-attr-constituent-digit+ (go RATIODIGIT))
        (#.+char-attr-delimiter+
         (unread-char char stream)
         (return (make-ratio stream)))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     DOTS ; saw "dot {dot}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (simple-reader-error stream "too many dots"))
      (case (char-class char *readtable*)
        (#.+char-attr-constituent-dot+ (go DOTS))
        (#.+char-attr-delimiter+
         (unread-char char stream)
         (simple-reader-error stream "too many dots"))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     SYMBOL ; not a dot, dots, or number
      (let (#|(stream (in-synonym-of stream))|#)
        ;; CLOS stream
        (prog ()
          SYMBOL-LOOP
          (ouch-read-buffer char)
          (setq char (read-char stream nil :eof))
          (when (eq char :eof) (go RETURN-SYMBOL))
          (case (char-class char *readtable*)
            (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
            (#.+char-attr-delimiter+ (unread-char char stream)
                                     (go RETURN-SYMBOL))
            (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
            (#.+char-attr-package-delimiter+ (go COLON))
            (t (go SYMBOL-LOOP)))))
     SINGLE-ESCAPE ; saw a single-escape
      ;; Don't put the escape character in the read buffer.
      ;; READ-NEXT CHAR, put in buffer (no case conversion).
      (let ((nextchar (read-char stream nil nil)))
        (unless nextchar
          (reader-eof-error stream "after single-escape character"))
        (push *ouch-ptr* escapes)
        (ouch-read-buffer nextchar))
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char *readtable*)
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
      MULT-ESCAPE
      (setq seen-multiple-escapes t)
      (do ((char (read-char stream t) (read-char stream t)))
          ((multiple-escape-p char))
        (if (single-escape-p char) (setq char (read-char stream t)))
        (push *ouch-ptr* escapes)
        (ouch-read-buffer char))
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char *readtable*)
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
      COLON
      (casify-read-buffer escapes)
      (unless (zerop colons)
        (simple-reader-error stream
                             "too many colons in ~S"
                             (read-buffer-to-string)))
      (setq colons 1)
      (setq package-designator
            (if (plusp *ouch-ptr*)
                ;; FIXME: It seems inefficient to cons up a package
                ;; designator string every time we read a symbol with an
                ;; explicit package prefix. Perhaps we could implement
                ;; a FIND-PACKAGE* function analogous to INTERN*
                ;; and friends?
                (read-buffer-to-string)
                (if seen-multiple-escapes
                    (read-buffer-to-string)
                    *keyword-package*)))
      (reset-read-buffer)
      ; brt - ���� �������� ����������� ����� ��� ����� ������, ���������� ���
      (let ((found (find-package package-designator)))
        (unless found
          (error "package ~S not found" package-designator
                 ))
        (let ((custom-token-reader (budden-tools::get-custom-reader-for-package found)))
          (when custom-token-reader
            (return
             (funcall custom-token-reader stream t nil t)))))  
      (setq escapes ())
      (setq char (read-char stream nil nil))
      (unless char (reader-eof-error stream "after reading a colon"))
      (case (char-class char *readtable*)
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
      (setq char (read-char stream nil nil))
      (unless char
        (reader-eof-error stream "after reading a colon"))
      (case (char-class char *readtable*)
        (#.+char-attr-delimiter+
         (unread-char char stream)
         (simple-reader-error stream
                              "illegal terminating character after a colon: ~S"
                              char))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+
         (simple-reader-error stream
                              "too many colons after ~S name"
                              package-designator))
        (t (go SYMBOL)))
      RETURN-SYMBOL
      (casify-read-buffer escapes)
      (let ((found (if package-designator
                       (find-package package-designator)
                       (sane-package))))
        (unless found
          (error "package ~S not found" package-designator
                 ))
        ;brt
        (let ((custom-token-parsers (budden-tools::get-custom-token-parsers-for-package found)))
          (dolist (parser custom-token-parsers)
            (multiple-value-bind (result parsed) (funcall parser stream (subseq *read-buffer* 0 *ouch-ptr*) found)
              (when parsed
                (return-from read-token (values result t))))))

        (let ((symbol
               (block nil
                 (if (or (zerop colons) (= colons 2) (eq found *keyword-package*))
                     (if (= colons 2)
                         (return (budden-tools::intern-check-forbidden (subseq *read-buffer* 0 *ouch-ptr*) found))
                       (return (intern* *read-buffer* *ouch-ptr* found)))
                   (multiple-value-bind (symbol test)
                       (find-symbol* *read-buffer* *ouch-ptr* found)
                     (when (eq test :external) (return symbol))
                     (let ((name (read-buffer-to-string)))
                       (with-simple-restart (continue "Use symbol anyway.")
                         (error (if test
                                    "The symbol ~S is not external in the ~A package."
                                  "Symbol ~S not found in the ~A package.")
                                name (package-name found)
                                ))
                       (return (intern name found))))))))
          (when (not seen-multiple-escapes))
          (let ((readmacro (budden-tools::symbol-readmacro symbol)))
            (when (and readmacro (not budden-tools::*inhibit-readmacro*))
               ;(break)
              (return (funcall readmacro stream symbol))))
          (return symbol)
          )
        ))))
  )

;;; for semi-external use:
;;;
;;; For semi-external use: Return 3 values: the string for the token,
;;; a flag for whether there was an escape char, and the position of
;;; any package delimiter.
(defun read-extended-token (stream &optional (*readtable* *readtable*))
  (let ((first-char (read-char stream nil nil t)))
    (cond (first-char
           (multiple-value-bind (escapes colon)
               (internal-read-extended-token stream first-char nil)
             (casify-read-buffer escapes)
             (values (read-buffer-to-string) (not (null escapes)) colon)))
          (t
           (values "" nil nil)))))

;;; for semi-external use:
;;;
;;; Read an extended token with the first character escaped. Return
;;; the string for the token.
(defun read-extended-token-escaped (stream &optional (*readtable* *readtable*))
  (let ((first-char (read-char stream nil nil)))
    (cond (first-char
            (let ((escapes (internal-read-extended-token stream first-char t)))
              (casify-read-buffer escapes)
              (read-buffer-to-string)))
          (t
            (reader-eof-error stream "after escape")))))

;;;; number-reading functions

(defmacro digit* nil
  `(do ((ch char (inch-read-buffer)))
       ((or (eofp ch) (not (digit-char-p ch))) (setq char ch))
     ;; Report if at least one digit is seen.
     (setq one-digit t)))

(defmacro exponent-letterp (letter)
  `(member ,letter '(#\E #\S #\F #\L #\D #\e #\s #\f #\l #\d)))

;;; FIXME: It would be cleaner to have these generated automatically
;;; by compile-time code instead of having them hand-created like
;;; this. The !COLD-INIT-INTEGER-READER code below should be resurrected
;;; and tested.
(defvar *integer-reader-safe-digits*
  #(nil nil
    26 17 13 11 10 9 8 8 8 7 7 7 7 6 6 6 6 6 6 6 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5)
  #-no-sb-doc
  "the mapping of base to 'safe' number of digits to read for a fixnum")
(defvar *integer-reader-base-power*
  #(nil nil
    67108864 129140163 67108864 48828125 60466176 40353607
    16777216 43046721 100000000 19487171 35831808 62748517 105413504 11390625
    16777216 24137569 34012224 47045881 64000000 85766121 113379904 6436343
    7962624 9765625 11881376 14348907 17210368 20511149 24300000 28629151
    33554432 39135393 45435424 52521875 60466176)
  #-no-sb-doc
  "the largest fixnum power of the base for MAKE-INTEGER")
(declaim (simple-vector *integer-reader-safe-digits*
                        *integer-reader-base-power*))
#|
(defun !cold-init-integer-reader ()
  (do ((base 2 (1+ base)))
      ((> base 36))
    (let ((digits
          (do ((fix (truncate most-positive-fixnum base)
                    (truncate fix base))
               (digits 0 (1+ digits)))
              ((zerop fix) digits))))
      (setf (aref *integer-reader-safe-digits* base)
            digits
            (aref *integer-reader-base-power* base)
            (expt base digits)))))
|#

(defun make-integer ()
  #-no-sb-doc
  "Minimizes bignum-fixnum multiplies by reading a 'safe' number of digits,
  then multiplying by a power of the base and adding."
  (let* ((base *read-base*)
         (digits-per (aref *integer-reader-safe-digits* base))
         (base-power (aref *integer-reader-base-power* base))
         (negativep nil)
         (number 0))
    (declare (type fixnum digits-per base-power))
    (read-unwind-read-buffer)
    (let ((char (inch-read-buffer)))
      (cond ((char= char #\-)
             (setq negativep t))
            ((char= char #\+))
            (t (unread-buffer))))
    (loop
     (let ((num 0))
       (declare (type fixnum num))
       (dotimes (digit digits-per)
         (let* ((ch (inch-read-buffer)))
           (cond ((or (eofp ch) (char= ch #\.))
                  (return-from make-integer
                               (let ((res
                                      (if (zerop number) num
                                          (+ num (* number
                                                    (expt base digit))))))
                                 (if negativep (- res) res))))
                 (t (setq num (+ (digit-char-p ch base)
                                 (the fixnum (* num base))))))))
       (setq number (+ num (* number base-power)))))))

(defun make-float (stream)
  ;; Assume that the contents of *read-buffer* are a legal float, with nothing
  ;; else after it.
  (read-unwind-read-buffer)
  (let ((negative-fraction nil)
        (number 0)
        (divisor 1)
        (negative-exponent nil)
        (exponent 0)
        (float-char ())
        (char (inch-read-buffer)))
    (if (cond ((char= char #\+) t)
              ((char= char #\-) (setq negative-fraction t)))
        ;; Flush it.
        (setq char (inch-read-buffer)))
    ;; Read digits before the dot.
    (do* ((ch char (inch-read-buffer))
          (dig (digit-char-p ch) (digit-char-p ch)))
         ((not dig) (setq char ch))
      (setq number (+ (* number 10) dig)))
    ;; Deal with the dot, if it's there.
    (when (char= char #\.)
      (setq char (inch-read-buffer))
      ;; Read digits after the dot.
      (do* ((ch char (inch-read-buffer))
            (dig (and (not (eofp ch)) (digit-char-p ch))
                 (and (not (eofp ch)) (digit-char-p ch))))
           ((not dig) (setq char ch))
        (setq divisor (* divisor 10))
        (setq number (+ (* number 10) dig))))
    ;; Is there an exponent letter?
    (cond ((eofp char)
           ;; If not, we've read the whole number.
           (let ((num (make-float-aux number divisor
                                      *read-default-float-format*
                                      stream)))
             (return-from make-float (if negative-fraction (- num) num))))
          ((exponent-letterp char)
           (setq float-char char)
           ;; Build exponent.
           (setq char (inch-read-buffer))
           ;; Check leading sign.
           (if (cond ((char= char #\+) t)
                     ((char= char #\-) (setq negative-exponent t)))
               ;; Flush sign.
               (setq char (inch-read-buffer)))
           ;; Read digits for exponent.
           (do* ((ch char (inch-read-buffer))
                 (dig (and (not (eofp ch)) (digit-char-p ch))
                      (and (not (eofp ch)) (digit-char-p ch))))
                ((not dig)
                 (setq exponent (if negative-exponent (- exponent) exponent)))
             (setq exponent (+ (* exponent 10) dig)))
           ;; Generate and return the float, depending on FLOAT-CHAR:
           (let* ((float-format (case (char-upcase float-char)
                                  (#\E *read-default-float-format*)
                                  (#\S 'short-float)
                                  (#\F 'single-float)
                                  (#\D 'double-float)
                                  (#\L 'long-float)))
                  (result (make-float-aux (* (expt 10 exponent) number)
                                          divisor float-format stream)))
             (return-from make-float
               (if negative-fraction (- result) result))))
          (t (error "bad fallthrough in floating point reader")))))

(defun make-float-aux (number divisor float-format stream)
  (handler-case
      (coerce (/ number divisor) float-format)
    (type-error (c)
      (error 'reader-impossible-number-error
             :error c :stream stream
             :format-control "failed to build float"))))

(defun make-ratio (stream)
  ;; Assume *READ-BUFFER* contains a legal ratio. Build the number from
  ;; the string.
  ;;
  ;; Look for optional "+" or "-".
  (let ((numerator 0) (denominator 0) (char ()) (negative-number nil))
    (read-unwind-read-buffer)
    (setq char (inch-read-buffer))
    (cond ((char= char #\+)
           (setq char (inch-read-buffer)))
          ((char= char #\-)
           (setq char (inch-read-buffer))
           (setq negative-number t)))
    ;; Get numerator.
    (do* ((ch char (inch-read-buffer))
          (dig (digit-char-p ch *read-base*)
               (digit-char-p ch *read-base*)))
         ((not dig))
         (setq numerator (+ (* numerator *read-base*) dig)))
    ;; Get denominator.
    (do* ((ch (inch-read-buffer) (inch-read-buffer))
          (dig ()))
         ((or (eofp ch) (not (setq dig (digit-char-p ch *read-base*)))))
         (setq denominator (+ (* denominator *read-base*) dig)))
    (let ((num (handler-case
                   (/ numerator denominator)
                 (arithmetic-error (c)
                   (error 'reader-impossible-number-error
                          :error c :stream stream
                          :format-control "failed to build ratio")))))
      (if negative-number (- num) num))))

;;;; cruft for dispatch macros

#|(defun make-char-dispatch-table ()
  (make-hash-table))

(defun dispatch-char-error (stream sub-char ignore)
  (declare (ignore ignore))
  (if *read-suppress*
      (values)
      (simple-reader-error stream
                           "no dispatch function defined for ~S"
                           sub-char)))

(defun make-dispatch-macro-character (char &optional
                                           (non-terminating-p nil)
                                           (rt *readtable*))
  #-no-sb-doc
  "Cause CHAR to become a dispatching macro character in readtable (which
   defaults to the current readtable). If NON-TERMINATING-P, the char will
   be non-terminating."
  (set-macro-character char #'read-dispatch-char non-terminating-p rt)
  (let* ((dalist (dispatch-tables rt))
         (dtable (cdr (find char dalist :test #'char= :key #'car))))
    (cond (dtable
           (error "The dispatch character ~S already exists." char))
          (t
           (setf (dispatch-tables rt)
                 (push (cons char (make-char-dispatch-table)) dalist)))))
  t)

(defun set-dispatch-macro-character (disp-char sub-char function
                                               &optional (rt *readtable*))
  #-no-sb-doc
  "Cause FUNCTION to be called whenever the reader reads DISP-CHAR
   followed by SUB-CHAR."
  ;; Get the dispatch char for macro (error if not there), diddle
  ;; entry for sub-char.
  (when (digit-char-p sub-char)
    (error "SUB-CHAR must not be a decimal digit: ~S" sub-char))
  (let* ((sub-char (char-upcase sub-char))
         (rt (or rt *standard-readtable*))
         (dpair (find disp-char (dispatch-tables rt)
                      :test #'char= :key #'car)))
    (if dpair
        (setf (gethash sub-char (cdr dpair)) (coerce function 'function))
        (error "~S is not a dispatch char." disp-char))))

(defun get-dispatch-macro-character (disp-char sub-char
                                     &optional (rt *readtable*))
  #-no-sb-doc
  "Return the macro character function for SUB-CHAR under DISP-CHAR
   or NIL if there is no associated function."
  (let* ((sub-char (char-upcase sub-char))
         (rt (or rt *standard-readtable*))
         (dpair (find disp-char (dispatch-tables rt)
                      :test #'char= :key #'car)))
    (if dpair
        (values (gethash sub-char (cdr dpair)))
        (error "~S is not a dispatch char." disp-char))))
|#

(defun read-dispatch-char (stream char)
  ;; Read some digits.
  (system::read-dispatch-char stream char)
  #|(let ((numargp nil)
        (numarg 0)
        (sub-char ()))
    (do* ((ch (read-char stream nil *eof-object*)
              (read-char stream nil *eof-object*))
          (dig ()))
         ((or (eofp ch)
              (not (setq dig (digit-char-p ch))))
          ;; Take care of the extra char.
          (if (eofp ch)
              (reader-eof-error stream "inside dispatch character")
              (setq sub-char (char-upcase ch))))
      (setq numargp t)
      (setq numarg (+ (* numarg 10) dig)))
    ;; Look up the function and call it.
    (let ((dpair (find char (dispatch-tables *readtable*)
                       :test #'char= :key #'car)))
      (if dpair
          (funcall (the function
                     (gethash sub-char (cdr dpair) #'dispatch-char-error))
                   stream sub-char (if numargp numarg nil))
          (simple-reader-error stream
                               "no dispatch table for dispatch char"))))|#
  )

;;;; READ-FROM-STRING

(defun read-from-string2 (string &optional (eof-error-p t) eof-value
                                &key (start 0) end
                                preserve-whitespace)
  #-no-sb-doc
  "The characters of string are successively given to the lisp reader
   and the lisp object built by the reader is returned. Macro chars
   will take effect."
  (declare (string string))
  ;(with-array-data ((string string :offset-var offset)
  ;                  (start start)
  ;                  (end end)
  ;                  :check-fill-pointer t)
    (let ((stream (make-string-input-stream string start end)))
      (values (if preserve-whitespace
                  (read-preserving-whitespace2 stream eof-error-p eof-value)
                  (read2 stream eof-error-p eof-value))
              (- (budden-tools::extract-file-position stream) start)))
  ;  )
  )

;;;; PARSE-INTEGER

#|(defun parse-integer (string &key (start 0) end (radix 10) junk-allowed)
  #-no-sb-doc
  "Examine the substring of string delimited by start and end
  (default to the beginning and end of the string)  It skips over
  whitespace characters and then tries to parse an integer. The
  radix parameter must be between 2 and 36."
  (macrolet ((parse-error (format-control)
               `(error 'simple-parse-error
                       :format-control ,format-control
                       :format-arguments (list string))))
    (with-array-data ((string string :offset-var offset)
                      (start start)
                      (end end)
                      :check-fill-pointer t)
      (let ((index (do ((i start (1+ i)))
                       ((= i end)
                        (if junk-allowed
                            (return-from parse-integer (values nil end))
                            (parse-error "no non-whitespace characters in string ~S.")))
                     (declare (fixnum i))
                     (unless (whitespace[1]p (char string i)) (return i))))
            (minusp nil)
            (found-digit nil)
            (result 0))
        (declare (fixnum index))
        (let ((char (char string index)))
          (cond ((char= char #\-)
                 (setq minusp t)
                 (incf index))
                ((char= char #\+)
                 (incf index))))
        (loop
         (when (= index end) (return nil))
         (let* ((char (char string index))
                (weight (digit-char-p char radix)))
           (cond (weight
                  (setq result (+ weight (* result radix))
                        found-digit t))
                 (junk-allowed (return nil))
                 ((whitespace[1]p char)
                  (loop
                   (incf index)
                   (when (= index end) (return))
                   (unless (whitespace[1]p (char string index))
                      (parse-error "junk in string ~S")))
                  (return nil))
                 (t
                  (parse-error "junk in string ~S"))))
         (incf index))
        (values
         (if found-digit
             (if minusp (- result) result)
             (if junk-allowed
                 nil
                 (parse-error "no digits in string ~S")))
         (- index offset)))))|#

;;;; reader initialization code

(defun sharp-colon (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (with-reader ()
    (multiple-value-bind (token escapep colon) (read-extended-token stream)
      (declare (simple-string token) (ignore escapep))
      (cond
       (*read-suppress* nil)
       (colon
        (simple-reader-error
         stream "The symbol following #: contains a package marker: ~S" token))
       (t
        (make-symbol token))))))


(defun !reader-cold-init ()
  (!cold-init-constituent-trait-table)
  ;(!cold-init-standard-readtable)
  ;; FIXME: This was commented out, but should probably be restored.
  #+nil (!cold-init-integer-reader))

;b(def!method print-object ((readtable readtable) stream)
;b  (print-unreadable-object (readtable stream :identity t :type t))) 


(!reader-cold-init)