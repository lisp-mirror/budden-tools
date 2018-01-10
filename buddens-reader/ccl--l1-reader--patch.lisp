;;;-*-Mode: LISP; Package: CCL; coding: utf-8; system :buddens-reader;  -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; READ and related functions.

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *warn-if-redefine-kernel* nil))

(eval-when (:compile-toplevel :execute)
  (defconstant readtable-case-keywords '((:upcase . 1) (:downcase . 2) (:preserve . 0)
                                         (:invert . -1) (:studly . -2)
                                         ; brt
                                         (:upcase-if-uniform . -3)))
  (defmacro readtable-case-keywords () `',readtable-case-keywords))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	   
(eval-when (:compile-toplevel :execute)
  (def-accessors %svref
      token.string
    token.ipos
    token.opos
    token.len
    )

  (defmacro with-token-buffer ((name) &body body &environment env)
    (multiple-value-bind (body decls) (parse-body body env nil)
      `(let* ((,name (vector (%get-token-string 16) 0 0 16 nil)))
        (declare (dynamic-extent ,name))
        (unwind-protect
             (locally ,@decls ,@body)
          (%return-token-string ,name)))))
  )


;;; "escapes" is a list of escaped character positions, in reverse order
(defun %casify-token (token escapes)
  (let* ((case (readtable-case *readtable*))
         (opos (token.opos token))
         (string (token.string token)))
    (declare (fixnum opos))
    ;; brt begin
    (when (and (zerop (length escapes)) (eq case :upcase-if-uniform))
      (when (budden-tools::all-ascii-chars-in-same-case-p string)
        (dotimes (i opos)
          (setf (%schar string i) (budden-tools::char-upcase-ascii (%schar string i))))
        (return-from %casify-token (values))))
    ;; brt end
    (if (and (null escapes) (eq case :upcase))          ; Most common case, pardon the pun
      ; %strup is faster - boot probs tho
      (dotimes (i opos)
        (setf (%schar string i) (char-upcase (%schar string i))))
      (unless (eq case :preserve)
        (when (eq case :invert)
          (let* ((lower-seen nil)
                 (upper-seen nil))
            (do* ((i (1- opos) (1- i))
                  (esclist escapes)
                  (nextesc (if esclist (pop esclist) -1)))
                 ((< i 0) (if upper-seen (unless lower-seen (setq case :downcase))
                                         (when lower-seen (setq case :upcase))))
              (declare (fixnum i nextesc))
              (if (= nextesc i)
                (setq nextesc (if esclist (pop esclist) -1))
                (let* ((ch (%schar string i)))
                  (if (upper-case-p ch)
                    (setq upper-seen t)
                    (if (lower-case-p ch)
                      (setq lower-seen t))))))))
        (if (eq case :upcase)
          (do* ((i (1- opos) (1- i))
                  (nextesc (if escapes (pop escapes) -1)))
               ((< i 0))
            (declare (fixnum i nextesc))
            (if (= nextesc i)
                (setq nextesc (if escapes (pop escapes) -1))
                (setf (%schar string i) (char-upcase (%schar string i)))))
          (if (eq case :downcase)
            (do* ((i (1- opos) (1- i))
                  (nextesc (if escapes (pop escapes) -1)))
               ((< i 0))
            (declare (fixnum i nextesc))
            (if (= nextesc i)
                (setq nextesc (if escapes (pop escapes) -1))
                (setf (%schar string i) (char-downcase (%schar string i)))))))))))

;;; MCL's reader has historically treated ||:foo as a reference to the
;;; symbol FOO in the package which has the null string as its name.
;;; Some other implementations treat it as a keyword.  This takes an
;;; argument indicating whether or not something was "seen" before the
;;; first colon was read, even if that thing caused no characters to
;;; be added to the token.

(defun %token-package (token colonpos seenbeforecolon stream)
  (declare (ignorable stream))
  (if colonpos
    (if (and (eql colonpos 0) (not seenbeforecolon))
      *keyword-package*
      (let* ((string (token.string token)))
        (or (%find-pkg string colonpos)
            (subseq string 0 colonpos)
            #+nomore
            (signal-reader-error stream "Reference to unknown package ~s." (subseq string 0 colonpos)))))
    *package*))

;;; Returns 4 values: reversed list of escaped character positions,
;;; explicit package (if unescaped ":" or "::") or nil, t iff any
;;; non-dot, non-escaped chars in token, and t if either no explicit
;;; package or "::"

(defun %collect-xtoken (token stream 1stchar)
  (let* ((escapes ())
         (nondots nil)
         (explicit-package *read-suppress*)
         (double-colon t)
         (multi-escaped nil))
    (do* ((attrtab (rdtab.ttab *readtable*))
          (char 1stchar (read-char stream nil :eof )))
         ((eq char :eof))
      (flet ((add-note-escape-pos (char token escapes)
               (push (token.opos token) escapes)
               (%add-char-to-token char token)
               escapes))
        (let* ((attr (%character-attribute char attrtab)))
          (declare (fixnum attr))
          (when (or (= attr $cht_tmac)
                    (= attr $cht_wsp))
            (when (or (not (= attr $cht_wsp)) %keep-whitespace%)
              (unread-char char stream))
            (return ))
          (if (= attr $cht_ill)
              (signal-reader-error stream "Illegal character ~S." char)
              (if (= attr $cht_sesc)
                  (setq nondots t 
                        escapes (add-note-escape-pos (%read-char-no-eof stream) token escapes))
                  (if (= attr $cht_mesc)
                      (progn 
                        (setq nondots t)
                        (loop
                            (multiple-value-bind (nextchar nextattr) (%next-char-and-attr-no-eof stream attrtab)
                              (declare (fixnum nextattr))
                              (if (= nextattr $cht_mesc) 
                                  (return (setq multi-escaped t))
                                  (if (= nextattr $cht_sesc)
                                      (setq escapes (add-note-escape-pos (%read-char-no-eof stream) token escapes))
                            (setq escapes (add-note-escape-pos nextchar token escapes)))))))
                  (let* ((opos (token.opos token)))         ; Add char to token, note 1st colonpos
                    (declare (fixnum opos))
                    (if (and (eq char #\:)       ; (package-delimiter-p char ?)
                             (not explicit-package))
                      (let* ((nextch (%read-char-no-eof stream)))
                        (if (eq nextch #\:)
                          (setq double-colon t)
                          (progn
			    (unread-char nextch stream)
                            (setq double-colon nil)))
                        (%casify-token token escapes)
                        (setq explicit-package (%token-package token opos nondots stream)
                              nondots t
                              escapes nil)
                        (setf (token.opos token) 0))
                      (progn
                        (unless (eq char #\.) (setq nondots t))
                        (%add-char-to-token char token))))))))))
    (values (or escapes multi-escaped) (if *read-suppress* nil explicit-package) nondots double-colon
            ;; brt
            multi-escaped)))
          
;;; If we're allowed to have a single "." in this context, DOT-OK is some distinguished
;;; value that's returned to the caller when exactly one dot is present.
(defun %parse-token (stream firstchar dot-ok)
  (with-token-buffer (tb)
    (multiple-value-bind (escapes explicit-package nondots double-colon
                                  ;; brt
                                  seen-multiple-escapes)
        (%collect-xtoken tb stream firstchar)
      (unless *read-suppress* 
        (let* ((string (token.string tb))
               (len (token.opos tb))
               (colons (cond (double-colon 2)
                             (explicit-package 1)
                             (t 0))))
          (declare (fixnum len))
          (if (not nondots)
            (if (= len 1)
              (or dot-ok
                  (signal-reader-error stream "Dot context error in ~s." (%string-from-token tb)))
              (signal-reader-error stream "Illegal symbol syntax in ~s." (%string-from-token tb)))
            ;; Something other than a buffer full of dots.  Thank god.
            (let* ((num (if (null escapes)
                            (handler-case
                                (%token-to-number tb (%validate-radix *read-base*))
                              (arithmetic-error (c)
                                (error 'impossible-number
                                       :stream stream
                                       :token (%string-from-token tb)
                                       :condition c))))))
              (if (and num (not explicit-package))
                num
                (if (and (zerop len) (null escapes))
                  (%err-disp $XBADSYM)
                  (progn                  ; Muck with readtable case of extended token.
                    (%casify-token tb (unless (atom escapes) escapes))
                    (let* ((pkg (if explicit-package (pkg-arg explicit-package) *package*)))
                      ;;brt
                      (let ((custom-token-parsers (budden-tools::get-custom-token-parsers-for-package pkg)))
                        (dolist (parser custom-token-parsers)
                          (multiple-value-bind (result parsed) (funcall parser stream (%string-from-token tb) pkg)
                            (when parsed
                              (return-from %parse-token result)))))

                      ;; brt
                      (when buddens-reader-extensions:*return-package-and-symbol-name-from-read*
                        (return-from %parse-token (buddens-reader-extensions:make-potential-symbol :package pkg :casified-name (%string-from-token tb) :qualified colons)))

                      ;; brt
                      (let ((symbol
                             (block nil
                               (cond
                                 ((= colons 2)
                                  (return (budden-tools::intern-check-forbidden (%string-from-token tb) pkg stream t)))
                                 ((eq pkg *keyword-package*)
                                  (return (intern (%string-from-token tb) pkg)))
                                 ((zerop colons)
                                  (return (budden-tools::intern-check-forbidden (%string-from-token tb) pkg stream nil)))
                                 (t 
                                  (multiple-value-bind (symbol accessibility)
                                      (%find-symbol string len pkg)
                                    (when (eq accessibility :external) (return symbol))
                                    (let ((name (%string-from-token tb)))
                                      (with-simple-restart (continue "Use symbol anyway.")
                                        (error 'simple-reader-error
                                               ; :package pkg
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
                              (return-from %parse-token
                                (funcall readmacro stream symbol)))))
                        (when (boundp 'budden-tools:*fn-before-return-symbol-from-reader-hook*)
                          (setf symbol (funcall budden-tools:*fn-before-return-symbol-from-reader-hook* symbol stream)))
                        (return-from %parse-token symbol)
                        ))))))))))))
                    

(defun %parse-token-test (string &key dot-ok (case (readtable-case *readtable*)))
  (let* ((*readtable* (copy-readtable *readtable*))
         (stream (make-string-input-stream string)))
    (setf (readtable-case *readtable*) case)
    (%parse-token stream (read-char stream t) dot-ok)))

(defun do-parse-token-tests ()
  (assert (eq (%parse-token-test "ABC") 'abc))
  (assert (eq (%parse-token-test "BUDDEN-TOOLS::*INHIBIT-READMACRO*") 'budden-tools::*inhibit-readmacro*))
  (assert (numberp (%parse-token-test "3.14159")))
  (assert (typep (nth-value 1
                            (ignore-errors (%parse-token-test "BAD-PACKAGE:WORSE-SYMBOL")))
                 'condition))
  (assert (typep (nth-value 1
                            (ignore-errors (%parse-token-test "CCL::")))
                 'condition))
  (assert (string= (%parse-token-test ":zzzDebugger" :case :preserve) "zzzDebugger"))
  (assert (eq (%parse-token-test ":foo") :foo)))

(do-parse-token-tests)

;;; FIXME сделать это обёрткой
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *warn-if-redefine-kernel* t))

; end
