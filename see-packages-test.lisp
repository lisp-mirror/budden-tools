; -*- coding: windows-1251-dos; -*-

(defpackage see-packages-test 
  (:use :cl :budden-tools)
  (:shadowing-import-from :iterate #:iter)
  (:shadowing-import-from :trivial-deftest #:deftest)
  #+nil (:shadowing-import-from :budden-tools))

(in-package :see-packages-test)

(ignore-errors (budden-tools::unregister-readtable :see-packages-test-readtable))
(ignore-errors (budden-tools::unregister-readtable :see-packages-test-readtable-a))
(defreadtable :see-packages-test-readtable (:merge))
(see-packages-on :see-packages-test-readtable)
(defreadtable :see-packages-test-readtable-a (:merge))
(see-packages-on :see-packages-test-readtable-a)
(setf (budden-tools::readtable-case-advanced (find-readtable :see-packages-test-readtable-a)) :ignore-case-if-uniform)

(defmacro with-good-readtable (&body body)
  `(let1 *readtable* (budden-tools::packages-seen-p :see-packages-test-readtable)
     ,@body))

(defmacro with-my-readtable (&body body)
  `(let1 *readtable* (find-readtable :see-packages-test-readtable)
     ,@body))

(defmacro with-my-readtable-a (&body body)
  `(let1 *readtable* (find-readtable :see-packages-test-readtable-a)
     ,@body))


(iter (:for pname in '(:tst :p1 :p2))
  (ignore-errors (delete-package pname)))

(defpackage :tst (:use :cl))
(defpackage :tst2 (:use))
(intern "S3" :tst)
(intern "S3" :tst2)
(defpackage :p1 (:export :sym :s1))
(defpackage :p2 (:export :sym :s2 :s3))


(defmacro def-rd-test (name rd &key (test ''equalp))
  `(deftest ,name 
            (with-good-readtable (read-from-string ,rd))
            (with-my-readtable (read-from-string ,rd))
            :test ,test
            ))

(defmacro def-rd-eval-test (name rd &key (test ''equalp))
  `(deftest ,name 
            (eval (with-good-readtable (read-from-string ,rd)))
            (eval (with-my-readtable (read-from-string ,rd)))
            :test ,test
            ))


(defmacro def-rd-ignore-error-test (name rd &key (test ''equalp))
  `(deftest ,name 
            (nth-value 0 (ignore-errors (with-good-readtable (read-from-string ,rd))))
            (nth-value 0 (ignore-errors (with-my-readtable (read-from-string ,rd))))
            :test ,test
            ))

(def-rd-test cons.1 "(aa . bb)")
(def-rd-test list.1 "(aa bb)")
(def-rd-test symbol.1 "aa")
(def-rd-test symbol.2 "t")
(def-rd-test symbol.3 "\\t")

(deftest symbol.3.5
                         (let1 *package* bu::*keyword-package* 
                           (with-good-readtable
                             (read-from-string "\\`")))
                         (let1 *package* bu::*keyword-package* 
                           (with-my-readtable
                             (read-from-string "\\`"))))

(def-rd-test symbol.3.5 "\\'") 

(def-rd-test symbol.4 "|I love Russia|")
(def-rd-test symbol.5 "asdf::defsystem")
(def-rd-test symbol.5.5 "asdf::a-in-the-start-of-the-symbol")
(def-rd-test symbol.6 "D\\'Artanian")
(def-rd-test symbol.7 ":a-keyword")
;(def-rd-ignore-error-test symbol.8 "#:foo")

(defun compare-uninterned-symbols (x y)
  (flet ((tst (z) 
           (and (symbolp z)
                (null (symbol-package z)))))
    (and (tst x)
         (tst y)
         (not (eq x y))
         (string= x y))))

(def-rd-test symbol.8 "#:foo" :test 'compare-uninterned-symbols)

(def-rd-test symbol.9 "#:|foo|" :test 'compare-uninterned-symbols)

(def-rd-test symbol.9a "cl-user::|foo|")

(def-rd-test list.3 "`a")
(def-rd-eval-test list.4 "`(a ,'b)")
(def-rd-eval-test list.5 "(let1 bb (list 'x) (list `(a ,bb) `(a ,@bb) `(a ,@bb ,bb)))")
(def-rd-eval-test list.6 "(let1 bb (list 'x) (list `(,bb) `(,@bb) `(,.bb)))")

(def-rd-test vector.1 "#(1 |2| \\3 \\4a |hAllo|)")
(def-rd-test character.1 "#\\Newline")
#+lispworks (def-rd-ignore-error-test character.2 "#\\meta-\\)") 
#+lispworks (def-rd-test character.2a "#\\Meta-\\\\")

#+ccl (def-rd-test character.3 
          "#\\Arabic_Letter_Beh_With_Three_Dots_Pointing_Upwards_Below_And_Two_Dots_Above")
(def-rd-test character.4 "#\\\\")

(def-rd-test spaces.1 "( a b #( b v e ) )")
(def-rd-eval-test spaces.2 "(let1 b 
\( list ( quote x ) ) 
  \( list `(,b ) `( ,@b ) `( ,.b )))")

(def-rd-test conditional.1 "(#-(or common-lisp) t)")
(def-rd-test conditional.2 "(#+foo asdf::oos #+(or common-lisp) asdf::oos)")

(def-rd-test intern.1 "(def-rd-eval-test cl-user::let1)") ; intern of existing symbol
(def-rd-ignore-error-test intern.2 "cl-user:let1") ; non-external-symbol
(def-rd-ignore-error-test intern.2 "ducker:duck")  ; non-existing-package


(deftest package-prefix.1
                         (with-my-readtable (read-from-string "keyword::(a b c)"))
                         '(:a :b :c))

(deftest package-prefix.2
                         (with-my-readtable (read-from-string "keyword::(a _:let c)"))
                         '(:a let :c))

(defpackage :tst2 (:use))

(defun parser1 (stream string package)
  (declare (ignore stream package))
  (if (equal string "FOO")
      (values :|We-got-FOO| t)
    (values :|we-got-not-a-FOO| t)))

(setf (get-custom-token-parsers-for-package :tst2) `(,#'parser1))

(deftest custom-parsers-and-preserve-case.1
                         (let1 *package* (find-package :tst2) 
                           (with-my-readtable (read-from-string "(foo bar)")))
                         '(:|We-got-FOO| :|we-got-not-a-FOO|))

(deftest case.1
         (with-my-readtable-a (symbol-name (read-from-string ":preserve")))
         "PRESERVE"
         :test 'string=)


(defun open-bracket-macro-char (stream macro-char)
  (declare (ignore macro-char))
  (let ((range (read-delimited-list #\> stream t)))
    (loop for x in range 
          :collect x 
          :collect '*-*)
    ))

(setf (gethash (find-package :tst) org.tfeb.hax.hierarchical-packages:*per-package-alias-table*)
      `(("HP" . "ORG.TFEB.HAX.HIERARCHICAL-PACKAGES")))

(deftest
 per-package-aliases.1
 (with-my-readtable (let1 *package* (find-package :tst) (read-from-string "hp::foo")))
 'ORG.TFEB.HAX.HIERARCHICAL-PACKAGES::FOO)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; minimal portable backquote test. Place it to the appropriate place!
;(deftest symbolic-bq 
;         (with-my-readtable (eval (read-from-string "\\`'(foo \\,(+ 4 8))")))
;         `'(foo ,(+ 4 8)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; see-packages tets

 
#+really-see-packages 
(with-my-readtable ; et ((*readtable* *my-readtable*))
  (iter 
    (:with budden-tools::*print-normalize-seen-symbols* = nil)
    (:with budden-tools::*intern-to-qualified-package-silently* = nil)
    (:for x :in 
         '(((in-package :tst))
           (*package*)
           ((see-packages))
           ;"Now should err as there is no symbol p1::s2" 
           ((multiple-value-list
            (ignore-errors
              (read-from-string "p1::s2")))
            )
           ((see-packages :p1 :p2))
           ((read-from-string "((s1 . s2) (p1::s1 p2:s2))")
            "((S1 . S2) (S1 S2))")
           ;"Now should err as sym is ambigious"
           ((multiple-value-list
            (ignore-errors
              (read-from-string "sym"))))
           ((read-from-string "(p1:sym p2:sym)")
            "(P1:SYM P2:SYM)")
           ;"Now should err as s3 is ambigious"
           ((multiple-value-list
            (ignore-errors
              (read-from-string "s3"))) :error)
           ((read-from-string "tst::s3")
            "TST::S3"
            )

           ((setf budden-tools::*print-normalize-seen-symbols* t))
           ((read-from-string "p2:s2")
            "P2:S2")
           ((setf budden-tools::*print-normalize-seen-symbols* nil))


           ((read-from-string "(s1 s2 tst::s3 p2:s3)")
            "(S1 S2 TST::S3 P2:S3)"
            )
           ))
    (if (stringp x)
        (format t "~%~A" x)
      (let* ((code (car x))
             (must-be (cadr x))
             (test-result (eval code))
             (test-text (format nil "~%~S => ~S~%" x test-result)))
        (cond
         ((stringp must-be)
          (assert (equalp (prin1-to-string test-result) must-be) () "~A" test-text))
         ((eq must-be :error)
          (assert (typep test-result '(cons null (cons error null))) () "~A" test-text)))
        ))))


; (import 'org.tfeb.hax.hierarchical-packages::hp-find-package)
(defpackage pp1)
(defpackage pp1.pp2)

(let1 *package* (find-package :pp1)
  (print `(1 ,(hp-find-package :.pp2))))

