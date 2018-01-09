;;; -*- Encoding: utf-8; -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
;non-toplevel form
(defpackage :test-sbcl-reader-budden-tools-lispworks 
  (:use :cl :budden-tools :named-readtables)
  (:shadowing-import-from :iterate-keywords #:ITER)
  ))

(in-package :test-sbcl-reader-budden-tools-lispworks)

(ignore-errors (budden-tools::unregister-readtable :tsrbtl-rt))
(ignore-errors (budden-tools::unregister-readtable :tsrbtl-rt-a))

(with-output-to-string (*error-output*)                                
  (budden-tools::defreadtable :tsrbtl-rt (:merge :standard))
  (budden-tools::enable-buddens-readtable-extensions :tsrbtl-rt)
  (budden-tools::defreadtable :tsrbtl-rt-a (:merge :standard))
  (budden-tools::enable-buddens-readtable-extensions :tsrbtl-rt-a)
  )

(setf (budden-tools::readtable-case-advanced (find-readtable :tsrbtl-rt-a)) :upcase-if-uniform)

(defmacro with-good-readtable (&body body)
  `(let1 *readtable* (copy-readtable nil)
     ,@body))

(defmacro with-my-readtable (&body body)
  `(let1 *readtable* (find-readtable :tsrbtl-rt)
     ,@body))

(defmacro with-my-readtable-a (&body body)
  `(let1 *readtable* (find-readtable :tsrbtl-rt-a)
     ,@body))


(iter (:for pname in '(:tst :p1 :p2))
  (ignore-errors (delete-package pname)))

(defpackage :tst (:use :cl))
                     
(with-output-to-string (*error-output*)                                
  (defpackage :tst2 (:use)))

(intern "S3" :tst)
(intern "S3" :tst2)
(defpackage :p1 (:export :sym :s1))
(defpackage :p2 (:export :sym :s2 :s3))

(with-output-to-string (*error-output*) ; suppress warnings
  (eval 
   '(def-merge-packages::! :p1+p2 (:use :p1 :p2)) ; sym is forbidden
   ))


(with-output-to-string (*error-output*) ; suppress warnings
  (eval 
   '(def-merge-packages::! :p-test-local-nicknames (:local-nicknames :r :cl-user))))


(def-trivial-test::! local-nicknames.1
                     (with-my-readtable (let ((*package* (find-package :p-test-local-nicknames))) (read-from-string "r::cons")))
                     'cons)

(def-trivial-test::! altered-find-package.1
                     (let ((*package* (find-package :p-test-local-nicknames)))
                       (find-package :r))
                     (find-package :cl-user))
                       


(defmacro def-rd-test (name rd &key (test ''equalp))
  `(def-trivial-test::! ,name 
            (with-good-readtable (read-from-string ,rd))
            (with-my-readtable (read-from-string ,rd))
            :test ,test
            ))

(defmacro def-rd-eval-test (name rd &key (test ''equalp))
  `(def-trivial-test::! ,name 
            (eval (with-good-readtable (read-from-string ,rd)))
            (eval (with-my-readtable (read-from-string ,rd)))
            :test ,test
            ))

(defmacro def-rd-ignore-error-test (name rd &key (test ''equalp))
  `(def-trivial-test::! ,name 
            (nth-value 0 (ignore-errors (with-good-readtable (read-from-string ,rd))))
            (nth-value 0 (ignore-errors (with-my-readtable (read-from-string ,rd))))
            :test ,test
            ))

(def-rd-test cons.1 "(aa . bb)")
(def-rd-test list.1 "(aa bb)")
(def-rd-test symbol.1 "aa")
(def-rd-test symbol.2 "t")
(def-rd-test symbol.3 "\\t")

(def-trivial-test::! symbol.3.5
                         (let1 *package* budden-tools::*keyword-package* 
                           (with-good-readtable
                             (read-from-string "\\`")))
                         (let1 *package* budden-tools::*keyword-package* 
                           (with-my-readtable
                             (read-from-string "\\`"))))

(def-rd-test symbol.3.6 "\\'") 

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

#+lispworks6 (def-rd-test symbol.9a "lispworks:quit")

(def-rd-test string.1 "\"\"")
(def-rd-test string.2 "\"\\\"\"")

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

(def-rd-test intern.1 "(def-rd-eval-test cl-user::cons)") ; intern of existing symbol
(def-rd-ignore-error-test intern.2 "cl-user:&key") ; non-external-symbol
(def-rd-ignore-error-test intern.3 "ducker:duck")  ; non-existing-package

(def-trivial-test::! clash.4 ; sym is forbidden due to clash so error must occur
                     (null
                      (nth-value 1 
                                 (ignore-errors
                                   (with-my-readtable (read-from-string "p1+p2::sym")))))
                     nil)
                      

#|2012-12-19 (def-trivial-test::! package-prefix.1
                         (with-my-readtable (read-from-string "keyword::(a b c)"))
                         '(:a :b :c))|#

#|2012-12-19 (def-trivial-test::! package-prefix.2
                         (with-my-readtable (read-from-string "keyword::(a _:let c)"))
                         '(:a let :c))|#

(with-output-to-string (*error-output*)
  (eval
   '(def-merge-packages::! :tst2 
                            (:use)
                            (:custom-token-parsers parser1)
                            )))

(defun parser1 (stream string package)
  (declare (ignore stream package))
  (if (equal string "FOO")
      (values :|We-got-FOO| t)
    (values :|we-got-not-a-FOO| t)))

(def-trivial-test::! custom-parsers-and-preserve-case.1
                         (let1 *package* (find-package :tst2) 
                           (with-my-readtable (read-from-string "(foo bar)")))
                         '(:|We-got-FOO| :|we-got-not-a-FOO|))

(def-trivial-test::! case.1
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

#|commented out 2012-09-03 as we're removing hp dependency
(setf (gethash (find-package :tst) org.tfeb.hax.hierarchical-packages:*per-package-alias-table*)
      `(("HP" . "ORG.TFEB.HAX.HIERARCHICAL-PACKAGES")))

 (def-trivial-test::!
 per-package-aliases.1
 (with-my-readtable (let1 *package* (find-package :tst) (read-from-string "hp::foo")))
 'ORG.TFEB.HAX.HIERARCHICAL-PACKAGES::FOO) |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; minimal portable backquote test. Place it to the appropriate place!
;(def-trivial-test::! symbolic-bq 
;         (with-my-readtable (eval (read-from-string "\\`'(foo \\,(+ 4 8))")))
;         `'(foo ,(+ 4 8)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; see-packages tets

 



; (import 'org.tfeb.hax.hierarchical-packages::hp-find-package)
#|commented out 2012-09-03 as we're removing hp dependency
 (defpackage pp1)
 (defpackage pp1.pp2)

 (let1 *package* (find-package :pp1)
   (print `(1 ,(hp-find-package :.pp2))))
|#

