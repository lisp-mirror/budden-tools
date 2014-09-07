;;; -*- lisp -*- system definition

(in-package #:asdf)
;;; I (Joerg Hoehle) totally object ASDF's cluttering my package list
;;; with dozens of tiny definition packages and even more so the
;;; typically empty ASDFNNNN packages. Please give me some package
;;; like ASDF-SYSTEMS or -USER to throw in such trivial definitions.
  
(defsystem :iterate-keywords
    :description "Jonathan Amsterdam's iterator/gatherer/accumulator facility patched to use keywords instead of ordinary symbols"
    :version "1.4.3.1"
    :components ((:file "package")
		 (:file "iterate" :depends-on ("package"))
                 )
    )

(defsystem :iterate-keywords-pg
  :depends-on (:iterate-keywords :pg)		; Eric Marsden's pg.lisp
  :components ((:file "iterate-pg")))

(defsystem :iterate-keywords-tests
  :depends-on (:iterate-keywords #+sbcl sb-rt #-sbcl :rt)
  :serial t
  :components ((:file "iterate-ktest")))

(defmethod asdf:perform ((op asdf:test-op) (c (eql (find-system ':iterate-keywords))))
  (asdf:operate 'asdf:load-op ':iterate-keywords-tests)
  (asdf:operate 'asdf:test-op ':iterate-keywords-tests)
  )

(defmethod asdf:perform ((op asdf:test-op) (c (eql (find-system ':iterate-keywords-tests))))
  (funcall (intern "DO-TESTS" (find-package #+sbcl "SB-RT"
					    #-sbcl "REGRESSION-TEST"))))

(defmethod asdf:perform :after ((o asdf:load-op) (c (eql (find-system ':iterate-keywords))))
  (provide '#:iterate-keywords))

