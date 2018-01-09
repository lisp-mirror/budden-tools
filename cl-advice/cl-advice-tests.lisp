;; -*- coding: utf-8; -*-
;; This file must be loaded by load-cl-advice.lisp

(in-package :cl-advice)

'#.(mapcar 'unintern '(cl-advice-test-fn ff1 ff2))

(defun cl-advice-test-fn (x) x)

(defun cl-advice-test-fn-d (fn arg)
  ;(print `(fn-in-d ,fn))
  (+ (funcall fn arg) arg))

(install-advice
 'cl-advice-test-fn
 #'cl-advice-test-fn-d
 :advice-name 'd)

(assert (= (cl-advice-test-fn 1) 2))

(defun cl-advice-test-fn-e (fn arg)
  ;(print `(fn-in-e ,fn))
  (* (funcall fn arg) 5))

;; do the same twice. encapsulate would encapsulate the same function twice
;; We are a bit more clever and replace pre-existing encapsulation

(define-advice cl-advice-test-fn 'cl-advice-test-fn-e :advice-name e)

(assert (= (cl-advice-test-fn 1) 10))

(install-advice 'cl-advice-test-fn 'cl-advice-test-fn-e :advice-name 'e)

(assert (= (cl-advice-test-fn 1) 10))

;;; also check if install-advice accepts #'
(install-advice 'cl-advice-test-fn #'cl-advice-test-fn-e :advice-name 'f)

(assert (= (cl-advice-test-fn 1) 50))


(uninstall-advice 'cl-advice-test-fn :advice-name 'e)
(uninstall-advice 'cl-advice-test-fn :advice-name 'f)

(assert (= (cl-advice-test-fn 1) 2))

(install-advice
 'cl-advice-test-fn
 'cl-advice-test-fn-d :advice-name 'k)

(assert (= (cl-advice-test-fn 1) 3))


#+example
(progn ; evaluate it, not compile
  (defmacro original (symbol) `',symbol)
  (defmacro decorate-original (symbol) `(list :decorated (,(decorate-macro-get-undecorated-invoker 'original) ,symbol)))
  (decorate-macro 'original 'decorate-original)
  (print (original 'asdf))
  (undecorate-macro 'original))

#+SBCL (defun ff1 ())
#+SBCL (defun b-ff1 (fn) (declare (ignore fn)))
#+SBCL (install-advice 'ff1 #'b-ff1 :advice-name 'b)
#+SBCL (define-advice ff1 #'b-ff1 :advice-name b)
#+SBCL (defun e-ff1 (fn) (declare (ignore fn)))
#+SBCL (define-advice ff1 'e-ff1 :advice-name e)
#+SBCL (uninstall-advice 'ff1)
#+SBCL (define-advice ff1 'e-ff1 :advice-name e)
#+SBCL (install-advice 'ff1 'e-ff1 :advice-name 'e)
#+SBCL (install-advice 'ff1 (lambda (fn) (declare (ignore fn )) (print "λ")) :advice-name 'λ)
#+SBCL (install-advice 'ff1 (lambda (fn) (declare (ignore fn)) (print "λ")) :advice-name 'λ)


#+SBCL (assert (= 4
           (length
            (remove-duplicates
             (mapcar
              'sb-introspect::definition-source-character-offset
              (union
               (SB-INTROSPECT::ENCAPSULATION-DEFINITION-SOURCES 'ff1)
               (SB-INTROSPECT:FIND-DEFINITION-SOURCES-BY-NAME 'ff1 :function))
               ))))
        () "There should be 4 known definition sources for ff1")

