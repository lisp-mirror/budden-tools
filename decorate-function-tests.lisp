;;; -*- coding: utf-8; System :decorate-function;  -*-

(in-package :decorate-function)

#.(unintern 'decorate-function-test-fn)

(defun decorate-function-test-fn (x) x)

(defun decorate-function-test-fn-d (fn arg)
  (+ (funcall fn arg) arg))

(decorate-function
 'decorate-function-test-fn
 #'decorate-function-test-fn-d
 :advice-name 'd)

(defun decorate-function-test-fn-e (fn arg)
  (* (funcall fn arg) 5))

;; do the same twice. encapsulate would encapsulate the same function twice
;; We are a bit more clever and replace pre-existing encapsulation

(decorate-function 'decorate-function-test-fn 'decorate-function-test-fn-e :advice-name 'e)
(decorate-function 'decorate-function-test-fn 'decorate-function-test-fn-e :advice-name 'e)

(assert (= (decorate-function-test-fn 1) 10))

(undecorate-function
 'decorate-function-test-fn
  :advice-name 'e)

(assert (= (decorate-function-test-fn 1) 2))

(decorate-function
 'decorate-function-test-fn
 'decorate-function-test-fn-d :advice-name 'k)

(assert (= (decorate-function-test-fn 1) 3))


#+example
(progn ; evaluate it, not compile
  (defmacro original (symbol) `',symbol)
  (defmacro decorate-original (symbol) `(list :decorated (,(decorate-macro-get-undecorated-invoker 'original) ,symbol)))
  (decorate-macro 'original 'decorate-original)
  (print (original 'asdf))
  (undecorate-macro 'original)
  )


(defun ff1 ())
(defun b-ff1 (fn))
(decorate-function 'ff1 #'b-ff1 :advice-name 'b)
(decorate-function 'ff1 #'b-ff1 :advice-name 'b)
(defun e-ff1 (fn))
(decorate-function 'ff1 'e-ff1 :advice-name 'e)
(decorate-function 'ff1 'e-ff1 :advice-name 'e)
(decorate-function 'ff1 (lambda (fn) (declare (ignore fn)) (print "λ")) :advice-name 'λ)
(decorate-function 'ff1 (lambda (fn) (declare (ignore fn)) (print "λ")) :advice-name 'λ)

(assert (= 3 
           (length
            (remove-duplicates
             (mapcar
              'sb-introspect::definition-source-character-offset
              (SB-INTROSPECT::ENCAPSULATION-DEFINITION-SOURCES 'ff1)))))
        () "There should be 3 known definition sources for ff1")
