;;; -*- coding: utf-8; system :budden-tools; -*-

(named-readtables:in-readtable nil)

(def-merge-packages::! :beyond-bordeaux-threads
                       (:use :cl :budden-tools :bordeaux-threads)
  (:always t)
  #+(or CCL SBCL)
  (:export "beyond-bordeaux-threads:holding-lock-p"))

(in-package :beyond-bordeaux-threads)

(defun holding-lock-p (lock)
  "Return t if lock is held by current thread"
  #+CCL
  (eq (ccl::%%lock-owner lock) (bt:current-thread))
  #+SBCL
  (sb-thread:holding-mutex-p lock)
  #-(or CCL SBCL)
  (let "holding-lock-p is not defined for this lisp") ; I dislike things like #.(error)
  )

(defun holding-lock-quick-test ()
  (let ((lock (bt:make-lock "holding-lock-quick-test-lock")))
    (assert (not (holding-lock-p lock)))
    (with-lock-held (lock)
      (assert (holding-lock-p lock)))))

(holding-lock-quick-test)

