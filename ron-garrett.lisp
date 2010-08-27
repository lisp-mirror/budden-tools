(let ( (r (copy-readtable nil)) )
  (defun read-symbol (stream)
    (let ( (*readtable* r) )
      (read-preserving-whitespace stream))))

(defun symbol-reader-macro-reader (stream char)
  (unread-char char stream)
  (let* ((s (read-symbol stream))
         (f (get s 'symbol-reader-macro)))
      (if f (funcall f stream s) s)))

(map nil (lambda (c)
           (set-macro-character c 'symbol-reader-macro-reader t))
     "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_")

(defun set-macro-symbol (symbol readfn)
  (setf (get symbol 'symbol-reader-macro) readfn)
  t)

; Example:

(defun make-special-form-reader (nargs)
  (lambda (stream symbol)
    (cons symbol (loop for i from 1 upto nargs collect
                   (if (listen stream) (read stream))))))

(set-macro-symbol 'assign
  (lambda (stream symbol) `(setf ,(read stream) ,(read stream))))

(set-macro-symbol 'while
  (lambda (stream symbol) `(do () ((not ,(read stream))) ,(read
stream))))

(set-macro-symbol 'display
  (lambda (stream symbol) `(print ,(read stream)))) 
