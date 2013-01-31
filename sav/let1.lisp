;;; -*- Encoding: utf-8; -*-
(in-package :cl-user)

(defmacro portably-without-package-locks (&body body)
`(#+sbcl sb-ext:without-package-locks
#+allegro excl::without-package-locks
#+cmu ext:without-package-locks
#+lispworks let 
#+lispworks 
((lw:*handle-warn-on-redefinition* :warn)
 (*packages-for-warn-on-redefinition* nil))
#+clisp ext:without-package-lock #+clisp ()
#+ccl let
#+ccl ((ccl:*warn-if-redefine-kernel* nil)) 
#-(or allegro lispworks sbcl clisp cmu ccl) 
progn
,@body))

;(cl-user::portably-without-package-locks
; (intern (symbol-name '#:let1))
; (export 'let1 :cl))

;; reusing cl symbols to avoid interning to cl-user
(cl-user::portably-without-package-locks
  (intern (symbol-name '#:let1) :cl)
  (export 'cl::let1 :cl)
  (defmacro cl::let1 (variable + &body progn) 
    "Shortcut for (let ((a b)) . c) or (destructuring-bind a b . c)"
    (if (atom variable)
        `(let ((,variable ,+)) ,@progn)
      `(destructuring-bind ,variable ,+ ,@progn)))

  )

