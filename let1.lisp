(in-package :cl-user)

;; For me (budden), let1 is in cl package. For the rest, it is in 
;; budden-tools package

(defmacro portably-without-package-locks (&body body)
`(#+sbcl sb-ext:without-package-locks
#+allegro excl::without-package-locks
#+cmu ext:without-package-locks
#+lispworks let 
#+lispworks 
((lw:*handle-warn-on-redefinition* :warn)
 ; (dspec:*redefinition-action* :warn)
 (hcl:*packages-for-warn-on-redefinition* nil))
#+clisp ext:without-package-lock #+clisp ()
#+ccl let
#+ccl ((ccl:*warn-if-redefine-kernel* nil)) 
#-(or allegro lispworks sbcl clisp cmu ccl) 
progn
,@body))

;; reusing cl symbols to avoid interning to cl-user
(cl-user::portably-without-package-locks
  #+budden (intern (symbol-name '#:let1) :cl)
  #+budden (export 'cl::let1 :cl)
  (defmacro 
      #+budden cl::let1 
    #-budden budden-tools::let1
    (variable + &body progn) 
    "Shortcut for (let ((a b)) . c) or (destructuring-bind a b . c)"
    (if (atom variable)
        `(let ((,variable ,+)) ,@progn)
      `(destructuring-bind ,variable ,+ ,@progn)))

  )

