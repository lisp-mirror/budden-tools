;;; -*- Encoding: utf-8; -*-

;;; Lispworks, Restarting functions with &key arguments
;; budden-tools for Lispworks by budden73, 2013-2014
;; to restart function, use :restart-with-keywords-risky debugger command
;; note that current values of arguments are used, not initial ones.
;; So, if default values are changed and function is recompiled, 
;; or if arugments are altered inside a function, restart will be not
;; equivalent to initial call. 

(def-merge-packages::! :res-fun-with-keyargs-in-a-debugger
 (:use :cl :hcl :lispworks)
 (:import-from :alexandria.0.dev
  alexandria.0.dev:parse-ordinary-lambda-list)
 (:always t))


(in-package :res-fun-with-keyargs-in-a-debugger)

#| Useful code for debugger study, don't delete it

(defvar *restart-risky* nil
  "If it is t, we try to restart frame even if its lambda-list contains &key arguments")

(defadvice (dbg::get-frame-arguments my-hack :around)
  (&rest all-args ; hmm, unable to pass it to call-next-advice? 
              )
  "This function is called when we try to restart a frame"
  (destructuring-bind (dbg::frame function DBG::same-arguments-p &rest keys)
      all-args
    (let (risky-args-with-keys)
      (cond
       ((null *restart-risky*)
        (apply #'call-next-advice dbg::frame function DBG::same-arguments-p keys))
       ((null DBG::same-arguments-p) ; не знаем, что это и не будем рисковать.
        (apply #'call-next-advice dbg::frame function DBG::same-arguments-p keys)
        ) 
       ((setf risky-args-with-keys  
              (ignore-errors (process-risky-lambda-list function)))
      ; это ключи - попытаем удачи
        (dbg::dbg-eval `(list ,@risky-args-with-keys)))
       (t
        (apply #'call-next-advice dbg::frame function DBG::same-arguments-p keys)
        )))))
  
(defun convert-keyword-def-to-call (something keyword-parameters)
  "Returns a list to add to parameter list"
  (when (eq something '&key)
    (return-from convert-keyword-def-to-call nil))
  (dolist (k keyword-parameters)
    (destructuring-bind ((keyword-name name) init supplied-p) k
      (declare (ignore init supplied-p))
      (when (eq name something)
        (return-from convert-keyword-def-to-call
          `(,keyword-name ,name)))))
  (list something))


(defun ignored (&rest args)
  "useful utility function"
  (declare (ignore args)))

(defun process-risky-lambda-list (function)
  "Prepare a form for dbg-eval to calculate arguments for
  restarting function with &key arguments"
  (let (lambda-list
        required-parameters
        optional-parameters
        rest-parameter-name
        keyword-parameters
        allow-other-keys-present
        aux-parameters-specification
        result
        )
    (setf lambda-list (function-lambda-list function))
    (dolist (wrong-keyword '(&optional &rest #|&key|# &aux &body &whole &allow-other-keys &environment))
      (when (member wrong-keyword lambda-list)
        (warn "unable to process risky lambda list ~S" lambda-list)
        (return-from process-risky-lambda-list nil)))
    (multiple-value-setq
        (required-parameters
         optional-parameters
         rest-parameter-name
         keyword-parameters
         allow-other-keys-present
         aux-parameters-specification)
        (parse-ordinary-lambda-list lambda-list))

    (ignored required-parameters)
    
    (when (or optional-parameters rest-parameter-name
              allow-other-keys-present
              aux-parameters-specification)
      (warn "unable to process complex lambda list ~S" lambda-list)
      (return-from process-risky-lambda-list nil))
    (dolist (something lambda-list)
      (setf result (append result (convert-keyword-def-to-call something keyword-parameters))))
    result
    ))
|#

  
(defun risky-restart-frame-command (ignore &optional (samep t))
  "Wrapper around dbg::restart-frame-command to allow restarting functions with &key"
  (let ((*restart-risky* t))
    (DBG::restart-frame-command ignore samep)))


;;; alter debugger 
(pushnew '(:restart-with-keywords-risky
           risky-restart-frame-command
           "Restart frame even if function lambda list contains keywords - experimental")
         DBG::*default-debugger-commands*
         :key 'car)
                                     

#| for test
(defun inner (a b)
  (break)
  (cons a b))

(defun outer (a &key b)
  (break)
  (cons a b)
  ;(inner a b)
  )

(defun o-outer ()
  (break)
  (outer 1 :b 2))
|#




; misc useful functions
; (delete-advice 'DBG::frame-can-be-restarted-p 'always-t)
; (defun DBG::frame-can-be-restarted-p (&rest ignore) t); 
; controls if the frame is restartable from GUI debugger
;(defadvice (DBG::frame-can-be-restarted-p always-t :around) (&rest ignore)
;  (declare (ignore ignore))
; dbg::dbg-eval `(list ,@(function-lambda-list 'inner))    
;   t)

