(asdf::of-system :editor-budden-tools)
(in-package #+budden :editor-budden-tools #-budden :editor)

; Show status bar of lispwork's 4.4.6 listener and fill it with whatever you want
; written by Denis Budyak, 2012
; Public Domain
; requires 
; http://code.google.com/p/def-symbol-readmacro/

; Put the code in #||# at early stage of your Lispworks boot (before any listener is created);
#| 
;-------------для статус бара в листенере ---------------------------------------
; это было для локальной подмны подсказки в листенере. но т.к. не смогли выцепить
; из листенера его пакет и нормально показать на activate, решили оставить как есть
#+nil (pushnew '(lispworks:*prompt* . lispworks:*prompt*) mp:*process-initial-bindings* :test 'equalp)

; как можно раньше, желательно - в init.lisp
(let ((LISPWORKS:*HANDLE-WARN-ON-REDEFINITION* nil))
  (defmethod initialize-instance :around ((i lispworks-tools:listener) &rest initargs) 
    (let* ((new-initargs (copy-list initargs))) 
      (setf (getf new-initargs :message-area) t)
      (apply #'call-next-method i new-initargs))))

;--------------------------------------------------------------------------------
|#
; End of the code to put to Lispworks boot process

(defun get-the-listener-interface ()
  "Get listener for current process"
  (getf (funcall 
         #-:LISPWORKS4.4 'mp::process-private-plist  ; for lispworks 6.*
         #+:LISPWORKS4.4 'mp:process-plist 
         mp:*current-process*)
        'capi::default-interface)
  )

; to be redefined later in sheet.lisp
(unless (fboundp 'buddens-listener-message)
; non-toplevel
(defun buddens-listener-message ()
    "Is called in the interace's process. Produce the string you want to put on status bar"
    "loading...") 

)


(defun send-with-result (process function)
  "From Lispworks docs"
  (let ((remote-result :none))
    (flet ((resultp ()
             (listp remote-result))
           (run-it ()
             (setq remote-result
                   (multiple-value-list (funcall function)))))
      (mp:process-send process (list #'run-it))
      (mp:process-wait "Waiting for result" #'resultp)
      (values-list remote-result))))

(defun listener-activate-callback (i activate)
  (when activate
    (capi:execute-with-interface 
     i
     (lambda () (setf (capi:titled-pane-message i)
                      (buddens-listener-message))
       ))))

(defun set-right-message-for-the-listener ()
  "Assigns a callback to listener's interface. Called way too frequently, sorry"
  #-(or :LISPWORKS4.4 :lispworks6) 
  #.(error "wrong cl version in set-message-for-the-listener, see process-private-plist, maybe you need redefine set-right-message-for-the-listener")
  (let ((i (get-the-listener-interface))
        )
    ; (set-the-editor-prompt) ; здесь предполагалась подмена подсказки, убрали, см. в начале файла
    (unless (and (slot-boundp i 'capi::activate-callback)
                 (slot-value i 'capi::activate-callback))
      (setf (slot-value i 'capi::activate-callback) 'listener-activate-callback))
    (capi:execute-with-interface 
     i
     (lambda () 
       (setf (capi:titled-pane-message i)
             (buddens-listener-message)))
     )))

; fixme replace package-prompt with system::current-prompt
(defun decorated-package-prompt (fn package)
  "New function to produce status bar string at the time when
listener prompt is produced"
  (ignore-errors (set-right-message-for-the-listener))
  (funcall fn package))

; install budden-tools from http://code.google.com/p/def-symbol-readmacro/
(budden-tools::decorate-function 'system::package-prompt #'decorated-package-prompt)

;(budden-tools::undecorate-function 'system::package-prompt)


#| Some comments for the future
вот сюда надо повеситься?

Call to (METHOD CAPI::UPDATE-REPRESENTATION :AFTER (CAPI:INTERFACE T)) (offset 482)
  CAPI::SELF : #<LISPWORKS-TOOLS:LISTENER "Listener 4" 2465AC04>
  CAPI::REP  : #<Representation WIN32:R-TOP-LEVEL-INTERFACE #<LISPWORKS-TOOLS:LISTENER "Listener 4" 2465AC04>>

или есть ещё process-sleep-function 

|#

