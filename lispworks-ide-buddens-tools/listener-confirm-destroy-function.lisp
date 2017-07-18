;; -*- coding: utf-8 ; -*-


(asdf::of-system :editor-budden-tools)

(in-package :editor-budden-tools)



(defun listener-confirm-destroy-function (listener)
  ;(declare (ignorable listener))
  (let (ЭтоСерверПриложений)
    (cond
     ((and (find-package :appserver)
           (budden-tools::budden-tools-find-symbol "*good-trace-output*"
                                                   (find-package :appserver)))
      (let* (gto buffer ep buffer-from-listener)
        (setf gto (eval (read-from-string "APPSERVER::*good-trace-output*")))
        (setf buffer (editor::point-buffer (editor::rubber-stream-output-point gto)))
        (when buffer 
          (setf ep (slot-value listener 'capi:editor-pane))
          (setf buffer-from-listener (slot-value ep 'capi::buffer))
          (when (eq buffer-from-listener buffer)
            (setf ЭтоСерверПриложений t))))))
    (y-or-n-p
     (if ЭтоСерверПриложений
         "В данном окне запущен сервер приложений. Если его закрыть, то приложения перестанут работать! Закрыть листенер?"
       "Закрыть листенер?"
       )
     )))

(defun set-confirm-destroy-function-for-interface (interface)
  (setf (slot-value interface 'CAPI::confirm-destroy-function)
        #'listener-confirm-destroy-function))

(defmethod shared-initialize :after ((interface lispworks-tools:listener) slot-names &rest initargs &key &allow-other-keys)
  (set-confirm-destroy-function-for-interface interface)
  )

(defun set-confirm-destroy-function-for-existing-interfaces ()
  (dolist (interface (capi:screen-interfaces (capi:convert-to-screen)))
    (when (typep interface 'LISPWORKS-TOOLS:listener)
      (set-confirm-destroy-function-for-interface interface))))

(set-confirm-destroy-function-for-existing-interfaces)


