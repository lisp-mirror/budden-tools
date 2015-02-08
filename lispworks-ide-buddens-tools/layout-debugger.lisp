;; Change the layout of the lispworks debugger so that 
;; condition pane would be larger

(in-package :editor-budden-tools)

;(defvar *the-debugger*)
;(defvar *the-pane*)
;(defvar *the-data*)

(defvar *debuggers-already-changed* 
  (make-hash-table :weak-kind :key)
  "Key is a debugger")

(defmethod capi::capi-activate-callback :before ((x lispworks-tools:debugger) activatep) 
  (when (and activatep
             (not (gethash x *debuggers-already-changed*)))
    (layout-debugger x)
    (setf (gethash x *debuggers-already-changed*) t)
    ))

(defun do-layout-debugger (debugger-layout)
  (symbol-macroletf ((ratios (capi::layout-y-ratios debugger-layout)))
    (setf ratios
          (case (length (slot-value debugger-layout 'capi::panes))
            (3 '(2 nil 1 nil))
            (5 '(2 nil 1 nil 2))))))
  

(defun layout-debugger (x)
  (let* ((debugger-layout (slot-value x 'lispworks-tools::debugger-layout))
         (condition-pane (slot-value x 'lispworks-tools::condition-pane)))
    (capi:apply-in-pane-process condition-pane #'do-layout-debugger debugger-layout)))


#|(defmethod initialize-instance :after ((x lispworks-tools:debugger) &rest initargs)
  (let* ((condition-pane (slot-value x 'lispworks-tools::condition-pane))
         (backtrace-pane (slot-value x 'lispworks-tools::backtrace-list-pane))
         (parent (slot-value condition-pane 'capi::parent)))
    (setf *the-debugger* x)
    (setf *the-pane* condition-pane)
    (setf *the-data* initargs)
    (layout-debugger)
    #|(capi:apply-in-pane-process condition-pane
                                'capi:set-hint-table condition-pane
                                '(:visible-min-height (:character 20)))
    (capi:apply-in-pane-process backtrace-pane
                                'capi:set-hint-table backtrace-pane
                                '(:visible-max-height (:character 5)))|#
    ;(break)
    ;(y-or-n-p (string (class-name (class-of parent))))
    ;(setf (capi::layout-x-ratios (slot-value condition-pane 'capi::parent)) '(3 nil 1 nil 3))
  ))|#

