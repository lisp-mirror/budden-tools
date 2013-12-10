(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :protect-listener-text 
    (:use :cl :lispworks :editor)
    (:export #:*enable-protection*)
  ))

(in-package :protect-listener-text) 

(EDITOR:define-editor-variable protected-offset 0
   "Internal variable to keep track of last listener prompt. One can not modify prompt unless variable is bound to -1")

(defun real-point-offset-2 (point) 
  "Maybe this is stupid"
  (+ (editor::point-offset point)
     (slot-value (editor::point-bigline point) 'editor::start-char))
  )

#| for debug 
 (editor:defcommand "show real point offset" (p)
     (declare (ignore p))
     (editor::message "real-point-offset=~S,ediot-region-stream-last-read-offset=~S"
                      (real-point-offset-2 (EDITOR:current-point))
                      nil ;(EDITOR::editor-region-stream-last-read-offset )
                      )
  ) |#

(defun get-protected-offset (buffer)
  (or
   (EDITOR:variable-value-if-bound 'protected-offset :buffer buffer)
   0)) 

(defun set-protected-offset (buffer value)
  (setf (EDITOR:variable-value 'protected-offset :buffer buffer) value))


(defun get-last-prompt-offset (buffer)
  "Point is not used really"
  (editor:with-point ((last-prompt (editor::buffer-%end buffer))
                      (search-lim (editor::buffer-%end buffer)))
    (unless (character-offset search-lim (- editor::*maximum-prompt-search*))
      (setf search-lim (EDITOR::buffer-%start buffer)))
    (let ((len (catch-editor-error
                (editor::do-regexp-search
                 nil (variable-value 'prompt-regexp-string)
                 last-prompt nil search-lim))))
      (cond
       ((not len)  ; no prompt - let it be at the beginning
        0)
       (t
        (+ len (real-point-offset-2 last-prompt)))
       )
      )
    ))

(EDITOR:defcommand "show last prompt offset" (p)
     (declare (ignore p))
     (editor::message "last prompt offset is ~A"
                      (get-last-prompt-offset (current-buffer))))


(defun maybe-fix-protected-offset (point fix-even-if-at-the-end)
  "If current point is after protected-offset, fixes protection point and returns t. 
  Put even-at-the-end to t when checking protected-offset before deleting characters.
  Can slow down entry essentially as it is called at each insertion command, e.g. self insert"
  (let* (
         (buffer (EDITOR:point-buffer point))
         current-protected-offset
         point-offset
         prompt-offset
         )
    (unless (eq (slot-value buffer 'EDITOR::flag) :listener)
      (return-from maybe-fix-protected-offset nil))
    (when (and (point= point (EDITOR::buffer-%end buffer))
               (not fix-even-if-at-the-end))
      ; optimization: do not find a prompt when just typing 
      ; at the end of the listener stream
      (return-from maybe-fix-protected-offset nil))
    (setf current-protected-offset (get-protected-offset buffer))
    (when (= current-protected-offset -1)
      (return-from maybe-fix-protected-offset nil))
    (setf point-offset (real-point-offset-2 point))
    (when (>= point-offset current-protected-offset))
    (setf prompt-offset (get-last-prompt-offset buffer))
    (when (> prompt-offset current-protected-offset)
      (set-protected-offset buffer prompt-offset)
      t)
    ))

(defvar *enable-protection* t) 

(defun inner-check-editable-point (point buffer fix-even-if-at-the-end) 
  (let ((offset (real-point-offset-2 point)))
    (maybe-fix-protected-offset point fix-even-if-at-the-end)
    (let ((protected-offset (get-protected-offset buffer)))
    (cond
     ((= protected-offset -1)
      )
     ((< offset protected-offset)
      (EDITOR::editor-beep)
      (when *enable-protection*
        (EDITOR::signal-editing-in-a-read-only-window (EDITOR:point-buffer point))))
     ))))
        

(defun check-editable-point (point &optional fix-even-if-at-the-end (delta 0))
  "If point in listener is at last prompt of before, disable editing"
  (when point
    (let* ((buffer (EDITOR:point-buffer point)))
      (when (eq (slot-value buffer 'EDITOR::flag) :listener)
        (cond
         ((= delta 0)
          (inner-check-editable-point point buffer fix-even-if-at-the-end))
         (t
          (with-point ((p point))
            (character-offset p delta)
            (inner-check-editable-point p buffer fix-even-if-at-the-end)))
         )))))


(defadvice (editor::insert-string disallow-if-protected :before)
    (point string &optional (start 0) (end (length string)))
  (declare (ignore start end))
  (check-editable-point point nil 0)
  )

(defadvice (editor::in-insert-string disallow-if-protected :before)
    (point string start end before-point properties)
  (declare (ignore string start end properties))
  (check-editable-point point nil 0)
  (check-editable-point before-point nil 0)
  )

(defadvice (EDITOR:insert-character disallow-if-protected :before)
    (point character &optional combine-char)
  (declare (ignore character combine-char))
  (check-editable-point point nil 0))

(defadvice (EDITOR::delete-characters disallow-if-protected :before)
    (point &optional (n 1))
  (declare (ignore n))
  (check-editable-point point t -1))
                                    

(defadvice (EDITOR:delete-between-points disallow-if-protected :before)
    (start end &optional deleted-string)
  (declare (ignore deleted-string))
  (check-editable-point start 0 0)
  (check-editable-point end t -1)
  )

(defadvice (EDITOR::%kill-region disallow-if-protected :before)
    (start end &optional (current-type :kill-forward))
  (declare (ignore current-type))
  (check-editable-point start 0 0)
  (check-editable-point end t -1))

(defadvice (editor::clear-listener allow-even-if-protected :around)
    ()
  (let ((buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-protected-offset buffer -1)
          (call-next-advice))
      (set-protected-offset buffer 0)
      (maybe-fix-protected-offset (buffers-end buffer) 1))))
  

           

