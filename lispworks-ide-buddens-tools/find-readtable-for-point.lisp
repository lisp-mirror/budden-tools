;; -*- Encoding: utf-8 ; -*- 

(in-package :editor)


(defmacro buddens-editor-string (s)
  `(coerce ,s 'simple-text-string))


(defmacro buddens-with-locked-point (point &body body)
  `(with-point-locked (,point)
    ,@body))

(defmacro buddens-with-locked-point-no-modification (point &body body)
  `(with-point-locked (,point :for-modification nil)
    ,@body)) 


(defun keywordize-if-exists (string)
  (find-symbol string :keyword))


(defun find-readtable-name-for-point (buffer &key (direction :forward) point limit (cachep t) (search-head "(in-readtable")
                                           &allow-other-keys)
  "For sbcl/clcon/oduvanchik, see oduvanchik::readtable-at-point instead"
  (let* ((p (copy-point (or point
                            (if (eq direction :forward)
                                (buffer-%start buffer)
                              (buffer-point buffer)))
                        :temporary))
         (search-string (buddens-editor-string search-head))
         (length (length search-string))
         (pattern (get-search-pattern search-string direction))
         (*debug-find-package-name-for-point-cache* t)
         )
    (buddens-with-locked-point-no-modification  ; :temporary points below assume no-mod
        p
      (let* ((cache (buffer-value buffer 'in-readtable-cache))
             (cached-in-package-position (first cache))
             (cached-limit-position (second cache))
             (start-position (and cachep
                                  (eq direction :backward)
                                  (point-position p)))
             (use-limit limit)
             (cache-limited-search-p nil)
             (debug (and (boundp '*debug-find-package-name-for-point-cache*)
                         *debug-find-package-name-for-point-cache*)))
        (when (and start-position
                   cached-in-package-position
                   (= (buffer-modified-tick buffer) (third cache)))
          (when debug
            (print (list 'check-cache start-position cache) *terminal-io*))
          (if (<= start-position cached-limit-position)
              (when (<= cached-in-package-position start-position)
                (when debug
                  (print (list 'get-cache cache) *terminal-io*))
                (return-from find-readtable-name-for-point (fourth cache)))
            (when (> cached-limit-position 0)
              (let ((cache-use-limit (copy-point p :temporary)))
                (setf (point-position cache-use-limit) cached-limit-position)
                (when (or (not use-limit)
                          (point<= use-limit cache-use-limit))
                  ;; limit the search to cached end
                  (setq use-limit cache-use-limit)
                  (setq cache-limited-search-p t))))))
        (let ((name (i-find-package-name-for-point
                     p direction length pattern point use-limit)))
          (when debug
            (print (list 'searched (and use-limit (point-position use-limit)) start-position cache-limited-search-p name) *terminal-io*))
          (when start-position
            (when-let (in-package-position
                       (cond (name
                              (point-position p))
                             (cache-limited-search-p
                              ;; extend the end of the cached region
                              (setq name (fourth cache))
                              (when debug
                                (print (list 'merge-cache cached-in-package-position name) *terminal-io*))
                              cached-in-package-position)
                             (use-limit
                              ;; don't set cache to nil if limit prevented a full search
                              nil)
                             (t 0)))
              (setf (buffer-value buffer 'in-readtable-cache)
                    (setq cache
                          (list in-package-position
                                start-position
                                (buffer-modified-tick buffer)
                                name)))
              (when debug
                (print (list 'set-cache cache) *terminal-io*))))
          (when debug
            (print (list 'in-package name) *terminal-io*))
          name)))))

; Не удалось использовать следующие ф-ии: 
; compile-defun-command, compile-buffer-command, evaluate-defun-command, evaluate-last-form-command
; они вызываются в процессе редактора, а компиляция - в background execute.

(defun find-readtable-or-lose (readtable-name-string)
  (etypecase readtable-name-string
    (null (named-readtables:find-readtable nil))
    (string
     (let ((name-as-symbol (keywordize-if-exists readtable-name-string)))
       (etypecase name-as-symbol
         (null (named-readtables:find-readtable nil))
         (symbol (named-readtables:find-readtable name-as-symbol)))))))

(defmacro with-guessed-buffer-readtable (buffer &body body)
  (alexandria:with-gensyms (rt-name)
    `(let* ((,rt-name (find-readtable-name-for-point ,buffer :cachep t))
            (*readtable* (find-readtable-or-lose ,rt-name)))
       ,@body)))
    

(defadvice (region-lisp-compile note-readtable-change :around) (buffer &rest rest)
  "Также может представлять интерес region-compile"
  (with-guessed-buffer-readtable buffer
    ; (budden-tools:show-expr `(region-lisp-compile ,*readtable*))
    (apply #'call-next-advice buffer rest)))


(defadvice (region-lisp-eval note-readtable-change :around) (buffer &rest rest)
  "Также может представлять интерес region-compile"
  (with-guessed-buffer-readtable buffer
    ;(budden-tools:show-expr `(region-lisp-eval ,*readtable*))
    (apply #'call-next-advice buffer rest)))






  
;  (budden-tools:show-expr (keywordize-if-exists (find-readtable-name-for-point (current-buffer) :cachep nil)))
