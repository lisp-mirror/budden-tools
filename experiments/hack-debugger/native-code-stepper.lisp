;;; -*- Encoding: utf-8; -*-

;  Concept of native code stepper
;  Tested as 32-bit LW for Windows (and works poorly)
;  Unlikely to work on other systems
;  The code is not GC-safe as I found no way to suspend GC
;  Crashes sometimes at last test and is completely unreliable
;  Ask your Lisp vendor to implement it correctly.
;  Advantages over stepper:
;  No need for recompilation or "restart frame stepping"
;  Can modify function currently on stack so that 
;  Can work in a conjunction with "break on return from frame" 
;  Close to VS experience
;  Disadvantages: less verbose


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :native-code-stepper
    (:use :cl :hcl :lispworks :mp :system)
    (:shadowing-import-from #:system #:with-fast-lock)
    (:export 
     #:*tracing-enabled*
     #:*stepping-enabled*
     #:stepize-fn
     #:unstepize-fn
     )
    ))

(eval-when (:execute)
  (error "Use compile-load sequence to run the concept"))

(in-package :native-code-stepper)

(defun unintern-all-internal-symbols-of-package (package)
  (let ((package (find-package package)))
    (do-symbols (s package)
      (when (eq (symbol-package s) package)
        (unintern s package)))))

(proclaim '(optimize (space 0) (speed 0) (debug 3) (fixnum-safety 3)))


;;;;  DE-OPTIMIZING MATHS
(eval-when (:compile-toplevel :load-toplevel)

  (defun remove-some-optimizations ()
    "Remove transforms so that to get more steppable points"
    (dolist (fn-name '(+ - * / = eq))
      (dolist (property '(COMPILER::lwx86-fndefs compiler::%lwx86-p2-transforms compiler::lwx86-syslisp-primitive))
        (setf (get fn-name property) nil))))

  (remove-some-optimizations)

  )


(defun my+ (&rest args) (apply #'cl:+ args))

(defvar *tracing-enabled* t
  "If true, step points are printed")

(defvar *stepping-enabled* t
  "If this is true, every step point breaks")
  
(defvar *in-my-do-break* nil "Bound to t in a call to (break) made inside my-do-break")
(defvar *stepped-source-is-shown-already-in-the-debugger* nil
  "When debugger is opened and dbg::debugger-select-frame is called for the first time, we show stepped source")
;(defvar *stepper-call-from* nil)
(defvar *stepper-call-to* nil)
;(defvar *stepper-call-args* nil)
(defvar *step-into-flag* nil "is set to t inside (break)<-(my-do-break) to step into call")
(defvar *my-do-break-call-done* nil "is set to t to avoid double calling of call-to from my-do-break")


(defun my-do-break (call-from call-to call-args)
  (when *tracing-enabled*
    (format t "~%native stepper break, from ~S into ~S, args=~S" call-from call-to call-args))
  (cond
   (*stepping-enabled*
    (let ( ; bindings for both break and apply
          (*step-into-flag* nil)
          ) 
      (let ( ; bindings for break only
            (DBG::*hidden-symbols*
             (append '(break my-do-break invoke-debugger make-breakpoint) DBG::*hidden-symbols*))
            (DBG::*default-debugger-commands*
             (append 
              '((:so step-over "step over")
                (:sc step-continue "continue")
                (:si step-into "step into")
                )
              DBG::*default-debugger-commands*))
          ;(DBG:*print-invisible-frames* nil)
          ;(*stepper-call-from* call-from)
            (*stepper-call-to* call-to)
            ;(*stepper-call-args* call-args)
            (*in-my-do-break* t)
            (*stepped-source-is-shown-already-in-the-debugger* nil))
        (break "Stepper break before call from ~S into ~S with args=~S~%Current source should be highlighted in the editor.~%type :so for step over, :si for step in,:sc for continue" call-from call-to call-args)
        ; step-* functions are called from break that may stepize other functions 
        ; and/or set *step-into-flag*
        )
      (apply call-to call-args)))
   (t
    (apply call-to call-args)
    )))

;(dolist (name '(make-breakpoint
;                ; break
;                invoke-debugger
;                my-do-break))
;  (pushnew name DBG::*hidden-symbols*))


(defvar *active-steppoints* nil "list of created breakpoints")


;;;  Can we step this? ;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *non-steppable-calls* '(
                                      invoke-debugger
                                      dbg::get-call-frame
                                      dbg::debug1
                                      conditions::in-break
                                      runtime:bad-args-or-stack ; useless
                                      system::*%+$any-stub ; does not work
                                      system::*%-$any-stub ; does not work
                                      break ; bad things would happen
                                      ) 
  "Functions call to which we don't touch while making function steppable")

(defvar *non-stepizable-fns*
  '(
    my-do-break
    ;; no need to mention system symbols as they're 
    ;; already mentioned in 
    ;invoke-debugger
    ;break
    ;dbg::get-call-frame
    ;dbg::debug1
    ;conditions::in-break
    ;runtime:bad-args-or-stack ; useless
    ;system::*%+$any-stub ; does not work
    ;system::*%-$any-stub ; does not work
    ;break ; bad things would happen
    )
  "Functions we not trying to stepize")

(defparameter *packages-of-non-stepizable-functions*
  (copy-list *packages-for-warn-on-redefinition*)
  "Calls to functions in that packages can be step points, but we never step in those function. 
  Take them into our variable so that don't 
   lose in a situation where *packages-for-warn-on-redefinition* is bound to nil"
  )


(defun call-steppable-p (call-into call-kind)
  (when (= call-kind 0)
       ; don't attempt to step direct calls as they
       ; use stack in some other way. 
    (return-from call-steppable-p nil))
  (typecase call-into
    (symbol
     (and 
      ; (potentially-steppable-function-name-p call-into)
      (not (member call-into *non-steppable-calls*))
      (not (= call-kind 0))))
    (function 
     (not (member (coerce call-into 'function) *non-steppable-calls* :key (lambda (x) (coerce x 'function)))))
    (t ; other constants, e.g. numbers. 
     nil)))


(defun package-is-not-for-stepping (p)
  "Accepts nil, package, or package name"
  (etypecase p
    (package
     (package-is-not-for-stepping (package-name p)))
    ((or symbol string)
     (member p *packages-of-non-stepizable-functions* :test 'string=))
    (null
     nil)))

(defun stepizible-function-name-p (fn-name)
  "Function we can step into"
  (cond
   ((member fn-name *non-steppable-calls*) nil)
   ((member fn-name *non-stepizable-fns*) nil)
   ((dbg-never-stop-complex-name-p fn-name) nil)
   ((and (symbolp fn-name)
         (package-is-not-for-stepping (symbol-package fn-name)))
          nil)
   (t t)))


(defstruct breaker-info fn offset old-called kind)

(defun make-breakpoint (fn offset old-called kind)
  "breakpoint is indeed a closure and a change in a function code and constants. 
  Only function calls can be used as a breakpoint locations, otherwise debugger
  is unable to find source location so the entire mechanism becomes useless"
  (let* ((breaker-symbol
          (if (symbolp old-called)
              (make-symbol (concatenate 'string
                                        "stepper-call-"
                                        (package-name (symbol-package old-called))
                                        "::"
                                        (symbol-name old-called)))
            (gensym) ; we can include printable representation of function here, but we need to strip away address of it
            )
          )
         (breaker ; lambda call to which is places instead of original call
          (cond
           ;((eq old-called #'system::*%+$any-stub)
           ; (my-do-break-2 fn old-called args)
           ; )
           (t
            (lambda (&rest args)
              (my-do-break fn old-called args))))))
    (setf (get breaker-symbol 'breaker-info)
          (make-breaker-info :fn fn :offset offset :old-called old-called :kind kind))
    (setf (symbol-function breaker-symbol) breaker)
    (set-breakpoint-in-function fn offset breaker-symbol kind old-called)
    (push breaker-symbol *active-steppoints*)
    (values old-called breaker)))


(defun breaker-symbol-p (x)
  (and (symbolp x)
       (typep (get x 'breaker-info) 'breaker-info)))

(defun delete-breakpoint (breaker-symbol)
  "Delete a breakpoint. It must be really a breakpoint. "
  (assert (member breaker-symbol *active-steppoints*) ()
    "Breakpoint handler ~S is not on a breakpoint list" breaker-symbol)
  (let* ((info (get breaker-symbol 'breaker-info))
         ; (breaker-fn (symbol-function breaker-symbol))
         )
    (assert (breaker-info-p info) ()
                            "~S is not a breakpoint handler function name" breaker-symbol)
    (with-slots (fn offset old-called kind) info
      (set-breakpoint-in-function fn offset old-called kind breaker-symbol)
      (setf *active-steppoints*
            (delete breaker-symbol *active-steppoints*))
      nil)))


(defun extract-address-from-function (function)
  "Возвращает адрес по объекту функции. Lame: extracts address from printed representation"
  (assert (functionp function))
  ; (object-address function) is not that precise
  (let* ((text (format nil "~A" function))
         (length (length text))
         (offset (- length 9))
         (hex-address (subseq text offset (+ offset 8)))
         (*read-base* 16)
         (address (read-from-string hex-address)))
    address))

(defun extract-code-size-from-function (function)
  "Returns a code from a function (LAME: with the help of inspector)"
  (assert (functionp function))
  (let* ((code (first (nth-value 1 (get-inspector-values function nil))))
         (printed-code (prin1-to-string code))
         (end-pos (position #\) printed-code :from-end t))
         (beg-pos (position #\( printed-code :from-end t))
         (length-as-string (subseq printed-code (1+ beg-pos) end-pos))
         (*read-base* #10r10)
         (length (read-from-string length-as-string)))
    length))
    

(defun poke (addr byte)
  "Write byte at addr"
  (setf (fli:dereference (fli:make-pointer :address addr :type :unsigned-byte)) byte))

(defun poke-unsigned (addr integer)
  (setf (fli:dereference (fli:make-pointer :address addr :type :unsigned)) integer))

(defun peek-unsigned (addr)
  (fli:dereference (fli:make-pointer :address addr :type :unsigned)))

(defun peek-byte (addr)
  "Read byte from addr"
  (fli:dereference (fli:make-pointer :address addr :type :unsigned-byte)))

(defun read-byte-from-hex-string (s)
  "For debugging"
  (parse-integer s :radix #10r16))


(defun extract-n-bytes (addr n) 
  "Достаёт n байт и возвращает список"
  (let (result)
    (dotimes (i n)
      (push (peek-byte addr) result)
      (incf i)
      (incf addr))
    (nreverse result)))
    

(defparameter +number-of-bytes-in-word+ 4 "Magic constant")

(defparameter +length-of-indirect-call-command+ 6 "Magic constant")
(defparameter +direct-call+ 1)
(defparameter +indirect-call+ 2)

(defun poke-opcode (function-object offset opcode-list)
  (let* ((dest-address (+ (extract-address-from-function function-object) offset))
         )
    (dolist (byte opcode-list)
      (poke dest-address byte) 
      (incf dest-address))
    ;(format t "~%dest address is ~X~%poke opcode ~S" dest-address opcode-list)
    ))

(defun n-to-hex (x)
  "Превращает в строку"
  (format nil "~X" x))



(defun n-to-32-bytes (x)
  "Дано на входе 32-разрядное число, разбиваем на байты"
  (do ((quotient nil)(remainder nil)(result nil)(i 0 (+ i 1)))
      ((= i +number-of-bytes-in-word+) result)
    (multiple-value-setq (quotient remainder) (floor x #x100))
    (push remainder result)
    ;(format t "~%~X" quotient)
    (setf x quotient)
    ))

(defun 32-bytes-to-n (x)
  "Дан список байт, построить число из них (в обратном порядке)"
  (assert (typep x '(or (cons integer) null)))
  (let* ((len (length x))
         (result 0))
    (dotimes (i len)
      (setf result (+ (* result #x100) (elt x (- len i 1)))))
    result))

#|(defun maybe-twos-complement (x)
  "Вроде не нужна?"
  (cond
   ((>= x 0) x)
   (t 
    (int32-to-integer (int32-1+ (int32-lognot (integer-to-int32 (- x)))))
   )))|#

(defparameter +hackish-address-shift+ 5)

(defun calc-call-offset (dst-address address-to-call)
  "Offset for direct call"
  (- address-to-call dst-address +hackish-address-shift+))

(defparameter +hackish-symbol-value-offset+ 4 ; currently unused
  )


(defstruct breakpoint-key function-name offset)

(defparameter *breakpoints* (make-hash-table :test 'equalp)
  "breakpoint-key => smashed function")

(defparameter +magic-reference-table-offset+ 12)
(defparameter +magic-word-size+ 4)

(defun object-to-entry-in-reference-table (obj call-kind)
  "We have an object. Lets' calc a value for it that fits into reference table"
  (ecase call-kind
    (1 (+ (object-address obj)
          +hackish-symbol-value-offset+ ; magic
          (- call-kind) ; magic
          ))
    (0 (extract-address-from-function obj))))

(defun do-gc ()
  (gc-generation t))

; (defun fn-reference-entry-address (

(defun replace-fn-reference (fn new-fn call-kind old-fn)
  "Changes fn's reference from old-fn to new-fn in an fn's reference table. Call-kind must be of old-fn"
  (let* ((code-size (extract-code-size-from-function fn))
         (references (extract-function-references fn))
         (pos (position old-fn references))
         (gc-done (do-gc))
         (fn-address (extract-address-from-function fn))
         (table-address (+ fn-address code-size +magic-reference-table-offset+))
         (new-value (object-to-entry-in-reference-table new-fn call-kind))
         (check-old-value (object-to-entry-in-reference-table old-fn call-kind))
         entry-address
         real-old-value)
    (declare (ignore gc-done))
    (unless pos
      (return-from replace-fn-reference nil) ; changed already or something's wrong
      )
    (setf entry-address (+ table-address (* pos +magic-word-size+)))
    (setf real-old-value (peek-unsigned entry-address))
    (assert (= real-old-value check-old-value) () "Didn't find correct old value in reference table")
    (poke-unsigned entry-address new-value)
    (do-gc)
    ; (assert (= new-value (extract-address-from-function fn)) () "Your image is likely to be smashed (0)")
    (format t "~%Old entry in a reference table: ~X" real-old-value)
    (format t "~%code-size=~D,pos=~S,table-address=~X" code-size pos table-address)
    
    ; do nothing, just imitate
    ))
    

(defun temp-breaker (&rest ignore)
  "When substitution of calls with lambda is too complex, substitue it with temp-breaker instead"
  (declare (ignore ignore))
  (print "I'm temp-breaker"))

(defun set-breakpoint-in-function (fn offset breaker-symbol call-kind old-called)
  "Подменяет вызов на функцию. offset at function name must point to call, either direct or indirect"
  (progn ; with-other-threads-disabled
    (let* ((fn (coerce fn 'function))
           (breaker-fn (symbol-function breaker-symbol))
           fn-address
           new-opcode
           replace-to-address
           )
      (assert (functionp fn))
      ; (gc-generation t)
      ; (setf fn-address (extract-address-from-function fn))
      (setf new-opcode
          (ecase call-kind
           (1 ; call [symbol]
                   ; если funcall (car list) превращается в call [адрес (car list)], мы сломаемся. FIXME проверить
            (assert (symbolp old-called) () "Can't handle other indirect calls than from a symbol")
            (do-gc)
            (replace-fn-reference fn breaker-symbol 1 old-called)
            ;(setf breaker #'temp-breaker)
            (do-gc)
            (setf fn-address (extract-address-from-function fn))
            (setf replace-to-address (+ (object-address breaker-symbol) +hackish-symbol-value-offset+))
            replace-to-address
            )
           (0
            (do-gc)
            (replace-fn-reference fn breaker-fn 0 old-called)
            (do-gc)
            (setf fn-address (extract-address-from-function fn))
            (setf replace-to-address (extract-address-from-function breaker-fn))
            (calc-call-offset (+ (extract-address-from-function fn) offset -1) replace-to-address)
           )))
    ;(format t "~%fn-address=~X, replace-to-address=~X, current-call=~S"
    ;        fn-address replace-to-address current-call)
      ;(poke-opcode fn offset new-opcode)
      (poke-unsigned (+ fn-address offset) new-opcode)
      (assert (= (extract-address-from-function fn) fn-address)
          () "your image is likely smashed (1)")
      (ecase call-kind
       (1
        (assert (= (+ (object-address breaker-symbol) +hackish-symbol-value-offset+) replace-to-address)
            () "your image is likely smashed (2)"))
       (0
        (assert (= (extract-address-from-function breaker-fn) replace-to-address)
            () "your image is likely smashed (3)")))
      (assert (or (functionp old-called) (and (symbolp old-called)
                                              (fboundp old-called))))
      ;(print new-opcode)
      old-called
    ;(normal-gc)
      )))


(defun extract-function-references (function-object)
  "Functions we refer to"
  (assert (functionp function-object))
  (let* ((constants (SYSTEM::function-constants function-object))
         (limit (- (length constants) 1))
         (result nil))
    (dotimes (i limit)
      (push (elt constants (+ i 1)) result))
    (nreverse result)))


#|(defun extract-function-breakable-offsets (function-object)
  "OBSOLETE. Uses mysterious consts to find breakable points which debugger can handle"
  (assert (functionp function-object))
  (let* ((constants (SYSTEM::function-constants function-object))
         (first-constants (first constants))
         (locations (elt first-constants 6))
         (result nil))
    (dotimes (i (length locations))
      (when (= 0 (mod i 3))
        (push (elt locations i) result)))
    result))|#
  


(defun stepize-fn (function-or-name)
  "Extract all steppable points from compiled function and set breakpoints on them"
  (cond
   ((breaker-symbol-p function-or-name)
    (error "attemp to step breaker-symbol"))
   ((function-has-step-points-p function-or-name)
    ; stepper points are set already - do nothing
    )
   (t
    (let* ((fn (coerce function-or-name 'function))
           (constants (SYSTEM::compute-callable-constants fn)))
      (format t "~%stepizing ~S~%constants=~S~%" fn constants)
      (dolist (rec constants)
        (destructuring-bind (offset call-into call-kind) rec
          (when (call-steppable-p call-into call-kind)
            (make-breakpoint fn offset call-into call-kind))))))))
  

(defun unstepize-fn (function-or-name)
  "Remove all steppable points from fn"
  (let* ((fn (coerce function-or-name 'function))
         (constants (SYSTEM::compute-callable-constants fn)))
    (dolist (rec constants)
      (destructuring-bind (offset call-into call-kind) rec
        (declare (ignore offset call-kind))
        (when (breaker-symbol-p call-into)
          (delete-breakpoint call-into))))))
      
    
  
(defun poke-int3 (function offset &optional (count-of-nops 0))
  (assert (functionp function))
  (let ((offs (+ (extract-address-from-function function) offset)))
    (poke offs #xCC) ; int3 int 3
    (dotimes (i count-of-nops)
      (incf offs)
      (poke offs #x90 ; nop
            ))))  


(defun dbg-get-some-frame ()
  (assert dbg::*debugger-stack*)
  (slot-value DBG::*debugger-stack* 'DBG::current-frame)
  )


;;; Study of stack frames and operations on them ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dbg-top-frame (&optional initial-frame)
  "Top of debugger stack"
  (setf initial-frame (or initial-frame (dbg-get-some-frame)))
  (do ((this initial-frame r)
       (r initial-frame (slot-value r 'dbg::prev)))
      ((null r) this)))

(defun dbg-find-stepped-code ()
  "When called from a debugger, returns toplevel stepped code frame"
  (error "Unlikely to be correct")
  (let ((initial-frame (dbg-top-frame)))
    (unless initial-frame
      (return-from dbg-find-stepped-code nil))
    (do ((frame initial-frame (slot-value frame 'dbg::%next)))
        ((null frame) nil)
      (when (slot-exists-p frame 'dbg::function-name)
        (let* ((function-name
                (slot-value frame 'dbg::function-name)))
          (when (equalp function-name '(subfunction 1 make-breakpoint))
            (return-from dbg-find-stepped-code
              (slot-value frame 'dbg::%next))))))))
      
  

;;;;;;;;;;;; Tune the IDE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice (lispworks-tools::debugger-select-frame find-source-just-at-select-frame-time :before) (frame self &optional update-backtrace)
  (declare (ignore update-backtrace))
  (when (and *in-my-do-break* *stepping-enabled*
             (not *stepped-source-is-shown-already-in-the-debugger*))
    (let ((stepped-code-frame
           (dbg-find-topmost-stepizible-frame (dbg-top-frame frame))
           ; (dbg-find-stepped-code frame)
           ))
      (when stepped-code-frame
        (setf *stepped-source-is-shown-already-in-the-debugger* t)
        (ignore-errors
          (SYSTEM::DBG-EDIT-FRAME-INTERNAL stepped-code-frame self))
        ))))
        
(defadvice (DBG::DEBUG-TO-LISTENER-P dont-debug-to-listener :around) (condition)
  (if *in-my-do-break* nil (call-next-advice condition)))

;;;; IDE commands for stepper ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun step-continue (&rest ignore)
  "Continue execution w/o stepping"
  (declare (ignore ignore))
  (setf *stepping-enabled* nil)
  (setf *step-into-flag* nil)
  ;(setf *in-my-do-break* nil) действительно это надо было?
  ;(dolist (breaker-symbol *active-steppoints*)
  ;  (delete-breakpoint breaker-symbol))
  (dbg::dbg-continue))


(defun function-has-step-points-p (function-or-name)
  "Are there step point in function now?"
  (let* ((fn (coerce function-or-name 'function))
         (constants (SYSTEM::compute-callable-constants fn)))
    (dolist (rec constants)
      (destructuring-bind (offset call-into call-kind) rec
        (declare (ignore offset call-kind))
        (when (breaker-symbol-p call-into)
          (return-from function-has-step-points-p t))))
    nil))

(defun dbg-never-stop-complex-name-p (name)
  (equalp name '(subfunction 1 make-breakpoint)))

(defun dbg-stepizible-frame-p (frame)
  (when (slot-exists-p frame 'dbg::function-name)
    (let ((fn-name (slot-value frame 'dbg::function-name)))
      (stepizible-function-name-p fn-name)
      )))

(defun dbg-find-topmost-stepizible-frame (down-from-frame)
  "Find potentially steppable fn on debugger stack below from given frame"
  ;(let ((top (dbg-top-frame down-from-frame)))
  (do ((frame down-from-frame (slot-value frame 'dbg::%next)))
      ((null frame) nil)
    (when (dbg-stepizible-frame-p frame)
      (return-from dbg-find-topmost-stepizible-frame frame))
    ))

(defun dbg-stepize-stepizible-frame (frame)
  (stepize-fn (slot-value frame 'DBG::function-name)))


(defun step-over (&rest ignore)
  "Step current function not entering the calls"
  ; должна быть переменная "текущая шагаемая функция" и в зависимости от неё мы останавливаемся на step-поинтах или не останавливаемся. Но переменной мы её не можем сделать, т.к. не знаем способа указать на точку в стеке. Вместо этого можем завести special переменную и искать на стеке её биндинги. А вообще, может быть и ничего. Если мы пришли в step-point, то она и есть вершина видимого стека.
  (declare (ignore ignore))
  (assert DBG::*debugger-stack*)
  (let ((frame (dbg-get-some-frame)))
    (cond
     (*in-my-do-break* ; we're already at step-point break
      
      ; what about stepping out? FIXME - cover only a case of normal return to next frame
      ; non-local transfers and exceptions are not covered
      (let* ((our-frame
              (dbg-find-topmost-stepizible-frame (dbg-top-frame frame))
              ; this is currently stepped frame as *in-my-do-break* is t, hence (my-do-break)->(break) is on the stack
              )
             (next-stepizible-frame
              (dbg-find-topmost-stepizible-frame our-frame)
              ))
        (when next-stepizible-frame 
          (dbg-stepize-stepizible-frame next-stepizible-frame)))
      (setf *step-into-flag* nil)
      (dbg::dbg-continue)
      )
     ((dbg-stepizible-frame-p frame)
      ; entering step mode in a selected frame
      (stepize-fn (slot-value frame 'DBG::function-name))
      (setf *stepping-enabled* t)
      (setf *step-into-flag* nil)
      (dbg::dbg-continue))
     (t ; ent
      (capi:display-message "Selected frame is not steppable"))
     )))

(defun step-into (&rest ignore)
  (declare (ignore ignore))
  (cond
   (*in-my-do-break*
    (cond
     ((null *stepper-call-to*)
      (CAPI:display-message "Looks like a bug: *stepper-call-into* is nil while *in-my-do-break* is t"))
     ((stepizible-function-name-p *stepper-call-to*)
      (stepize-fn *stepper-call-to*)
      (setf *step-into-flag* t)
      (DBG::dbg-continue))
     (t
      (capi:display-message "function ~S is not steppable" *stepper-call-to*)
      )))          
   (t
    (step-over)))
  )
;  "Step into function to call"
;  (cond
;   ((breaker-symbol-p 
;    (
  
    
;;;;  -------------------------------- TESTS -------------------------------------------------

(defun subroutine-of-x-and-y (x &key y)
  "test function"
  (format t "~%subroutine-of-x-and-y is running. Args: ~S,~S~%" x y)
  (+ y x))

(defun subroutine-of-x (x)
  "test function"
  (format t "~%subroutine-of-x is running. Arg: ~S~%" x)
  (+ 1 x))

(defun subroutine-with-no-args ()
  "test function"
  (print "subroutine-with-no-args is running"))


(defun test-fn (x)
  ;(+ x 1)
  (cond
   ((eq x 2)
    (+ (test-fn (+ x 1)) 3))
   (t 
    (subroutine-of-x-and-y x :y 1)
    ))
  )

(disassemble #'test-fn)
(stepize-fn #'test-fn)
(disassemble #'test-fn)

(defun do-test ()
  (let (
        (*stepping-enabled* t)
        )
    (test-fn 2)))
(format t "~%calling test-fn...")
(do-test)

#|
 ; Direct call test. If use that, remove 
 ;  (when (= call-kind 0)
 ;      ; don't attempt to step direct calls as they
 ;      ; use stack in some other way. 
 ;   (return-from call-steppable-p nil))
 ; at the beginning of call-steppable-p 

(compile `(defun test-fnd ()
            (funcall ,#'subroutine-with-no-args)
            ))

(stepize-fn 'test-fnd)
(test-fnd)
|#


; some useful staff
;; system::compute-callable-constants #'test-fn-5 - возвращает интересноенам.
; (LISPWORKS-TOOLS::inspect-an-object #'test-fn-2)
; SYSTEM::disassembly-objetc-find-reference-for-offset coco 28 
; делает то, что мы уже начились
; RAW::fixup-moved-function интересно, но вряд ли разберусь. 
