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
  
(defvar *in-my-do-break* t)
(defvar *stepped-source-is-shown-already-in-the-debugger* nil)
(defvar *stepper-call-from* nil)
(defvar *stepper-call-to* nil)


(defun my-do-break (call-from call-to args)
  (declare (ignorable args))
  (when *tracing-enabled*
    (format t "~%native stepper break, from ~S into ~S, args=~S" call-from call-to args))
  (when *stepping-enabled*
    (let ((DBG::*hidden-symbols*
           (append '(break my-do-break invoke-debugger make-breakpoint) DBG::*hidden-symbols*))
          ;(DBG:*print-invisible-frames* nil)
          (*stepped-source-is-shown-already-in-the-debugger* nil)
          (*stepper-call-from* call-from)
          (*stepper-call-to* call-to)
          (*in-my-do-break* t)
          )
      (break "Stepper break before call from ~S into ~S with args=~S~%If all is Ok, your current source is shown in the editor. Use 'continue' command to step further" call-from call-to args)))
  (apply call-to args)
  )

;(dolist (name '(make-breakpoint
;                ; break
;                invoke-debugger
;                my-do-break))
;  (pushnew name DBG::*hidden-symbols*))

(defparameter *non-steppable-calls* '(runtime:bad-args-or-stack ; useless
                                      system::*%+$any-stub ; does not work
                                      system::*%-$any-stub ; does not work
                                      break ; bad things would happen
) 
  "Functions call to which we don't touch while making function steppable")


(defvar *active-steppoints* nil "list of created breakpoints")

(defun call-steppable-p (call-into call-kind)
  (and (not (= call-kind 0)) ; 
       ; don't attempt to step direct calls as they
       ; use stack in some other way. 
  (typecase call-into
    (symbol
     (and 
      (not (member call-into *non-steppable-calls*))
      (not (= call-kind 0))))
    (function 
     (not (member (coerce call-into 'function) *non-steppable-calls* :key (lambda (x) (coerce x 'function)))))
    (t ; other constants, e.g. numbers. 
     nil))))


(defstruct breaker-info fn offset old-called kind)

(defun make-breakpoint (fn offset old-called kind)
  "breakpoint is indeed a closure and a change in a function code and constants. 
  Only function calls can be used as a breakpoint locations, otherwise debugger
  is unable to find source location so the entire mechanism becomes useless"
  (let* ((breaker-symbol
          (if (symbolp old-called)
              (make-symbol (concatenate 'string
                                        "step-"
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

(defun calc-call-opcode (dst-address address-to-call size)
  "Вычисляет адрес, чтобы вместо прямого вывова был вызов replace-to-address. Добивает до size нопами"
  (assert (>= size 5))
  (let ((result nil)
        (call-offset (- address-to-call dst-address +hackish-address-shift+)))
    (dotimes (i (- size 5))
      (push #x90 ; nop 
            result))
    (setf result
          (nconc (nreverse
                  (n-to-32-bytes
                  ;(maybe-twos-complement
                   call-offset
                   ;)
                   ))
                 result))
    (push #xE8 result)
    result))

(defun calc-indirect-opcode (replace-to-address)
  "Вычисляет адрес, чтобы вместо косвенного вывова был вызов replace-to-address. Нам, по сути, не нужен"
  (let ((call-offset replace-to-address))
    (list* #xFF #x15 (reverse
                      (n-to-32-bytes
                  ;(maybe-twos-complement
                       call-offset
                   ;)
                       )))))

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

(defun replace-fn-reference (fn new-fn call-kind old-fn)
  "Changes fn's reference from old-fn to new-fn in an fn's reference table. Call-kind must be of old-fn"
  (let* ((code-size (extract-code-size-from-function fn))
         (references (extract-function-references fn))
         (pos (position old-fn references))
         (gc-done (gc-generation t))
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
    ;(assert (= new-value (extract-address-from-function fn)) () "Your image is likely to be smashed (0)")
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
            (gc-generation t)
            (replace-fn-reference fn breaker-symbol 1 old-called)
            ;(setf breaker #'temp-breaker)
            (gc-generation t)
            (setf fn-address (extract-address-from-function fn))
            (setf replace-to-address (+ (object-address breaker-symbol) +hackish-symbol-value-offset+))
            (incf offset -2)
            (calc-indirect-opcode replace-to-address)
            ;(poke-unsigned (+ (extract-address-from-function fn) offset)
            ;               (extract-address-from-function breaker))
            ;(setf replace-to-address (extract-address-from-function breaker))
            ;(incf offset -2)
            ;(calc-call-opcode (+ (extract-address-from-function fn) offset) replace-to-address 6)
            )
           (0
            (gc-generation t)
            (replace-fn-reference fn breaker-fn 0 old-called)
            (gc-generation t)
            (setf fn-address (extract-address-from-function fn))
            (setf replace-to-address (extract-address-from-function breaker-fn))
            (incf offset -1)        
            (calc-call-opcode (+ (extract-address-from-function fn) offset) replace-to-address 5)
           )))
    ;(format t "~%fn-address=~X, replace-to-address=~X, current-call=~S"
    ;        fn-address replace-to-address current-call)
      (poke-opcode fn offset new-opcode)
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
  (unless (function-has-step-points-p function-or-name)
    (let* ((fn (coerce function-or-name 'function))
           (constants (SYSTEM::compute-callable-constants fn)))
      (format t "constants=~S" constants)
      (dolist (rec constants)
        (destructuring-bind (offset call-into call-kind) rec
          (when (call-steppable-p call-into call-kind)
            (make-breakpoint fn offset call-into call-kind)))))))


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

;;; DBG utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dbg-top-frame (&optional (some-frame DBG::*dbg-eval-frame*))
  "Top of debugger stack"
  (cond
   ((null some-frame)
    nil)
   (t
    (do ((this some-frame r)
         (r some-frame (slot-value r 'dbg::prev)))
        ((null r) this)))))

(defun find-stepped-code (&optional (some-frame DBG::*dbg-eval-frame*))
  "When called from a debugger, returns toplevel stepped code frame"
  (let ((initial-frame (dbg-top-frame some-frame)))
    (unless initial-frame
      (return-from find-stepped-code nil))
    (do ((frame initial-frame (slot-value frame 'dbg::%next)))
        ((null frame) nil)
      (when (slot-exists-p frame 'dbg::function-name)
        (let* ((function-name
                (slot-value frame 'dbg::function-name)))
          (when (equalp function-name '(subfunction 1 make-breakpoint))
            (return-from find-stepped-code
              (slot-value frame 'dbg::%next))))))))
      
  

;;;;;;;;;;;; Tune the IDE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice (lispworks-tools::debugger-select-frame find-source-just-at-select-frame-time :before) (frame self &optional update-backtrace)
  (declare (ignore update-backtrace))
  (when (and *in-my-do-break*
             (not *stepped-source-is-shown-already-in-the-debugger*))
    (let ((stepped-code-frame
           (find-stepped-code frame)))
      (when stepped-code-frame
        (setf *stepped-source-is-shown-already-in-the-debugger* t)
        (ignore-errors
          (SYSTEM::DBG-EDIT-FRAME-INTERNAL stepped-code-frame self))
        ))))
        
(defadvice (DBG::DEBUG-TO-LISTENER-P dont-debug-to-listener :around) (condition)
  (if *in-my-do-break* nil (call-next-advice condition)))

;;;; IDE commands for stepper ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun step-continue ()
  "Continue execution w/o stepping"
  (dolist (breaker-symbol *active-steppoints*)
    (delete-breakpoint breaker-symbol))
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


(defvar *never-step-fns*
  '(dbg::get-call-frame
    dbg::debug1
    invoke-debugger
    conditions::in-break
    break
    my-do-break))

(defun never-stop-complex-name-p (name)
  (equalp name '(subfunction 1 make-breakpoint)))

(defun potentially-steppable-dbg-frame-p (frame)
  (when (slot-exists-p frame 'dbg::function-name)
    (let ((fn-name (slot-value frame 'dbg::function-name)))
      (cond
       ((member fn-name *never-step-fns*) nil)
       ((never-stop-complex-name-p fn-name) nil)
       (t t)))))

;(defun dbg-find-topmost-potentially-steppable-frame ()
;  "Find potentially steppable fn on debugger stack"
;  (let ((top (dbg-top-frame)))
;    (do ((frame top (slot-value frame 'dbg::%next)))
;        ((null frame) nil)
;      )))     


(defun step-step ()
  "Step current function - WRONG"
  ; должна быть переменная "текущая шагаемая функция" и в зависимости от неё мы останавливаемся на step-поинтах или не останавливаемся
  (let ((frame DBG::*dbg-eval-frame*))
    (cond
     ((not (potentially-steppable-dbg-frame-p frame))
      (y-or-n-p "Selected frame is not steppable"))
     ((not (function-has-step-points-p (slot-value frame 'dbg::function-name)))
      (stepize-fn (slot-value frame 'DBG::function-name))
      (dbg::dbg-continue))
     (t
      (DBG::dbg-continue))
     )))

(defun step-into ()
  "Step into function to call"
  (cond
   ((breaker-symbol-p *stepper-call-to*)
    (
  
    
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


;(defun test-fn-1 (x)
;  (+ 2 (subroutine-of-x (subroutine-of-x x))))

(defun test-fn-1 (x)
  ;(+ x 1)
  (cond
   ((eq x 2)
    (test-fn-1 (+ x 1)))
   (t 
    (subroutine-of-x-and-y x :y 1)
    ))
  )

(disassemble #'test-fn-1)
(stepize-fn #'test-fn-1)
(disassemble #'test-fn-1)

(format t "~%calling test-fn-1 (no source code location)...")
(defun do-test-1 ()
  (let (
        ;(*stepping-enabled* t)
        )
    (test-fn-1 2)))
(do-test-1)



; some useful staff
;; system::compute-callable-constants #'test-fn-5 - возвращает интересноенам.
; (LISPWORKS-TOOLS::inspect-an-object #'test-fn-2)
; SYSTEM::disassembly-objetc-find-reference-for-offset coco 28 
; делает то, что мы уже начились
; RAW::fixup-moved-function интересно, но вряд ли разберусь. 
