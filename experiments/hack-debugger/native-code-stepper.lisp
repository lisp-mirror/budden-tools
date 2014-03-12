
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :native-code-stepper
    (:use :cl :hcl :lispworks :mp :system)
    (:shadowing-import-from #:system #:with-fast-lock)
    (:export 
     #:*tracing-enabled*
     #:*stepping-enabled*
     #:set-step-points-everywhere-on-fn
     )
    ))

(in-package :native-code-stepper)

#| 
  Что тут есть? Умеем подменять вызов call [address] и call address на вызов нашей ф-ии, к-рая выполняет код
  и затем передаёт выполнение той ф-ии, к-рая была изначально. Ведём список таких подмен. 
  Убирать подмену не умеем пока. 

 План дальнейших действий:
 - убираем все консы
 - функция "подготовить функцию к степу":
 -- идём по векторам и находим все точки, где можно притулиться. Везде ставим брекпойнты
 -- когда первый брекпойнт срабатывает, устанавливаем "прерывание на возврате"
 -- если делаем "continue", отключаем все такие брекпойнты 

|#


(defvar *tracing-enabled* t
  "If true, step points are printed")

(defvar *stepping-enabled* nil
  "If this is true, every step point breaks")
  

(defun my-do-break (call-from call-to args)
  (declare (ignorable args))
  (when *tracing-enabled*
    (format t "~%native stepper break, from ~S into ~S, args=~S" call-from call-to args))
  (when *stepping-enabled*
    (break "Stepper break before call from ~S into ~S with args=~S~%Use GUI debugger to find current source" call-from call-to args))
  (apply call-to args)
  )

(dolist (name '(my-do-break make-breakpoint break invoke-debugger))
  (pushnew name DBG::*hidden-symbols*))

(defvar *active-breakpoints* nil "list of created breakpoints")

(defun make-breakpoint (function-name offset)
  "breakpoint is indeed a closure and a change in a function code. 
  Only function calls can be used as a breakpoint locations, otherwise debugger
  is unable to find source location so the entire mechanism becomes useless"
  (let* ((pointer (cons nil nil))
         (breaker ; lambda call to which is places instead of original call
          (lambda (&rest args)
            (my-do-break function-name (car pointer) args)))
         (smashed-fn ; function call to which we substituted
          (set-breakpoint-in-function function-name offset breaker)))
    (setf (car pointer) smashed-fn)
    (push breaker *active-breakpoints*)
    (values smashed-fn breaker)))

(defun extract-address-from-function (function)
  "Возвращает адрес по объекту функции. Lame: extracts address from printed representation"
  (assert (functionp function))
  (let* ((text (format nil "~A" function))
         (length (length text))
         (offset (- length 9))
         (hex-address (subseq text offset (+ offset 8)))
         (*read-base* 16)
         (address (read-from-string hex-address)))
    address))

(defun poke (addr byte)
  "Write byte at addr"
  (setf (fli:dereference (fli:make-pointer :address addr :type :unsigned-byte)) byte))

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


(defun locate-call-from-next-command-offset-inner (next-command-address next-command-offset)
  "По адресу находится косвенный вызов, или по следующему за ним адресу находится косвенный вызов. Извлечь уточнённый offset, вид операции вызова и объект вызываемой ф-ии. 
  Операция вызова: 1 - прямая, 2 - косв"
  (let* ((approx-dest-address (- next-command-address +length-of-indirect-call-command+))
         (this-command-offset (- next-command-offset +length-of-indirect-call-command+))
         (apd approx-dest-address)
         (first-byte (peek-byte apd))
         (second-byte (peek-byte (+ 1 apd)))
         (call-address-as-list-of-bytes nil))
    (cond
     ((= second-byte #xE8 ; direct call
         ) 
      (setf call-address-as-list-of-bytes
            (extract-n-bytes (+ apd 2) 4))
      (values (+ this-command-offset 1) +direct-call+ call-address-as-list-of-bytes)
      )
     ((and (= #xFF first-byte)
           (= #x15 (peek-byte (+ apd 1)))
           ; indirect call 
           )
      (setf call-address-as-list-of-bytes
            (extract-n-bytes (+ apd 2) +number-of-bytes-in-word+))
      (values this-command-offset +indirect-call+ call-address-as-list-of-bytes))
     (t
      (error "extract-function-call-from-offset is unable to handle ~S command" first-byte)))))


(defun locate-call-from-next-command-offset (next-command-address next-command-offset)
  "Предыдущая команда - это косвенный или прямой call. Return 'call' command offset in a code vector, kind of a call (1-direct,2-indirect) and a function called"
  (multiple-value-bind (offset call-kind bytes)
      (locate-call-from-next-command-offset-inner next-command-address next-command-offset)
    (let (address)
      (cond
       ((= call-kind +direct-call+)
        (setf address (32-bytes-to-n bytes))
        (setf address (+ address next-command-address)))
       ((= call-kind +indirect-call+)
        (setf address (32-bytes-to-n bytes))
        )
       (t
        (error "unknown call-kind")
        )
       )
       (values offset
               call-kind
               (pointer-from-address address)))))
  

   
;    (dotimes (i 5)

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

(defun set-breakpoint-in-function (function-or-name next-command-offset fn-to-put)
  "Подменяет вызов на функцию. offset at function name must point to call, either direct or indirect"
  (progn ; with-other-threads-disabled
    (let* ((function-object (coerce function-or-name 'function))
           fn-address
           offset ; offset of command to modify
           new-opcode
           replace-to-address
           call-kind
           current-fn
           )
      (assert (functionp function-object))
      (assert (integerp next-command-offset))
      (gc-generation t)
      (setf fn-address (extract-address-from-function function-object))
      (multiple-value-setq (offset call-kind current-fn)
          (locate-call-from-next-command-offset (+ fn-address next-command-offset) next-command-offset))
      (setf new-opcode
          (ecase call-kind
           (2
            ; полезный код для подмены именованной на именованную, но нам он тут вроде не нужен
            ; (setf replace-to-address (+ (object-address fn-to-put) +hackish-symbol-value-offset+))
            ; (calc-indirect-opcode replace-to-address))
            (gc-generation t)
            (setf replace-to-address (extract-address-from-function fn-to-put))
            (calc-call-opcode (+ (extract-address-from-function function-object) offset) replace-to-address 6)
            )
           (1
            (gc-generation t)
            (setf replace-to-address (extract-address-from-function fn-to-put))
            (calc-call-opcode (+ (extract-address-from-function function-object) offset) replace-to-address 5))
           ))
    ;(format t "~%fn-address=~X, replace-to-address=~X, current-call=~S"
    ;        fn-address replace-to-address current-call)
      (poke-opcode function-object offset new-opcode)
      (assert (or (functionp current-fn) (and (symbolp current-fn)
                                              (fboundp current-fn))))
      ;(print new-opcode)
      current-fn
    ;(normal-gc)
      )))

; (defun unset-breakpoint-in-function (



(defun extract-function-breakable-offsets (function-object)
  "Unfinished"
  (assert (functionp function-object))
  (let* ((constants (SYSTEM::function-constants function-object))
         (first-constants (first constants))
         (locations (elt first-constants 6))
         (result nil))
    (dotimes (i (length locations))
      (when (= 0 (mod i 3))
        (push (elt locations i) result)))
    result))

(defun set-step-points-everywhere-on-fn (function-or-name)
  (let* ((fn (coerce function-or-name 'function))
         (offsets (extract-function-breakable-offsets fn)))
    (mapcar (lambda (o) (make-breakpoint fn o)) offsets)))
    
  
  

;;;;  -------------------------------- TESTS -------------------------------------------------

(defun subroutine-of-x (x) (format t "~%subroutine-of-x is running. Arg: ~S~%" x) (list 0 x))
(defun subroutine-with-no-args () (print "subroutine-with-no-args is running"))
(defparameter test-fn "just an anchor")

;; тест для непосредственно вызываемых функций
(compile `(defun test-fn-1 ()
            (funcall ,#'subroutine-of-x 3)
            ))

(eval '(make-breakpoint 'test-fn-1 33))

(format t "~%calling test-fn-1 (no source code location)...")

(test-fn-1)

(defun test-fn-2 (x)
  (subroutine-of-x x))


; (make-breakpoint 'test-fn-2 33) 
(set-step-points-everywhere-on-fn 'test-fn-2)

(format t "~%calling test-fn-2 in step mode...")

(let ((*stepping-enabled* t))
  (test-fn-2 'test-param-for-test-fn-2))

(defun test-fn-3 (x)
  (subroutine-of-x (subroutine-of-x x)))

(set-step-points-everywhere-on-fn 'test-fn-3)
(format t "~%calling test-fn-3...")
(format t "~%test-fn-3 returned ~S~%" (test-fn-3 1))

(defun test-fn-4 (x)
  (+ (+ x 1))) 
; breakpoint do not work here for an unknown reason

; (LISPWORKS-TOOLS::inspect-an-object #'test-fn-2)

  
