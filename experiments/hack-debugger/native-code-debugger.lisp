(def-merge-packages::! :npd
  (:use :cl :hcl :lispworks :mp :system)
  (:shadowing-import-from #:system #:with-fast-lock)
  )

(in-package :npd)

(defvar *current-smashed-function* nil
  "Здесь сохраняется адрес той функции, вызов которой мы подменили")

(defvar *do-break* nil
  "Если это истина, то вызываем break в maybe-break, а затем *current-smashed-function*. Если нет, то не вызываем")
  

(defun maybe-break (&rest args)
  (declare (ignorable args))
  (when (or t *do-break*)
    (print "native stepper break"))
  ;(apply *current-smashed-function* args)
  )

(defparameter *maybe-break-function* #'maybe-break)


(defun extract-address-from-function (function)
  "Возвращает адрес по объекту функции"
  (assert (functionp function))
  (let* ((text (format nil "~A" function))
         (length (length text))
         (offset (- length 9))
         (hex-address (subseq text offset (+ offset 8)))
         (*read-base* 16)
         (address (read-from-string hex-address)))
    address))

(defun poke (addr byte)
  (setf (fli:dereference (fli:make-pointer :address addr :type :unsigned-byte)) byte))

(defun peek-byte (addr)
  (fli:dereference (fli:make-pointer :address addr :type :unsigned-byte)))

(defun read-byte-from-hex-string (s)
  (parse-integer s :radix #10r16))


(defun extract-n-bytes (addr n) 
  "Достаёт n байт и возвращает список"
  (let (result)
    (dotimes (i n)
      (push (peek-byte addr) result)
      (incf i)
      (incf addr))
    (nreverse result)))
    

(defparameter +number-of-bytes-in-word+ 4)

(defun extract-function-call-from-offset (function-object offset)
  (let* ((dest-address (+ (extract-address-from-function function-object) offset))
         (result nil)
         (first-byte (peek-byte dest-address))
         second-byte
         (call-address-as-list-of-hex-strings nil))
    (cond
     ((= first-byte #xE8)
      (setf call-address-as-list-of-hex-strings
            (extract-n-bytes (+ dest-address 1) 4))
      (setf result (list* first-byte call-address-as-list-of-hex-strings))
      result)
     ((and (= #xFF first-byte)
           (= #x15 (setf second-byte (peek-byte (+ dest-address 1)))))
      (setf call-address-as-list-of-hex-strings
            (extract-n-bytes (+ dest-address 2) +number-of-bytes-in-word+))
      (setf result (list* first-byte second-byte call-address-as-list-of-hex-strings)))
     (t
      (error "extract-function-call-from-offset is unable to handle ~S command" first-byte)))))
   
;    (dotimes (i 5)

(defun poke-opcode (function-object offset opcode-list)
  (let* ((dest-address (+ (extract-address-from-function function-object) offset))
         )
    (dolist (byte opcode-list)
      (poke dest-address byte) ; read-byte-from-hex-string byte))
      (incf dest-address))
    (format t "~%dest address is ~X~%poke opcode ~S" dest-address opcode-list)))

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

#|(defun maybe-twos-complement (x)
  "Вроде не нужна?"
  (cond
   ((>= x 0) x)
   (t 
    (int32-to-integer (int32-1+ (int32-lognot (integer-to-int32 (- x)))))
   )))|#

(defun calc-call-opcode (fn-address offset replace-to-address)
  "Вычисляет адрес, чтобы вместо прямого вывова был вызов replace-to-address"
  (let ((call-offset (- replace-to-address fn-address offset 5)))
    (list* #xE8 (reverse
                 (n-to-32-bytes
                  ;(maybe-twos-complement
                   call-offset
                   ;)
                   )))))

(defparameter +hackish-address-shift+ 5)

(defun calc-indirect-opcode (replace-to-address)
  "Вычисляет адрес, чтобы вместо косвенного вывова был вызов replace-to-address"
  (let ((call-offset replace-to-address))
    (list* #xFF #x15 (reverse
                      (n-to-32-bytes
                  ;(maybe-twos-complement
                       call-offset
                   ;)
                       )))))

(defparameter +hackish-symbol-value-offset+ 4)

(defun set-breakpoint-in-function (function-object offset)
  (progn ; with-other-threads-disabled
    (gc-generation t) ; no way to guarantee absence of gc.
    (assert (functionp function-object))
    (assert (integerp offset))
    (let* (fn-address 
           new-opcode
           current-call
           replace-to-address)
      (gc-generation t)
      (setf current-call (extract-function-call-from-offset function-object offset))
      (gc-generation t)
      (setf fn-address (extract-address-from-function function-object))
      (setf new-opcode
          (cond
           ((every '= '(#xFF #x15) current-call)
            (setf replace-to-address (+ (object-address 'maybe-break)
                                        +hackish-symbol-value-offset+))
            (calc-indirect-opcode ; fn-address offset
             replace-to-address))
           ((= #xE8 (first current-call))
            (setf replace-to-address (extract-address-from-function #'maybe-break))
            (calc-call-opcode fn-address offset replace-to-address))
           (t
            (error "unknown command ~S to set breakpoint" current-call))))
    ;(format t "~%fn-address=~X, replace-to-address=~X, current-call=~S"
    ;        fn-address replace-to-address current-call)
      (poke-opcode function-object offset new-opcode)
    ;(normal-gc)
      )))

; (defun unset-breakpoint-in-function (

(defun function-to-smash () (print "function-to-smash is running"))
(defun fn-to-smash-2 () (print "fn-to-smash-2 is running"))


(defun extract-function-breakable-points (function-object)
  (assert (functionp function-object))
  (let* ((constants (SYSTEM::function-constants function-object))
         (offsets-vector (first constants)))))

(defparameter test-fn "just an anchor")

;; тест для непосредственно вызываемых функций
#|
(compile `(defun test-fn ()
            (funcall ,#'function-to-smash)
            ;(function-to-smash)
            ))
|#


(defun test-fn ()
  (function-to-smash))

(format t "~%old call is ~S" (extract-function-call-from-offset #'test-fn 23))
(eval '(set-breakpoint-in-function #'test-fn 23))
(format t "~%new call is ~S" (extract-function-call-from-offset #'test-fn 23))
(test-fn)
(LISPWORKS-TOOLS::inspect-an-object #'test-fn)

  
