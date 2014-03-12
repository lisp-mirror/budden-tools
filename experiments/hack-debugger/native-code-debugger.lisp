(def-merge-packages::! :npd
  (:use :cl :hcl :lispworks :mp :system)
  (:shadowing-import-from #:system #:with-fast-lock)
  )

(in-package :npd)

#| 
  ��� ��� ����? ����� ��������� ����� call [address] � call address �� ����� ����� �-��, �-��� ��������� ���
  � ����� ������� ���������� ��� �-��, �-��� ���� ����������. ���� ������ ����� ������. 
  ������� ������� �� ����� ����. 

 ���� ���������� ��������:
 - ������� ��� �����
 - ������� "����������� ������� � �����":
 -- ��� �� �������� � ������� ��� �����, ��� ����� �����������. ����� ������ ����������
 -- ����� ������ ��������� �����������, ������������� "���������� �� ��������"
 -- ���� ������ "continue", ��������� ��� ����� ���������� 

|#


(defvar *stepping-enabled* t
  "���� ��� ������, �� ��� ������� ���-�� ������")
  

(defun my-do-break (fn args)
  (declare (ignorable args))
  (when *stepping-enabled*
    (format t "~%native stepper break, fn=~S, args=~S" fn args))
  (apply fn args)
  )

(defvar *active-breakpoints* nil "list of created breakpoints")

(defun make-breakpoint (function-name offset)
  "breakpoint is indeed a closure and a change in a function code. 
  Only function calls can be used as a breakpoint locations, otherwise debugger
  is unable to find source location so the entire mechanism becomes useless"
  (let* ((pointer (cons nil nil))
         (breaker ; lambda call to which is places instead of original call
          (lambda (&rest args) (my-do-break (car pointer) args)))
         (smashed-fn ; function which was called before
          (set-breakpoint-in-function function-name offset breaker)))
    (setf (car pointer) smashed-fn)
    (push breaker *active-breakpoints*)
    (values smashed-fn breaker)))

(defun extract-address-from-function (function)
  "���������� ����� �� ������� �������. Lame: extracts address from printed representation"
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
  "������ n ���� � ���������� ������"
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
  "�� ������ ��������� ��������� �����, ��� �� ���������� �� ��� ������ ��������� ��������� �����. ������� ��������� offset, ��� �������� ������ � ������ ���������� �-��. 
  �������� ������: 1 - ������, 2 - ����"
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
  "���������� ������� - ��� ��������� ��� ������ call. Return 'call' command offset in a code vector, kind of a call (1-direct,2-indirect) and a function called"
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
    (format t "~%dest address is ~X~%poke opcode ~S" dest-address opcode-list)))

(defun n-to-hex (x)
  "���������� � ������"
  (format nil "~X" x))



(defun n-to-32-bytes (x)
  "���� �� ����� 32-��������� �����, ��������� �� �����"
  (do ((quotient nil)(remainder nil)(result nil)(i 0 (+ i 1)))
      ((= i +number-of-bytes-in-word+) result)
    (multiple-value-setq (quotient remainder) (floor x #x100))
    (push remainder result)
    ;(format t "~%~X" quotient)
    (setf x quotient)
    ))

(defun 32-bytes-to-n (x)
  "��� ������ ����, ��������� ����� �� ��� (� �������� �������)"
  (assert (typep x '(or (cons integer) null)))
  (let* ((len (length x))
         (result 0))
    (dotimes (i len)
      (setf result (+ (* result #x100) (elt x (- len i 1)))))
    result))

#|(defun maybe-twos-complement (x)
  "����� �� �����?"
  (cond
   ((>= x 0) x)
   (t 
    (int32-to-integer (int32-1+ (int32-lognot (integer-to-int32 (- x)))))
   )))|#

(defparameter +hackish-address-shift+ 5)

(defun calc-call-opcode (dst-address address-to-call size)
  "��������� �����, ����� ������ ������� ������ ��� ����� replace-to-address. �������� �� size ������"
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
  "��������� �����, ����� ������ ���������� ������ ��� ����� replace-to-address. ���, �� ����, �� �����"
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
  "��������� ����� �� �������. offset at function name must point to call, either direct or indirect"
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
            ; �������� ��� ��� ������� ����������� �� �����������, �� ��� �� ��� ����� �� �����
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
      (print new-opcode)
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

(defun put-breakpoints-everywhere-on-fn (function-or-name)
  (let* ((fn (coerce function-or-name 'function))
         (offsets (extract-function-breakable-offsets fn)))
    (mapcar (lambda (o) (make-breakpoint fn o)) offsets)))
    
  
  

;;;;  -------------------------------- TESTS -------------------------------------------------

(defun fn-to-smash-of-x (x) (format t "~%fn-to-smash-of-x is running. Arg: ~S~%" x))
(defun fn-to-smash-with-no-args () (print "fn-to-smash-with-no-args is running"))
(defparameter test-fn "just an anchor")

;; ���� ��� ��������������� ���������� �������
(compile `(defun test-fn-1 ()
            (funcall ,#'fn-to-smash-of-x 3)
            ;(function-to-smash)
            ))

(eval '(make-breakpoint 'test-fn-1 33))
(test-fn-1)

(defun test-fn-2 (x)
  (fn-to-smash-of-x x))


;(format t "~%old call is ~S" (locate-call-from-next-command-offset (+ (extract-address-from-function #'test-fn-2) 27)))
(make-breakpoint 'test-fn-2 33) 
;(format t "~%new call is ~S" (locate-call-from-next-command-offset (+ (extract-address-from-function #'test-fn-2) 27)))
(test-fn-2 'test-param-for-test-fn-2
 )

; (LISPWORKS-TOOLS::inspect-an-object #'test-fn-2)

  
