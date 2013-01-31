;;; -*- Encoding: utf-8; -*-
;;; Loading of the file sets up #L readmacro 

(in-package :buddens-readtable)

;; SharpL 
(defun sharpL-reader (stream subchar n-args)
  (declare (ignore subchar))
  (let* ((form (read stream t nil t))
         (bang-vars (sort (bang-vars form) #'< :key #'bang-var-num))
         (bang-var-nums (mapcar #'bang-var-num bang-vars))
         (max-bv-num (if bang-vars
                         (reduce #'max bang-var-nums :initial-value 0)
                       0)))
    (cond 
     ((null n-args)
      (setq n-args max-bv-num))
     ((< n-args max-bv-num)
      (error "#L: digit-string ~d specifies too few arguments" n-args)))
    (let* ((bvars (let ((temp nil))
                    (dotimes (i n-args (nreverse temp))
                      (push (make-bang-var (1+ i)) temp))))
           (args (mapcar #'copy-symbol ; #'(lambda (x) (declare (ignore x)) (gensym))
                         bvars))
           (ignores (set-difference bvars bang-vars))
           (decl (if ignores `(declare (ignore .,ignores)) nil))
           (body (if (list-of-forms? form)
                     (if decl (cons decl form) form)
                   (if decl (list decl form) (list form))))
           (subbed-body (sublis (pairlis bvars args) body)))
      `#'(lambda ,args ,.subbed-body))))
  

(defun make-bang-var (n)
  (intern (format nil "!~d" n)))

(defun list-of-forms? (x)
  (and (consp x) (consp (car x))
       (not (eq (caar x) 'lambda))))

(defun bang-vars (form)
  (delete-duplicates (bang-vars-1 form '()) :test #'eq))

(defun bang-vars-1 (form vars)
  (cond
   ((consp form)
    (bang-vars-1 (cdr form)
                 (bang-vars-1 (car form) vars)))
   ((and (symbolp form) (bang-var? form)) (cons form vars))
   (t vars)))

(defun bang-var? (sym)
  (char= (char (symbol-name sym) 0) #\!))

(defun bang-var-num (sym)
  (let ((num (read-from-string (subseq (symbol-name sym) 1))))
    (if (not (and (integerp num) (> num 0)))
        (error "#L: ~a is not a valid variable specifier" sym)
      num)))

;;; Sharp-dot
(defvar *read-eval-stream* 
  "Something's wrong with #.? " 
  "Bound to current input stream in the scope of #. ") 

(defun sbcl-sharp-dot (stream sub-char numarg) 
  "code borrowed from sbcl"
  (declare (ignore sub-char numarg))
  (let ((token (read stream t nil t))
        (*read-eval-stream* stream))
    (unless *read-suppress*
      (unless *read-eval*
        (error "can't read #. from ~S while *READ-EVAL* is NIL" stream)))
    (prog1 
        (eval token)
      ))
  )

;;;; Sharp-double-quote
(defun unread-char* (char stream)
  "unreads a character to a new stream"
  (make-concatenated-stream 
   (make-string-input-stream (concatenate 'string (list char))) 
   stream))

(defun sharp-double-quote-readmacro (s c n)
  (declare (ignore n c))
  (let ((double-quote-char (read-char s)))
    (nsubstitute #\" double-quote-char
      (read (unread-char* #\" s))))) 





;(equalp #"@@" "\"")
;(equalp #"a@" "@")
;(equalp #"^$(^#content^).load(^~A.clhp^)" 
;        "$(\"#content\").load(\"~A.clhp\")")
