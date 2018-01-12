;;; -*- Encoding: utf-8; system :buddens-reader ; -*-

;;; Портативные функции, обобщающие разные реализации CL 

(named-readtables:in-readtable nil)

(def-merge-packages::! :cl-impl
                       (:always t)
                       (:nicknames :sbcl-in-ccl)
                       (:use #+CCL :ccl
                             :cl :cl-advice :defpackage-budden)
                       (:export "
  cl-impl:truly-the
  cl-impl:while 
  cl-impl:singleton-p
  cl-impl:symbolicate
  cl-impl:store-defsetf-method-simple
  cl-impl:fast-&rest-nth
  cl-impl:filter-dolist-declarations
  cl-impl:coerce-callable-to-fun
  cl-impl:do-rest-arg
  cl-impl:output-object
  cl-impl:index
  DEFPACKAGE-BUDDEN:find-package-or-lose-a-la-sbcl
  DEFPACKAGE-BUDDEN:find-undeleted-package-or-lose-a-la-sbcl
"))

(in-package :cl-impl)

(defun coerce-callable-to-fun (callable)
  #+SBCL (sb-impl::%coerce-callable-to-fun callable)
  #-SBCL (coerce callable 'function))

(defmacro truly-the (type-specifier form) `(the ,type-specifier ,form))

;; from SBCL
(defun filter-dolist-declarations (decls)
  (mapcar (lambda (decl)
            `(declare ,@(remove-if
                         (lambda (clause)
                           (and (consp clause)
                                (or (eq (car clause) 'type)
                                    (eq (car clause) 'ignore))))
                         (cdr decl))))
          decls))

; since they use tagbody, while & until BOTH return NIL
(defmacro while (test &body body)
  (let ((testlab (gensym))
        (toplab (gensym)))
    `(tagbody
       (go ,testlab)
      ,toplab
      (progn ,@body)
      ,testlab
      (when ,test (go ,toplab)))))

(declaim (inline singleton-p))
(defun singleton-p (list)
  (and (cl:listp list) (null (cl:rest list)) list))

;;; Concatenate together the names of some strings and symbols,
;;; producing a symbol in the current package.
(defun symbolicate (&rest things)
  #+SBCL (declare (cl:dynamic-extent things))
  (values
     (intern
      (if (singleton-p things)
          (string (first things))
          (let* ((length (cl:reduce #'+ things
                                 :key (lambda (x) (cl:length (string x)))))
                 (name (cl:make-array length :element-type 'cl:character))
                 (index 0))
            (cl:dolist (thing things name)
              (let ((x (string thing)))
                (cl:replace name x :start1 index)
                (incf index (cl:length x)))))))))

(defun store-defsetf-method-simple (name fn)
  #+SBCL (sb-impl::%defsetf name fn)
  #+CCL (ccl::store-setf-method name fn)
  #-(OR SBCL CCL) (let "store-defsetf-method-simple is not implemented"))

(defmacro fast-&rest-nth (n obj)
  #+SBCL `(sb-impl::fast-&rest-nth ,n ,obj)
  #+CCL `(nth ,n ,obj)
  #-(OR SBCL CCL) (let "fast-&rest-nth is not implemented"))

(deftype index ()
  #+SBCL 'sb-impl::index
  #-SBCL `(integer 0 ,array-dimension-limit))

(defmacro do-rest-arg (((var &optional index-var) rest-var
                        &optional (start 0) result)
                       &body body)
  ;; If the &REST arg never needs to be reified, this is slightly quicker
  ;; than using a DX list.
  (let ((index (gensym "INDEX")))
    `(let ((,index ,start))
       (loop
        (cond ((< (truly-the index ,index) (length ,rest-var))
               (let ((,var (fast-&rest-nth ,index ,rest-var))
                     ,@(if index-var `((,index-var ,index))))
                 ,@body)
               (incf ,index))
              (t
               (return ,result)))))))

(declaim (inline output-object))
(defun output-object (object stream)
  #+SBCL (sb-kernel:output-object object stream)
  #+CCL (ccl::write-1 object stream)
  #-(OR SBCL CCL) (let "output-object is not implemented"))
