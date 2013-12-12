;;; -*- Encoding: utf-8; -*-
;;; infrastructure for making lispworks debugger macro friendly

(in-package :lw-macro-friendly-dbg)


(defadvice (compiler::wombat-2 hack-two-conses :around)
    (form &optional (table COMPILER::*source-level-form-table*))
  (setf *w-table* table
        *w-form* form)
  (call-next-advice form table))


(defun find-source-address-in-a-hash (source)
  (gethash source *w-table*)) 


(defun set-source-location-substitution (source-place real-code)
  "source-place - место в настоящем исходнике, 
   real-code - форма, к-рая будет там показывться.
   "
  (typecase *compile-time-substitution-table*
    (hash-table
     (unless (eq source-place real-code)
       (setf (gethash real-code *compile-time-substitution-table*) source-place)
       t
       ))
    (t
     (warn "set-source-location-substitution is out of with-source-location-substitutions scope")
     nil)
    )
  )


(defun end-source-location-substitutions-fn ()
  "Converts map between conses to map between numeric addresses"
  (incf *with-source-location-substitutions-level* -1) 
  (unless (= *with-source-location-substitutions-level* 0)
    (return-from END-SOURCE-LOCATION-SUBSTITUTIONS-FN nil))
  (let ((address-substitution-table (make-hash-table :test 'eq)))
    (maphash
     (lambda (real-code source-place)
       (let ((source-place-address (find-source-address-in-a-hash source-place)))
         (typecase source-place-address
           (integer
            (let ((real-code-address (find-source-address-in-a-hash real-code)))
              (typecase real-code-address
                (integer
                 (unless (eql real-code-address source-place-address)
                   (setf (gethash real-code-address address-substitution-table)
                         source-place-address)))
                (t #+nil (warn "code-address not found for ~S" real-code)))))
           (COMPILER::MULTIPLE-TRANSFORMS-RECORD
            (warn "source-place-address=~S" source-place-address)
            )
           (t #+nil (warn "source-place address not found for ~S" source-place)))))
     *compile-time-substitution-table*)
    ;(print `("number of substitutions is" ,(hash-table-count *address-substitution-table*)) *trace-output*)
    nil)) 



(defadvice (COMPILER::process-form bind-compile-time-substitution-table :around
                                   :documentation "Isolates *compile-time-substitution-table* variable from other processes")
    (i-form)
  (WITH-BOUND-COMPILE-TIME-SUBSTITUTION-TABLE
    (call-next-advice i-form)))

(defadvice (lispworks-tools::stepize bind-compile-time-substitution-table :around
                                     :documentation "Isolates *compile-time-substitution-table* variable from other processes")
    (context form)
  (WITH-BOUND-COMPILE-TIME-SUBSTITUTION-TABLE
    (CALL-NEXT-ADVICE context form)))
     

(defun begin-source-location-substitutions-fn ()
  (setf *compile-time-substitution-table*
        (or *compile-time-substitution-table* (make-hash-table :test 'eq)))
  (incf *with-source-location-substitutions-level*) 
  )




(defun hack-source-level-form-table (real-code)
  "Находит real-code в *COMPILE-TIME-SUBSTITUTION-TABLE* и подставляет адрес
из source-place в него. Возвращает код или подменённый код, а вторым значением - t, если сделала изменения"
  (let* ((maybe-source-form
          (typecase *COMPILE-TIME-SUBSTITUTION-TABLE*
            (HASH-TABLE
             (gethash real-code *COMPILE-TIME-SUBSTITUTION-TABLE* real-code))
            (t
             real-code)))
         (maybe-other-address
          (and maybe-source-form
               (typecase COMPILER::*SOURCE-LEVEL-FORM-TABLE*
                 (hash-table
                  (gethash maybe-source-form COMPILER::*SOURCE-LEVEL-FORM-TABLE*
                           ))))))
    (typecase COMPILER::*SOURCE-LEVEL-FORM-TABLE*
      (hash-table
       (cond ((and (numberp maybe-other-address)
                   (not (eq MAYBE-SOURCE-FORM real-code))
                   (not (eq (gethash real-code COMPILER::*SOURCE-LEVEL-FORM-TABLE*)
                            MAYBE-OTHER-ADDRESS)))
              (setf (gethash real-code COMPILER::*SOURCE-LEVEL-FORM-TABLE*)
                    maybe-other-address)
              (values maybe-source-form t))
             (t
              (values maybe-source-form nil))))
      (t
       (values MAYBE-SOURCE-FORM nil)))))


(defadvice (compiler::get-form-path take-code-from-compile-time-substitution-table :around)
    (form)
  (CALL-NEXT-ADVICE (HACK-SOURCE-LEVEL-FORM-TABLE form))
  )

