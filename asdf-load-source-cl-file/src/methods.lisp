;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
(in-package :asdf-load-source-cl-file)

(defmethod asdf:output-files :around ((op asdf:compile-op) (c load-source-cl-file))
  (let* ((output-if-it-was-cl-source-file (call-next-method)))
    (loop for o in output-if-it-was-cl-source-file
      :for filename = (pathname-name o)
      :for fake-fasl-filename = (make-pathname :name filename :type "fake-lsof-fasl"
                                             :defaults o)
      :collect fake-fasl-filename)))

(defmethod asdf:perform ((op asdf:compile-op) (c load-source-cl-file))
  (dolist (o (asdf:output-files op c))
    (with-open-file (ou o :direction :output :if-does-not-exist :create :if-exists :supersede)
      (format ou "fake compile succeeded"))))

(defmethod asdf:perform ((op asdf:load-op) (c load-source-cl-file))
  (dolist (o (asdf:input-files (make-instance 'asdf:compile-op) c))
    (load o)))



