;;; Universal location recorder

#|
����� ����. �� ����� ����������� �������� � ���� ���������� �������. 
���������� ����� ���������� � ����������, ��������� �� ��������� ����� �����.
��� ��� �� ������ ����� ����� �����, ��� ���� �������� ������. 

������ ������ - �� �������� ��� �����, � ���� �������������� ���������. 

����� ����� ���� ��������������?

������� � `(:l ,???) 
pd/pds � ������ ��� ����
fbody
:~


�� ������ �� �������� ���� ��� ������ �� ���� �������������� � ��� ����� ����� 
������ ��������� � ������ ��������. ���� � ���� ���������� ��������� �����������
����� ������, �� ������ ����� ����� ��� ����� �������. 

��� ��������������, � ���� �������, ����� �������� �� ����������, ����� ���:
(str+)
(format)
(with-output-to-string (s) (princ ... s))

��� ������� �������� ��� ��� ��������� ���, ��� ��� ������������.
����� ����������� �������� ������ � �������� � ������� gray streams.
���������� ����� �� ������� ������ � ������ change-class, 
����� ����������� ������ ��������. � ���������� ������ ����� �����
��������� �������� ��� �������. 

��� �������� � ������, ����� ����� � ������ eq ���-�������, ��� ����������
*nplm = *non-persistent-locations-memory*
��� ������, ����� ����� � ������� string-equal ���-�������, ��� ����������
*nplf = *non-persistent-locations-files*

��� with-output-to-string ��������� ������� ��� �� ����������. 
������, �� �������� ����������� ���������� � ������������ � �����
�������� ����������� �� ������. ����������� �������� ��������� ���������� (make-symbol)

���� � ��� ���� �����-�� ����� ��� �� �������� ������, �� ������ � ��� ����� ������ ��� ���. 
�� ���������� �������� ����. ��� ����� ������������ �����������. � ������ ������ �� 
�������� make-symbol � ��������� � ������ ���-�������
*slmd = *stream-location-map-delegates*
� ������ ������� �� ����� ���� ��� ������. 

����� ����� ���� ������� � ����-��������, ������� ��� ������.
� ������ ����-�������, ����� ������ ������ ������ � ����� �������, �� �� � ��� �������
� ������ ������, ����� ������ ������ ��� ���� ����������
� ������ �����, ����� ������ ������ ��� ���� ���������� �����, �� ��� ��������
�� � ���������� 

� ����� ������,���������� ���������� �� ������ ���� �������

������ N1 - ��� persistent ��������. 

olm = object-location-map
segment-sources = ses
source-location = slo

� ��������� ����������-��������. ������� fbody ����� ������ � load-time, ��� ��� ������ 
��������������� ����. 


API


extract-source-filename-from-stream - ���� ����� ������ � ������, ���������� ��� ���
l/ohbr = location/object-has-been-read - ��������� ������ �� ���������. ������� ��� ��������������
read-and-record-non-persistent-location - ��������� ������ � ��������� ��� ��������������

l/princ - ���������� ������ � ����� � ���������� ������������� ��� location � �����, 
��������� � ���� �������

l/add-to-location-map delegate dst-beg dst-end object src src-beg src-end - ������������ � ���������
������ � ������������

l/pass-from-delegate-to-object delegate object - ��������� ����������, ��������������� � ���������, �� ������. ��� ���� ������� - ���� ������� ��� ���-�� ���� � ������� ��� ����������, ���� ����� ��������.
l/pass-from-delegate-to-file - �� ��, ��� ����� (???)

l/return-object-recording-location = l/rorl - �����, ������� ������� ��������� ������� � ������ ���
srcpl - symbol-readmacro. ��������� ������ � ����������������� ����������� ��� ����� � load-time.
(���� ����� �������, ����� �� ������� ���������� ������������ ����� ���� ���-��������)
|#

(in-package :budden-tools)



(defvar *nplm 
  #+NIL (make-hash-table :test 'eq)
  (SWANK-BACKEND:MAKE-WEAK-KEY-HASH-TABLE :test 'eq) 
  "��������� �������� � ������������ ��� �������� � ������")

(defvar *nplf 
  (make-hash-table :test 'equal)
  "�������� � ������������, ��� �̨� ������ (namestring). ������� �������, �.�. ����� ���������� �����")

(defvar *slmd
  (SWANK-BACKEND:MAKE-WEAK-KEY-HASH-TABLE :test 'eq)
  "�������� �������")

(defstruct olm ; ����� ���������� ��� ������ 
  ses-list ; ������������� ������ �������� ses
  )

; � ������������ ���� (with-output-to-string) �� � ��� ������������� ���������� � ������������. 
; �� ����� ������ ����� ���� ����������� olm � ������ destination � ������������� �
; *source-code-output-stream-destinations* ��� ������� ������. ��������:

;(let ((l-i-h (make-olm)))
;  (values 
;   (with-output-to-string (stream)
;     (setf (gethash stream *source-code-output-stream-destinations*) l-i-h)
;     (princ-and-integrate-location "asdf" stream)
;     )
;   l-i-h))

(defstruct ses ; ��� ������� ������� ����� �������� ��������� ����������. ��� ��������� ����� ������� � ��������� destination, ��������, 
    ; � olm
  beg ; � �������������� �������
  end
  sources ; ������ ��������� slo, �������� ��������� ������� ������� (���� ��� ����)
  )

(defstruct slo ; ������ ������ ������ ������
  source ; ��������, ��� �����
  beg ; � �������� �������
  end)



(defmacro l/with-output-to-string ((VAR &REST SOME-MORE) 
                                   &BODY BODY)
  (with-gensyms (location-delegate)
  `(let (,location-delegate)
     (l/pass-from-delegate-to-object 
      (with-output-to-string (,var ,@some-more)
        (setf ,location-delegate (get-stream-location-map-delegate ,var :if-not-exists :create))
        ,@body
        #+nil (print (gethash ,location-delegate *nplm)))
      ,location-delegate))))

(defun l/pass-from-delegate-to-object (object delegate)
  "delegate ����� ���� nil. ���������� ������"
  (unless (eq delegate object)
    (setf (gethash object *nplm) (simplify-location-map (gethash delegate *nplm)))
    (remhash delegate *nplm)
    )
  object)

(defun l/pass-from-stream-to-file (stream)
  "������ ���� ������� � �������. ����������� �� � ������"
  (let1 delegate (get-stream-location-map-delegate stream)
    (setf (gethash (namestring (extract-source-filename-from-stream stream))
                   *nplf)
          (simplify-object-location-map delegate))
    (remhash delegate *nplm))
  nil)      

(defun l/add-to-location-map (delegate dst-beg dst-end object)
  (let* ((oli (gethash object *nplm))
         (dli (gethash delegate *nplm))
         (result nil)
         )
    (when oli
      (unless dli
        (setf dli 
              (setf (gethash delegate *nplm)
                    (make-olm))))
      (setf result (make-ses :beg dst-beg :end dst-end :sources (list oli)))
      (push result (olm-ses-list dli)))
    result
    )
  )

(defun l/subseq (string start &optional end (simplify t))
  "������ �� ��������� ��������"
 (proga
   (let map (if simplify
                 (simplify-object-location-map string)
               (get-non-persistent-object-locations string)))
    (let result (subseq string start end))
    (when map
      ; ����� �������������. ������ � ����� ����� ���� ��������
      (the* olm map)
      (_f copy-olm map) ; �������� �����, ���� ��� ��������������������� ��� ����� ������      
      (let len (length string))
      (setf end (if end (min end len) len))
      (with-conc-name map olm-
        (iter
          (:for sub in map.ses-list)
          (with-conc-name sub ses-
            (cond
             ((or (> start sub.end) (< end sub.beg) ; no intersection
                  ))
             (t
              (let ((new-sub (copy-ses sub)))
                (setf (ses-beg new-sub) (max (- sub.beg start) 0)
                      (ses-end new-sub) (- (min sub.end end) start))
                (:collect new-sub into new-ses-list)))))
          (:finally (setf map.ses-list new-ses-list))))
      (setf (get-non-persistent-object-locations result) map)
      )
    result))


(defun l/find-sources-in-file (filename offset &key (strict nil))
  "��� ����, ����� �����"
  (let1 res
      (l/find-sources-in-map (gethash (namestring filename) *nplf) offset :strict strict)
    (_f remove-duplicates res :test 'equalp)
    (if (and res (> (length res) 1))
        (list 
         (nth (progn 
                (format *query-io* "Sources are ~A~%Which (from 1)?" res) 
                (- (read *query-io*) 1))
              res))
      res)))

(defun l/find-sources-in-map (map offset &key (strict nil))
  "���� �����, ����� �����, ������ ����� ��������. ���������� ������ ����� (���� ������ �����). ���� strict=nil, 
����� ���������� � ������ ��� ����� (��� �������)"
  (etypecase map
    (null nil)
    (slo
     (let ((source (slo-source map))
           (beg (slo-beg map))
           (end (slo-end map)))
       (when (or source (not strict))
         (list (list source (+ offset beg) end)))))
    (ses
     (let ((beg (ses-beg map))
           (end (ses-end map)))
       (when (and (<= beg offset) (< offset end))
         (iter (:for slo in (ses-sources map))
           (:for pos = (l/find-sources-in-map slo (- offset beg) 
                                              :strict strict))
           (when pos 
             (flet ((outer-collecting (x) (:collecting x)))
               (iter (:for (f1 b1 e1) in pos)
                 (outer-collecting `(,f1 ,b1 ,e1)))))))))
    (olm
     (iter (:for ses in (olm-ses-list map))
       (:for poss = (l/find-sources-in-map ses offset :strict strict))
       (when poss (:appending poss))))))
     

(defun move-olm-by-offset (olm offset)
  "Destructively modifies self, copies subs"
  (with-conc-name olm olm- 
    (setf olm.ses-list (copy-list olm.ses-list))
    (iter (:for ses on olm.ses-list)
      (when (car ses)
        (_f copy-ses (car ses))
        (:for carses = (car ses))
        (incf (ses-beg carses) offset)
        (incf (ses-end carses) offset)))))

(defun simplify-olm (olm &key sort)
  "����� ����������"
  (proga
    (unless olm (return-from simplify-olm olm))
    (with-conc-name olm olm-)
    (let already-copied nil)
    (iter 
      (:for sub1 in olm.ses-list)
      (proga
        (cond
         ((ses-p sub1)
          (with-conc-name sub1 ses-)
          (cond 
           ((= (length sub1.sources) 1)
            (let sub2 (first sub1.sources))
            (cond 
             ((olm-p sub2)
              (unless
                  already-copied
                (_f copy-olm olm)
                (setf already-copied t))
              (with-conc-name sub2 olm-)
              (_f simplify-location-map sub2)
              (move-olm-by-offset sub2 sub1.beg)
              (:appending  (mapcar 'simplify-location-map sub2.ses-list) :into new-ses-list))
             (t ; hence sub2 is not an olm
              (:collecting (simplify-location-map sub1) :into new-ses-list))))
           (t ; hence sub1.sources is longer than 1 element
            (:collecting (simplify-location-map sub1) :into new-ses-list))))
         (t ; hence sub1 is not ses-p
          (:collecting (simplify-location-map sub1) :into new-ses-list))))
      (:finally 
       (setf olm.ses-list new-ses-list)))
    (assert (every 'ses-p olm.ses-list))
    (when sort 
      (setf olm.ses-list (sort olm.ses-list #'< :key 'ses-beg)))
    olm))


(defun simplify-ses (ses)
  (with-conc-name ses ses-
    (_f copy-ses ses)
    (iter 
      (flet ((outer-append (x) (:appending x :into new-sources)))
        (:for sub = (simplify-location-map (pop ses.sources)))
        (:while sub)
        (:collect sub into new-sources)
        #+nil (cond ((olm-p sub)
               (with-conc-name sub olm-
                 (cond
                  ((and (= (length sub.ses-list) 1)
                        (typep (car sub.ses-list) 'ses))
                   (let1 subses (car sub.ses-list)
                     (move-olm-by-offset sub ses.beg)
                     (:appending 
                      (mapcar 'simplify-location-map (ses-sources subses))
                      :into new-sources)))
                  (t 
                   (:collect sub into new-sources)))))
              (t
               (:collect sub into new-sources)))
        (:finally (setf ses.sources new-sources))
        )
      )
    ses))

(defun simplify-location-map (map)
  (etypecase map
    ((null) nil)
    (ses (simplify-ses map))
    (slo map)
    (olm (simplify-olm map))))

(defun simplify-object-location-map (object) 
  (proga
    (let map (get-non-persistent-object-locations object))
    (let new-map (simplify-location-map map))
    (unless (eq map new-map)
      (setf (get-non-persistent-object-locations object) new-map))
    new-map))      

#|(defun l/simplify-location-map (map)
  "������ ����� �����, ���� olm, ����������. ����� ���� nil"
  (typecase map
    (slo 
     (if (slo-source map) map nil))
    (ses (iter 
           (:for sub in (ses-sources map))
           (
  )|#


(defun l/princ (object &optional (stream *standard-output*))
  (let*
      ((dst-beg (extract-file-position stream))
       (delegate (get-stream-location-map-delegate stream)))
    (prog1
        (princ object stream)
      (let* ((dst-end (extract-file-position stream)))
        (when delegate 
          (l/add-to-location-map delegate dst-beg dst-end object)))
      )))

(defun l/str+ (&rest objects)
  "����� ���� �����-�� �������, �.�. (string x) � ������� str+ ����� ���������� �� (princ-to-string x) �����. ����� ����� ����������."
  (let1 res
      (l/with-output-to-string (ss) 
        (mapcar (lambda (x) (l/princ x ss)) objects)
        )
    (simplify-object-location-map res)
    res))
 
(defun get-stream-location-map-delegate (stream &key if-not-exists)
  (etypecase stream
    (editor::editor-region-stream stream)
    (stream::file-stream stream)
    (stream::ef-file-stream stream)
    (stream 
     (ecase if-not-exists
       (:create (nth-value 0 (ensure-gethash-2 stream *slmd (gensym "LOCATION-DELEGATE"))))
       ((nil) (gethash stream *slmd))
       ))))

(defun extract-source-filename-from-stream (stream)
  "���������� � SLIME. ����������� ���_�� �٨, ����� ������"
  #-lispworks (error "Not implemented")
  #+lispworks
  (typecase stream
    (editor::editor-region-stream
     (slot-value (slot-value (slot-value stream 'editor::point) 'editor::buffer) 'editor::%pathname))
    (stream::ef-file-stream (slot-value (slot-value stream 'stream::underlying-stream) 'stream::path))
    (stream::file-stream (slot-value stream 'stream::path))
    (string-stream nil)
    (t
     (break "Can't extract source filename here"))))

(defun real-point-offset (point) 
  "����� ����������� ������� �� editor-budden-tools"
  (+ (editor::point-offset point) (slot-value (editor::point-bigline point) 'editor::start-char)))

(defun extract-file-position (stream)
  (typecase stream
    (editor::editor-region-stream
     #+lispworks
     (+ (file-position stream) 
        (real-point-offset (slot-value stream 'editor::start)))
     #-lispworks (error "define me!"))
    (t (file-position stream))))
 
        
;;; ������� ���������� ��� �������. 
;;; ����� ������� �������-���������

;;; ��������� read-and-record-non-persistent-location



#| (trivial-deftest:deftest 
       non-persistent-locations-work 
       (not (null (with-open-file (in "l:/lisp/lib/cl-fix/see-packages.asd")
                    (let* ((line (read-line in)))
                      (record-non-persistent-location in 0 line)
                      (get-non-persistent-object-locations line)
                      ))))
       t)

|#





(defun l/rorl (obj position)
;   (print (list obj source beg end))
  (let (source beg end)
    (when position
      (dsetq (source beg end) position))
    (when source
      "����� ��� ����� ������� �����. ���� ������ ��� ����� ������ � ���� ������������, 
�� ������������ ����� ��. ����, �� �����-�� ����, ��� ����� ����� ��� � �������
����� ������, ����� ����� ���� ������ �� ��� �� ����. ��������� �� �� ����� ������
�� ����, �� �� ����� ���� ������� ���� �� ���, �� ������ ���� ����� �����������"
      (assert (>= end beg))
      (cond 
       ((symbolp obj) ; ������ ���������� ��� ����� ��������, ��� �����. � ��������� ����� �����-����� ���������� �� �����, �� ���� ������ ����������� ����� ��� ������, �� �� ����� ����� �����
      ; �� ����� �� ���������� ������ � ���� �����, �� ���, ���������, ��� ����!
        (make-slo :source source :beg beg :end end))
       (t       
        (let ((locs (get-non-persistent-object-locations obj)))
          (or locs 
              (setf (get-non-persistent-object-locations obj) (make-slo :source source :beg beg :end end)))))))
    obj
    ))

(defun l/substitute-subseq (seq sub rep &key (start 0) end
                          (test #'eql) (key #'identity))
  "Like cllib:substitute-subseq, but takes locations into account. seq and result are strings" 
  (declare (sequence seq sub rep))
  (loop :with olen = (length sub)
        :and last = start
        :for next = (search sub seq :start2 last :end2 end :test test :key key)
        :unless (or next all) :return seq
        :collect (l/subseq seq last next) :into all
        :while next :collect rep :into all :do (setq last (+ next olen))
        :finally (return (reduce (lambda (s0 s1) (l/str+ s0 s1))
                                 all :initial-value (l/subseq seq 0 start)))))


(defun l/rorlf (object source beg end)
  "�����, ������� ������� �������� ��������� ������� � ������ ���"
  `(l/rorl ,object ,source ,beg ,end))
 

(defun srcpl-reader (stream symbol)
  (declare (ignore symbol))
  (let* ((before (extract-file-position stream))
         after
         (object (read stream)))
    (setf after (extract-file-position stream))
    (it-is-a-car-symbol-readmacro 
     `(l/rorl ,object ,(extract-source-filename-from-stream stream) ,before ,after))))


(setf (symbol-readmacro (intern "SRCPL" :budden-tools)) #'srcpl-reader)
