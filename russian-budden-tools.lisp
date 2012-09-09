; -*- coding: windows-1251-dos; -*- 
; ������ � �������� �������
; ��� ������ ���������� ����� ����� � sbcl ����� ������ (setf sb-impl::*default-external-format* :windows-1251) 

(in-package :russian-budden-tools)

(eval-when (:execute)
  (error "Do not :execute the file, use compile/load sequence"))
(eval-when (:load-toplevel :compile-toplevel)
    (setf *readtable* (copy-readtable nil))
    nil)

(defparameter *cyrillic-characters* '(#\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\� #\�)
)

(defparameter *reversible-cyrillic-translit-table* (make-hash-table :test 'eql))

(iter 
  (:for (from . to) :in
   (list-to-alist 
    (split-sequence:split-sequence-if 
     'cl-ppcre::whitespacep
     "� a � b � v � g � d � e � jo � zh � z � i � jj � k � l � m � n � o � p � r � s � t � u 
� f � kh � c � ch � sh � shh � w � y � q � eh � ju � ja 
� A � B � V � G � D � E � JO � ZH � Z � I � JJ � K � L � M � N � O � P � R � S � T � U 
� F � KH � C � CH � SH � SHH � W � Y � Q � EH � YU � YA" :remove-empty-subseqs t)))
  (setf (gethash (elt from 0) *reversible-cyrillic-translit-table*) to))


;;; FIXME ������� �������� ��������������
(defun translit-reversibly (string-designator)
  "���� string-designator � ���������� ������������������� ������"
  (declare (optimize speed))
  (let* 
      ((s (typecase (the* string-designator string-designator)
            (string string-designator)
            (t (string string-designator))))
       (l (length s))
       (res (make-string (* 3 l))))
;    (declare (dynamic-extent res)) �� ��������. ������? 
    (iter 
      (:with i = 0)
      (:for c in-string s)
      (:for out = (gethash c *reversible-cyrillic-translit-table* c))
      (etypecase out
        (string
         (let1 outl (length out)
           (setf (subseq res i (incf i outl))
                 out)))
        (character
         (setf (elt res i) out)
         (incf i)))
      (:finally (return (subseq res 0 i))))))


(let* ((numchars (/ (length *cyrillic-characters*) 2))
       (up (make-hash-table :test #'eql))
       (down (make-hash-table :test #'eql)))
  (check-type numchars integer)
  (iter 
    (:for i from 0 to (- numchars 1))
    (:for j from numchars to (- (* 2 numchars) 1))
    (:for lochar = (elt *cyrillic-characters* i))
    (:for hichar = (elt *cyrillic-characters* j))
    (setf (gethash lochar up) hichar)
    (setf (gethash hichar down) lochar))

; non-toplevel
(defun char-upcase-cyr (char) "��������� ������� �� ������"
    (or (gethash char up) (char-upcase char)))

; non-toplevel
(defun char-downcase-cyr (char) "�������� ������� ��-������"
    (or (gethash char down) (char-downcase char)))

; non-toplevel
(defun char-equal-cyr (c1 c2) "���������� ������� � ������ ���������"
    (or (char-equal c1 c2)
        (char-equal (char-upcase-cyr c1) (char-upcase-cyr c2))))

; non-toplevel
(defun string-upcase-cyr (s)
    (map 'string 'char-upcase-cyr s))

; non-toplevel
(defun string-downcase-cyr (s)
    (map 'string 'char-downcase-cyr s))

; non-toplevel
(defun string-equal-cyr (s1 s2)
    (let* ((s1 (string s1))
           (s2 (string s2)))
      (and (= (length s1) (length s2))
           (every 'char-equal-cyr s1 s2))))

; non-toplevel
(defun textual-equal-cyr (s1 s2)
  "�� �� ����� ���������� equal-cyr, �.�. �� ����� ������ �� ���� ����� ������. �� ���� ���."
  (flet ((stringify (x)
           (typecase x
             (string x)
             (symbol (string x))
             (t (prin1-to-string x)))))
    (string-equal-cyr (stringify s1) (stringify s2))))

; non-toplevel
(defun cyrillic-char-p (x) 
  (and (characterp x) 
       #+(and nil lispworks win32) 
       ; ���� �� ����� ����� ��-�� �
       (let1 c (char-code x)
         (or 
          (<= #.(char-code #\�) c #.(char-code #\�))
          (eq x #\�)
          (eq x #\�)))
       #-(and nil lispworks win32)
       (or (gethash x up) (gethash x down))
       t
       ))

)

;; �������������� ������� �������� ��� ������-������ � open-pipe
#+(and lispworks (or windows win32))
(defmacro define-open-pipe-character-translators ()
  (let ((lisp->dos (make-hash-table :test #'eql :size 256))
        (dos->lisp (make-hash-table :test #'eql :size 256)))
    (iter 
      (:for c in (append *cyrillic-characters* '(#\�)))
      (:for sou = 
       (with-output-to-string (ou) 
         (system:call-system-showing-output
          (format nil "echo ~A" c) :prefix "" :show-cmd nil :output-stream ou)))
      (:for ou = (elt sou 0))
      (setf (gethash c lisp->dos) ou)
      (setf (gethash ou dos->lisp) c)
      )
    ; (iter (:for (a b) :in-hashtable lisp->dos) (print `(,a ,(char-code b))))
    `(progn
       (defun lisp-string-to-dos (s)
         (declare (optimize (speed 3) (safety 0) (debug 0)))
         (let1 s (copy-seq s)
           (iter 
             (:for i from 0)
             (:for c in-string s)
             (:for ou = (gethash c ,lisp->dos))
             (when ou (setf (aref s i) ou)))
           s))
       (defun dos-string-to-lisp (s)
         (declare (optimize (speed 3) (safety 0) (debug 0)))
         (let1 dest (make-string (length s) :element-type 'lispworks:simple-char)
           (iter 
             (:for i from 0)
             (:for c in-string s)
             (:for ou = (gethash c ,dos->lisp))
             (setf (aref dest i) (or ou c)))
           dest))
       (compile 'lisp-string-to-dos)
       (compile 'dos-string-to-lisp)
       )))

(defmacro define-open-pipe-character-translators-by-sample (sample x-to-lisp lisp-to-x)
  "�������� �� ��������� ������������� ������, �������� ����� ��������� (concatenate 'string (append bu::*cyrillic-characters* '(#\�))). 
   ��������� �-� x-to-lisp, ����� ��������� ������ �� ����� ��������� � lisp-to-x, ����� �������� � ��������"
  (let ((lisp->x (make-hash-table :test #'eql :size 256))
        (x->lisp (make-hash-table :test #'eql :size 256))
        (character-list (append *cyrillic-characters* '(#\�))))
    (assert (= (length sample) (length (concatenate 'string character-list))))
    (assert (= (length sample) (length (remove-duplicates (concatenate 'list sample) :test '= :key 'char-code))) () "������� ������ ��������� ��� ��������� �����")
    (iter 
      (:for c :in character-list)
      (:for ou :in-sequence sample)
      (setf (gethash c lisp->x) ou)
      (setf (gethash ou x->lisp) c)
      )
    ; (iter (:for (a b) :in-hashtable lisp->dos) (print `(,a ,(char-code b))))
    `(progn
       (defun ,lisp-to-x (s)
         (declare (optimize (speed 3) (safety 0) (debug 0)))
         (let1 s (copy-seq s)
           (iter 
             (:for i from 0)
             (:for c in-string s)
             (:for ou = (gethash c ,lisp->x))
             (when ou (setf (aref s i) ou)))
           s))
       (defun ,x-to-lisp (s)
         (declare (optimize (speed 3) (safety 0) (debug 0)))
         (let1 dest (make-string (length s) :element-type 'lispworks:simple-char)
           (iter 
             (:for i from 0)
             (:for c in-string s)
             (:for ou = (gethash c ,x->lisp))
             (setf (aref dest i) (or ou c)))
           dest))
       (compile ',lisp-to-x)
       (compile ',x-to-lisp)
       )))


(def-trivial-test::! all-ascii-chars-in-same-case-p 
                     (list (budden-tools::all-ascii-chars-in-same-case-p "������")
                           (budden-tools::all-ascii-chars-in-same-case-p "������")
                           (budden-tools::all-ascii-chars-in-same-case-p "������")
                           (budden-tools::all-ascii-chars-in-same-case-p "aureki")
                           (budden-tools::all-ascii-chars-in-same-case-p "AUReki")
                           (budden-tools::all-ascii-chars-in-same-case-p "AUREKI")
                           )
                     (list :ignore-case :ignore-case :ignore-case
                           :lowercase nil :uppercase))
