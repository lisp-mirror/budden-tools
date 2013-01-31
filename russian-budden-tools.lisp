;;; -*- Encoding: utf-8; -*-
; -*- coding: windows-1251-dos; -*- 
; работа с Русскими буквами
; при ручной компиляции этого файла в sbcl нужно делать (setf sb-impl::*default-external-format* :windows-1251) 

(in-package :russian-budden-tools)

(eval-when (:execute)
  (error "Do not :execute the file, use compile/load sequence"))
(eval-when (:load-toplevel :compile-toplevel)
    (setf *readtable* (copy-readtable nil))
    nil)

(defparameter *cyrillic-characters* '(#\а #\б #\в #\г #\д #\е #\ё #\ж #\з #\и #\й #\к #\л #\м #\н #\о #\п #\р #\с #\т #\у #\ф #\х #\ц #\ч #\ш #\щ #\ъ #\ы #\ь #\э #\ю #\я #\А #\Б #\В #\Г #\Д #\Е #\Ё #\Ж #\З #\И #\Й #\К #\Л #\М #\Н #\О #\П #\Р #\С #\Т #\У #\Ф #\Х #\Ц #\Ч #\Ш #\Щ #\Ъ #\Ы #\Ь #\Э #\Ю #\Я)
)

(defparameter *reversible-cyrillic-translit-table* (make-hash-table :test 'eql))

(iter 
  (:for (from . to) :in
   (list-to-alist 
    (split-sequence:split-sequence-if 
     'cl-ppcre::whitespacep
     "а a б b в v г g д d е e ё jo ж zh з z и i й jj к k л l м m н n о o п p р r с s т t у u 
ф f х kh ц c ч ch ш sh щ shh ъ w ы y ь q э eh ю ju я ja 
А A Б B В V Г G Д D Е E Ё JO Ж ZH З Z И I Й JJ К K Л L М M Н N О O П P Р R С S Т T У U 
Ф F Х KH Ц C Ч CH Ш SH Щ SHH Ъ W Ы Y Ь Q Э EH Ю YU Я YA" :remove-empty-subseqs t)))
  (setf (gethash (elt from 0) *reversible-cyrillic-translit-table*) to))


;;; FIXME сделать обратное преобразование
(defun translit-reversibly (string-designator)
  "Берёт string-designator и возвращает транслитерированную строку"
  (declare (optimize speed))
  (let* 
      ((s (typecase (the* string-designator string-designator)
            (string string-designator)
            (t (string string-designator))))
       (l (length s))
       (res (make-string (* 3 l))))
;    (declare (dynamic-extent res)) не работает. Почему? 
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
(defun char-upcase-cyr (char) "поднимает регистр по Русски"
    (or (gethash char up) (char-upcase char)))

; non-toplevel
(defun char-downcase-cyr (char) "опускает регистр по-Русски"
    (or (gethash char down) (char-downcase char)))

; non-toplevel
(defun char-equal-cyr (c1 c2) "сравнивает символы с учётом кириллицы"
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
  "Мы не можем определить equal-cyr, т.к. не умеем ходить по всем типам данных. Но хоть так."
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
       ; вряд ли имеет смысл из-за Ё
       (let1 c (char-code x)
         (or 
          (<= #.(char-code #\А) c #.(char-code #\я))
          (eq x #\ё)
          (eq x #\Ё)))
       #-(and nil lispworks win32)
       (or (gethash x up) (gethash x down))
       t
       ))

)

;; преобразование русских символов для чтения-записи в open-pipe
#+(and lispworks (or windows win32))
(defmacro define-open-pipe-character-translators ()
  (let ((lisp->dos (make-hash-table :test #'eql :size 256))
        (dos->lisp (make-hash-table :test #'eql :size 256)))
    (iter 
      (:for c in (append *cyrillic-characters* '(#\№)))
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
  "Получили из источника представление строки, понятное этому источнику (concatenate 'string (append bu::*cyrillic-characters* '(#\№))). 
   Определим ф-ю x-to-lisp, чтобы прочитать строку из этого источника и lisp-to-x, чтобы записать в источник"
  (let ((lisp->x (make-hash-table :test #'eql :size 256))
        (x->lisp (make-hash-table :test #'eql :size 256))
        (character-list (append *cyrillic-characters* '(#\№))))
    (assert (= (length sample) (length (concatenate 'string character-list))))
    (assert (= (length sample) (length (remove-duplicates (concatenate 'list sample) :test '= :key 'char-code))) () "Образец должен содержать все различные буквы")
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
                     (list (budden-tools::all-ascii-chars-in-same-case-p "аУреки")
                           (budden-tools::all-ascii-chars-in-same-case-p "АУРЕКИ")
                           (budden-tools::all-ascii-chars-in-same-case-p "ауреки")
                           (budden-tools::all-ascii-chars-in-same-case-p "aureki")
                           (budden-tools::all-ascii-chars-in-same-case-p "AUReki")
                           (budden-tools::all-ascii-chars-in-same-case-p "AUREKI")
                           )
                     (list :ignore-case :ignore-case :ignore-case
                           :lowercase nil :uppercase))
