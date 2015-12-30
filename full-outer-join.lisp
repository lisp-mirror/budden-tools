;;; -*- Encoding: utf-8; system :budden-tools ; -*-

(in-package :budden-tools)

(defun check-appropriate-for-full-outer-join (list key test)
  (assert (every 'identity list))
  (assert (null (collect-duplicates list :key key :test test)))
  )

(defun full-outer-join (left right &key (key 'car) (test 'eql))
  "left и right - это списки элементов le,re. Для каждого le находятся такие re, что у них key совпадает с key от le в смысле test. Если таких нет, то берётся одна пара (list le nil). То же делается для каждого re. Каждая пара участвует в соединении только один раз. Предполагается и проверяется, что все ключи le и re различны в каждом списке и что в них нет nil"
  (perga-implementation:perga
   (check-appropriate-for-full-outer-join left key test)
   (check-appropriate-for-full-outer-join right key test)
   (let rest-of-right (copy-list right))
   (let result nil)
   (dolist (le left)
     (let pos (position (funcall key le) rest-of-right :key key :test test))
     (cond
      (pos
       (push (list le (elt rest-of-right pos)) result)
       (_f rmsubseq rest-of-right :start pos :count 1)
       )
      (t
       (push (list le nil) result))))
   (dolist (re rest-of-right)
     (push (list nil re) result))
   (nreverse result)))

(def-trivial-test::! full-outer-join.1
  (set-difference
   (full-outer-join '(1 2 3) '(2 3 4) :key 'identity :test '=)
   '((1 nil) (2 2) (3 3) (nil 4))
   :test 'equalp)
  nil) 

