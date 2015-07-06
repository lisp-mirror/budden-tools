;;; -*- Encoding: utf-8; system :see-packages -*-

(in-package :budden-tools)
(in-readtable nil)

(defvar *use-decorated-package-system-fns* nil)

;;; FIXME - проверить работу иерархических пакетов,
;;; основать def-merge-packages::! на них

(defun symbol-is-in-package (symbol package external-only)
  "Возвращает два значения: 1. t, если данный символ доступен в данном пакете. Если external-only, то возвращает t, только если он внешний в данном пакете
   2. статус из find-symbol, если символ доступен"
  (perga-implementation:perga
    (:@ mlvl-bind (other-symbol status) (find-symbol (symbol-name symbol) package))
    (cond
     ((null symbol)
      (cond ((null other-symbol) (values t status))
            (t nil)))
     ((null other-symbol) nil)
     ((not (eq symbol other-symbol)) nil)
     ((eq status :EXTERNAL) (values t status))
     ((not external-only) (values t status))
     (t nil))))


(defun may-symbol-complete-symbol (symbol default-package partial-name external-only all-chars-in-same-case-p)
  (perga-implementation:perga
    (cond
     ((not (symbol-is-in-package symbol default-package external-only))
           nil)
     (t 
      (alexandria.0.dev:starts-with-subseq 
       partial-name
       (typecase symbol (symbol (symbol-name symbol)) (t (string symbol)))
       :test (if all-chars-in-same-case-p #'char-equal #'char=)))
     )))


(defun do-complete-symbol-with-budden-tools (str editor-package editor-error-fn menu-from-list-fn &key (yes-or-no-p-fn 'yes-or-no-p))
  "Функция, позволяющая сделать завершение символа.
Str - входная строка, для которой необходимо завершение.
editor-package - текущий пакет в контексте редактора
editor-error-fn - ф-я, подобная error по сигнатуре, вызывается в случае ошибки

На выходе строка с завершенным символом или nil, если пользователь
отказался от выбора (в случае неоднозначности).

Функция учитывает наличие local-nickname пакетов.

Таким образом, на вход может подаваться следующая информация:

1) Часть имени без указания пакета: symb
2) Часть имени внешнего символа, с указанием префикса пакета: package:symb
3) Часть имени внутреннего символа с указанием префикса пакета: package::symb
4,5) Повтор пп. 2 и 3, где в качестве префикса указывается local-nickname пакет.
6) Пакет указан, но в имени пакета есть ошибка, т.е. нельзя найти пакет по префиксу.
   Действуем по п.1, исключив префикс.
"  
  (let* ((partial-name str)
         ;; Позиция первого двоеточия
         (colon-pos (position #\: 
                              partial-name))
         ;; Позиция второго двоеточия
         (2colon-pos (when colon-pos
                       (position #\: 
                                 partial-name 
                                 :start (1+ colon-pos))))
         ;; Текст до двоеточий, может отсутствовать
         (prefix (and colon-pos 
                      (> colon-pos 0)
                      (subseq partial-name 0 colon-pos)))

         (casified-prefix (and prefix
                               (string (read-from-string (concatenate 'string "#:" prefix)))))

         ;; Текст после двоеточий, может отсутствовать
         (suffix (if colon-pos
                     (subseq partial-name 
                             (1+ 
                              (max (if (null colon-pos)
                                       0 colon-pos)
                                   (if (null 2colon-pos)
                                       0 2colon-pos))))
                   partial-name))

         ;; Завершаемый символ внутренний?
         (external? (and colon-pos (not 2colon-pos)))

         ;; Имя пакета, в котором находится редактор (есть всегда)
         ;(editor-package-name (when editor-package
         ;                       (package-name editor-package)))

         ;; Пакет, полученный из префикса
         (found-package (cond
                         (casified-prefix
                          (or 
                           (budden-tools:hp-find-package casified-prefix editor-package)
                           (funcall editor-error-fn "Package or local-nickname ~S not found" casified-prefix)))
                         ((and
                           (null casified-prefix)
                           (eql colon-pos 0)
                           (not 2colon-pos))
                          *keyword-package*)))

         ;; Имя пакета из префикса, если удалось найти.
         (found-package-name (when found-package
                               (package-name found-package)))

         ;; Пакет является 
         (fake-package? (and found-package-name 
                             prefix
                             (not (string-equal 
                                   found-package-name 
                                   prefix))))
         (camel-case-suffix?
          (not (def-merge-packages::all-ascii-chars-in-same-case-p suffix))))

    (let ((raw-list ())
          (list-of-completes ())
          (show-list ())
          (ext (if found-package external? nil))
          (pkg (if found-package 
                   found-package 
                 editor-package)))

      (setf raw-list
            (remove-duplicates 
            ; (identity ; append
              (iter
                (:for sym 
                 :in-package pkg
                 :external-only nil)
                (:for storage = (nth-value 1 (find-symbol (string sym) pkg)))
                (when (and ext (not (eq storage :external)))
                  (:next-iteration))
                (:for name = (subseq (prin1-to-string (make-symbol (symbol-name sym))) 2))
                (:for pkg2 = (symbol-package sym))
                (when pkg2 ; can be inherited uninterned symbol so it has no home package
                  (:collect 
                   (list name (package-name pkg2) storage sym))))
              #|(if (not ext)
                  (iter
                    (:for sym 
                     :in-package pkg
                     :external-only nil)
                    (:collect 
                     (let* ((name (subseq (prin1-to-string (make-symbol (symbol-name sym))) 2))
                            (pkg (symbol-package sym)))
                       (list name (package-name pkg) :internal sym)))))|# 
             :from-end t :key #'first :test #'string=))
              

      (labels ((casify-name (nm) 
                            ; если пользователь набрал имя в верхнем регистре, а имя может быть прочитано в обоих, 
                            ; приводим продолженный символ к верхнему регистру
                 (if 
                     (and suffix
                          (eq (all-ascii-chars-in-same-case-p suffix) :uppercase)
                          (all-ascii-chars-in-same-case-p nm)
                          (not (find #\| nm :test 'char=))
                          (not (find #\\ nm :test 'char=))
                          (member (readtable-case-advanced *readtable*) '(:upcase :upcase-if-uniform)))
                     (string-upcase-ascii nm)
                   nm))
               (do-format-item (x rem-prefix home-package)
                 (let* ((name (first x)) ; имя для печати
                        (p-name (second x))
                        (status (third x))
                        (symbol (fourth x))
                        (colons "")
                        (package-name "")
                        (do-delete-prefix
                         (cond
                          ((null prefix)
                           t)
                          ((eq rem-prefix :prompt)
                           (funcall yes-or-no-p-fn "Символ ~S доступен в текущем пакете. Убрать префикс?" symbol))
                          (t rem-prefix))))
                
                   (setf package-name
                         (if (and (string-equal p-name 
                                                found-package-name)
                                  fake-package?)
                             prefix
                           p-name))
                   (setf colons
                         (cond
                          ((and (string-equal p-name 
                                              found-package-name)
                                (eq :external status))
                           ":")
                           ((and (string-equal p-name
                                              (if (symbol-package symbol)
                                                  (package-name (symbol-package symbol))
                                                ""))
                                (eq :external status))
                            ":")
                           (t                         
                           "::")))
                   (format nil "~A~A~A" 
                           (if do-delete-prefix "" (if home-package (if fake-package? prefix home-package) package-name))
                           (cond
                            ((eq (symbol-package symbol) *keyword-package*)
                             ":")
                            (do-delete-prefix "")
                            (home-package (if (eq status :external) ":" "::"))
                            (t colons))
                           (casify-name name))))
               (format-item (x)
                 (do-format-item x nil nil)))

        (setf list-of-completes
              (sort
               (remove-if #'null
                          (mapcar #'(lambda (x)
                                      (if (alexandria:starts-with-subseq 
                                           suffix
                                           (first x)
                                           :test (if camel-case-suffix?
                                                     #'char= 
                                                   #'char-equal))
                                          x nil))
                                  raw-list))
               #'string-lessp :key #'first))

        (setf show-list 
              (mapcar #'format-item list-of-completes))

        (let* ((s (if (= 1 (length show-list)) 
                      (first show-list) 
                    (funcall menu-from-list-fn show-list)))
               (pos (position s show-list :test #'string=))
               (delete-prefix? nil)
               (src (when pos (nth pos list-of-completes)))
               ;(name (first src)) ; имя для печати
               ;(p-name (second src))
               ;(status (third src))
               (symbol (fourth src))
              )
          (when (and
                 ;; указан префикс
                 prefix 
                 ;; символ доступен в текущем пакете 
                 (symbol-is-in-package symbol editor-package nil)
                 )
            (setf delete-prefix? :prompt))
          (when src
            (do-format-item src delete-prefix? found-package-name)))))))
