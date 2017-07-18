;;; -*- coding: utf-8; -*-
;;; При генерации кода запоминаем информацию о положении исходных объектов или
;;; исходного текста в памяти, с возможностью последующего ответа на вопрос
;;; "откуда взялся данный исходник"

(named-readtables:in-readtable nil)
(in-package :КАРТЫ-ИСХОДНИКОВ-ТЕЛО)

#|
Общая идея. Мы хотим отслеживать исходник в ходе порождения текстов. 
Порождение может начинаться с примитивов, считанных из исходного файла лиспа.
Для них мы должны уметь знать место, где этот примитив считан. 

Дальше задача - не потерять это место, в ходе преобразования исходника. Для этого нужно
преобразовывать с соблюдением правил (особыми, а не обычными функциями, если речь идёт о строках и потоках).

API
===

ЧТЕНИЕ
------

Некоторые объекты способны помнить в себе своё местоположение. Для других есть l/rorl . 

l/rorl = l/return-object-recording-location - вызывается при чтении объекта, неспособного содержать место исходника в себе (например, таковы cons или строка). Запоминает место определения объекта в глобальной базе данных и возвращает объект.

ПРЕОБРАЗОВАНИЕ
--------------
Если преобразование состоит в построении парсем из лексем, способных помнить в себе своё местоположение, то ничего вроде бы делать не надо - главное, чтобы был способ по каждой парсеме узнать, где находится её исходник (иногда может нарушаться топология или выворачиваться середина из краёв и т.п. - это не дело данного API). Если объекты уже были напечатаны в строки и мы работаем со строками, то помогут

l/subseq , l/substitute-subseq , l/str+

Если для исходных строк была информация об исходниках, эта информация будет должным образом преобразована в информацию для результата. 

ВЫВОД
-----
get-stream-location-map-delegate - получить или создать делегата для выходного потока. Он нужен для того, чтобы в формируемой потоком строке или файле работал поиск исходников.

l/princ - напечатать объект, имеющий место исходника, в поток 
l/add-to-location-map - примитив, к-рый собственно заносит информацию и вызывается из l/princ

l/pass-from-delegate-to-object delegate object - вызывается после того, как поток полностью закончил написание строки
l/pass-from-stream-to-file - вызывается после того, как поток полностью закончил написание файла

Необходимые действия с делегатами для вывода в строку делаются в 
l/with-output-to-string . Для вывода в файл см. пример около l/pass-from-stream-to-file


УТИЛИТЫ
-------

extract-source-filename-from-stream - если поток связан с файлом, возвращает его имя (утилита, может использоваться клиентами при сборе инф-ии о хранимых объектах)

srcpl - symbol-readmacro. Прочитать объект и запрограммировать запоминание его места в load-time.
(надо потом сделать, чтобы по желанию рекурсивно записывались места всех под-объектов)

КАРТЫ
=====

Карта говорит о том, где находится исходник данного объекта. 

Карта может быть связана с лисп-объектом, строкой или файлом.
В случае лисп-объекта, карта хранит данные только о самом объекте, но не о его начинке (не факт)
В случае строки или файла, текст может быть разбит на сегменты, для каждого из которых указано отдельное место в исходном тексте.

Виды карт:
```
olm = object-location-map
segment-sources = ses
source-location = slo
```
Для объектов - строк или объектов, которые можно вывести в строку, карта может состоять из многих сегментов, 
Для строк исходного текста, существующих только в памяти, карты живут в слабой eq хеш-таблице, она называется
*nplm = *non-persistent-locations-memory* , (строка -> КАРТА). Ранее было написано, что вместо строки может быть и другой объект, но я не умыслю, какой ещё объект может быть. Там же живут карты для выходных потоков, которые пишутся в данный момент, а точнее, для делегатов этих потоков.

Для файлов, карты живут в сильной string-equal хеш-таблице, она называется
*nplf = *non-persistent-locations-files* , (namestring -> КАРТА)

Дыра состоит в том, что информация хранится в образе, а не в самом файле. Предстоит сделать систему выгрузки инфы в файлы, режащие рядом с файлом исходника. 

ДЕЛЕГАТЫ
========

Если у нас есть какой-то поток или мы собираем строку, то результирующей строки у нас прямо сейчас ещё нет. 
Но информацию записать надо. Для этого используется заместитель (делегат). В случае потока он 
является make-symbol и находится в слабой хеш-таблице (хотя мы могли бы использовать и непосредственно поток?)
*slmd = *stream-location-map-delegates* (поток->делегат)


В отношении компиляции-загрузки. Лексемы fbody живут только в load-time, для них запись 
программируется явно. 

|#


; (proclaim '(optimize (speed 3) (debug 0) (safety 0)))

; disable stepping
(declaim (optimize (debug 3) (compilation-speed 3) (safety 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro get-non-persistent-object-locations (object)
    "Здесь object - это делегат потока или строка, полученная в результате вывода в поток"
    `(gethash ,object *nplm)))

(defvar *nplm 
  #+NIL (make-hash-table :test 'eq)
  (SWANK-BACKEND:MAKE-WEAK-KEY-HASH-TABLE :test 'eql) 
  "Временные сведения о расположении для объектов в памяти")

(defvar *nplf 
  (make-hash-table :test 'equal)
  "Сведения о расположении, для ИМЁН файлов (namestring). Сильная таблица, т.к. файлы существуют долго")

(defvar *slmd
  (SWANK-BACKEND:MAKE-WEAK-KEY-HASH-TABLE :test 'eql)
  "Делегаты потоков")

(defvar *record-locations* t "Если истина, то fbody, fsel и родственники генерируют код, позволяющий отследить исходники")


(defun track-locations ()
  "Эта ф-я возвращает истину, если мы хотим в рантайме использовать информацию об исходниках. Поиск исходника sql не будет работать, если эта ф-я возвращает nil. Также он не будет работать, если соотвтствующий код был создан в то время, когда *record-locations* была связана в nil. Пока для простоты задействуем одну и ту же переменную *record-locations* и для записи исходников, и для использования уже записанных исходников"
  *record-locations*)


(defstruct olm ; карта исходников для объкта 
  ses-list ; упорядоченный список структур ses
  simplified ; t, если уже упрощена
  )

; в конструкциях типа (with-output-to-string) не с чем ассоциировать информацию о расположении. 
; на такие случаи МОЖЕТ ЯВНО создаваться olm с пустым destination и прописываться в
; *source-code-output-stream-destinations* для данного потока. Например:

;(let ((l-i-h (make-olm)))
;  (values 
;   (with-output-to-string (stream)
;     (setf (gethash stream *source-code-output-stream-destinations*) l-i-h)
;     (princ-and-integrate-location "asdf" stream)
;     )
;   l-i-h))


;; Система координат важна!!! Для лисповых файлов - смещение в байтах. Для Яра - смещение в буквах
(defstruct ses ; для данного отрезка может задавать несколько исходников. Эту структуру нужно хранить в контексте destination, например, 
    ; в olm
  (beg nil :type (nullable integer)) ; в результирующем объекте. Для лисповых файлов - смещение в байтах. Для Яра - смещение в буквах
  (end nil :type (nullable integer)) ; Для лисповых файлов - смещение в байтах. Для Яра - смещение в буквах
  sources ; список элементов slo, задающий исходники данного отрезка (пока что один)
  simplified ; t, если уже упрощена
  )


(deftype |Система-координат| () "ОЯ::|системы-координат-в-исходном-тексте-и-их-преобразование|" '(member |сико-ПБу| |сико-ПФ|))

(defstruct slo ; откуда взялся данный объект cl:cons
  source ; например, имя файла. А также, в порядке эксперимента, попробуем БУКВЫ-И-МЕСТА-В-ФАЙЛЕ:|Ссылка-на-источник|
  (|Система-координат| '|сико-ПБу| :type |Система-координат|)
  beg 
  end)

(deftype КАРТА ()
  "Карта говорит о том, где находится исходник данного объекта. Для объектов - строк или объектов, которые можно вывести в строку, карта может состоять из многих сегментов, для каждого из которых указано отдельное место в исходном тексте"
    '(or olm ses slo))

(defmacro l/make-string-output-stream (&rest args)
  "Создаёт поток и делегата для него. Возвращает два значения - поток и делегат. После завершении не забудьте вызвать l/pass-from-delegate-to-object, подобно l/with-output-to-string"
  (with-gensyms (Поток)
    `(let ((,Поток (make-instance 'ПОТОКИ-ЗПТ-СЧИТАЮЩИЕ-БУКВЫ-СТРОКИ-И-КОЛОНКИ:|Считающий-выходной-поток-литер|
                                  :|Поток| (make-string-output-stream ,@args))))
       (values ,Поток (get-stream-location-map-delegate ,Поток :if-not-exists :create)))))

(defmacro l/with-output-to-string ((VAR &REST string-and-key) 
                                   &BODY BODY)
  (with-gensyms (var-var location-delegate)
    `(let (,location-delegate)
       (l/pass-from-delegate-to-object 
        (with-output-to-string (,var-var ,@string-and-key)
          (let ((,var (make-instance 'ПОТОКИ-ЗПТ-СЧИТАЮЩИЕ-БУКВЫ-СТРОКИ-И-КОЛОНКИ:|Считающий-выходной-поток-литер|
                                     :|Поток| ,var-var)))
            (setf ,location-delegate (get-stream-location-map-delegate ,var :if-not-exists :create))
            ,@body
            ))
        ,location-delegate))))

(defun |Удалить-карты-для-генерируемого-файла| (namestring)
  (remhash namestring *nplf))

(defun |Имя-файла-карты-по-имени-файла-исходного-текста| (|Имястрока-файла-с-исходными-текстами|)
  (str++ |Имястрока-файла-с-исходными-текстами| ".яр-кит"))

(defun |Сохранить-карту-для-файла-в-файл| (namestring)
  (perga
   (let Карта (gethash namestring *nplf))
   (let Имя-файла-карты (|Имя-файла-карты-по-имени-файла-исходного-текста| namestring))
   (:@ with-open-file (Вы Имя-файла-карты :direction :output :if-does-not-exist :create :if-exists :supersede))
   (let *print-readably* t)
   (print Карта Вы)))

(defun l/pass-from-delegate-to-object (object delegate)
  "object - строка исходного текста, например, полученная в with-output-to-string при одновременном выводе информации о положении.
   delegate - делегат выходного потока (этого же with-output-to-string) или nil (тогда никакая инфа об исходниках не запишется).
   Данная ф-я вызывается после завершения вывода в поток и переносит данные о местоположении с делегата на объект. После этого мы можем узнать, где определены буквы этой строки. 

   См. также l/pass-from-stream-to-file"
;  (declare (optimize speed))
  (when (track-locations)
    (unless (eq delegate object)
      (setf (gethash object *nplm) (simplify-location-map (gethash delegate *nplm)))
      (remhash delegate *nplm)
      ))
  object)

(defun l/pass-from-stream-to-file (stream)
  "ПРАВЬМЯ переименовать в l/pass-from-delegate-to-file. 

Выводили в файл посредством потока stream с одновременным выводом информации о положении данные были связаны с потоком. Ассоциируем данные о местоположении с файлом. После этого мы сможем
 находить места, где определены буквы из этого файла

Пример:
(perga
 (:@ with-open-file ou \"бла-бла\" :direction :output :if-exists :supersede)
 (get-stream-location-map-delegate ou :if-not-exists :create)
 (l/princ Некая-строка-с-ассоциированной-инфой-об-исходнике ou)
 (l/pass-from-stream-to-file ou))

См. также l/pass-from-delegate-to-object
"
;  (declare (optimize speed))
  (when (track-locations)
    (let1 delegate (get-stream-location-map-delegate stream)
      (setf (gethash (namestring (extract-source-filename-from-stream stream))
                     *nplf)
            (simplify-object-location-map delegate))
      (remhash delegate *nplm))
    nil))

(defun l/add-to-location-map (delegate dst-beg dst-end object)
  "Мы напечатали объект в поток, имеющий делегата delegate. Объект разместился между смещениями dst-beg и dst-end (не знаю, включительно ли). 
Запоминаем, что данному месту в данном выходном потоке соответствует этот объект и места в исходниках, из которых объект получен. Объект - в простейшем случае - просто строка

   dst-beg - смещение и dst-end - числа - смещения в потоке, который представлен делегатом delegate. 
   object - объект исходного текста. Например, просто строка (см. l/princ)"
;  (declare (optimize speed))
  (when (track-locations) 
    (let* ((oli (gethash object *nplm)) ; оbject-location-information
           (dli (gethash delegate *nplm)) ; delegate-location-information
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
    ))

(defun l/subseq (string start &optional end (simplify t))
  "Только во временном хранении"
;  (declare (optimize speed))
  (perga-implementation:perga 
    (let result (subseq string start end))
    (when (track-locations)
      (let map
        (if simplify
            (simplify-object-location-map string)
          (get-non-persistent-object-locations string)))
      (when map
      ; карта отсортирована. Начало и конец могут быть обрезаны
        (the* olm map)
        (_f copy-olm map) ; копируем карту, ведь для подпоследовательности она будет другой      
        (let len (length string))
        (setf end (if end (min end len) len))
        (let-with-conc-type map olm map
          (iter
            (:for sub in map.ses-list)
            (let-with-conc-type sub ses sub
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
        ))
    result))


(defun l/find-sources-in-file (filename offset &key (strict nil))
  "Дан файл, найти точку"
;  (declare (optimize speed))
  (let1 res
      (l/find-sources-in-map (gethash (namestring filename) *nplf) offset :strict strict)
    (_f remove-duplicates res :test 'equalp)
    (if (and res (> (length res) 1))
        (list 
         (nth #-lispworks (progn 
                            (format *query-io* "Sources are ~A~%Which (from 1)?" res) 
                            (- (read *query-io*) 1))
              #+lispworks (let* ((choice-list (mapcar 'str++ res))
                                 (choice (capi:prompt-with-list choice-list "Choose one of the sources"))
                                 (pos 
                                  (if choice 
                                      (position choice choice-list :test 'equalp)
                                    (return-from l/find-sources-in-file nil))
                                  ))
                            pos)
              res))
      res)))

(defun l/find-sources-in-map (map offset &key (strict nil))
  "Дана карта, найти точку, откуда растёт исходник. Возвращает список троек (файл начало конец). Если strict=nil, 
может возвращать и пустое имя файла (для отладки)"
;  (declare (optimize speed))
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
  (let-with-conc-type olm olm olm
    (setf olm.ses-list (copy-list olm.ses-list))
    (iter (:for ses on olm.ses-list)
      (when (car ses)
        (let-with-conc-type carses ses (car ses)
          (setf (car ses) 
                (make-ses :beg (+ carses.beg offset)
                          :end (+ carses.end offset)
                          :sources carses.sources
                          :simplified carses.simplified)
                ))))))

(defun simplify-olm (olm &key sort)
  "Может копировать"
;  (declare (optimize speed))
  (proga
    (unless olm (return-from simplify-olm olm))
    (let-with-conc-type olm olm olm)
    (when olm.simplified (return-from simplify-olm olm))
    (let already-copied nil)
    (iter 
      (:for sub1 in olm.ses-list)
      (proga
        (cond
         ((ses-p sub1)
          (with-conc-name sub1 ses)
          (cond 
           ((= (length sub1.sources) 1)
            (let sub2 (first sub1.sources))
            (cond 
             ((olm-p sub2)
              (unless
                  already-copied
                (_f copy-olm olm)
                (setf already-copied t))
              (with-conc-name sub2 olm)
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
    (setf olm.simplified t)
    olm))


(defun simplify-ses (ses)
;  (declare (optimize speed))
  (if (ses-simplified ses) 
      ses
    (let-with-conc-type ses ses (copy-ses ses)
      (iter 
      ;(flet ((outer-append (x) (:appending x :into new-sources)))
        (:for sub = (simplify-location-map (pop ses.sources)))
        (:while sub)
        (:collect sub into new-sources)
        (:finally (setf ses.sources new-sources))
       ; )
        )
      (setf ses.simplified t)
      ses)))

(defun simplify-location-map (map)
;  (declare (optimize speed))
  (etypecase map
    (null nil)
    (ses (simplify-ses map))
    (slo map)
    (olm (simplify-olm map))))

(defun simplify-object-location-map (object) 
;  (declare (optimize speed))
  (proga
    (let map (get-non-persistent-object-locations object))
    (let new-map (simplify-location-map map))
    (unless (eq map new-map)
      (setf (get-non-persistent-object-locations object) new-map))
    new-map))      

#|(defun l/simplify-location-map (map)
  "Создаёт новую карту, типа olm, упрощённую. Может быть nil"
  (typecase map
    (slo 
     (if (slo-source map) map nil))
    (ses (iter 
           (:for sub in (ses-sources map))
           (
  )|#


(defun l/princ (object &optional (stream *standard-output*))
;  (declare (optimize speed))
  (let*
      ((track-locations-value (track-locations))
       (dst-beg (and track-locations-value (input-stream-position-in-chars stream)))
       (delegate (and track-locations-value (get-stream-location-map-delegate stream)))
       )
    (prog1
        (typecase object
          (string (write-string object stream))
          (t (princ object stream)))
      (when track-locations-value
        (let* ((dst-end (extract-file-position stream)))
          (when delegate 
            (l/add-to-location-map delegate dst-beg dst-end object)))
        ))))

(defun l/str+ (&rest objects)
  "Могут быть какие-то отличия, т.к. (string x) в обычном str+ может отличаться от (princ-to-string x) здесь. Тогда нужно подправить."
;  (declare (optimize speed))
  (let1 res
      (l/with-output-to-string (ss) 
        (mapcar (lambda (x) (l/princ x ss)) objects)
        )
    (when (track-locations) (simplify-object-location-map res))
    res))
 
(defun get-stream-location-map-delegate (stream &key if-not-exists)
  "ПРАВЬМЯ русифицировать, особенно безграмотное имя ключа"
;  (declare (optimize speed))
  (perga
   (when (track-locations)
     (let Рез (gethash stream *slmd))
     (or Рез
         (etypecase stream
           #+lispworks (editor::editor-region-stream stream)
           #+lispworks (stream::file-stream stream)
           #+lispworks (stream::ef-file-stream stream)
           ; #+sbcl (t (break "add sbcl code to get-stream-location-map-delegate"))
           (stream 
            (ecase if-not-exists
              (:create (nth-value 0 (ensure-gethash-2 stream *slmd (gensym "LOCATION-DELEGATE"))))
              (:error (error "Ошидалось, что у потока ~S есть делегат для записи информации об исходных текстах, а его нет" stream)) 
              ((nil) nil)
              )))))))
   
(defun extract-source-filename-from-stream (stream)
  "Посмотреть в SLIME. По идее, должно всегда работать pathname stream, но данная ф-я чётко ограничивает список классов потоков, с которыми работает, и возвращает nil вместо ошибки для потоков, читающих не из файла"
;  (declare (optimize speed))
  #-(or lispworks sbcl)
  (note-not-implemented-for-this-lisp extract-source-filename-from-stream)
  #+lispworks
  (etypecase stream
    (editor::editor-region-stream
     (slot-value (slot-value (slot-value stream 'editor::point) 'editor::buffer) 'editor::%pathname))
    (stream::ef-file-stream (slot-value (slot-value stream 'stream::underlying-stream) 'stream::path))
    (stream::file-stream (slot-value stream 'stream::path))
    (string-stream nil)
    )
  #+sbcl
  (etypecase stream
    (sb-impl::fd-stream (sb-impl::fd-stream-file stream))
    (file-stream (pathname stream)) ; (slot-value stream 'file)
    (string-stream nil))
  )

(defvar *stream-line-count* 
  (make-weak-key-hash-table :test 'eq)
  "Для потока отражает число прочитанных строк, если, конечно, функция чтения его прописывает. См., например
   ystok.meta::read-char1")

(defun read-char-recording-line-number (&optional stream &rest args)
  "аналог (read-char), но записывает информацию о номере строки, к-рую можно получить с помощью (get-stream-line-number).
 Информация будет верной только, если весь поток читается с помощью функций, считающих строки. Например, если читаем файл не сначала, наверняка будет неправильно"
  (let ((res (apply 'read-char stream args)))
    (when (eql res #\Newline)
      (incf (gethash stream *stream-line-count* 0) 
            ))
    res))

(defun stream-get-line-number (stream &key )
  "Если происходит запись информаци о номере строки, возвращает её. Иначе, возвращает 0"
  (assert (open-stream-p stream))
  (gethash stream *stream-line-count* 0)
  )

(defstruct string-hider "Structure for hiding strings when printing" string)
(defmethod print-object ((x string-hider) stream)
  (print-unreadable-object (x stream :type t :identity t) 
    ))

; FIXME rename в file-source-offset или что-нибудь в этом роде
(defstruct row-col-offset "Хранит положение в файле"
  (row 0) (col 0)
  b-offset ; положение в буквах
  )

#+lispworks 
(defun buffer-offset-to-row-col-offset (buffer offset-or-point)
  "По смещению в буфере возвращает row-col-offset. UNTESTED"
  (editor::with-point
     ((point (etypecase offset-or-point
               (editor::point offset-or-point)
               (integer (editor:buffers-start buffer)))))
    (when (typep offset-or-point 'integer)
      (editor:character-offset point offset-or-point))
   
    ; stolen from EDITOR:what-line-command
    (let* ((row
            (1+ (editor::count-lines
                 (editor:buffers-start buffer) point)))
           
           ; stolen from editor::show-cursor-position-info <- EDITOR:what-cursor-position-command
           (col (editor::point-column point))
           (b-offset (editor::point-to-offset point)))
      (make-row-col-offset :row row :col col
                           :b-offset b-offset))))


(defun numeric-file-position-to-buffer-offset (nfp)
  (etypecase nfp
    (lexem-pos:file-and-file-position
     (let ((file (lexem-pos:file-and-file-position-file nfp))
           (position (lexem-pos:file-and-file-position-position nfp)))
       (cond
        (file 
         (show-expr "budden-tools::numeric-file-position-to-buffer-offset - Здесь вызывается очень медленная функция")      
         (fix-offset-2 file position))
        (t
         (show-expr "numeric-file-position-to-buffer-offset - ничего не могу сделать - информация о файле утеряна")
         position))))
    (lexem-pos:file-offset-chars-newline-is-1
     (lexem-pos:file-offset-chars-newline-is-1-offset nfp))))

(defun row-col-offset-to-buffer-offset (offset-or-row-col-offset)
  "На вход принимает row-col-offset или число (уже смещение в буфере). Возвращает смещение в буфере"
  (etypecase offset-or-row-col-offset
    (integer
     offset-or-row-col-offset)
    (row-col-offset
     (row-col-offset-b-offset offset-or-row-col-offset)
     )
    (lexem-pos:numeric-file-position
     (numeric-file-position-to-buffer-offset offset-or-row-col-offset))
    (lexem-pos:char-frc
     (break "Ой, меня не написали"))
    ))


(defmacro note-not-implemented-for-this-lisp (string)
  "Issues warning at compile-time and error at runtime"
  (let ((format "~A не написана для этой реализации лиспа"))
    (warn format string)
    `(error ,format ',string)))
     
; Здесь переделать в row-col-offset и проследить, где используется, там заменить.

(defgeneric ИСХОДНИК-ЦЕЛИКОМ-В-ВИДЕ-СТРОКИ (ИСХОДНИК)
  (:documentation "Возвращает весь текст из исходника как одну строку (когда это возможно)"))

#+lispworks
(defmethod ИСХОДНИК-ЦЕЛИКОМ-В-ВИДЕ-СТРОКИ ((ИСХОДНИК editor:buffer))
  (perga-implementation:perga
    (let start (editor:buffers-start ИСХОДНИК))
    (let end (editor:buffers-end ИСХОДНИК))
    (EDITOR:points-to-string start end)))

(defmethod ИСХОДНИК-ЦЕЛИКОМ-В-ВИДЕ-СТРОКИ ((ИСХОДНИК string-stream))
  "Крадёт строку из входящего потока чтения из строки"
  #+lispworks
  (let ((s (the* SYSTEM::string-input-stream ИСХОДНИК)))
    (slot-value (slot-value ИСХОДНИК 'stream::buffer-state) 'stream::input-buffer))
  #+sbcl
  (sb-impl::string-input-stream-string ИСХОДНИК)
  ;(let ((s (the* sb-impl::string-input-stream stream)))
  ;  (slot-value s 'string))
  #-(or sbcl lispworks)
  (note-not-implemented-for-this-lisp "defmethod КАРТЫ-ИСХОДНИКОВ-ЛИЦО:ИСХОДНИК-ЦЕЛИКОМ-В-ВИДЕ-СТРОКИ ((ИСХОДНИК string-stream))")
  )

(def-trivial-test::! string-stream-extract-string.1
                     (with-input-from-string (s "asdf")
                       (read-char s)
                       (ИСХОДНИК-ЦЕЛИКОМ-В-ВИДЕ-СТРОКИ s))
                     "asdf"
                     :test 'string=)
                     

(defun file-stream-extract-encoding (stream)
  #+lispworks
  (etypecase stream
    (STREAM::ef-file-stream
     (slot-value
      (slot-value stream 'STREAM::ef-details)
      'stream::ef-spec
      )))
  #+sbcl
  (etypecase stream
    (sb-sys::fd-stream
     (slot-value stream 'sb-impl::external-format)))
  #-(or lispworks sbcl)
  (note-not-implemented-for-this-lisp file-stream-extract-encoding)
  )


(defmethod ИСХОДНИК-ЦЕЛИКОМ-В-ВИДЕ-СТРОКИ ((ИСХОДНИК file-stream))
  "ПРАВЬМЯ не проверяем устарелость исходника"
  (break "untested and unused!")
  (let* ((filename (extract-source-filename-from-stream ИСХОДНИК))
         (ef (file-stream-extract-encoding ИСХОДНИК)))
    (assert filename)
    (read-file-into-string filename :external-format ef)))

(defun extract-entire-string-from-stream (stream)
  (ИСХОДНИК-ЦЕЛИКОМ-В-ВИДЕ-СТРОКИ stream))

(defun fp-half-divide-to-floor (f field)
  "Ищет в двумерном массиве field Nx2 по первому эл-ту второй. Находит вхождение, где первый эл-т, меньше или равен f. Если несколько эл-тов одинаковы, выберет какой-то из них. Если эл-т меньше минимального, то это ошибка. Если больше максимального - вернёт максимальный"
  (perga-implementation:perga function
    (let len (array-dimension field 0))
    (let a 0)
    (let b (- len 1))
    (let f_a (aref field a 0))
    (let f_b (aref field b 0))
    (flet do-return (x f_x)
      (return-from function (values x f_x (aref field x 1))))
    (loop
     ;(when (= b a)
     ;  (return-from function (values a f_a (aref field a 1))))
     (cond
      ((not (<= f_a f))
       (error "hp-half-divide: out of range"))
      ((<= f f_a) (do-return a f_a))
      ((<= f_b f) (do-return b f_b))
      ((> (+ a 2) b) (do-return a f_a))
      )
     (let xm (ceiling (/ (+ a b) 2)))
     (let f_xm (aref field xm 0))
     (cond
      ((<= f f_xm)
       (setf b xm f_b f_xm))
      ((<= f_xm f) 
       (setf a xm f_a f_xm))
      (t
       (error "something is wrong")))
     ;(print xm)
     )))

(defun build-file-position-to-char-position-map (stream)
  "Возвращает nil либо 2-мерный массив соответствий, по к-рому нужно искать двоичным поиском. Непонятно, почему не просто массив, позволяющий за константное время получить искомое соответствие - видимо, ошибка проектирования"
  (perga-implementation:perga
    (:lett stream file-stream stream)
    (let map-list nil)
    (let cur-char-position 0)
    (let filename (extract-source-filename-from-stream stream))
    (let ef (file-stream-extract-encoding stream))
    (when filename
      (:@ with-open-file (in filename :external-format ef))
      (push (cons 0 0) map-list)
      (loop
        (let line (read-line in nil nil))
        (unless line (return))
        (_f + cur-char-position (length line) 1)
        (push (cons (file-position in) cur-char-position) map-list)
        )
      (_f nreverse map-list)
      (let map-size (length map-list))
      (let result (make-array (list map-size 2)))
      (let i 0)
      (dolist (l map-list)
        (setf (aref result i 0) (car l))
        (setf (aref result i 1) (cdr l))
        (incf i))
      result)))

(defun file-position-and-map-to-char-position (file-position map)
  "map получаем из build-file-position-to-char-position-map"
  (perga-implementation:perga
   (cond
    ((null map) 0)
    (t
     (:@ mlvl-bind (index start-file-offset start-char-offset)
         (fp-half-divide-to-floor file-position map))
     (ignored index)
     (+ file-position (- start-file-offset) start-char-offset)))))

(defvar *stream-to-file-position-to-char-position-maps*
  (SWANK-BACKEND:make-weak-key-hash-table :test 'eq)
  "Потоку сопоставляем карту соответствий между file-position и char-position")

(defun ensure-file-position-to-char-position-for-stream (stream)
  (perga-implementation:perga
    (:lett stream file-stream stream)
    (let res (gethash stream *stream-to-file-position-to-char-position-maps*))
    (unless res
      (setf res (build-file-position-to-char-position-map stream))
      (setf (gethash stream *stream-to-file-position-to-char-position-maps*) res))
    res))

(defun input-stream-position-in-chars (stream)
  "Возвращает текущую позицию в потоке в буквах. В отличие от обычного file-position, к-рый извлекает её в буквах исходного файла - может отличаться на cr/lf. 
  См. также fix-offset-2"
  (etypecase stream
    (ПОТОКИ-ЗПТ-СЧИТАЮЩИЕ-БУКВЫ-СТРОКИ-И-КОЛОНКИ:|Считающий-входной-поток-литер|
     (ПОТОКИ-ЗПТ-СЧИТАЮЩИЕ-БУКВЫ-СТРОКИ-И-КОЛОНКИ:|Счётчик-литер-из| stream))                                            
    (string-stream (file-position stream))
    (file-stream
     (let ((map (ensure-file-position-to-char-position-for-stream stream)))
       (file-position-and-map-to-char-position (file-position stream) map)))
    #+sbcl
    (two-way-stream 0)
    #+lispworks
    (editor::editor-region-stream
     (let ((point (slot-value stream 'editor::point)))
       (buffer-offset-to-row-col-offset
        (EDITOR:point-buffer point)
        point)))
    (synonym-stream
     (or
      ;; что-то случилось в SBCL 1.3.18 и в консоли попытка получить положение перестала работать
      (ignore-errors 
       (input-stream-position-in-chars (symbol-value (synonym-stream-symbol stream))))
      0))
    ))

(defun fix-offset-2 (pathname offset)
  "Имеется числовой offset, к-рый вернул file-position. Давайте попробуем превратить его в смещение в буквах, считая #\newline за 1 букву. Предпочтительнее (гораздо эффективнее) использовать input-stream-position-in-chars"
  (with-open-file (stream pathname)
    (let ((map (ensure-file-position-to-char-position-for-stream stream)))
       (file-position-and-map-to-char-position offset map))))

; FIXME Deprecated - используй input-stream-position-in-chars 
(defun extract-file-position (stream)
  "Возвращает текущую позицию в потоке в буквах. В отличие от обычного file-position, к-рый извлекает её в буквах исходного файла - может отличаться на cr/lf. " 
  (input-stream-position-in-chars stream))
       
;;; сделать аналогично для лексемы. 
;;; потом сделаем слияние-упрощение

;;; ПРИМЕНИТЬ read-and-record-non-persistent-location



#| (trivial-deftest:deftest 
       non-persistent-locations-work 
       (not (null (with-open-file
                      (in "c:/lisp/lisp/lib/budden-tools/see-packages.asd")
                    (let* ((line (read-line in)))
                      (record-non-persistent-location in 0 line)
                      (get-non-persistent-object-locations line)
                      ))))
       t)

|#

(defgeneric l/rorl (obj position)
  (:documentation
   "l/return-object-recording-location - объект образовался в результате некоего процесса (чтения) исходного файла из места, выраженного position. Запомнить в базе данных, что этот объект был считан в этом месте, и вернуть объект"))


#|
Пример: META-PARSE-FIREBIRD:GENERATE-CODE-FROM-FIREBIRD-LEXEMS

 `(budden-tools::l/princ ; запоминает куда вывели
                       (budden-tools::l/rorl ; запоминает откуда взяли, и извлекает текст из лексемы
                        ,(lexem-text lexem)
                        ',(lexem-position lexem))
                       cos)

|#

(defmethod l/rorl (obj (position null))
  obj)

(defmethod l/rorl (obj (position lexem-pos:lexem-pos))
;   (print (list obj source beg end))
  "l/return-object-recording-location"
  (when (track-locations)
    (let (source beg end)
      (when position
        (setf source (lexem-pos:lexem-pos-file position))
        (setf beg (lexem-pos:lexem-pos-start position))
        (setf end (lexem-pos:lexem-pos-end position))
        )
      (setf beg (row-col-offset-to-buffer-offset beg))
      (setf end (row-col-offset-to-buffer-offset end)) ; было beg - очевидная ошибка ПРАВЬМЯ - или я что-то недоучёл, исправляя её.
      (when source
        "ПРАВЬМЯ Нам нужно взять не какой-то один источник, а все. Поскольку мы не умеем ходить по всем, мы не можем даже выбрать один из них, мы просто берём самый специфичный. Это нужно исправлять синхронно с остальными методами"
        (assert (>= end beg))
        (cond 
         ((symbolp obj) ; символ запоминает своё место навсегда, это плохо. В контексте можно более-менее избавиться от этого, но если символ встречается много раз подряд, то всё равно будет плохо
      ; мы могли бы копировать символ в этом месте, но это, наверняка, ещё хуже!
          (make-slo :source source :beg beg :end end))
         (t       
          (let ((locs (get-non-persistent-object-locations obj)))
            (or locs 
                (setf (get-non-persistent-object-locations obj) (make-slo :source source :beg beg :end end)))))))
      ))
  obj
  )

(defun l/substitute-subseq (seq sub rep &key (start 0) end
                          (test #'eql) (key #'identity))
  "Like cllib:substitute-subseq, but takes locations into account. seq and result are strings" 
;  (declare (optimize speed))
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
  "форма, которая запишет заданное положение объекта и вернёт его"
;  (declare (optimize speed))
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

(defun do-my-sanity-check ()
  (let ((ss (open #. *compile-file-truename*)))
    (assert (= (input-stream-position-in-chars ss) 0))
    (read-line ss)
    (read-line ss)
    (show-expr (input-stream-position-in-chars ss))
    (show-expr (file-position ss))
    (assert (= (input-stream-position-in-chars ss)
               (or
                #+(and sbcl win32) 106
                #+(and lispworks win32) 106
                #+(and sbcl unix) 106
               -1000 ; write me
               )))
    (print "sanity check ok")
    ))

(do-my-sanity-check)
    
      
