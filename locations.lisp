;;; -*- Encoding: utf-8; -*-
;;; Universal location recorder

#|
Общая идея. Мы хотим отслеживать исходник в ходе порождения текстов. 
Порождение может начинаться с примитивов, считанных из исходного файла лиспа.
Для них мы должны уметь знать место, где этот примитив считан. 

Дальше задача - не потерять это место, в ходе преобразования исходника. 

Какие могут быть преобразования?

вставка в `(:l ,???) 
pd/pds в строку или файл
fbody
~~


мы должны не потерять инфу при каждом из этих преобразований и для этого нужно 
внести изменения в каждый примитив. Если в ходе выполнения примитива порождается
новый объект, мы создаём новую карту для этого объекта. 

Эти преобразования, в свою очередь, могут состоять из примитивов, таких как:
(str+)
(format)
(with-output-to-string (s) (princ ... s))

Нам придётся заменить все эти примитивы там, где они используются.

Для объетков в памяти, карты живут в слабой eq хеш-таблице, она называется
*nplm = *non-persistent-locations-memory*
Для файлов, карты живут в сильной string-equal хеш-таблице, она называется
*nplf = *non-persistent-locations-files*

При with-output-to-string выходного объекта ещё не существует. 
Значит, мы временно ассоциируем информацию с заместителем и потом
заменяем заместитель на строку. Заместитель является локальной переменной (make-symbol)

Если у нас есть какой-то поток или мы собираем строку, то строки у нас прямо сейчас ещё нет. 
Но информацию записать надо. Для этого используется заместитель. В случае потока он 
является make-symbol и находится в слабой хеш-таблице
*slmd = *stream-location-map-delegates*
В других случаях он может быть чем угодно. 

Карта может быть связана с лисп-объектом, строкой или файлом.
В случае лисп-объекта, карта хранит данные только о самом объекте, но не о его начинке
В случае строки, карта хранит данные обо всех подстроках
В случае файла, карта хранит данные обо всех подстроках файла, но она хранится
не в переменной 

В любом случае,отсутствие информации не должно быть ошибкой

Задача N1 - без persistent хранения. 

olm = object-location-map
segment-sources = ses
source-location = slo

В отношении компиляции-загрузки. Лексемы fbody живут только в load-time, для них запись 
программируется явно. 


API


extract-source-filename-from-stream - если поток связан с файлом, возвращает его имя
l/ohbr = location/object-has-been-read - прочитали объект из исходника. запишем его местоположение
read-and-record-non-persistent-location - прочитать объект и запомнить его местоположение

l/princ - напечатать объект в поток и попытаться интегрировать его location в карту, 
связанную с этим потоком

l/add-to-location-map delegate dst-beg dst-end object src src-beg src-end - сопоставляем с делегатом
данные о расположении

l/pass-from-delegate-to-object delegate object - переносим информацию, ассоциированную с делегатом, на объект. Тут есть ловушка - если делегат уже где-то учтён в списках или структурах, инфа будет потеряна.
l/pass-from-delegate-to-file - то же, для файла (???)

l/return-object-recording-location = l/rorl - форма, которая запишет положение объекта и вернёт его
srcpl - symbol-readmacro. Прочитать объект и запрограммировать запоминание его места в load-time.
(надо потом сделать, чтобы по желанию рекурсивно записывались места всех под-объектов)
|#

(in-package :budden-tools)

; (proclaim '(optimize (speed 3) (debug 0) (safety 0)))



(defvar *nplm 
  #+NIL (make-hash-table :test 'eq)
  (SWANK-BACKEND:MAKE-WEAK-KEY-HASH-TABLE :test 'eq) 
  "Временные сведения о расположении для объектов в памяти")

(defvar *nplf 
  (make-hash-table :test 'equal)
  "Сведения о расположении, для ИМЁН файлов (namestring). Сильная таблица, т.к. файлы существуют долго")

(defvar *slmd
  (SWANK-BACKEND:MAKE-WEAK-KEY-HASH-TABLE :test 'eq)
  "Делегаты потоков")

(defvar *record-locations* t)

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

(defstruct ses ; для данного отрезка может задавать несколько исходников. Эту структуру нужно хранить в контексте destination, например, 
    ; в olm
  beg ; в результирующем объекте
  end
  sources ; список элементов slo, задающий исходники данного отрезка (пока что один)
  simplified ; t, если уже упрощена
  )

(defstruct slo ; откуда взялся данный объект
  source ; например, имя файла
  beg ; в исходном объекте
  end)



(defmacro l/with-output-to-string ((VAR &REST SOME-MORE) 
                                   &BODY BODY)
  (with-gensyms (location-delegate)
  `(let (,location-delegate)
     (l/pass-from-delegate-to-object 
      (with-output-to-string (,var ,@some-more)
        (setf ,location-delegate (get-stream-location-map-delegate ,var :if-not-exists :create))
        ,@body
        )
      ,location-delegate))))

(defun l/pass-from-delegate-to-object (object delegate)
  "delegate может быть nil. Возвращает объект"
;  (declare (optimize speed))
  (unless (eq delegate object)
    (setf (gethash object *nplm) (simplify-location-map (gethash delegate *nplm)))
    (remhash delegate *nplm)
    )
  object)

(defun l/pass-from-stream-to-file (stream)
  "данные были связаны с потоком. Ассоциируем их с файлом"
;  (declare (optimize speed))
  (let1 delegate (get-stream-location-map-delegate stream)
    (setf (gethash (namestring (extract-source-filename-from-stream stream))
                   *nplf)
          (simplify-object-location-map delegate))
    (remhash delegate *nplm))
  nil)      

(defun l/add-to-location-map (delegate dst-beg dst-end object)
;  (declare (optimize speed))
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
  "Только во временном хранении"
;  (declare (optimize speed))
  (proga
    (let map (if simplify
                 (simplify-object-location-map string)
               (get-non-persistent-object-locations string)))
    (let result (subseq string start end))
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
      )
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
      ((dst-beg (extract-file-position stream))
       (delegate (get-stream-location-map-delegate stream)))
    (prog1
        (typecase object
          (string (write-string object stream))
          (t (princ object stream)))
      (let* ((dst-end (extract-file-position stream)))
        (when delegate 
          (l/add-to-location-map delegate dst-beg dst-end object)))
      )))

(defun l/str+ (&rest objects)
  "Могут быть какие-то отличия, т.к. (string x) в обычном str+ может отличаться от (princ-to-string x) здесь. Тогда нужно подправить."
;  (declare (optimize speed))
  (let1 res
      (l/with-output-to-string (ss) 
        (mapcar (lambda (x) (l/princ x ss)) objects)
        )
    (simplify-object-location-map res)
    res))
 
(defun get-stream-location-map-delegate (stream &key if-not-exists)
;  (declare (optimize speed))
   
  (etypecase stream
    #+lispworks (editor::editor-region-stream stream)
    (stream::file-stream stream)
    #+lispworks (stream::ef-file-stream stream)
    (stream 
     (ecase if-not-exists
       (:create (nth-value 0 (ensure-gethash-2 stream *slmd (gensym "LOCATION-DELEGATE"))))
       ((nil) (gethash stream *slmd))
       )))
  )

(defun extract-source-filename-from-stream (stream)
  "Посмотреть в SLIME"
;  (declare (optimize speed))
  #-lispworks (error "Not implemented")
  #+lispworks
  (etypecase stream
    (editor::editor-region-stream
     (slot-value (slot-value (slot-value stream 'editor::point) 'editor::buffer) 'editor::%pathname))
    (stream::ef-file-stream (slot-value (slot-value stream 'stream::underlying-stream) 'stream::path))
    (stream::file-stream (slot-value stream 'stream::path))
    (string-stream nil)
    ))

(defun real-point-offset (point) 
  "Дубль аналогичной функции из editor-budden-tools"
  #+lispworks
  (+ (editor::point-offset point) (slot-value (editor::point-bigline point) 'editor::start-char))
  #-lispworks 
  (error "Not implemented"))

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


(defstruct row-col-offset "Хранит положение в файле. 
  Строки и колонки начинаются с 1 (надо отметить, что в EMACS колонки начинаются с 0). Может опционально хранить offset в буфере или файле"
  (row 0) (col 0) b-offset f-offset)

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


(defun row-col-offset-to-buffer-offset (offset-or-row-col-offset)
  "На вход принимает row-col-offset или число (уже смещение в буфере). Возвращает смещение в буфере"
  (etypecase offset-or-row-col-offset
    (integer
     offset-or-row-col-offset)
    (row-col-offset
     (row-col-offset-b-offset offset-or-row-col-offset)
     )))
      
      
#+lispworks 
(defun point-file-offset (point)
  (let* ((buffer (editor::point-buffer point))
         (lineno (editor::count-lines (editor::buffers-start buffer) point)))
    (+ (real-point-offset point) lineno)))


; Здесь переделать в row-col-offset и проследить, где используется, там заменить.

(defun extract-file-position (stream)
  "Возвращает текущую позицию в потоке. Для editor-region-stream - в виде row-col-offset, иначе - число - смещение в файле" 
  (typecase stream
    #+lispworks
    (editor::editor-region-stream
     (let ((point (slot-value stream 'editor::point)))
       (buffer-offset-to-row-col-offset
        (EDITOR:point-buffer point)
        point)))
    (t (file-position stream))))
 
        
;;; сделать аналогично для лексемы. 
;;; потом сделаем слияние-упрощение

;;; ПРИМЕНИТЬ read-and-record-non-persistent-location



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
  "l/return-object-recording-location"
  (let (source beg end)
    (when position
      (dsetq (source beg end) position))
    (setf beg (row-col-offset-to-buffer-offset beg))
    (setf end (row-col-offset-to-buffer-offset beg))
    (when source
      "Здесь нам нужен нелёгкий выбор. Если объект уже имеет данные о своём расположении, 
мы предпочитаем взять их. Хотя, на самом-то деле, нам нужно взять все и сделать
набор команд, чтобы можно было ходить по ним по всем. Поскольку мы не умеем ходить
по всем, мы не можем даже выбрать один из них, мы просто берём самый специфичный"
      (assert (>= end beg))
      (cond 
       ((symbolp obj) ; символ запоминает своё место навсегда, это плохо. В контексте можно более-менее избавиться от этого, но если символ встречается много раз подряд, то всё равно будет плохо
      ; мы могли бы копировать символ в этом месте, но это, наверняка, ещё хуже!
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
