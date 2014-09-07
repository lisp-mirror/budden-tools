(defpackage :iterk
  (:nicknames :iterate-keywords-tiny-export)
  (:use #:cl #:iterate-keywords)
  (:import-from #:iterate-keywords #:keywordize)
  (:export #:iter #:dsetq #:keywordize))
