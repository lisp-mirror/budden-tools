(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel)  
  (error "Don't compile me!"))

(eval-when (:execute)
  (print "file-to-load is loading as a source"))
