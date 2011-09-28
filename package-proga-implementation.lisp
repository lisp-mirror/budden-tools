(merge-packages-simple:defpackage-autoimport :proga-implementation
                                             (:use :cl)
                                             (:auto-import-from :budden-tools :trivial-deftest :defstruct-meta)
                                             (:auto-import-first-clashing t)
                                             )