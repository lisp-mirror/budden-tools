#+clisp (setf *default-file-encoding* 
      (ext:make-encoding :charset 'charset:windows-1251 :line-terminator :dos))

(require :asdf)
(push "/home/denis/clbuild/systems/" asdf:*central-registry*)
(push "/home/denis/sw/" asdf:*central-registry*)
(push #p"/home/denis/lisp/lib/systems/" asdf:*central-registry*) 

(defvar asdf::*source-to-target-mappings*  
  '(("/usr/local/lib/sbcl/" nil))) 

(asdf:oos 'asdf:load-op :asdf-binary-locations)
(asdf:oos 'asdf:load-op :iterate-keywords)

(load "/home/denis/lisp/lib/cl-fix/asdf-tools")
(load "/home/denis/lisp/lib/cl-fix/merge-packages-and-reexport")

(asdf::! :see-packages)

