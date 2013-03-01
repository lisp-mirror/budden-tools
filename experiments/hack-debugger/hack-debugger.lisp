(in-package :budden)
(in-readtable :buddens-readtable-a)

(defun f(x) (incf x 2)(print x)(break))
(defun g() (f 3))

#|
0 dbg::call-frame-edit-path > ...
  >> dbg::c : #<dbg::call-frame g>
0 dbg::call-frame-edit-path < ...
  << VALUE-0 : 30
0 dspec:find-dspec-locations > ...
  >> dspec:dspec : g
0 dspec:find-dspec-locations < ...
  << VALUE-0 : (((defun g) #P"C:/lisp/sw/fb2/test4.lisp"))
0 editor::execute-in-window-process > ...
  >> capi::pane          : #<capi:listener-pane capi:editor-pane  216ADFAB>
  >> capi::function-spec : (editor::find-lisp-dspec-location ((defun g) #P"C:/lisp/sw/fb2/test4.lisp" 30))
0 editor::execute-in-window-process < ...
  << VALUE-0 : nil

|#

#|

0 editor::find-lisp-dspec-location > ...
  >> editor::loc : ((defun budden::g) #P"C:/lisp/sw/fb2/test4.lisp" 30)
  1 editor::prepare-to-show-dspec-in-buffer > ...
    >> editor:buffer      : #<editor:buffer test4.lisp>
    >> editor::dspec      : #S(editor::lisp-dspec-location :file #P"C:/lisp/sw/fb2/test4.lisp" :dspec (defun budden::g) :path 30)
    >> editor::no-error-p : t
    >> editor::point      : #<editor::point "test4.lisp" 0 offset 107 200F5A77>
    2 editor:find-dspec-in-buffer > ...
      >> editor::dspec : #S(editor::lisp-dspec-location :file #P"C:/lisp/sw/fb2/test4.lisp" :dspec (defun budden::g) :path 30)
      >> editor:buffer : #<editor:buffer test4.lisp>
      >> editor::point : #<editor::point "test4.lisp" 0 offset 107 200F5A77>
      3 editor::find-dspec-in-buffer-for-lisp-mode > ...
        >> editor::dspec     : #S(editor::lisp-dspec-location :file #P"C:/lisp/sw/fb2/test4.lisp" :dspec (defun budden::g) :path 30)
        >> editor:buffer     : #<editor:buffer test4.lisp>
        >> editor::point     : #<editor::point "test4.lisp" 0 offset 107 200F5A77>
        >> editor::predicate : #<Function dspec:dspec-equal 209B6152>
      3 editor::find-dspec-in-buffer-for-lisp-mode < ...
        << VALUE-0 : #<editor::point "test4.lisp" 0 offset 107 200F5A77>
    2 editor:find-dspec-in-buffer < ...
      << VALUE-0 : #<editor::point "test4.lisp" 0 offset 107 200F5A77>
  1 editor::prepare-to-show-dspec-in-buffer < ...
    << VALUE-0 : (#<editor::point "test4.lisp" 0 offset 107 200F5A77>)
0 editor::find-lisp-dspec-location < ...
  << VALUE-0 : #<editor::i-point "test4.lisp" 0 offset 107 2378950B>
  << VALUE-1 : #<editor:buffer test4.lisp>


|#


#|
0 editor::find-dspec-in-buffer-for-lisp-mode > ...
  >> editor::dspec     : #S(editor::lisp-dspec-location :file #P"C:/lisp/sw/fb2/test4.lisp" :dspec (defun budden::g) :path 30)
  >> editor:buffer     : #<editor:buffer test4.lisp>
  >> editor::point     : #<editor::point "test4.lisp" 0 offset 107 200BEBDB>
  >> editor::predicate : #<Function dspec:dspec-equal 209B6152>
  1 editor::find-form-region-from-path-no-modify > ...
    >> editor::path  : 30
    >> editor::start : #<editor::point "test4.lisp" 0 offset 96 200BEBDB>
    >> editor::end   : #<editor::point "test4.lisp" 0 offset 96 200BD5B7>
  1 editor::find-form-region-from-path-no-modify < ...
    << VALUE-0 : #<editor::point "test4.lisp" 0 offset 112 200BD5B7>
0 editor::find-dspec-in-buffer-for-lisp-mode < ...
  << VALUE-0 : #<editor::point "test4.lisp" 0 offset 107 200BEBDB>
|#

; это мы выяснили, кто отвечает за переход к определению, а как оно вычисляетя, мы не знаем. Но нам это и не нужно
; мы можем сделать файловый транслятор и перехватывать выделение. 