;;----------------------------------------------------------------------------
;; This is bitmap split tool
;; based on examples/capi/graphics/images.lisp
;; which has Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;
;; work made by Budden is in public domain
;;
;; This piece of software can be used to fix problem with 16-bit colors in IDE.
;; Lispworks IDE does not have 16-bit icons so they become Malevich' black squares
;; when connected via RDP (Remote Desktop Protocol) client.
;; 
;; One can switch to 32 bit color or take earlier version of IDE, then make a shot 
;; of screen with icons (Alt-Prt Sc), save shot as 8-bit bmp with standard 
;; paint tool and then split image to 16x16 images with the help of this tool. 
;; 
;; Main function is 
;;
;; (split-8bit-bmp-to-icons:extract-all-icons) 
;; 
;; it splits file created by Budden and shows all bmp's it created. 
;;
;; Some care is taken to transparency index, but some icons are still ugly. 
;; 
;; Tested on 32 bit Windows XP in Lispworks 6.1 personal
;;----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :split-8bit-bmp-to-icons
    (:use :cl :lispworks)
    (:export #:extract-all-icons)))

(in-package :split-8bit-bmp-to-icons)

(defparameter *super-image* nil)
(defparameter *super-port* nil)
(defparameter *my-dir* (namestring (pathname-location #.(current-pathname))))

;;----------------------------------------------------------------------------
;; Define an interface
;;----------------------------------------------------------------------------

(capi:define-interface icon-source-tool ()
  ((image :initform nil)
   (transparent-color-index :initform nil :initarg :transparent-color-index)
   (background-color :initform :white :initarg :background-color))
  (:panes
   (viewer capi:output-pane
           :display-callback 'display-icon-source-tool
           :background :white
           :horizontal-scroll t
           :vertical-scroll t
           :visible-min-width 500
           :visible-min-height 375)
   (controller capi:push-button-panel
               :items '(:change... :close)
               :callbacks '(icon-source-tool-change-image
                            icon-source-tool-close-image)
               :callback-type :interface
               :print-function 'string-capitalize))
  (:layouts
   (main-layout capi:column-layout
                '(viewer controller)))
  (:default-initargs
   :layout 'main-layout
   :best-width 200
   :best-height 200))

(defmethod initialize-instance :after ((self icon-source-tool) &key
                                       &allow-other-keys)
  (setf (slot-value (slot-value self 'viewer) 'capi::background)
        (slot-value self 'background-color))
  (update-icon-source-tool-enabled self))

(defun update-icon-source-tool (interface)
  (with-slots (viewer image) interface
    (gp:invalidate-rectangle viewer)
    (capi:set-horizontal-scroll-parameters viewer :min-range 0 :max-range (if image (gp:image-width image) 0))
    (capi:set-vertical-scroll-parameters viewer :min-range 0 :max-range (if image (gp:image-height image) 0))
    (update-icon-source-tool-enabled interface)))

(defun update-icon-source-tool-enabled (interface)
  (with-slots (controller image) interface
    (if image
        (capi:set-button-panel-enabled-items
         controller
         :set t)
      (capi:set-button-panel-enabled-items
       controller
       :set t
       :disable '(:close)))))

(defun display-icon-source-tool (pane x y width height)
  (with-slots (image) (capi:top-level-interface pane)
    (when image
      (when (gp:rectangle-overlap x y (+ x width) (+ y height)
                                  0 0 (gp:image-width image) (gp:image-height image))
        (gp:draw-image pane image 0 0)))))

(defvar *image-file-filters* '("Bitmap" "*.bmp;*.dib" 
                               "GIF"    "*.gif"  
                               "JPEG"   "*.jpg;*.jpeg" 
                               "PNG"    "*.png" 
                               "TIFF"   "*.tif;*.tiff"))

(defvar *image-file-types* '("bmp" "dib" "gif" "jpg" "jpeg" "png" "tif" "tiff"))

(defun icon-source-tool-change-image (interface &optional filename)
  (with-slots (viewer image) interface
    (let ((file (or filename
                    (capi:prompt-for-file "Choose a bitmap"
                                          :pathname *my-dir*
                                          :filter (second *image-file-filters*)
                                          :filters *image-file-filters*
                                          :ok-check #'(lambda (file)
                                                        (member (pathname-type file) *image-file-types* :test 'equalp))))))
      (when (and file (probe-file file))
        (let ((external-image 
               (gp:read-external-image
                file :transparent-color-index 
                (slot-value interface 'transparent-color-index))))
          (when image
            (gp:free-image viewer image))
          (setf image (gp:load-image viewer external-image))
          (update-icon-source-tool interface))))))

(defun icon-source-tool-close-image (interface)
  (with-slots (viewer image) interface
    (gp:free-image viewer (shiftf image nil))
    (update-icon-source-tool interface)))


(defun show-external-image (ei)
  (let* ((b (capi:contain (make-instance 'capi:pinboard-layout)))
         (copy (gp:load-image b ei :editable :with-alpha)))
    (capi:manipulate-pinboard
     b
     (make-instance 'capi:image-pinboard-object
                    :image copy :x 0 :y 0)
     :add-top)))



(defun open-icon-source-tool (name &optional (transparent-color-index 8) (background-color :grey84))
  "Brings up a window with a screenshot so that sub-images can be created"
  (let* (;(*transparent-color-index* (or *transparent-color-index* transparent-color-index))
         (interface 
          (capi:display 
           (make-instance 'icon-source-tool 
                          :transparent-color-index transparent-color-index
                          :background-color background-color))))
    (icon-source-tool-change-image 
     interface
     name)
    interface))


(defun extract-and-save-one-icon (x y transparent-color-index &optional (name "test"))
  (let* ((sub
          (gp:make-sub-image *super-port* *super-image* x y 16 16))
         (sube 
          (gp:externalize-image *super-port* sub))
         real-name)
    (show-external-image sube)
    (when name
      (setf real-name (concatenate 'string *my-dir* "../img/" name ".bmp"))
      (gp:write-external-image sube real-name
                               :if-exists :supersede
                               )
      (open-icon-source-tool real-name transparent-color-index :blue))))


(defun extract-all-icons ()
  "Open ../img/many-icons.bmp and split it by hand written coordinates"
  (setf *super-port* (setf *super-port* 
                           (open-icon-source-tool 
                            (concatenate 'string *my-dir* "../img/many-icons.bmp"))))
  (setf *super-image* (slot-value *super-port* 'image)) 
  
  (extract-and-save-one-icon (+ 9 (* 24 2)) 6 0 "lispworks-tools.refresh") 
  (extract-and-save-one-icon (+ 9 (* 24 2)) 6 2 "lispworks-tools.preferences") 

  (extract-and-save-one-icon 185 6 nil "lispworks-tools.breakpoint")
  (extract-and-save-one-icon (+ 185 24) 6 nil "macroexpand")
  (extract-and-save-one-icon (+ 185 (* 24 2)) 6 nil "lispworks-tools.compile-buffer")
  (extract-and-save-one-icon (+ 185 (* 24 3)) 6 nil "lispworks-tools.compile-definition")
  (extract-and-save-one-icon (+ 185 (* 24 4)) 6 nil "lispworks-tools.evaluate-definition")

  (extract-and-save-one-icon (+ 9 (* 24 1)) 6 1 "capi.clone")
  (extract-and-save-one-icon 94 6 0 "history-previous")
  (extract-and-save-one-icon 133 6 0 "history-next")
  (extract-and-save-one-icon 440 6 0 "continue")
  (extract-and-save-one-icon (+ 440 24) 6 0 "abort")
  (extract-and-save-one-icon (+ 440 (* 24 2)) 6 0 "lispworks-tools.previous-frame")
  (extract-and-save-one-icon (+ 440 (* 24 3)) 6 0 "lispworks-tools.next-frame")
  (extract-and-save-one-icon (+ 441 (* 24 4)) 6 0 "lispworks-tools.backtrace")
  (extract-and-save-one-icon (+ 441 (* 24 5)) 6 0 "lispworks-tools.print-bindings")
  (extract-and-save-one-icon (+ 441 (* 24 6)) 6 nil "lispworks-tools.find-source")
  (extract-and-save-one-icon (+ 441 (* 24 7)) 6 0 "lispworks-tools.debugger")
  (extract-and-save-one-icon 308 6 0 "listen")
  (extract-and-save-one-icon (+ 308 24) 6 0 "lispworks-tools.find-object-source")
  (extract-and-save-one-icon (+ 308 (* 2 24)) 6 0 "inspect")
  (extract-and-save-one-icon (+ 308 (* 3 24)) 6 0 "class") 

  (extract-and-save-one-icon 638 8 0 "step")
  (extract-and-save-one-icon (+ 638 24) 8 0 "lispworks-tools.step-through-call")
  (extract-and-save-one-icon (+ 638 (* 2 24)) 8 0 "lispworks-tools.step-to-call")
  (extract-and-save-one-icon (+ 638 (* 3 24)) 8 0 "lispworks-tools.step-to-value")
  (extract-and-save-one-icon (+ 638 (* 4 24)) 8 0 "lispworks-tools.step-next")
  (extract-and-save-one-icon (+ 638 (* 5 24)) 8 0 "lispworks-tools.step-to-end")
  (extract-and-save-one-icon (+ 638 (* 6 24)) 8 0 "lispworks-tools.step-to-cursor") 

  (extract-and-save-one-icon (+ 638 (* 7 24)) 8 0 "continue")
  (extract-and-save-one-icon 924 8 0 "lispworks-tools.undo-macroexpand")
  (extract-and-save-one-icon (+ 441 (* 24 6)) 6 0 "lispworks-tools.show-current-source") 

  (extract-and-save-one-icon 471 38 nil "lispworks-tools.start-search")  
  (extract-and-save-one-icon (+ 471 23) 38 nil "lispworks-tools.stop-search")  
  (extract-and-save-one-icon (+ 471 (* 2 23)) 38 nil "lispworks-tools.query-replace") 

  (extract-and-save-one-icon (+ 517 23) 38 nil "lispworks-tools.query-replace")



  (extract-and-save-one-icon 10 38 nil "find-listener")
  (extract-and-save-one-icon (+ 24 10) 38 nil "find-editor")
  (extract-and-save-one-icon (+ (* 2 24) 10) 38 nil "find-output-browser")
  (extract-and-save-one-icon (+ (* 3 24) 10) 38 nil "find-inspector")
  (extract-and-save-one-icon (+ (* 4 24) 10) 38 nil "find-class-browser")
  (extract-and-save-one-icon (+ (* 5 24) 10) 38 nil "find-gf-browser")

  (extract-and-save-one-icon (+ (* 6 24) 10) 38 nil "find-object-clipboard")
  (extract-and-save-one-icon (+ (* 7 24) 10) 38 nil "find-function-call-browser")
  (extract-and-save-one-icon (+ (* 8 24) 10) 38 nil "find-system-browser")
  (extract-and-save-one-icon (+ (* 9 24) 10) 38 nil "find-compilation-conditions-browser")

  (extract-and-save-one-icon (+ (* 10 24) 10) 38 nil "find-search-files")
  (extract-and-save-one-icon (+ (* 11 24) 10) 38 nil "find-profiler")
  (extract-and-save-one-icon (+ (* 12 24) 10) 38 nil "find-stepper")
  (extract-and-save-one-icon (+ (* 13 24) 10) 38 nil "find-window-browser")
  (extract-and-save-one-icon (+ (* 14 24) 10) 38 nil "find-process-browser")
  (extract-and-save-one-icon (+ (* 15 24) 10) 38 nil "find-shell")


  (extract-and-save-one-icon 587 37 nil "lispworks-tools.kill")
  (extract-and-save-one-icon 540 38 nil "lispworks-tools.stop-process")
  (extract-and-save-one-icon (+ 417 24) 6 nil "lispworks-tools.unstop-process")

  (extract-and-save-one-icon 632 40 nil "find-tracer")
  (extract-and-save-one-icon 610 39 nil "find-symbol-browser")
  

  (extract-and-save-one-icon 667 38 nil "clear-output")
  (extract-and-save-one-icon (+ 23 667) 38 nil "lispworks-tools.collapse-all")
  (extract-and-save-one-icon (+ (* 2 23) 667) 38 nil "lispworks-tools.restore-cleared-output")
  (extract-and-save-one-icon 738 38 nil "lispworks-tools.compile-and-load-system")
  (extract-and-save-one-icon (+ 23 738) 38 nil "compile-system")
  (extract-and-save-one-icon (+ 23 23 738) 38 nil "load-system")
 
  )


