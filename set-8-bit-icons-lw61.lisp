;;---------------------------------------------------
;; This was intended to be a part of Budden's tools for 
;; Lispworks
;; 
;; Change lispworks IDE icons to 8 bit bmp-s 
;; to avoid watching Malevich' black square when sreen
;; resolution is 16 bit (e.g. through RDP in Windows XP)
;; To enable or disable icons, use 
;; (set-8-bit-icons-lw61::! [t or nil]) 
;; 
;; I extracted icons from a screenshots of 4.4 and 6.1 
;; Personal IDE but I can't publish them as I have no 
;; permission from Lispworks ltd. 
;;
;; So you can extract them from screenshot yourself, 
;; draw your own icon set of request icons 
;; from Lispworks ltd.
;; 
;; Public domain
;; Created by budden73 2013-12-10
;; 
;; Known problems: 
;; 1. I don't know if external images need to be freed. 
;; 2. Some icons are ugly.
;;---------------------------------------------------

(defpackage :set-8-bit-icons-lw61 
  (:use :cl :lispworks)
  (:export 
   #:*icon-map* ; mapping of image name to image object
   #:fill-icon-map
   ))

(in-package :set-8-bit-icons-lw61)

(defparameter *icon-map* (make-hash-table :test 'eq))
(defparameter *my-dir* (namestring (pathname-location #.(current-pathname))))

(defun ! (on)
  (cond
   (on 
    (defadvice (CAPI:toolbar-button-image use-custom-icons :around) (toolbar-button) 
      (or (gethash (slot-value toolbar-button 'capi::name) *icon-map*)
          (lispworks:call-next-advice toolbar-button)))
    )
   (t
    (remove-advice 'capi:toolbar-button-image 'use-custom-icons)
    )))

(defun setimg (button-name filename &optional transparent-color-index)
  (setf (gethash button-name *icon-map*) 
        (gp:read-external-image 
         (concatenate 'string *my-dir* "img/" filename ".bmp")
         :transparent-color-index transparent-color-index)))

(defun fill-icon-map ()     
  (setimg 'lispworks-tools::refresh "lispworks-tools.refresh" 2)
  (setimg 'lispworks-tools::preferences "lispworks-tools.preferences" 2)
  (setimg 'lispworks-tools::clone "capi.clone" 1)
  (setimg 'capi-toolkit::history-previous "history-previous" nil)
  (setimg 'capi-toolkit::history-next "history-next" nil)
  (setimg 'lispworks-tools::breakpoint "lispworks-tools.breakpoint" nil) 
  (setimg 'break "lispworks-tools.breakpoint" nil) 
  (setimg 'continue "continue" nil)
  (setimg 'abort "abort" nil)
  (setimg 'lispworks-tools::previous-frame "lispworks-tools.previous-frame" nil)
  (setimg 'lispworks-tools::next-frame "lispworks-tools.next-frame" nil)
  (setimg 'lispworks-tools::print-bindings "lispworks-tools.print-bindings" nil)
  (setimg 'lispworks-tools::backtrace "lispworks-tools.backtrace" nil)
  (setimg 'lispworks-tools::find-source "lispworks-tools.find-source" nil)
  (setimg 'lispworks-tools::debugger "lispworks-tools.debugger" nil)
  (setimg 'listen "listen" nil)
  (setimg 'lispworks-tools::find-object-source "lispworks-tools.find-object-source" nil)
  (setimg 'inspect "inspect" nil)
  (setimg 'class "class" nil)
  (setimg 'macroexpand "macroexpand" nil)
  (setimg 'lispworks-tools::compile-buffer "lispworks-tools.compile-buffer")
  (setimg 'lispworks-tools::compile-definition "lispworks-tools.compile-definition")
  (setimg 'lispworks-tools::evaluate-definition "lispworks-tools.evaluate-definition")
  (setimg 'step "step")
  (setimg 'lispworks-tools::step-through-call "lispworks-tools.step-through-call")
  (setimg 'lispworks-tools::step-to-call "lispworks-tools.step-to-call")
  (setimg 'lispworks-tools::step-to-value "lispworks-tools.step-to-value")
  (setimg 'lispworks-tools::step-next "lispworks-tools.step-next")
  (setimg 'lispworks-tools::step-to-end "lispworks-tools.step-to-end")
  (setimg 'lispworks-tools::step-to-cursor "lispworks-tools.step-to-cursor")
  (setimg 'continue "continue")
  (setimg 'lispworks-tools::show-current-source "lispworks-tools.show-current-source")
  (setimg 'lispworks-tools::undo-macroexpand "lispworks-tools.undo-macroexpand")
  (setimg 'lispworks-tools::start-search "lispworks-tools.start-search")
  (setimg 'lispworks-tools::stop-search "lispworks-tools.stop-search")
  (setimg 'lispworks-tools::query-replace "lispworks-tools.query-replace")
  (setimg :find-listener "find-listener")
  (setimg :find-editor "find-editor")
  (setimg :find-ouput-browser "find-output-browser")
  (setimg :find-inspector "find-inspector")
  (setimg :find-class-browser "find-class-browser")
  (setimg :find-gf-browser "find-gf-browser")
  (setimg :find-symbol-browser "find-symbol-browser")
  (setimg :find-object-clipboard "find-object-clipboard")
  (setimg :find-function-call-browser "find-function-call-browser")
  (setimg :find-system-browser "find-system-browser")
  (setimg :find-compilation-conditions-browser "find-compilation-conditions-browser")
  (setimg :find-search-files "find-search-files")
  (setimg :find-profiler "find-profiler")
  (setimg :find-tracer "find-tracer")
  (setimg :find-stepper "find-stepper")
  (setimg :find-window-browser "find-window-browser")
  (setimg :find-process-browser "find-process-browser")
  (setimg :find-shell "find-shell")

  (setimg 'lispworks-tools::kill "lispworks-tools.kill")
  (setimg 'lispworks-tools::stop-process "lispworks-tools.stop-process")
  (setimg 'lispworks-tools::unstop-process "lispworks-tools.unstop-process")

  (setimg 'clear-output "clear-output")
  (setimg 'lispworks-tools::collapse-all "lispworks-tools.collapse-all")
  (setimg 'lispworks-tools::restore-cleared-output "lispworks-tools.restore-cleared-output")

  (setimg 'lispworks-tools::compile-and-load-system "lispworks-tools.compile-and-load-system")  
  (setimg 'compile-system "compile-system")
  (setimg 'load-system "load-system")
  )


(fill-icon-map)
