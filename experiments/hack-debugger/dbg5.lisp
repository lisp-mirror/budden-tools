
(in-package :cl-user)

(defvar -source-level-form-table- t)
(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro print-source-level-form-table ()
  (print COMPILER::*SOURCE-LEVEL-FORM-TABLE*))
(defmacro compiler-break ()
  (break "compiler-break ~A" COMPILER:*function-name*))
(defmacro capture-source-level-form-table ()
  (setf -source-level-form-table- COMPILER::*SOURCE-LEVEL-FORM-TABLE*) nil)
)


(defun smash-cons (literal expanded)
  (setf (car literal) (car expanded)
        (cdr literal) (cdr expanded)))


(defun fs () (swap-sources-progn (break "1") (break "2")))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro swap-sources-progn (a b)
  (set-dbg-source-substitution a b)
  (set-dbg-source-substitution b a)
  `(progn ,a ,b))
)

(defun f00 () (compiler-break) (let ((a 2)) a))

(defun f10 ()  
  (print 1) (print 2))

(defun f0 () (breakpoint-host let ((a (+ (break) 2))) a))
(defun f0my () (mylet ((a (+ (break) 2))) a))

(defun k () (print-source-level-form-table) (capture-source-level-form-table) (compiler-break) (+ 1))

#| 
������������� - ���� ��� ������
LISPWORKS-TOOLS::find-breakpoint-for-point - ����� ���� �������
lispworks-tools::stepize - ������� ��� ��� ��������

����� ��������� ��������� stepize: 
0. ������� ���������
2. ����������� �-�
3. ������ ��������� - � ���� ������ ���������� stepize




DBG::GENERATE-SCL-INFO  
  DBG::NEW-VAR-SPEC-FROM-VENV



����, ��� �����: 

������� ����������, �������� "������" ����. ������ ������, �����, ��������
� ���� �-� ����� print-source-level-form-table



isl-table - ������� ���������, �������� 

0. �������� scl-info - ������� ������������ ���������� 
1. �������� frame #<DBG::CALL-FRAME>
2. �� frame �������� ���������� (��������, call-frame-edit-path -> call-frame-scl-info -> 29
3. ��  29 find-entry-in-scl-info ��� �� (break) � DBG::FIND-IN-ISL-TABLE ������� ����:
  3914. 
4. �� ���� ������� ����� � ���������. 
  




������ (�������� ��� ������� ������ "source" � ��������� �� �-�� � (break)):
0 SYSTEM::DBG-EDIT-FRAME-INTERNAL > ...
  >> SYSTEM::FRAME : #<DBG::CALL-FRAME F0>
  >> SYSTEM::PANE  : #<LISPWORKS-TOOLS:DEBUGGER "Debugging CAPI Execution Listener 1" 21DC5FFF>
  1 DBG::CALL-FRAME-EDIT-PATH > ...
    >> DBG::C : #<DBG::CALL-FRAME F0>
    2 DBG::FIND-ENTRY-IN-SCL-INFO > ...
      >> DBG::PC-OFFSET : 29
      >> DBG::SCL-INFO  : #(61 978 NIL 29 3914 NIL)
    2 DBG::FIND-ENTRY-IN-SCL-INFO < ...
      << VALUE-0 : 29
      << VALUE-1 : 3914
      << VALUE-2 : NIL
  1 DBG::CALL-FRAME-EDIT-PATH < ...
    << VALUE-0 : 3914
  1 EDITOR::FIND-LISP-DSPEC-LOCATION > ...
    >> EDITOR::LOC : ((DEFUN F0) #P"C:/lisp/sw/dbg3.lisp" 3914)
  1 EDITOR::FIND-LISP-DSPEC-LOCATION < ...
    << VALUE-0 : #<EDITOR::I-POINT "dbg3.lisp" 0 offset 325 21D7AC47>
    << VALUE-1 : #<EDITOR:BUFFER dbg3.lisp>
0 SYSTEM::DBG-EDIT-FRAME-INTERNAL < ...
  << VALUE-0 : T

����� ������� �� generate-scl-info. ��������� �����. 







� ��� ���������������� �-� �� ����� �� �����?
  ������ �-� � (break), ��� find-source
   ����� dbg::call-frame-edit-path ��� interpreted-call-frame
   (! isl = interpreted-source-locations, scl - compiled-source-locations) 

������:
0 DBG::PATH-FROM-INTERPRETED-FRAME > ...
  >> DBG::FRAME : #<DBG::CALL-FRAME SYSTEM::%EVAL>
  >> DBG::TABLE : (NIL . #.(BUDDEN-TOOLS:MKHASH (QUOTE (:TEST EQ)) (QUOTE (# 978 # 122 # 7 # 3 # 244 # 3914 # 123 # 1 # 30 # 1 # 1))))
  1 DBG::FRAME-VARS-IN-SCOPE > ...
    >> DBG::FRAME : #<DBG::CALL-FRAME SYSTEM::%EVAL>
  1 DBG::FRAME-VARS-IN-SCOPE < ...
    << VALUE-0 : (536840704)
  1 DBG::VALUE-FROM-VAR-SPEC > ...
    >> ARG-0 : 536840704
    >> ARG-1 : #<DBG::CALL-FRAME SYSTEM::%EVAL>
  1 DBG::VALUE-FROM-VAR-SPEC < ...
    << VALUE-0 : (BREAK)
  1 DBG::FIND-IN-ISL-TABLE > ...
    >> DBG::FORM  : (BREAK)
    >> DBG::TABLE : (NIL . #.(BUDDEN-TOOLS:MKHASH (QUOTE (:TEST EQ)) (QUOTE (# 978 # 122 # 7 # 3 # 244 # 3914 # 123 # 1 # 30 # 1 # 1))))
  1 DBG::FIND-IN-ISL-TABLE < ...
    << VALUE-0 : 3914
    << VALUE-1 : T
0 DBG::PATH-FROM-INTERPRETED-FRAME < ...
  << VALUE-0 : 3914

   

COMPILER::WOMBAT-2 - ������ ������ �� ���������, �� ������� ������� ������������. ��� ����


�� �������:
��������� �� ������� ���������
LISPWORKS-TOOLS::DEBUG-MENU-FIND-SOURCE
 - ���� ����� ��� ���������
 lispworks-tools::debugger-frame
   - ���� �� ��������� � ����
 lispworks-tools::debugger-backtrace-select-action (generic)
   ...
   system::dbg-edit-frame-internal
     dbg::call-frame-edit-path (generic)
       DBG::CALL-FRAME-SCL-INFO
         dbg::find-entry-in-scl-info 
     editor::find-lisp-dspec-location

  

LISPWORKS-TOOLS::EDITOR-TOGGLE-BREAKPOINT
  LISPWORKS-TOOLS::TOGGLE-BREAKPOINT
     LISPWORKS-TOOLS::STEPPER-BREAKPOINT-CONTEXT
     LISPWORKS-TOOLS::FIND-STEPPER-BREAKPOINT-IN-LIST
       - ����� ������� stepper-buffer � � �� stepable-points
       - � � ��� paths. �� ������ ��� ������� - ������.

  .... 





����: ���� ����� path.
compiler::find-node-source-path
compiler::get-form-path


SYSTEM::DECLARE-SOURCE-LEVEL-DEBUGGING-MACRO
COMPILER::*SOURCE-LEVEL-FORM-TABLE*
- ���-������� �����->��������.

��� ������������:
  SYSTEM::LOAD-TEXT-STREAM
  COMPILER::IN-PROCESS-FORMS-IN-FILE 
  

   

 


|#






; LISPWORKS-TOOLS::make-stepper-context
; dbg::dbg-edit-current-frame
; DBG::call-frame-edit-path


; 'DBG::call-frame-scl-info - ��� �������
; DBG::value-from-var-spec

; DBG::find-entry-in-scl-info
; COMPILER::add-pc-debug-info
; compiler::*source-form-stack*
; lispworks-tools::stepize - �������������� ��� ��������, ���������� ��� ��������� ���������� � �-��
; LISPWORKS-TOOLS::run-stepper-for-special-form  


#| lispworks-tools::stepable-point - �������:
   LISPWORKS-TOOLS::expandable-point
   LISPWORKS-TOOLS::function-point

�������������� ������������ ���� ���-� �� ������ |#


#|

#|
LISPWORKS-TOOLS::MAKE-CONTEXT-FOR-DSPEC - ��������
  EDITOR::READ-FORM-FROM-REGION
  LISPWORKS-TOOLS::BUFFER-MAKE-STEPPER-CONTEXT
  LISPWORKS-TOOLS::ENSURE-STEPPER-CONTEXT-SOURCE
  LISPWORKS-TOOLS::PREPARE-DEFINITION-FOR-STEPPING


LISPWORKS-TOOLS::STEPABLE-POINT-ACTIVE-PATH - ��, ��� ���

(EDITOR::FIND-LISP-DSPEC-LOCATION COMPILER::FIND-NODE-SOURCE-PATH DBG::FIND-ENTRY-IN-SCL-INFO SYSTEM::DECLARE-SOURCE-LEVEL-DEBUGGING-MACRO COMPILER::GET-FORM-PATH LISPWORKS-TOOLS::FIND-STEPPER-BREAKPOINT-IN-LIST DBG::CALL-FRAME-EDIT-PATH COMPILER::PROCESS-FORM SYSTEM::DBG-EDIT-FRAME-INTERNAL)


|#
