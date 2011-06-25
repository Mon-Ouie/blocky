;;; shell.lisp --- interactive visual programming shell

;; Copyright (C) 2011 David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see  <http://www.gnu.org/licenses/>.

;;; Code:

(in-package :ioforms)

;;; Trash can

(defblock trash :category :system)

(define-method run trash ())
(define-method execute trash ())
(define-method update trash ())
(define-method accept trash (item)
  (push item %inputs))

(defun trash-status-string (count)
  (format nil "trash (~S items)" count))

(define-method layout trash ()
  (setf %width (dash 4 (font-text-extents 
		       (trash-status-string 
			(length %inputs))
		       *block-font*)))
  (setf %height (dash 4 (font-height *block-font*))))

(define-method draw trash ()
  (draw-background self)
  (draw-label-string self (trash-status-string (length %inputs))
		     "yellow"))

;;; Interactive editor shell

(defblock shell
  (selection :initform ()
  	     :documentation "Subset of selected blocks.")
  (script :initform nil)
  (default-events :initform
		  '(((:tab) :tab)
		    ((:tab :control) :backtab)
		    ((:x :alt) :command-line)
		    ((:g :control) :escape)
		    ((:escape) :escape)))
  menubar terminal
  (drag :initform nil 
  	:documentation "Block being dragged, if any.")
  (hover :initform nil
	 :documentation "Block being hovered over, if any.")
  (highlight :initform nil
	     :documentation "Block being highlighted, if any.")
  (ghost :initform (new block))
  (buffer :initform nil)
  (focused-block :initform nil)
  (click-start :initform nil
	      :documentation "A cons (X . Y) of widget location at moment of click.")
  (drag-start :initform nil
	      :documentation "A cons (X . Y) of widget location at start of dragging.")
  (drag-offset :initform nil
	       :documentation "A cons (X . Y) of mouse click location on dragged block.")
  (modified :initform nil 
	  :documentation "Non-nil when modified since last save."))

(define-method layout shell ()
  (setf %x 0 %y 0 
	%width *screen-width* 
	%height *screen-height*)
  (with-fields (x y width height) %script
    (setf x %x y %y
	  width %width
	  height %height)))

(define-method update shell ()
  (update %script))

(define-method initialize shell (script)
  (setf %script (find-object script))
  (assert %script)
  (setf %menubar (new menubar (make-menu (symbol-value '*system-menu*))))
  (add-block %script %menubar)
  (setf %terminal (new terminal))
  (add-block %script %terminal)
  (register-uuid self))

(define-method script-blocks shell ()
  (field-value :inputs %script))

(define-method open-script shell (script) 
  (setf %script script))
  
(define-method add-block shell (new-block &optional x y)
  (add-block %script new-block x y))

(define-method select shell (block &optional only)
  (with-fields (selection) self
    (if only
	(setf selection (list block))
	(pushnew block selection 
		 :test 'eq :key #'find-parent))))

(define-method select-if shell (predicate)
  (with-fields (selection script) self
    (setf selection 
	  (remove-if predicate (field-value :inputs script)
		     :key #'find-parent))))

(define-method unselect shell (block)
  (with-fields (selection) self
    (setf selection (delete block selection 
			    :test 'eq :key #'find-parent))))

(define-method handle-event shell (event)
  (or (next%handle-event self event)
      (with-field-values (selection script) self
	(let ((block
		  (cond 
		    ;; only one block selected. use that.
		    ((= 1 (length selection))
		     (first selection))
		    ;; nothing selected, only 1 top-level block.
		    ((= 1 (count-toplevel-blocks script))
		     (first (toplevel-blocks script))))))
	  (when block 
	    (with-script script
	      (handle-event block event)))))))

(define-method hit shell (x y)
  ;; return self no matter where mouse is, so that we get to process
  ;; all the events.
  (declare (ignore x y))
  self)

;; (define-method hit-script shell (x y) 
;; "Recursively search the blocks in this script for a block intersecting
;; the point X,Y. We have to search the top-level blocks starting at the
;; end of `%INPUTS' and going backward, because the blocks are drawn in
;; list order (i.e. the topmost blocks for mousing-over are at the end of
;; the list.) The return value is the block found, or nil if none is
;; found."
;;   (with-fields (script) self
;;     (with-fields (inputs) script
;;       (with-script script 
;; 	(flet ((try (block)
;; 		 (when block (hit block x y))))
;; 	  (try (find-if #'try inputs :from-end t)))))))

(define-method hit-script shell (x y)
  (with-script %script 
    (labels ((try (b)
	       (when b
		 (hit b x y))))
      (let ((parent 
	     (find-if #'try 
		      (script-blocks self)
		      :from-end t)))
	(when parent
	  (try parent))))))

(define-method draw shell ()
  (layout self)
  (with-fields (script buffer drag-start selection inputs drag
		       focused-block highlight
		       modified hover ghost prompt)
      self
    (let ((blocks (script-blocks self)))
      ;; now start drawing blocks
      (with-script script 
	(dolist (block blocks)
	  ;; draw border around any selected blocks
	  ;; (when (find block selection :test 'eq :key #'find-object)
	  ;;   (draw-border block))
	  ;; draw the block itself
	  (draw block))
	;; during dragging we draw the dragged block.
	(if drag 
	    (progn (layout drag)
		   (when (field-value :parent drag)
		     (draw-ghost ghost))
		   ;; also draw any hover-over highlights 
		   ;; on objects you might drop stuff onto
		   (when hover 
		     (draw-hover hover))
		   (draw drag))
	    (when focused-block
	      (draw-focus focused-block)))
	(when highlight
	  (draw-highlight highlight))))))

(defparameter *minimum-drag-distance* 7)

(define-method command-line shell ()
  (setf %focused-widget 
	(get-prompt %terminal)))

(define-method escape shell ()
  (close-menus %menubar)
  (setf %focused-widget nil)
  (setf %selection nil))

(define-method tab shell (&optional backward)
  (with-fields (focused-block) self
    (when focused-block
      (with-fields (parent) focused-block
	(let ((index (position-within-parent focused-block)))
	  (when (numberp index)
	    (setf focused-block 
		  (with-fields (inputs) parent
		    (nth (mod (+ index
				 (if backward -1 1))
			      (length inputs))
			 inputs)))))))))

(define-method backtab shell ()
  (tab self :backward))

(define-method begin-drag shell (mouse-x mouse-y block)
  (with-fields (drag inputs script drag-start ghost drag-offset) self
    ;; save the block
    (setf drag block)
    ;; remove from script if it's a top-level block.
    (when (object-eq script (get-parent block))
      (unplug script block))
    (let ((dx (field-value :x block))
	  (dy (field-value :y block))
	  (dw (field-value :width block))
	  (dh (field-value :height block)))
      (with-fields (x y width height) ghost
	;; remember the relative mouse coordinates from the time the
	;; user began dragging, so that the block being dragged is not
	;; simply anchored with its top left corner located exactly at
	;; the mouse pointer.
	(let ((x-offset (- mouse-x dx))
	      (y-offset (- mouse-y dy)))
	  (when (null drag-start)
	    (setf x dx y dy width dw height dh)
	    (setf drag-start (cons dx dy))
	    (setf drag-offset (cons x-offset y-offset))))))))

(define-method drag-maybe shell (x y)
  ;; require some actual mouse movement to initiate a drag
  (with-fields (click-start focused-block) self
    (when click-start
      (destructuring-bind (x1 . y1) click-start
	(when (and (> (distance x y x1 y1)
		      *minimum-drag-distance*)
		   (not (is-pinned focused-block)))
	  (setf click-start nil)
	  (begin-drag self x y focused-block))))))

(define-method mouse-down shell (x y &optional button)
  (with-fields (click-start focused-block) self
    (when focused-block
      (lose-focus focused-block))
    (setf focused-block nil)
    (let ((block (hit-script self x y)))
      (if block
	  (case button
	    (1  (progn 
		  (setf focused-block block)
		  (setf click-start (cons x y))))
	    (3 (let ((menu (context-menu block)))
		 (when menu 
		   (with-script %script
		     (add-block *script* menu x y))))))
	  (setf focused-block nil)))))

(define-method mouse-move shell (mouse-x mouse-y)
  (with-fields (inputs hover highlight click-start drag-offset
		       drag-start drag) self
    (setf hover nil)
    (drag-maybe self mouse-x mouse-y)
    (if drag
	(destructuring-bind (ox . oy) drag-offset
	  (let ((target-x (- mouse-x ox))
		(target-y (- mouse-y oy)))
	    (let ((candidate (hit-script self target-x target-y)))
	      ;; obviously we dont want to plug a block into itself.
	      (setf hover (if (eq drag candidate) nil candidate))
	      (move-to drag target-x target-y))))
	(progn
	  (setf highlight (hit-script self mouse-x mouse-y))
	  (when (null highlight)
	    (when %menubar
	      (with-script %script (close-menus %menubar))))))))

(define-method mouse-up shell (x y &optional button)
  (with-fields 
      (drag-offset drag-start hover script selection drag
	      click-start focused-block modified) self
    (if drag
	;; we're dragging
	(let ((drag-parent (get-parent drag)))
	  (when (and (not (null drag))
		     (not (object-eq script drag-parent)))
	    (unplug-from-parent drag))
	  ;; where are we dropping?
	  (if (null hover)
	      ;; dropping on background
	      (add-block self drag)
	      ;; dropping on another block
	      (when (not (accept hover drag))
		;; hovered block did not accept drag. 
		;; just drop the block
		(add-block self drag)))
	  ;; select the dropped block
	  (select self drag)
	  (setf focused-block drag))
;	  (setf hover nil))
	;; ok, we're not dragging.
	;; instead it was a click.
	(progn
	  (setf selection nil)
	  (when focused-block
	    (select self focused-block)
	    (with-script script 
	      (click focused-block x y))
;	    (setf focused-block nil)
	    (setf click-start nil))))
    (setf drag-start nil
	  drag-offset nil
	  drag nil)
    (report-layout-change script)))

;;; shell.lisp ends here
