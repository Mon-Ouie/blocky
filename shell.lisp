;;; shell.lisp --- interactive visual programming shell

;; Copyright (C) 2011 David O'Toole

;; Author: David O'Toole <dto@ioforms.org>
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

;; This file implements the overall mouse and keyboard interface to
;; Blocky programming. 

(in-package :blocky)

(defvar *shell* nil
  "When non-nil, the UUID of the currently active shell object.")

;;; Trash can

(defblock trash 
  :category :system 
  :methods '(:empty))

(define-method evaluate trash ())

(define-method update trash ())

(define-method empty trash ()
  (setf %inputs nil))

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

(define-method draw-hover trash ())

;;; A global menu bar

(defblock menubar :category :menu :temporary t)

(define-method initialize menubar (&optional (menus *system-menu*))
  (apply #'super%initialize self 
	 (mapcar #'find-object menus))
  (with-fields (inputs) self
    (dolist (each inputs)
      (setf (field-value :main-menu-p each) t)
      (pin each))))

(define-method hit menubar (mouse-x mouse-y)
  (with-fields (x y width height inputs) self
    (when (within-extents mouse-x mouse-y x y (+ x width) (+ y height))
      ;; are any of the menus open?
      (let ((opened-menu (find-if #'is-expanded inputs)))
	(labels ((try (m)
		   (when m (hit m mouse-x mouse-y))))
	  (let ((moused-menu (find-if #'try inputs)))
	    (if (and ;; moused-menu opened-menu
		     (object-eq moused-menu opened-menu))
		;; we're over the opened menu, let's check if 
		;; the user has moused onto the other parts of the menubar
	        (flet ((try-other (menu)
			 (when (not (object-eq menu opened-menu))
			   (try menu))))
		  (let ((other (some #'try-other inputs)))
		    ;; are we touching one of the other menubar items?
		    (if (null other)
			;; nope, just hit the opened submenu items.
			(try opened-menu)
			;; yes, switch menus.
			(prog1 other
			  (unexpand opened-menu)
			  (expand other)))))
		;; we're somewhere else. just try the main menus in
		;; the menubar.
		(let ((candidate (find-if #'try inputs)))
		  (if (null candidate)
		      ;; the user moused away. close the menus.
		      self
		      ;; we hit one of the other menus.
		      (if opened-menu
			  ;; there already was a menu open.
			  ;; close this one and open the new one.
			  (prog1 candidate
			    (unexpand opened-menu)
			    (expand candidate))
			  ;; no menu was open---just hit the menu headers
			  (some #'try inputs)))))))))))
			  		    	  
(define-method draw-border menubar () nil)

(define-method layout menubar ()
  (with-fields (x y width height inputs) self
    (setf x 0 y 0 width *screen-width* height (dash 1))
    (let ((x1 (dash 1)))
      (dolist (item inputs)
	(move-to item x1 y)
	(layout item)
	(incf x1 (dash 1 (header-width item)))
	(setf height (max height (field-value :height item)))))))
        
(define-method draw menubar ()
  (with-fields (x y width inputs) self
    (let ((bar-height (dash 2 1 (font-height *block-font*))))
      (draw-box x y 
		width bar-height
		:color (find-color self))
      (draw-line x bar-height width bar-height
		 :color (find-color self :shadow))
      (with-fields (inputs) self
	(dolist (each inputs)
	  (draw each))))))

(define-method close-menus menubar ()
  (with-fields (inputs) self
    (when (some #'is-expanded inputs)
      (mapc #'unexpand %inputs))))

(define-method on-click menubar (x y)
  (declare (ignore x y))
  (close-menus self))

;; Don't allow anything to be dropped on the menus, for now.

(define-method draw-hover menubar () nil)

(define-method accept menubar (thing)
  (declare (ignore thing))
  nil)

;;; Interactive editor shell

(defblock shell
  (selection :initform ()
  	     :documentation "List (subset) of selected blocks.")
  (script :initform nil 
	  :documentation "The script object currently open in the shell.")
  (default-events :initform
		  '(((:tab) :tab)
		    ((:tab :control) :backtab)
		    ((:x :alt) :command-line)
		    ((:g :control) :escape)
		    ((:escape) :escape)))
  (menubar :initform nil
	   :documentation "The menubar widget.")
  (excluded-fields :initform '(:menubar)
		   :documentation "Don't serialize the menu bar.")
  (drag :initform nil 
  	:documentation "Block being dragged, if any.")
  (hover :initform nil
	 :documentation "Block being hovered over, if any.")
  (highlight :initform nil
	     :documentation "Block being highlighted, if any.")
  (ghost :initform (new block)
	 :documentation "Dummy block to hold original place of currently dragged block onscreen.")
  (focused-block :initform nil
		 :documentation "Block having current input focus, if any.")
  (click-start :initform nil
	      :documentation "A cons (X . Y) of widget location at moment of click.")
  (click-start-block :initform nil
		     :documentation "The block indicated at the beginning of a drag.")
  (drag-start :initform nil
	      :documentation "A cons (X . Y) of widget location at start of dragging.")
  (drag-offset :initform nil
	       :documentation "A cons (X . Y) of relative mouse click location on dragged block.")
  (modified :initform nil 
	  :documentation "Non-nil when modified since last save."))

(define-method after-deserialize shell ()
  (setf *shell* (find-uuid self))
  (setf %menubar (make-menubar)))

(define-method layout shell ()
  ;; take over the entire GL window
  (with-script %script
    (setf %x 0 %y 0 
	  %width *screen-width* 
	  %height *screen-height*)
    (with-fields (x y width height) %script
      (setf x %x y %y
	    width %width
	    height %height))
    ;; run menu bar across the top
    (layout %menubar)))

(define-method update shell ()
  ;; run script blocks every frame
  (with-script %script
    (update %script)))

(defun make-menubar ()
  (find-uuid 
   (new menubar 
	;; see also system.lisp
	(make-menu *system-menu*
		   :target *system*))))

(define-method initialize shell (script)
  (assert (blockyp script))
  (super%initialize self)
  (setf *shell* (find-uuid self))
  (setf %script (find-uuid script))
  (setf %menubar (make-menubar)))

(define-method script-blocks shell ()
  (field-value :inputs %script))

(define-method open-script shell (script) 
  (setf %script (find-uuid script)))
  
(define-method add-block shell (new-block &optional x y)
  (add-block %script new-block x y))

(define-method select shell (block &optional only)
  (with-script %script
    (with-fields (selection) self
      (if only
	  (setf selection (list block))
	  (progn 
	    (pushnew block selection 
		     :test 'eq :key #'find-parent)
	    (on-select block))))))
  
(define-method select-if shell (predicate)
  (with-script %script
    (with-fields (selection script) self
      (setf selection 
	    (remove-if predicate (field-value :inputs script)
		       :key #'find-parent)))))
  
(define-method unselect shell (block)
  (with-script %script
    (with-fields (selection) self
      (setf selection (delete block selection 
			      :test 'eq :key #'find-parent)))))
  
(define-method on-event shell (event)
  (with-script %script
    (or (super%on-event self event)
	(with-field-values (focused-block selection menubar script) self
	  (let ((block
		    (cond
		      ;; we're focused. send the event there
		      (focused-block
		       (prog1 focused-block
			 (assert (blockyp focused-block))))
		      ;; only one block selected. use that.
		      ((= 1 (length selection))
		       (first selection))
		      ;; nothing selected, only 1 top-level block.
		      ((= 1 (count-top-level-blocks script))
		       (first (top-level-blocks script)))
		      ;; fall back to menu
		      (t menubar))))
	    (when block 
	      (on-event block event)))))))

;;; Hit testing

(define-method hit shell (x y)
  ;; return self no matter where mouse is, so that we get to process
  ;; all the events.
  (declare (ignore x y))
  self)

(define-method hit-script shell (x y)
  "Recursively search the blocks in this script for a block
intersecting the point X,Y. We have to search the top-level blocks
starting at the end of `%INPUTS' and going backward, because the
blocks are drawn in list order (i.e. the topmost blocks for
mousing-over are at the end of the list.) The return value is the
block found, or nil if none is found."
  (with-script %script 
    (labels ((try (b)
	       (when b
		 (hit b x y))))
      ;; check menubar, then script
      (or (try %menubar)
	  (let ((parent 
		  (find-if #'try 
			   (script-blocks self)
		      :from-end t)))
	    (when parent
	      (try parent)))))))

(define-method draw shell ()
  (with-script %script
    (layout self)
    (with-fields (script drag-start selection inputs drag
			 focused-block highlight menubar
			 modified hover ghost prompt)
	self
      (let ((blocks (script-blocks self)))
	;; now start drawing blocks
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
	      (assert (blockyp focused-block))
	      (draw-focus focused-block)))
	(draw menubar)
	(when highlight
	  (draw-highlight highlight))))))
  
(defparameter *minimum-drag-distance* 7)
  
(define-method focus-on shell (block)
  ;; possible to pass nil
  (with-fields (script focused-block self) self
    (with-script script
      ;; there's going to be a new focused block. 
      ;; tell the current one it's no longer focused.
      (when focused-block
	(on-lose-focus focused-block))
      ;; now set up the new focus (possibly nil)
      (setf focused-block 
	    (when block (find-uuid block)))
      ;; sanity check
      (assert (or (null focused-block)
		  (blockyp focused-block)))
      ;; now tell the block it has focus
      (when block 
	(on-focus block)))))

(define-method begin-drag shell (mouse-x mouse-y block)
  (with-fields (drag inputs script drag-start ghost drag-offset) self
    (with-script script
      ;; save the block
      (setf drag (find-uuid block))
      (when (parent-is-script drag)
	(unplug-from-parent block))
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
	      (setf drag-offset (cons x-offset y-offset)))))))))

(define-method drag-maybe shell (x y)
  ;; require some actual mouse movement to initiate a drag
  (with-script %script
    (with-fields (focused-block click-start click-start-block) self
      (when click-start
	(destructuring-bind (x1 . y1) click-start
	  (when (and focused-block
		     (> (distance x y x1 y1)
			*minimum-drag-distance*)
		     (not (is-pinned click-start-block)))
	    (assert (blockyp click-start-block))
	    (begin-drag self x y click-start-block)
	    (setf click-start nil)
	    (setf click-start-block nil)))))))

(define-method mouse-move shell (mouse-x mouse-y)
  (with-fields (inputs hover highlight click-start drag-offset
		       drag-start drag) self
    (setf hover nil)
    (drag-maybe self mouse-x mouse-y)
    (if drag
	;; we're in a mouse drag.
	(destructuring-bind (ox . oy) drag-offset
	  (let ((target-x (- mouse-x ox))
		(target-y (- mouse-y oy)))
	    (let ((candidate (hit-script self target-x target-y)))
	      ;; obviously we dont want to plug a block into itself.
	      (setf hover (if (object-eq drag candidate) nil
			      (find-uuid candidate)))
	      ;; keep moving along with the mouse
	      (move-to drag target-x target-y))))
	;; not dragging, just moving
	(progn
	  (setf highlight (find-uuid (hit-script self mouse-x mouse-y)))
	  (when (null highlight)
	    (when %menubar
	      (with-script %script (close-menus %menubar))))))))

(define-method mouse-down shell (x y &optional button)
  (declare (ignore button))
  (with-fields (click-start click-start-block focused-block) self
    ;; now find what we're touching
    (assert (or (null focused-block)
		(blockyp focused-block)))
    (let ((block (hit-script self x y)))
      (if (null block)
	  (focus-on self nil)
	  (progn 
	    (setf click-start (cons x y))
	    (setf click-start-block (find-uuid block))
	    ;; now focus; this might cause another block to be
	    ;; focused, as in the case of the Listener
	    (focus-on self block))))))

(define-method mouse-up shell (x y &optional button)
  (with-fields 
      (drag-offset drag-start hover script selection drag click-start
	      click-start-block focused-block modified) self
    (if drag
	;; we're dragging
	(let ((drag-parent (get-parent drag)))
	  (when (and (not (null drag-parent))
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
	  (setf focused-block (find-uuid drag)))
	;; (setf hover nil)
	;;
	;; we're clicking instead of dragging
	(progn
	  (setf selection nil)
	  (when focused-block
	    (select self focused-block)
	    (with-script script 
	      (if 
	       ;; right click and control click are interpreted the same
	       (or (holding-control)
		   (= button 3))
	       (on-alternate-click focused-block x y)
	       (on-click focused-block x y))
	      (select self focused-block))
	    (setf click-start nil))))
    (setf drag-start nil
	  drag-offset nil
	  drag nil)
    (invalidate-layout script)))

(define-method tab shell (&optional backward)
  (with-fields (focused-block) self
    (when focused-block
      (assert (blockyp %focused-block))
      (with-fields (parent) focused-block
	(let ((index (position-within-parent focused-block)))
	  (when (numberp index)
	    (focus-on self
		      (with-fields (inputs) parent
			(nth (mod (+ index
				     (if backward -1 1))
				  (length inputs))
			     inputs)))))))))

(define-method backtab shell ()
  (tab self :backward))
  
(define-method escape shell ()
  (with-script %script
    (when %menubar (close-menus %menubar))
    (setf %focused-block nil)
    (setf %selection nil)))

;;; shell.lisp ends here
