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

(defvar *trash* nil)

(define-block trash 
  :category :system 
  :methods '(:empty))

(define-method evaluate trash ())

(define-method on-update trash ())

(define-method empty trash ()
  (setf *trash* nil))

(define-method accept trash (item)
  (push item *trash*))

(defun-memo trash-status-string (count)
    (:key #'first :test 'equal :validator #'identity)
  (format nil "trash (~S items)" count))

(define-method layout trash ()
  (setf %width (dash 4 (font-text-extents 
		       (trash-status-string 
			(length %inputs))
		       *font*)))
  (setf %height (dash 4 (font-height *font*))))

(define-method draw trash ()
  (draw-background self)
  (draw-label-string self (trash-status-string (length *trash*))
		     "yellow"))

(define-method draw-hover trash ())

;;; A global menu bar

(define-block menubar :category :menu :temporary t)

(define-method initialize menubar (&optional (menus *system-menu*))
  (apply #'super%initialize self menus)
  (with-fields (inputs) self
    (dolist (each inputs)
      (setf (field-value :top-level each) t)
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
    (let ((bar-height (dash 2 1 (font-height *font*))))
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

(define-method on-tap menubar (x y)
  (declare (ignore x y))
  (close-menus self))

;; Don't allow anything to be dropped on the menus, for now.

(define-method draw-hover menubar () nil)

(define-method accept menubar (thing)
  (declare (ignore thing))
  nil)

;;; Interactive editor shell

(define-block shell
  (selection :initform ()
  	     :documentation "List (subset) of selected blocks.")
  (buffer :initform nil 
	  :documentation "The buffer object currently open in the shell.")
  (default-events :initform
		  '(((:tab) :tab)
		    ((:tab :control) :backtab)
		    ((:x :alt) :enter-command-line)
		    ((:g :control) :escape)
		    ((:escape) :escape)))
  (menubar :initform nil
	   :documentation "The menubar widget.")
  (command-line :initform nil)
  (command-p :initform nil)
  (excluded-fields :initform 
		   '(:click-start :click-start-block :drag-origin :drag-start :drag-offset :focused-block :command-line :menubar :listener :drag :hover :highlight)
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
  (last-focus :initform nil)
  (click-start :initform nil
	      :documentation "A cons (X . Y) of widget location at moment of click.")
  (click-start-block :initform nil
		     :documentation "The block indicated at the beginning of a drag.")
  (drag-origin :initform nil
	       :documentation "The parent block originally holding the dragged block.")
  (drag-start :initform nil
	      :documentation "A cons (X . Y) of widget location at start of dragging.")
  (drag-offset :initform nil
	       :documentation "A cons (X . Y) of relative mouse click location on dragged block.")
  (modified :initform nil 
	  :documentation "Non-nil when modified since last save."))

(defun make-menubar ()
  (find-uuid 
   (new menubar 
	;; see also system.lisp
	(make-menu *system-menu*
		   :target *system*))))

(defun make-command-line ()
  (find-uuid (new command-line)))

(define-method enter-command-line shell ()
  (setf %command-p t)
  (setf %last-focus %focused-block)
  (focus-on self %command-line))

(define-method exit-command-line shell ()
  (setf %command-p nil)
  (focus-on self %last-focus)
  (setf %last-focus nil))

(define-method make-widgets shell ()
  (setf *shell* (find-uuid self))
  (setf %menubar (make-menubar))
  (setf %command-line (make-command-line)))

(define-method after-deserialize shell ()
  (clear-drag-data self)
  (make-widgets self))

(define-method layout shell ()
  ;; take over the entire GL window
  (with-buffer %buffer
    (setf %x 0 %y 0 
	  %width *screen-width* 
	  %height *screen-height*)
    ;; fill screen with buffer
    (with-fields (x y width height) %buffer
      (setf x %x y %y
	    width %width
	    height %height))
    ;; run menubar across top
    (with-style :rounded
      (layout %menubar))
    ;; run command line across bottom
    (layout %command-line)
    ;;
    (update-layout %buffer)))

(define-method on-update shell ()
  ;; run buffer blocks every frame
  (with-buffer %buffer
    (on-update %buffer)
    (on-update %command-line)))
    
(define-method initialize shell (buffer)
  (assert (blockyp buffer))
  (super%initialize self)
  (setf %buffer (find-uuid buffer))
  (make-widgets self))

(define-method buffer-blocks shell ()
  (field-value :inputs %buffer))

(define-method open-buffer shell (buffer) 
  (setf %buffer (find-uuid buffer)))
  
(define-method add-block shell (new-block &optional x y)
  (add-block %buffer new-block x y))

(define-method select shell (block &optional only)
  (with-buffer %buffer
    (with-fields (selection) self
      (if only
	  (setf selection (list block))
	  (progn 
	    (pushnew block selection 
		     :test 'eq :key #'find-parent)
	    (on-select block))))))
  
(define-method select-if shell (predicate)
  (with-buffer %buffer
    (with-fields (selection buffer) self
      (setf selection 
	    (remove-if predicate (field-value :inputs buffer)
		       :key #'find-parent)))))
  
(define-method unselect shell (block)
  (with-buffer %buffer
    (with-fields (selection) self
      (setf selection (delete block selection 
			      :test 'eq :key #'find-parent)))))
  
(define-method on-event shell (event)
  (with-buffer %buffer
    (or (super%on-event self event)
	(with-field-values (focused-block selection menubar command-line buffer) self
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
		      ((= 1 (count-top-level-blocks buffer))
		       (first (top-level-blocks buffer)))
		      ;; fall back to command-line
		      (t command-line))))
	    (when block 
	      (on-event block event)))))))

;;; Hit testing

(define-method hit shell (x y)
  ;; return self no matter where mouse is, so that we get to process
  ;; all the events.
  (declare (ignore x y))
  self)

(define-method hit-buffer shell (x y)
  "Recursively search the blocks in this buffer for a block
intersecting the point X,Y. We have to search the top-level blocks
starting at the end of `%INPUTS' and going backward, because the
blocks are drawn in list order (i.e. the topmost blocks for
mousing-over are at the end of the list.) The return value is the
block found, or nil if none is found."
  (with-buffer %buffer 
    (labels ((try (b)
	       (when b
		 (hit b x y))))
      ;; check menubar, then buffer
      (or 
       (when %command-p 
	 (try %command-line))
       (try %menubar)
       (let ((parent 
	       (find-if #'try 
			(buffer-blocks self)
			:from-end t)))
	 (when parent
	   (try parent)))))))

(define-method draw shell ()
  (with-buffer %buffer
    (layout self)
    (with-fields (buffer drag-start selection inputs drag
			 focused-block highlight menubar
			 command-line command-p
			 modified hover ghost prompt)
	self
      (let ((blocks (buffer-blocks self)))
	;; now start drawing blocks
	(dolist (block blocks)
	  ;; draw border around any selected blocks
	  ;; (when (find block selection :test 'eq :key #'find-object)
	  ;;   (draw-border block))
	  ;; draw the block itself
	  (draw block))
	;; possibly draw command line background
	(when command-p
	  (draw command-line))
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
	(with-style :rounded
	  (draw menubar))
	(when highlight
	  (draw-highlight highlight))))))
  
(defparameter *minimum-drag-distance* 7)
  
(define-method focus-on shell (block)
  ;; possible to pass nil
  (with-fields (buffer focused-block self) self
    (with-buffer buffer
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
  (with-fields (drag drag-origin inputs buffer drag-start ghost drag-offset) self
    (with-buffer buffer
      ;; save the block, possibly producing a new one
      (setf drag (find-uuid block))
      (when (find-parent drag)
	(setf drag-origin (find-parent drag))
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
  (with-buffer %buffer
    (with-fields (focused-block click-start click-start-block) self
      (when click-start
	(destructuring-bind (x1 . y1) click-start
	  (when (and focused-block click-start-block
		     (> (distance x y x1 y1)
			*minimum-drag-distance*)
		     (can-pick click-start-block))
	    (begin-drag self x y (pick click-start-block))
	    (setf click-start nil)
	    (setf click-start-block nil)))))))

(define-method on-point shell (mouse-x mouse-y)
  (with-fields (inputs hover highlight click-start drag-offset
		       drag-start drag) self
    (setf hover nil)
    (drag-maybe self mouse-x mouse-y)
    (if drag
	;; we're in a mouse drag.
	(destructuring-bind (ox . oy) drag-offset
	  (let ((target-x (- mouse-x ox))
		(target-y (- mouse-y oy)))
	    (let ((candidate (hit-buffer self target-x target-y)))
	      ;; obviously we dont want to plug a block into itself.
	      (setf hover (if (object-eq drag candidate) nil
			      (find-uuid candidate)))
	      ;; keep moving along with the mouse
	      (on-drag drag target-x target-y))))
	;; not dragging, just moving
	(progn
	  (setf highlight (find-uuid (hit-buffer self mouse-x mouse-y)))
	  (when (null highlight)
	    (when %menubar
	      (with-buffer %buffer (close-menus %menubar))))))))

(define-method on-press shell (x y &optional button)
  (declare (ignore button))
  (with-fields (click-start click-start-block focused-block) self
    ;; now find what we're touching
    (assert (or (null focused-block)
		(blockyp focused-block)))
    (let ((block (hit-buffer self x y)))
      (if (null block)
	  (progn
	    (focus-on self nil)
	    (when %command-p
	      (exit-command-line self)))
	  (progn 
	    (setf click-start (cons x y))
	    (setf click-start-block (find-uuid block))
	    ;; now focus; this might cause another block to be
	    ;; focused, as in the case of the Listener
	    (focus-on self block))))))

(define-method clear-drag-data shell ()
  (setf %drag-start nil
	%drag-offset nil
	%drag-origin nil
	%drag nil))

(define-method on-release shell (x y &optional button)
  (with-fields 
      (drag-offset drag-start hover buffer selection drag click-start
	      click-start-block drag-origin focused-block modified) self
    (if drag
	;; we're dragging
	(destructuring-bind (x0 . y0) drag-offset
	  (let ((drag-parent (get-parent drag))
		(drop-x (- x x0))
		(drop-y (- y y0)))
	    (if (not (can-escape drag))
		;; put back in halo or wherever
		(add-block drag-origin drag drop-x drop-y)
		;; ok, drop. where are we dropping?
		(progn 
		  (when drag-parent
		    (unplug-from-parent drag))
		  (if (null hover)
		      ;; dropping on background
		      (add-block self drag drop-x drop-y)
		      ;; dropping on another block
		      (when (not (accept hover drag))
			;; hovered block did not accept drag. 
			;; drop block if it wants to be dropped
			(add-block self drag drop-x drop-y)))))
	    ;; select the dropped block
	    (progn 
	      (select self drag)
	      (setf focused-block (find-uuid drag)))))
	;;
	;; we're clicking instead of dragging
	(progn
	  (setf selection nil)
	  (when focused-block
	    (select self focused-block)
	    (with-buffer buffer 
	      (if 
	       ;; right click and control click are interpreted the same
	       (or (holding-control)
		   (= button 3))
	       (on-alternate-tap focused-block x y)
	       (on-tap focused-block x y))
	      (select self focused-block))
	    (setf click-start nil))))
    (clear-drag-data self)
    (invalidate-layout buffer)))

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
  (with-buffer %buffer
    (when %menubar (close-menus %menubar))
    (focus-on self nil)
    (exit-command-line self)
    (setf %selection nil)))

;;; shell.lisp ends here
