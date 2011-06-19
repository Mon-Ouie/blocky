;;; shell.lisp --- interactive ioforms visual programming shell

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(in-package :ioforms)

;;; Lisp listener

(define-prototype block-prompt (:parent prompt)
  (operation :initform :prompt)
  output)

(define-method initialize block-prompt (output)
  (next/initialize self)
  (setf %output output))

(define-method set-output block-prompt (output)
  (setf %output output))

(define-method do-sexp block-prompt (sexp)
  (with-fields (output) self
    (assert output)
    (let ((*make-block-package* (find-package :ioforms))
	  (container (get-parent output)))
      (when container
	(let ((block 
		  (if (symbolp (first sexp))
		      (make-block sexp)
		      (make-block (first sexp)))))
	  (accept container block))))))

(define-prototype listener (:parent list)
  (type :initform :system)
  (schema :initform '((:prompt . :block))))

(defparameter *minimum-listener-width* 200)

(define-method initialize listener ()
  (with-fields (image inputs) self
    (let ((prompt (new block-prompt self)))
      (next/initialize self)
      (set-output prompt prompt)
      (setf inputs (list prompt))
      (set-parent prompt self)
      (pin prompt))))

(define-method run listener ()
  (with-fields (inputs) self
    (run (first inputs))))

;; forward keypresses to prompt for convenience
(define-method handle-event listener (event)
  (with-fields (inputs) self
    (handle-event (first inputs) event)))

(define-prototype terminal (:parent "IOFORMS:LISTENER")
  (scrollback-length :initform 100)
  (display-lines :initform 4))
  
(define-method layout terminal ()
  (with-fields (x y height width parent) self
    (setf x (field-value :x parent))
    (let ((y0 (- (field-value :height parent) (dash 1))))
	(setf height (font-height *block-font*))
	(setf width (dash 8))
	(dolist (element inputs)
	  (layout element)
	  (decf y0 (field-value :height element))
	  (move-to element (+ x (dash 1)) y0)
	  (incf height (+ (dash 1) (field-value :height element)))
	  (setf width (max width (field-value :width element))))
	(incf width (dash 10)))))

(define-method accept terminal (input &optional prepend)
  (declare (ignore prepend))
  (with-fields (inputs scrollback-length) self
    (assert (not (null inputs))) ;; we always have a prompt
    (prog1 t
      (assert (is-valid-connection self input))
      (let ((len (length inputs)))
	(when (> len scrollback-length)
	  ;; drop last item in scrollback
	  (setf inputs (subseq inputs 0 (1- len))))
	;; set parent if necessary 
	(when (get-parent input)
	  (unplug-from-parent input))
	  (set-parent input self)
	  (setf inputs 
		(nconc (first inputs)
		       (list input)
		       (nthcdr 2 inputs)))))))

;;; Interactive editor shell

(defblock shell
  (selection :initform ()
  	     :documentation "Subset of selected blocks.")
  (script :initform nil)
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

(define-method layout shell ())

(define-method update shell ()
  (update %script))

(define-method initialize shell ()
  (next/initialize self))

(define-method script-blocks shell ()
  (field-value :inputs %script))

(define-method open-script shell (script) 
  (setf %script script))
  
(define-method add shell (new-block &optional x y)
  (with-fields (script) self
    (add script new-block x y)))

(define-method select shell (block &optional only)
  (with-fields (selection) self
    (if only
	(setf selection (list block))
	(pushnew block selection 
		 :test 'eq :key #'find-parent))))

(define-method select-if shell (predicate)
  (with-fields (selection inputs) self
    (setf selection 
	  (remove-if predicate inputs
		     :key #'find-parent))))

(define-method unselect shell (block)
  (with-fields (selection) self
    (setf selection (delete block selection 
			    :test 'eq :key #'find-parent))))

(define-method handle-event shell (event)
  (with-field-values (selection script) self
    (with-field-values (inputs) script
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
  (with-fields (script buffer drag-start selection inputs drag
		       focused-block highlight
		       modified hover ghost prompt)
      self
    (let ((blocks (script-blocks self)))
      ;; now start drawing blocks
      (with-script script 
	(dolist (block blocks)
	  ;; draw border around any selected blocks
	  (when (find block selection :test 'eq :key #'find-object)
	    (draw-border block))
	  ;; draw the block itself
	  (draw block))
	;; during dragging we draw the dragged block.
	(if drag 
	    (progn (layout drag)
		   (when (field-value :parent drag)
		     (draw-ghost ghost))
		   (draw drag)
		   ;; also draw any hover-over highlights 
		   ;; on objects you might drop stuff onto
		   (when hover 
		     (draw-hover hover)))
	    (when focused-block
	      (draw-border focused-block)
	      (draw focused-block)))
	(when highlight
	  (draw-highlight highlight))))))

(defparameter *minimum-drag-distance* 7)

(define-method begin-drag shell (mouse-x mouse-y block)
  (with-fields (drag inputs script drag-start ghost drag-offset) self
    ;; save the block
    (setf drag block)
    ;; remove from script if it's a top-level block.
    (when (object-eq script (get-parent block))
      (delete-input script block))
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
  (let ((block (hit-script self x y)))
    (if block
	(case button
	  (1  (with-fields (click-start focused-block) self
		(setf focused-block block)
		(setf click-start (cons x y))))
	  (3 (let ((menu (context-menu block)))
	       (when menu 
		 (with-script %script
		   (add *script* menu x y))))))
	(setf %focused-block nil))))

(define-method mouse-move shell (mouse-x mouse-y)
  (with-fields (inputs hover highlight script click-start drag-offset
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
	    (let ((menu (field-value :menu script)))
	      (when menu
		(with-script script (close-menus menu)))))))))

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
	      (add self drag)
	      ;; dropping on another block
	      (when (not (accept hover drag))
		;; hovered block did not accept drag. 
		;; just drop the block
		(add self drag)))
	  ;; select the dropped block
	  (select self drag))
	;; ok, we're not dragging.
	(progn
	  (setf selection nil)
	  (when focused-block
	    (select self focused-block)
	    (with-script script 
	      (click focused-block))
	    (setf focused-block nil)
	    (setf click-start nil))))
    (setf drag-start nil
	  drag-offset nil
	  drag nil)
    (report-layout-change script)))

;;; shell.lisp ends here
