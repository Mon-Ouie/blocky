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

(define-prototype block-prompt (:parent =prompt=)
  (operation :initform :prompt)
  output 
  (rows :initform 10))

(define-method initialize block-prompt (output)
  (parent/initialize self)
  (setf ^output output))

(define-method set-output block-prompt (output)
  (setf ^output output))

(define-method do-sexp block-prompt (sexp)
  (with-fields (output rows) self
    (assert output)
    (let ((container (get-parent output)))
      (when container
	(message "SEXP: ~S" sexp)
	(accept container 
		 (let ((*make-block-package* (find-package :ioforms)))
		   (if (symbolp (first sexp))
		       (make-block-ext sexp)
		       (make-block-ext (first sexp)))))))))
	;; (when (> (count-inputs container) rows)
	;;   (pop container))))))

(define-prototype listener (:parent =list=)
  (type :initform :system)
  (schema :initform '((:prompt . :block))))

(defparameter *minimum-listener-width* 200)

(define-method initialize listener ()
  (with-fields (image inputs) self
    (let ((prompt (clone =block-prompt= self)))
      (parent/initialize self)
      (set-output prompt prompt)
      (setf inputs (list prompt))
      (pin prompt)
      (set-parent prompt self))))

(define-method run listener ()
  (with-fields (inputs) self
    (destructuring-bind (prompt) inputs
      (run prompt))))

;; forward keypresses to prompt for convenience
(define-method handle-event listener (event)
  (with-fields (inputs) self
    (handle-event (first inputs) event)))

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
  (ghost :initform (clone =block=))
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
  (update ^script))

(define-method initialize shell ()
  (parent/initialize self))

(define-method script-blocks shell ()
  (field-value :inputs ^script))

(define-method open-script shell (script) 
  (assert (ioforms:object-p script))
  (setf ^script script))
  
(define-method add shell (new-block &optional x y)
  (with-fields (script) self
    (add script new-block x y)))

(define-method delete-input shell (child)
  (delete ^script child))

(define-method select shell (new-block)
  (with-fields (selection inputs) self
    (pushnew new-block selection)))

(define-method select-if shell (predicate)
  (with-fields (selection inputs) self
    (setf selection (remove-if predicate inputs))))

(define-method unselect shell (the-block)
  (with-fields (selection) self
    (setf selection (delete the-block selection))))

(define-method handle-event shell (event)
  (with-fields (selection script) self
    (when (= 1 (length selection))
      (when (first selection)
	(with-script script
	  (handle-event (first selection) event))))))

(define-method begin-drag shell (mouse-x mouse-y block)
  (with-fields (drag inputs script drag-start ghost drag-offset) self
    (setf drag block)
    (when (is-member script block)
      (delete-input script block))
    (let ((dx (field-value :x block))
	  (dy (field-value :y block))
	  (dw (field-value :width block))
	  (dh (field-value :height block)))
      (with-fields (x y width height) ghost
	(let ((x-offset (- mouse-x dx))
	      (y-offset (- mouse-y dy)))
	  (when (null drag-start)
	    (setf x dx y dy width dw height dh)
	    (setf drag-start (cons dx dy))
	    (setf drag-offset (cons x-offset y-offset))))))))

(define-method hit shell (x y)
  self)

(define-method hit-script shell (x y)
  (labels ((try (b)
	     (hit b x y)))
    (let ((parent 
	   (find-if #'try 
		    (script-blocks self)
		    :from-end t)))
      (when parent
	(try parent)))))

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
	  (when (find block selection)
	    (draw-border block))
	  ;; draw the block itself
	  (draw block))
	;; during dragging we draw the dragged block.
	(if drag 
	    (progn (layout drag)
		   (when (field-value :parent drag)
		     (draw-ghost ghost))
		   (draw drag)
		   (when hover 
		     (draw-hover hover)))
	    (when focused-block
	      (draw-border focused-block)
	      (draw focused-block)))
	(when highlight
	  (draw-highlight highlight))))))

(defparameter *minimum-drag-distance* 7)

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
		 (with-script ^script
		   (add *script* menu x y))))))
	(setf ^focused-block nil))))

(define-method mouse-move shell (mouse-x mouse-y)
  (with-fields (inputs hover highlight script click-start drag-offset
  drag-start drag) self
    (setf hover nil)
    (drag-maybe self mouse-x mouse-y)
    (if drag
	(destructuring-bind (ox . oy) drag-offset
	  (let ((target-x (- mouse-x ox))
		(target-y (- mouse-y oy)))
	    (setf hover (hit-script self target-x target-y))
	    (move-to drag target-x target-y)))
	(progn
	  (setf highlight (hit-script self mouse-x mouse-y))
	  (when (null highlight)
	    (let ((menu (field-value :menu script)))
	      (when menu
		(with-script script (close-menus menu)))))))))

(define-method mouse-up shell (x y &optional button)
  (with-fields 
      (inputs drag-offset drag-start hover script selection drag
	      click-start focused-block modified) self
    (if drag
	;; we're dragging
	(let ((drag-parent (get-parent drag)))
	  (when drag-parent
	    (unplug-from-parent drag))
	  (let ((sink hover))
	    (if sink
		;; dropping on another block
		(unless (accept sink drag)
		  (add self drag))
		;; dropping on background
		(add self drag)))
	  (setf selection nil)
	  (select self drag))
	;; we're not dragging.
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
