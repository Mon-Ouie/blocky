;;; scripts.lisp --- interactive script creation

;; Copyright (C) 2010  David O'Toole

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

;;; Composing blocks into larger programs

(define-prototype script ()
  (blocks :initform '()
	  :documentation "List of blocks in the script.")
  (target :initform nil)
  (variables :initform (make-hash-table :test 'eq)))

(define-method initialize script (&key blocks variables target)
  (setf <blocks> blocks)
  (when variables (setf <variables> variables))
  (when target (setf <target> target)))

(defvar *script*)

(define-method set-target script (target)
  (setf <target> target))

(define-method is-member script (block)
  (with-fields (blocks) self
    (find block blocks)))

(define-method add script (block &optional x y)
  (with-fields (blocks) self
    (assert (not (find block blocks)))
    (setf blocks (nconc blocks (list block)))
    (setf (field-value :parent block) nil)
    (when (and (integerp x)
	       (integerp y))
      (/move block x y))))

(define-method run script (block)
  (with-fields (blocks target) self
    (/run block target)))
	    
(define-method bring-to-front script (block)
  (with-fields (blocks) self
    (when (find block blocks)
      (setf blocks (delete block blocks))
      (setf blocks (nconc blocks (list block))))))

(define-method delete script (block)
  (with-fields (blocks) self
    (assert (find block blocks))
    (setf blocks (delete block blocks))))

(define-method set script (var value)
  (setf (gethash var <variables>) value))

(define-method get script (var)
  (gethash var <variables>))

(defun script-variable (var-name)
  (/get *script* var-name))

(defun (setf script-variable) (var-name value)
  (/set *script* var-name value))

(defmacro with-script-variables (vars &rest body)
  (labels ((make-clause (sym)
	     `(,sym (script-variable ,(make-keyword sym)))))
    (let* ((symbols (mapcar #'make-non-keyword vars))
	   (clauses (mapcar #'make-clause symbols)))
      `(symbol-macrolet ,clauses ,@body))))

;;; Script editor widget and shell

(define-prototype block-prompt (:parent =prompt=)
  output 
  (rows :initform 10))

(define-method initialize block-prompt (output)
  (/parent/initialize self)
  (setf <output> output))
  
(define-method do-sexp block-prompt (sexp)
  (with-fields (output rows) self
    (assert output)
    (let ((container (/get-parent output)))
      (when container
	(/accept container 
		 (let ((*make-block-package* (find-package :ioforms)))
		   (if (symbolp (first sexp))
		       (make-block-ext sexp)
		       (make-block-ext (first sexp)))))
	(when (> (/length container) rows)
	  (/pop container))))))

(defblock listener
  (type :initform :system))

(defparameter *minimum-listener-width* 200)

(define-method initialize listener ()
  (with-fields (widget) self
    (/parent/initialize self)
    (let ((prompt (clone =block-prompt= self)))
      (/resize prompt 
	       :width *minimum-listener-width*
	       :height (+ (* 2 *dash-size*) 
			  (font-height *default-font*)))
      (setf widget prompt))))

;; (define-method layout listener ()
;;   (/parent/layout self)
;;   (with-fields (height width widget) self

(defblock editor
  (script :initform nil 
	  :documentation "The IOFORMS:=SCRIPT= object being edited.")
  (selection :initform ()
  	     :documentation "Subset of selected blocks.")
  (drag :initform nil 
  	:documentation "Block being dragged, if any.")
  (hover :initform nil
	 :documentation "Block being hovered over, if any.")
  (ghost :initform (clone =block=))
  (buffer :initform nil)
  (drag-start :initform nil
	      :documentation "A cons (X . Y) of widget location at start of dragging.")
  (drag-offset :initform nil
	       :documentation "A cons (X . Y) of mouse click location on dragged block.")
  (needs-redraw :initform t)
  (modified :initform nil 
	  :documentation "Non-nil when modified since last save."))

(define-method initialize editor ()
  (/parent/initialize self)
  (with-fields (script) self
    (setf script (clone =script=))))

(define-method select editor (block)
  (with-fields (selection blocks) self
    (pushnew block selection)))

(define-method select-if editor (predicate)
  (with-fields (selection blocks) self
    (setf selection (remove-if predicate blocks))))

(define-method unselect editor (block)
  (with-fields (selection) self
    (setf selection (delete block selection))))

(define-method handle-key editor (keys)
  (with-fields (selection needs-redraw) self
    (when (= 1 (length selection))
      (when (first selection)
	(/handle-key (first selection) keys)
	(setf needs-redraw t)))))

(define-method resize editor (&key width height)
  (with-fields (buffer prompt image) self
    (when (null buffer)
      (setf buffer (create-image width height)))
    (unless (and (= <width> width)
		 (= <height> height))
      (/parent/resize self :width width :height height)
      (when buffer
	(sdl:free buffer))
      (setf buffer (create-image width height)))))

(define-method redraw editor ()
  (with-fields (script buffer selection needs-redraw width height) self
    (with-fields (blocks) script
      (draw-box 0 0 width height 
		:color *background-color*
		:stroke-color *background-color*
		:destination buffer)
      (dolist (block blocks)
	(/layout block))
      (dolist (block blocks)
	(when (find block selection)
	  (/draw-border block buffer))
	(/draw block buffer))
      (setf needs-redraw nil))))

(define-method begin-drag editor (mouse-x mouse-y block)
  (with-fields (drag script drag-start ghost drag-offset) self
    (setf drag block)
    (when (/is-member script block)
      (/delete script block))
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

(define-method hit-blocks editor (x y)
  (with-fields (script) self
    (when script 
      (with-fields (blocks) script
	(labels ((hit (b)
		   (/hit b x y)))
	  (let ((parent (find-if #'hit blocks :from-end t)))
	    (when parent
	      (/hit parent x y))))))))

(define-method render editor ()
  (with-fields 
      (script needs-redraw image buffer drag-start selection
      drag modified hover ghost prompt) self
    (dolist (block selection)
      (let ((widget (/get-widget block)))
	(when widget 
	  (/render widget)
	  (/draw block image))))
    (labels ((copy ()
	       (draw-image buffer 0 0 :destination image)))
      (when script
	(when needs-redraw 
	  (/redraw self)
	  (copy))
	(when drag 
	  (copy)
	  (/layout drag)
	  (/draw-ghost ghost image)
	  (/draw drag image)
	  (when hover 
	    (/draw-hover hover image)))))))

(define-method mouse-down editor (x y &optional button)
  (with-fields (script) self 
    (let ((block (/hit-blocks self x y)))
      (when block
	(case button
	  (1 (/begin-drag self x y block))
	  (3 (/run script block)))))))

(define-method mouse-move editor (mouse-x mouse-y)
  (with-fields (script hover drag-offset drag-start drag) self
    (setf hover nil)
    (when drag
      (destructuring-bind (ox . oy) drag-offset
	(let ((target-x (- mouse-x ox))
	      (target-y (- mouse-y oy)))
	  (setf hover (/hit-blocks self target-x target-y))
	  (/move drag target-x target-y))))))

(define-method mouse-up editor (x y &optional button)
  (with-fields 
      (script needs-redraw drag-offset drag-start hover
	      selection drag modified) 
      self
    (with-fields (blocks) script
      (when drag
	(let ((drag-parent (/get-parent drag)))
	  (when drag-parent
	    (/unplug-from-parent drag))
	  (let ((target hover))
	    (if target
		;; dropping on another block
		(unless (/accept target drag)
		  (/add script drag))
		;; dropping on background
		(/add script drag)))))
      (setf selection nil)
      (when drag (/select self drag))
      (setf drag-start nil
	    drag-offset nil
	    drag nil
	    needs-redraw t))))

;;; scripts.lisp ends here
