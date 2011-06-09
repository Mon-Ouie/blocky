;;; shell.lisp --- interactive ioforms visual programming shell

;; Copyright (C) 2011  David O'Toole

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

;;; Interactive editor shell

(define-prototype block-prompt (:parent =prompt=)
;;  (operation :initform :prompt)
  output 
  (rows :initform 10))

(define-method initialize block-prompt (output)
  (parent/initialize self)
  (setf ^output output))

(define-method do-sexp block-prompt (sexp)
  (with-fields (output rows) self
    (assert output)
    (let ((container (get-parent output)))
      (when container
	(accept container 
		 (let ((*make-block-package* (find-package :ioforms)))
		   (if (symbolp (first sexp))
		       (make-block-ext sexp)

		       (make-block-ext (first sexp)))))
	(when (> (length container) rows)
	  (pop container))))))

(define-prototype listener (:parent =list=)
  (type :initform :system)
  (schema :initform '(:block)))

(defparameter *minimum-listener-width* 200)

(define-method initialize listener ()
  (with-fields (image inputs) self
    (let ((prompt (clone =block-prompt= self)))
      (parent/initialize self)
      (resize prompt 
	       :width *minimum-listener-width*
	       :height (+ (* 2 *dash*) 
			  (font-height *default-font*)))
      (assert (field-value :image prompt))
      (setf (first inputs) prompt))))

(define-method run listener ()
  (with-fields (inputs) self
    (destructuring-bind (prompt) inputs
      (run prompt))))

(define-prototype shell (:parent =block=)
  (selection :initform ()
  	     :documentation "Subset of selected blocks.")
  (script :initform nil)
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

(define-method layout shell ()
  (layout ^script))

(define-method initialize shell ()
  (parent/initialize self))

(define-method script-blocks shell ()
  (field-value :inputs ^script))

(define-method switch-to-script shell (script) 
  (assert (ioforms:object-p script))
  (setf ^script script))
  
(define-method add shell (block &optional x y)
  (with-fields (needs-redraw script) self
    (add script block x y)
    (setf needs-redraw t)))

(define-method delete-child shell (block)
  (delete ^script block))

(define-method select shell (block)
  (with-fields (selection inputs) self
    (pushnew block selection)))

(define-method select-if shell (predicate)
  (with-fields (selection inputs) self
    (setf selection (remove-if predicate inputs))))

(define-method unselect shell (block)
  (with-fields (selection) self
    (setf selection (delete block selection))))

(define-method handle-event shell (event)
  (with-fields (selection needs-redraw) self
    (when (= 1 (length selection))
      (when (first selection)
	(handle-event (first selection) event)))))

(define-method resize shell (&key width height)
  (with-fields (prompt buffer needs-redraw image) self
    (parent/resize self :width width :height height)
    (setf buffer (create-image width height))))

;; (define-method redraw shell ()
;;   (with-fields (buffer selection needs-redraw width height) self
;;     (let ((blocks (script-blocks self)))
;;       (draw-box 0 0 width height 
;; 		:color *background-color*
;; 		:stroke-color *background-color*
;; 		:destination buffer)
;;       (dolist (block blocks)
;; 	(layout block))
;;       (dolist (block blocks)
;; 	(when (find block selection)
;; 	  (draw-border block buffer))
;; 	(draw block buffer))
;;       (setf needs-redraw nil))))

(define-method begin-drag shell (mouse-x mouse-y block)
  (with-fields (drag inputs script drag-start ghost drag-offset) self
    (setf drag block)
    (when (is-member script block)
      (delete-child script block))
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

(define-method hit-blocks shell (x y)
  (labels ((try (b)
	     (hit b x y)))
    (let ((parent 
	   (find-if 
	    #'try 
	    (script-blocks self)
	    :from-end t)))
      (when parent
	(try parent)))))

(define-method draw shell ()
  (with-fields 
      (script buffer image drag-start selection
      drag modified hover ghost prompt) self
    ;; ;; render any selected blocks to their offscreen images
    ;; (dolist (block selection)
    ;;   (let ((block-image (field-value :image block)))
    ;; 	(when block-image 
    ;; 	  (render block))))
    ;; (draw-image buffer 0 0 :destination image)
    (when drag 
      (layout drag)
      (draw-ghost ghost image)
      (draw drag image)
      (when hover 
	(draw-hover hover image)))))

(define-method mouse-down shell (x y &optional button)
  (let ((block (hit-blocks self x y)))
    (when block
      (case button
	(1 (begin-drag self x y block))
	(3 (run block))))))

(define-method mouse-move shell (mouse-x mouse-y)
  (with-fields (inputs hover drag-offset drag-start drag) self
    (setf hover nil)
    (when drag
      (destructuring-bind (ox . oy) drag-offset
	(let ((target-x (- mouse-x ox))
	      (target-y (- mouse-y oy)))
	  (setf hover (hit-blocks self target-x target-y))
	  (move drag target-x target-y))))))

(define-method mouse-up shell (x y &optional button)
  (with-fields 
      (inputs needs-redraw drag-offset drag-start hover
	      selection drag modified) 
      self
    (when drag
      (let ((drag-parent (get-parent drag)))
	(when drag-parent
	  (unplug-from-parent drag))
	(let ((sink hover))
	  (if sink
	      ;; dropping on another block
	      (unless (accept sink drag)
		(add self drag))
	      ;; dropping on background
	      (add self drag)))))
    (setf selection nil)
    (when drag (select self drag))
    (setf drag-start nil
	  drag-offset nil
	  drag nil
	  needs-redraw t)))


;;; Other blocks

;; (defblock say 
;;   (type :initform :message)
;;   (schema :initform '(:string))
;;   (inputs :initform '("Hello!")))

;; (defblock move
;;   (type :initform :motion)
;;   (schema :initform '(:symbol :integer :symbol))
;;   (inputs :initform '(:north 10 :pixels)))

;; (defblock move-to
;;   (type :initform :motion)
;;   (schema :initform '(:unit :integer :integer))
;;   (inputs :initform '(:space 0 0)))

;; (defblock joystick-button
;;   (type :initform :sensing)
;;   (schema :initform '(:integer :symbol))
;;   (inputs :initform '(1 :down)))

;; (defblock visible?
;;   (type :initform :variables)
;;   (schema :initform nil)
;;   (inputs :initform nil))

;; (defblock set-variable 
;;   (type :initform :variables)
;;   (schema :initform '(:symbol :block))
;;   (inputs :initform '(:n nil)))

;; (defblock animate 
;;   (type :initform :looks)
;;   (schema :initform '(:string))
;;   (inputs :initform '(nil)))

;; (defblock play-music 
;;   (type :initform :sound)
;;   (schema :initform '(:string))
;;   (inputs :initform '("fanfare")))

;; (define-method execute play-music ()
;;   (play-music *target* (first ^results) :loop t))

;; (defblock play-sound 
;;   (type :initform :sound)
;;   (schema :initform '(:string))
;;   (inputs :initform '("boing")))

;; (defblock when 
;;   (type :initform :control)
;;   (schema :initform '(:block :block))
;;   (inputs :initform '(nil nil)))

;; (defblock unless
;;   (type :initform :control)
;;   (schema :initform '(:block :block))
;;   (inputs :initform '(nil nil)))

;; (defblock fire
;;   (type :initform :control)
;;   (schema :initform '(:block))
;;   (inputs :initform '(:south)))

;; (defblock see-player
;;   (type :initform :sensing)
;;   (schema :initform nil)
;;   (inputs :initform nil))

;; (defblock player-direction
;;   (type :initform :sensing)
;;   (schema :initform nil)
;;   (inputs :initform nil))

;; (defblock closer-than
;;   (type :initform :sensing)
;;   (schema :initform '(:block :block :block :block))
;;   (inputs :initform '(10 spaces to player)))
  
;; (defblock +
;;   (type :initform :operators)
;;   (schema :initform '(:number :number))
;;   (inputs :initform '(nil nil)))

;; (define-method execute + ()
;;   (with-fields (results) self
;;     (when (every #'integerp results)
;;       (apply #'+ results))))

;;; system.lisp ends here


;;; shell.lisp ends here
