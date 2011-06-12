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

;;; Interactive editor shell

(defblock shell
  (selection :initform ()
  	     :documentation "Subset of selected blocks.")
  (script :initform nil)
  (drag :initform nil 
  	:documentation "Block being dragged, if any.")
  (hover :initform nil
	 :documentation "Block being hovered over, if any.")
  (ghost :initform (clone =block=))
  (buffer :initform nil)
  (clicked-block :initform nil)
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
  (with-fields (selection) self
    (when (= 1 (length selection))
      (when (first selection)
	(handle-event (first selection) event)))))

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
		       modified hover ghost prompt)
      self
    (let ((blocks (script-blocks self)))
      ;; now start drawing blocks
      (dolist (block blocks)
	;; draw border around any selected blocks
	(when (find block selection)
	  (draw-border block))
	;; draw the block itself
	(draw block))
      ;; during dragging we draw the dragged block.
      (when drag 
	(layout drag)
	(draw-ghost ghost)
	(draw drag)
	(when hover 
	  (draw-hover hover))))))

(defparameter *minimum-drag-distance* 7)

(define-method drag-maybe shell (x y)
  ;; require some actual mouse movement to initiate a drag
  (with-fields (click-start clicked-block) self
    (when click-start
      (destructuring-bind (x1 . y1) click-start
	(when (> (distance x y x1 y1)
		 *minimum-drag-distance*)
	  (setf click-start nil)
	  (begin-drag self x y clicked-block))))))

(define-method mouse-down shell (x y &optional button)
  (let ((block (hit-script self x y)))
    (when block
      (case button
	(1  (with-fields (click-start clicked-block) self
	      (setf clicked-block block)
	      (setf click-start (cons x y))))
	(3 (run block))))))

(define-method mouse-move shell (mouse-x mouse-y)
  (with-fields (inputs hover click-start drag-offset drag-start drag) self
    (setf hover nil)
    (drag-maybe self mouse-x mouse-y)
    (when drag
      (destructuring-bind (ox . oy) drag-offset
	(let ((target-x (- mouse-x ox))
	      (target-y (- mouse-y oy)))
	  (setf hover (hit-script self target-x target-y))
	  (move drag target-x target-y))))))

(define-method mouse-up shell (x y &optional button)
  (with-fields 
      (inputs drag-offset drag-start hover script selection drag
	      click-start clicked-block modified) self
    (if drag
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
	(progn
	  (setf selection nil)
	  (select self clicked-block)
	  (setf click-start nil clicked-block nil)))
    (setf drag-start nil
	  drag-offset nil
	  drag nil)
    (report-layout-change script)))

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
