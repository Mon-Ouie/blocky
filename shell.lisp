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
  (drag-start :initform nil
	      :documentation "A cons (X . Y) of widget location at start of dragging.")
  (drag-offset :initform nil
	       :documentation "A cons (X . Y) of mouse click location on dragged block.")
  (needs-layout :initform t)
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
  
(define-method add shell (new-block &optional x y)
  (with-fields (needs-layout script) self
    (add script new-block x y)
    (setf needs-layout t)))

(define-method delete-child shell (child)
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
  (with-fields (selection needs-layout) self
    (when (= 1 (length selection))
      (when (first selection)
	(handle-event (first selection) event)))))

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
  (with-fields (script buffer drag-start selection inputs drag
		       needs-layout modified hover ghost prompt) 
      self
    ;; update layout if necessary 
    (when needs-layout 
      (dolist (block blocks)
	(layout block)
	(setf needs-layout nil)))
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
	(draw-hover hover)))))

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
      (inputs needs-layout drag-offset drag-start hover
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
	  needs-layout t)))


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
