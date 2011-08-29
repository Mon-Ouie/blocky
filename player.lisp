;;; player.lisp --- defining game objects 

;; Copyright (C) 2008, 2009, 2010, 2011  David O'Toole

;; Author: David O'Toole %dto@ioforms.org
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
;; along with this program.  If not, see %http://www.gnu.org/licenses/.

;;; Code:

(in-package :blocky)

;;; Block tags

(define-method has-tag block (tag)
  "Return non-nil if this block has the specified TAG.

Blocks may be marked with tags that influence their processing by the
engine. The field `%tags' is a set of keyword symbols; if a symbol
`:foo' is in the list, then the block is in the tag `:foo'.

Although a game built on BLOCKY can define whatever tags are
needed, certain base tags are built-in and have a fixed
interpretation:

 -    :obstacle --- Blocks movement and causes collisions
 -    :temporary --- This block is not preserved when exiting a world.
 -    :light-source --- This object casts light. 
 -    :opaque --- Blocks line-of-sight, casts shadows. 
"
  (member tag %tags))

(define-method add-tag block (tag)
  "Add this block to the specified TAG."
  (pushnew tag %tags))

(define-method delete-tag block (tag)
  "Remove this block from the specified TAG."
  (setf %tags (remove tag %tags)))

;;; Locating the block in grid space

(define-method is-grid-located block ()
  "Returns non-nil if this block is located somewhere on the grid."
  (and (integerp %row) (integerp %column)))

(define-method grid-coordinates block ()
  (values %row %column))

(define-method xy-coordinates block ()
  (let ((size (field-value :grid-size *world*)))
    (values (* %column size)
	    (* %row size))))

(define-method coordinates block ()
  (multiple-value-bind (x y) (xy-coordinates self)
    (values x y 0)))

(define-method draw block ()
  (with-fields (image) self
    (when image
      (set-blending-mode %blend)
      (multiple-value-bind (x y)
	  (xy-coordinates self)
	(draw-image image x y)))))

;;; Locating the player 

(define-method player-direction block ()
  "Return the general compass direction of the player from X0,Y0."
  (with-fields (player) *world*
    (multiple-value-bind (x0 y0) (xy-coordinates self)
      (multiple-value-bind (x1 y1) (xy-coordinates player)
	(direction-to y0 x0 y1 x1)))))

(define-method player-distance block ()
  "Return the straight line distance of the player from X0,Y0."
  (with-fields (player) *world*
    (multiple-value-bind (x0 y0) (xy-coordinates self)
      (multiple-value-bind (x1 y1) (xy-coordinates player)
	(distance x0 y0 x1 y1)))))

;;; Convenience macro for defining blocks.

(defmacro defblock (name &body args)
  "Define a block named NAME, with the fields ARGS as in a normal
prototype declaration. This is a convenience macro for defining new
blocks."
  `(define-prototype ,name (:super "BLOCKY:BLOCK")
     ,@args))

;;; Block death

(define-method destroy block ())

(define-method die block ()
  (destroy self)
  (delete-block *world* self %row %column))

;;; Block movement

(define-method move-to-grid block (r c)
  (delete-block *world* self %row %column)
  (drop-block *world* self r c))

(define-method move-to block (x y &optional z)
  (assert (and (numberp x) (numberp y)))
  (with-field-values (grid-size) *world*
    (let ((nearest-row (round y grid-size))
	  (nearest-column (round x grid-size)))
      (move-to-grid self nearest-row nearest-column))))

(define-method move block (direction &optional (distance 1) ignore-obstacles)
  (error "This move method needs rewriting."))
;;   "Move this block one step in DIRECTION on the grid. If
;; IGNORE-OBSTACLES is non-nil, the move will occur even if an obstacle
;; is in the way. Returns non-nil if a move occurred."
;;   (let ((world *world*))
;;     (multiple-value-bind (r c) 
;; 	(step-in-direction %row %column direction distance)
;;       (cond ((null (grid-location world r c)) ;; are we at the edge?
;; 	     ;; return nil because we didn't move
;; 	     (prog1 nil
;; 	     ;; edge conditions only affect player for now
;; 	       (when (is-player self)
;; 		 (ecase (field-value :edge-condition world)
;; 		   (:block nil)
;; 		   (:wrap nil) ;; TODO implement this for planet maps
;; 		   (:exit (exit *universe*))))))
;; 	    (t
;; 	     (when (or ignore-obstacles 
;; 		       (not (obstacle-at-p *world* r c)))
;; 	       ;; return t because we moved
;; 	       (prog1 t
;; 		 (move-block world self r c))))))))

(define-method bounding-box block ()
  (multiple-value-bind (x y)
      (xy-coordinates self)
    (let ((size (field-value :grid-size *world*)))
      (values x y size size))))

(define-method collide block (object)
  (declare (ignore object))
  "Respond to a collision detected with OBJECT."
  nil)

;;; Sprites

;; (defblock sprite 
;;   (collision-type :initform :aabb)
;;   (type :initform :sprite)
;;   (height :initform nil :documentation "The cached width of the bounding box.")
;;   (width :initform nil :documentation "The cached height of the bounding box."))

;; Convenience macro for defining blocks

;; (defmacro defblock (name &body args)
;;   `(define-prototype ,name (:super "BLOCKY:BLOCK")
;;      ,@args))

;; (defun is-block (ob)
;;   (when (eq :block (field-value :type ob))))

;; (defun is-block (ob)
;;   (when (eq :block (field-value :type ob))))

(define-method update-image-dimensions block ()
  (with-fields (image height width scale-x scale-y) self
    (when image
      (setf width (* scale-x (image-width image)))
      (setf height (* scale-y (image-height image))))))

(define-method draw-as-sprite block ()
  (with-fields (image x y z height opacity blend scale-x scale-y) self
    (when image
      (when (null height)
	(update-image-dimensions self))
      (draw-image image x y :z z 
			    :opacity opacity 
			    :blend blend 
			    :scale-x scale-x
			    :scale-y scale-y))))

(define-method change-image block (image)
  (assert (stringp image))
  (setf %image image)
  (update-image-dimensions self))

(define-method destroy block ()
  (remove-block *world* self)
  (discard self))

;;; Block locations

(define-method grid-coordinates block ()
  (values (truncate (/ %y (field-value :tile-size *world*)))
	  (truncate (/ %x (field-value :tile-size *world*)))))

(define-method xy-coordinates block ()
  (values %x %y))

(define-method coordinates block ()
  (values %x %y %z))

;; ;;; Layout

;; (define-method layout block ()
;;   (with-fields (height width image) self
;;     (setf height (image-height image))
;;     (setf width (image-width image))))

;(define-method layout block ())

;; (define-method draw-highlight block ())
;; (define-method draw-hover block ())
;; (define-method draw-border block ())

;;; Block movement

(define-method move-to block (x y &optional z)
  (assert (and (numberp x) (numberp y)))
  (setf %x x %y y)
  (when (numberp z)
    (assert (numberp z))
    (setf %z z)))

(define-method move-to-grid block (row column)
  (with-field-values (grid-size) *world*
    (move-to self (* grid-size row) (* grid-size column))))

(define-method move block (direction &optional (distance 1) force)
  (assert (member direction *compass-directions*))
  (with-field-values (x y) self
    (multiple-value-bind (y0 x0) 
	(blocky:step-in-direction y x direction distance)
      ;; TODO if collision
      (move-to self x0 y0))))

;;; Collision detection

(defun point-in-rectangle-p (x y width height o-top o-left o-width o-height)
  (let ((o-right (+ o-left o-width))
	(o-bottom (+ o-top o-height)))
    (not (or 
	  ;; is the top below the other bottom?
	  (< o-bottom y)
	  ;; is bottom above other top?
	  (< (+ y height) o-top)
	  ;; is right to left of other left?
	  (< (+ x width) o-left)
	  ;; is left to right of other right?
	  (< o-right x)))))

(define-method bounding-box block ()
  (when (null %height)
    (update-image-dimensions self))
  (values %x %y %width %height))

(define-method colliding-with-rectangle block (o-top o-left o-width o-height)
  ;; you must pass arguments in Y X order since this is TOP then LEFT
  (with-fields (x y width height) self
    (point-in-rectangle-p x y width height o-top o-left o-width o-height)))

(define-method colliding-with block (thing)
  (multiple-value-bind (x y width height) 
      (bounding-box thing)
    (colliding-with-rectangle self y x width height)))

(define-method collide block (thing))

;;; Analog gamepad control

(define-method aim block (direction)
  (setf %direction direction))

(define-method stick-move block (&optional multiplier)
  (destructuring-bind (horizontal vertical) *joystick-motion-axes*
    (let* ((x (poll-joystick-axis horizontal))
	   (y (poll-joystick-axis vertical)))
        (let ((direction 
		(cond 
		  ;; diagonal 
		  ((and (axis-pressed-p horizontal) (axis-pressed-p vertical))
		   (if (minusp y) 
		       (if (minusp x)
			   :northwest
			   :northeast)
		       (if (minusp x)
			   :southwest
			   :southeast)))
		  ;; horizontal 
		  ((axis-pressed-p horizontal)
		   (if (minusp x) :west :east))
		  ;; vertical 
		  ((axis-pressed-p vertical)
		   (if (minusp y) :north :south)))))
          (when direction 
            ;; if the player pushed a direction, move in that direction.
            (prog1 t 
	      (if multiplier
		  (move-to self 
			   (+ %x (* multiplier (axis-as-float horizontal)))
			   (+ %y (* multiplier (axis-as-float vertical))))
		  (move self direction multiplier))
	      ;; if the player is NOT pressing on the right stick, ALSO aim in this direction.
	      (destructuring-bind (aim-horz aim-vert) *joystick-aiming-axes*
		(when (not (or (axis-pressed-p aim-horz)
			       (axis-pressed-p aim-vert)))
		  (aim self direction)))))))))

  (define-method stick-aim block ()
    (with-fields (direction) self
      (destructuring-bind (horizontal vertical) *joystick-aiming-axes*
        (let ((x (poll-joystick-axis horizontal))
              (y (poll-joystick-axis vertical)))
          (cond ((and (axis-pressed-p horizontal) (axis-pressed-p vertical))
                 (aim self (if (minusp y) 
                                (if (minusp x)
                                    :northwest
                                    :northeast)
                                (if (minusp x)
                                    :southwest
                                    :southeast))))
                ((axis-pressed-p horizontal)
                 (aim self (if (minusp x) :west :east)))
                ((axis-pressed-p vertical)
                 (aim self (if (minusp y) :north :south))))))))

;;; Object dropping

;; (define-method drop block (thing &optional (delta-x 0) (delta-y 0))
;; ;;  (assert (is-block thing))
;;   (with-field-values (x y) self
;;     (drop-block *world* thing (+ x delta-x) (+ y delta-y))))

;;; Playing a sound

(define-method play-sound block (sample-name)
  (play-sample sample-name))

;;; Temporary text balloons

(defun seconds->frames (seconds)
  (truncate (* seconds blocky:*frame-rate*)))

(defblock balloon 
  :text "..."
  :font *font*
  :clock (seconds->frames 5))

(define-method initialize balloon (string &key (seconds 5.0) (font *font*))
  (with-fields (text clock) self
    (setf clock (seconds->frames seconds))
    (setf text string)
    (setf %font font)
    (multiple-value-bind (width height) 
	(font-text-extents string font)
      (setf %width width)
      (setf %height height))))

(define-method draw balloon ()
  (with-fields (x y clock text) self
    (decf clock)
    (if (plusp clock)
	(blocky:draw-string text x y :font %font :color "black")
	(remove-block *world* self))))


;;; player.lisp ends here
