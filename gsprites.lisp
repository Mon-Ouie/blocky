;;; sprites.lisp --- defining sprite objects

;; Copyright (C) 2008, 2009, 2010, 2011  David O'Toole

;; Author: David O'Toole ^dto@gnu.org
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
;; along with this program.  If not, see ^http://www.gnu.org/licenses/.

;;; Sprites are not restricted to the grid

;; These sit in a different layer, the ^sprite layer in the world
;; object.

(in-package :ioforms)

(define-prototype sprite (:parent =gcell=
				  :documentation 
"Sprites are IOFORMS game objects derived from gcells. Although most
behaviors are compatible, sprites can take any pixel location in the
world, and collision detection is performed between sprites and cells.")
  (x :initform 0 :documentation "The world x-coordinate of the sprite.") 
  (y :initform 0 :documentation "The world y-coordinate of the sprite.") 
  (proxy :initform nil)
  (occupant :initform nil)
  (saved-x :initform nil :documentation "Saved x-coordinate used to jump back from a collision.")
  (saved-y :initform nil :documentation "Saved y-coordinate used to jump back from a collision.")
  (image :initform nil :documentation "The arbitrarily sized image
  resource. This determines the bounding box.")
  (hit-width :initform nil :documentation "The cached width of the bounding box.")
  (hit-height :initform nil :documentation "The cached height of the bounding box.")
  (type :initform :sprite))

(define-method image-coordinates sprite ()
  (get-viewport-coordinates-* (field-value :viewport *world*) ^x ^y))

(define-method resize sprite ())
  ;; (with-fields (image height width) self
  ;;   (when image
  ;;     (sdl:init-sdl :video t :audio t :joystick t)
  ;;     (setf width (image-width image))
  ;;     (setf height (image-height image)))))
    

(define-method direction-to-player sprite ()
  (multiple-value-bind (r c) (grid-coordinates self)
    (multiple-value-bind (pr pc) (grid-coordinates (get-player *world*))
      (direction-to r c pr pc))))

(define-method move-to sprite (unit x y)
  (declare (ignore ignore-obstacles))
  (assert (and (integerp x) (integerp y)))
  (with-field-values (grid tile-size width height) *world*
    (let ((world-height (* tile-size height))
	  (world-width (* tile-size width)))
      (when (and (plusp x)
		 (plusp y)
		 (< x world-width)
		 (< y world-height))
	(setf ^x x
	      ^y y)))))
;;	  (setf ^x 0 ^y 0)))))
;;	  (do-collision self nil)))))

(define-method move sprite (direction &optional movement-distance unit)
  (when direction
    (let ((dist (or movement-distance 1)))
      (let ((y ^y)
	    (x ^x))
	(when (and y x)
	  (multiple-value-bind (y0 x0) (ioforms:step-in-direction y x direction dist)
	    (assert (and y0 x0))
	    (move-to self :pixel x0 y0)))))))
  

;;; Proxying and vehicles

(define-method proxy sprite (occupant)
  "Make this sprite a proxy for OCCUPANT."
  (let ((world *world*))
    (when ^occupant 
      (error "Attempt to overwrite existing occupant cell in proxying."))
    (setf ^occupant occupant)
    ;; The occupant should know when it is proxied, and who its proxy is.
    (add-category occupant :proxied)
    (setf (field-value :proxy occupant) self)
    ;; Hide the proxy if it's in a world already.
    (remove-sprite world occupant)
    ;; Don't let anyone step on occupied vehicle.
    (add-category self :obstacle)
    ;; Don't light up the map 
    (add-category self :light-source)
    ;; If it's the player register self as player.
    (when (is-player occupant)
      (add-category self :player)
      (set-player world self)
      (setf ^phase-number (1- (get-phase-number world))))))

(define-method unproxy sprite (&key dr dc dx dy)
  "Remove the occupant from this sprite, dropping it on top."  
  (let ((world *world*)
	(occupant ^occupant))
    (when (null occupant)
      (error "Attempt to unproxy non-occupied sprite."))
    (ecase (field-value :type occupant)
      (:cell
	 (multiple-value-bind (r c) (grid-coordinates self)
	   (drop-cell world occupant r c)))
      (:sprite
	 (multiple-value-bind (x y) (xy-coordinates self)
	   (drop-sprite self occupant 
			(+ x (or dx 0))
			(+ y (or dy 0))))))
    (delete-category occupant :proxied)
    (setf (field-value :proxy occupant) nil)
    (do-post-unproxied occupant)
    (when (is-player occupant)
      (delete-category self :light-source)
      (delete-category self :player)
      (delete-category self :obstacle)
      (set-player world occupant)
      (run occupant))
    (setf ^occupant nil)))

(define-method do-post-unproxied sprite ()
  "This method is invoked on the unproxied former occupant cell after
unproxying. By default, it does nothing."
  nil)

(define-method forward sprite (method &rest args)
  "Attempt to deliver the failed message to the occupant, if any."
  (if (and (is-player self)
	   (not (has-method method self))
	   (null ^occupant))
;;      (say self (format nil "The ~S command is not applicable." method) )
      ;; otherwise maybe we're a vehicle
      (let ((occupant ^occupant))
	(when (null occupant)
	  (error "Cannot forward message ~S. No implementation found." method))
	(apply #'send self method occupant args))))
  
;; (define-method embark sprite (&optional v)
;;   "Enter a vehicle V."
;;   (let ((vehicle (or v (category-at-p *world* ^row ^column :vehicle))))
;;     (if (null vehicle)
;; 	(>>say :narrator "No vehicle to embark.")
;; 	(if (null (field-value :occupant vehicle))
;; 	    (progn (>>say :narrator "Entering vehicle.")
;; 		   (proxy vehicle self))
;; 	    (>>say :narrator "Already in vehicle.")))))

;; (define-method disembark sprite ()
;;   "Eject the occupant."
;;   (let ((occupant ^occupant))
;;     (when (and occupant (in-category self :proxy))
;; 	  (unproxy self))))
  
;;; sprites.lisp ends here
