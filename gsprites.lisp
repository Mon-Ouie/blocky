;;; gsprites.lisp --- defining sprite objects

;; Copyright (C) 2008, 2009, 2010  David O'Toole

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

;;; Sprites are not restricted to the grid

;; These sit in a different layer, the <sprite> layer in the world
;; object.

(in-package :iosketch)

(define-prototype gsprite (:parent =gcell=
				  :documentation 
"Sprites are IOSKETCH game objects derived from gcells. Although most
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
  (width :initform nil :documentation "The cached width of the bounding box.")
  (height :initform nil :documentation "The cached height of the bounding box.")
  (type :initform :sprite))

(define-method image-coordinates gsprite ()
  (/get-viewport-coordinates-* (field-value :viewport *world*) <x> <y>))

;; Convenience macro for defining cells:

(defmacro defgsprite (name &body args)
  `(define-prototype ,name (:parent =gsprite=)
     ,@args))

(defun is-sprite (ob)
  (when (eq :sprite (field-value :type ob))))

(defun is-cell (ob)
  (when (eq :cell (field-value :type ob))))

(define-method resize gsprite ()
  (with-fields (image height width) self
    (when image
      (setf width (image-width image))
      (setf height (image-height image)))))
    
(define-method initialize gsprite ()
  (when <image>
    (/update-image self <image>)))

(define-method die gsprite ()
  (/remove-sprite *world* self))

(define-method update-image gsprite (image)
  (setf <image> image)
  (/resize self))

(define-method direction-to-player gsprite ()
  (multiple-value-bind (r c) (/grid-coordinates self)
    (multiple-value-bind (pr pc) (/grid-coordinates (/get-player *world*))
      (direction-to r c pr pc))))

(define-method move-to gsprite (unit x y)
  (declare (ignore ignore-obstacles))
  (assert (and (integerp x) (integerp y)))
  (with-field-values (grid tile-size width height) *world*
    (let ((world-height (* tile-size height))
	  (world-width (* tile-size width)))
      (when (and (plusp x)
		 (plusp y)
		 (< x world-width)
		 (< y world-height))
	(setf <x> x
	      <y> y)))))
;;	  (setf <x> 0 <y> 0)))))
;;	  (/do-collision self nil)))))

(define-method move gsprite (direction &optional movement-distance unit)
  (when direction
    (let ((dist (or movement-distance 1)))
      (let ((y <y>)
	    (x <x>))
	(when (and y x)
	  (multiple-value-bind (y0 x0) (iosketch:step-in-direction y x direction dist)
	    (assert (and y0 x0))
	    (/move-to self :pixel x0 y0)))))))
  
(define-method collide gsprite (gsprite)
  ;; (message "COLLIDING A=~S B=~S"
  ;; 	   (object-name (object-parent self))
  ;; 	   (object-name (object-parent gsprite)))
  (let ((x0 (field-value :x gsprite))
	(y0 (field-value :y gsprite))
	(w (field-value :width gsprite))
	(h (field-value :height gsprite)))
    (/collide-* self y0 x0 w h)))
    
(define-method would-collide gsprite (x0 y0)
  (with-field-values (tile-size grid sprite-grid) *world*
    (with-field-values (width height x y) self
      ;; determine squares gsprite would intersect
      (let ((left (1- (floor (/ x0 tile-size))))
	    (right (1+ (floor (/ (+ x0 width) tile-size))))
	    (top (1- (floor (/ y0 tile-size))))
	    (bottom (1+ (floor (/ (+ y0 height) tile-size)))))
	;; search intersected squares for any obstacle
	(or (block colliding
	      (let (found)
		(dotimes (i (max 0 (- bottom top)))
		  (dotimes (j (max 0 (- right left)))
		    (let ((i0 (+ i top))
			  (j0 (+ j left)))
		      (when (array-in-bounds-p grid i0 j0)
			(when (/collide-* self
					 (* i0 tile-size) 
					 (* j0 tile-size)
					 tile-size tile-size)
			  ;; save this intersection information
			  (vector-push-extend self (aref sprite-grid i0 j0))
			  ;; quit when obstacle found
			  (let ((obstacle (/obstacle-at-p *world* i0 j0)))
			    (when obstacle
			      (setf found obstacle))))))))
		(return-from colliding found)))
	    ;; scan for gsprite intersections
	    (block intersecting 
	      (let (collision num-gsprites ix)
		(dotimes (i (max 0 (- bottom top)))
		  (dotimes (j (max 0 (- right left)))
		    (let ((i0 (+ i top))
			  (j0 (+ j left)))
		      (when (array-in-bounds-p grid i0 j0)
			(setf collision (aref sprite-grid i0 j0))
			(setf num-gsprites (length collision))
			(when (< 1 num-gsprites)
			  (dotimes (i (- num-gsprites 1))
			    (setf ix (1+ i))
			    (loop do (let ((a (aref collision i))
					   (b (aref collision ix)))
				       (incf ix)
				       (assert (and (object-p a) (object-p b)))
				       (when (not (eq a b))
					 (let ((bt (field-value :y b))
					       (bl (field-value :x b))
					       (bh (field-value :height b))
					       (bw (field-value :width b)))
					   (when (collide y0 x0 width height bt bl bw bh)
					     (return-from intersecting t)))))
				  while (< ix num-gsprites)))))))))
	      nil))))))
	    
(define-method collide-* gsprite (o-top o-left o-width o-height)
  (with-field-values (x y width height) self
    (collide x y width height o-top o-left o-width o-height)))

(defun collide (x y width height o-top o-left o-width o-height)
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

(define-method do-collision gsprite (object)
  "Respond to a collision detected with OBJECT."
  nil)

(define-method save-excursion gsprite ()
  (setf <saved-x> <x>)
  (setf <saved-y> <y>))

(define-method undo-excursion gsprite ()
  (/move-to self <saved-x> <saved-y>))

(define-method viewport-coordinates gsprite ()
  (/get-viewport-coordinates-* (field-value :viewport *world*)
			      <x> <y>))

(define-method grid-coordinates gsprite ()
  (values (truncate (/ <y> (field-value :tile-size *world*)))
	  (truncate (/ <x> (field-value :tile-size *world*)))))

(define-method xy-coordinates gsprite ()
  (values <x> <y>))

(define-method drop gsprite (cell &optional (delta-row 0) (delta-column 0))
  (multiple-value-bind (r c)
      (/grid-coordinates self)
    (/drop-cell *world* cell (+ r delta-row) (+ c delta-column))))


;;; Proxying and vehicles

(define-method proxy gsprite (occupant)
  "Make this sprite a proxy for OCCUPANT."
  (let ((world *world*))
    (when <occupant> 
      (error "Attempt to overwrite existing occupant cell in proxying."))
    (setf <occupant> occupant)
    ;; The occupant should know when it is proxied, and who its proxy is.
    (/add-category occupant :proxied)
    (setf (field-value :proxy occupant) self)
    ;; Hide the proxy if it's in a world already.
    (/remove-sprite world occupant)
    ;; Don't let anyone step on occupied vehicle.
    (/add-category self :obstacle)
    ;; Don't light up the map 
    (/add-category self :light-source)
    ;; If it's the player register self as player.
    (when (/is-player occupant)
      (/add-category self :player)
      (/set-player world self)
      (setf <phase-number> (1- (/get-phase-number world))))))

(define-method unproxy gsprite (&key dr dc dx dy)
  "Remove the occupant from this sprite, dropping it on top."  
  (let ((world *world*)
	(occupant <occupant>))
    (when (null occupant)
      (error "Attempt to unproxy non-occupied sprite."))
    (ecase (field-value :type occupant)
      (:cell
	 (multiple-value-bind (r c) (/grid-coordinates self)
	   (/drop-cell world occupant r c)))
      (:sprite
	 (multiple-value-bind (x y) (/xy-coordinates self)
	   (/drop-sprite self occupant 
			(+ x (or dx 0))
			(+ y (or dy 0))))))
    (/delete-category occupant :proxied)
    (setf (field-value :proxy occupant) nil)
    (/do-post-unproxied occupant)
    (when (/is-player occupant)
      (/delete-category self :light-source)
      (/delete-category self :player)
      (/delete-category self :obstacle)
      (/set-player world occupant)
      (/run occupant))
    (setf <occupant> nil)))

(define-method do-post-unproxied gsprite ()
  "This method is invoked on the unproxied former occupant cell after
unproxying. By default, it does nothing."
  nil)

(define-method forward gsprite (method &rest args)
  "Attempt to deliver the failed message to the occupant, if any."
  (if (and (/is-player self)
	   (not (has-method method self))
	   (null <occupant>))
;;      (/say self (format nil "The ~S command is not applicable." method) )
      ;; otherwise maybe we're a vehicle
      (let ((occupant <occupant>))
	(when (null occupant)
	  (error "Cannot forward message ~S. No implementation found." method))
	(apply #'send self method occupant args))))
  
;; (define-method embark gsprite (&optional v)
;;   "Enter a vehicle V."
;;   (let ((vehicle (or v (/category-at-p *world* <row> <column> :vehicle))))
;;     (if (null vehicle)
;; 	(/>>say :narrator "No vehicle to embark.")
;; 	(if (null (field-value :occupant vehicle))
;; 	    (progn (/>>say :narrator "Entering vehicle.")
;; 		   (/proxy vehicle self))
;; 	    (/>>say :narrator "Already in vehicle.")))))

;; (define-method disembark gsprite ()
;;   "Eject the occupant."
;;   (let ((occupant <occupant>))
;;     (when (and occupant (/in-category self :proxy))
;; 	  (/unproxy self))))
  
;;; gsprites.lisp ends here
