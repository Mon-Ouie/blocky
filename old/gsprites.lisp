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

;; ;;; Lighting and line-of-sight

;; (defvar *lighting-hack-function* nil)
  
;; (define-method render-lighting world (cell)
;;   "When lighting is activated, calculate lit squares using light
;; sources and ray casting."
;;   (let* ((light-radius (field-value :light-radius cell))
;; 	 (ambient %ambient-light)
;; 	 (source-row (field-value :row cell))
;; 	 (source-column (field-value :column cell))
;; 	 (total (+ light-radius 
;; 		   (if (numberp ambient) ambient 0)))
;; 	 (octagon (make-array 100 :initial-element nil :adjustable t :fill-pointer 0))
;; 	 (line (make-array 100 :initial-element nil :adjustable t :fill-pointer 0)))
;;     ;; don't bother lighting if everything is lit.
;;     (when (not (eq :total ambient))
;;       ;; draw only odd-radius octagons that have a center pixel
;;       (when (evenp total)
;; 	(incf total))
;;       (labels ((light-square (row column)
;; 		 (when (array-in-bounds-p light-grid row column)
;; 		   (setf (aref light-grid row column) 1) nil))
;; 	       (collect-line-point (x y)
;; 		 (prog1 nil (vector-push-extend (list x y) line)))
;; 		 ;; (if (array-in-bounds-p light-grid x y)
;; 		 ;;     (prog1 nil (vector-push-extend (list x y) line))
;; 		 ;;     t))
;; 	       (make-line (row column)
;; 		 (setf (fill-pointer line) 0)
;; 		 (let ((flipped (trace-line #'collect-line-point 
;; 					    source-column source-row
;; 					    column row)))
;; 		   ;; Bresenham's swaps the input points around when x0 is to the
;; 		   ;; right of x1. We need to reverse the list of points if this
;; 		   ;; happens, otherwise shadows will be cast the wrong way.
;; 		   (if flipped
;; 		       (setf line (nreverse line))
;; 		       ;; Furthermore, when a non-flipped line is drawn, the endpoint 
;; 		       ;; isn't actually visited, so we append it to the line. (Maybe this 
;; 		       ;; is a bug in my implementation?)
;; 		       ;;
;; 		       ;; Make sure endpoint of ray is traced.
;; 		       (when (array-in-bounds-p grid row column)
;; 			 (vector-push-extend (list row column) line)))))
;; 	       (light-line (row column)
;; 		 (make-line row column)
;; 		 (block lighting 
;; 		   (dotimes (i (fill-pointer line))
;; 		     do (destructuring-bind (r c) (aref line i)
;; 			  (when (array-in-bounds-p grid r c)
;; 			    (light-square r c)
;; 			    ;; HACK
;; 			    (when *lighting-hack-function*
;; 			      (funcall *lighting-hack-function* 
;; 				       source-row source-column
;; 				       r c))
;; 			    ;; should we stop lighting?
;; 			    (when (tag-at self r c :opaque) ;;'(:opaque :obstacle))
;; 			      (return-from lighting t)))))))
;; 	       (collect-octagon-point (r c)
;; 		 (vector-push-extend (list r c) octagon) nil)
;; 	       (light-rectangle (row column radius)
;; 		 (trace-rectangle #'light-square 
;; 				  (- row radius)
;; 				  (- column radius) 
;; 				  (* 2 radius)
;; 				  (* 2 radius)
;; 				  :fill))
;; 	       (light-octagon (row column radius)
;; 		 (setf (fill-pointer octagon) 0)
;; 	       	 (trace-octagon #'collect-octagon-point 
;; 	       			row column radius :thicken)
;; 	       	 (dotimes (i (fill-pointer octagon))
;; 	       	   (destructuring-bind (row column) (aref octagon i)
;; 		     ;; HACK
;; 		     ;; (when *lighting-hack-funtcion*
;; 		     ;;   (funcall *lighting-hack-function* 
;; 		     ;; 		source-row source-column
;; 		     ;; 		row column "red"))
;; 	       	     (light-line row column)))))
;; 	(light-octagon source-row source-column total)
;; 	(light-octagon source-row source-column (- total 2))))))

;; (define-method clear-light-grid world ()
;;   (unless %automapped
;;     (let ((light-grid %light-grid))
;;       (when (arrayp light-grid)
;; 	(dotimes (i %grid-height)
;; 	  (dotimes (j %grid-width)	
;; 	    (setf (aref light-grid i j) 0)))))))
    
;; (define-method line-of-sight world (r1 c1 r2 c2 &optional (category :obstacle))
;;   "Return non-nil when there is a direct Bresenham's line of sight
;; along grid squares between R1,C1 and R2,C2."
;;   (let ((grid %grid))
;;     (when (and (array-in-bounds-p grid r1 c1) 
;; 	       (array-in-bounds-p grid r2 c2))
;;       (let ((line (make-array 100 :initial-element nil :adjustable t :fill-pointer 0))
;; 	    (num-points 0)
;; 	    (r0 r1)
;; 	    (c0 c1))
;; 	(labels ((collect-point (&rest args)
;; 		   (prog1 nil
;; 		     (vector-push-extend args line)
;; 		     (incf num-points))))
;; 	  (let ((flipped (trace-line #'collect-point c1 r1 c2 r2)))
;; 	    (if flipped 
;; 		(setf line (nreverse line))
;; 		(when (array-in-bounds-p grid r2 c2)
;; 		  (incf num-points)
;; 		  (vector-push-extend (list c2 r2) line)))
;; 	    (let ((retval (block tracing
;; 			    (let ((i 0))
;; 			      (loop while (< i num-points) do
;; 				(destructuring-bind (x y) (aref line i)
;; 				  (setf r0 x c0 y)
;; 				  (when *lighting-hack-function* 
;; 				    (funcall *lighting-hack-function* r0 c0 r1 c1))
;; 				  (if (and (= r0 r2)
;; 					   (= c0 c2))
;; 				      (return-from tracing t)
;; 				      (when (tag-at self r0 c0 category)
;; 					(return-from tracing nil))))
;; 				(incf i)))
;; 			    (return-from tracing t))))
;; 	      (prog1 retval nil))))))))

;; (define-method generate world (&rest parameters)
;;   "Generate a world, reading generation parameters from the plist
;; PARAMETERS and interpreting the world's grammar field %GRAMMAR."
;;   (declare (ignore parameters))
;;   (with-fields (grammar stack) self
;; ;    (setf blocky:*grammar* grammar)
;;     (let ((program (generate 'world)))
;;       (or program (message "WARNING: Nothing was generated from this grammar."))
;;       (message (prin1-to-string program))
;;       (unless %grid
;; 	(create-default-grid self))
;;       (dolist (op program)
;; 	(typecase op
;; 	  (keyword (if (has-method op self)
;; 		       (send nil op self)
;; 		       (message "WARNING: Found keyword without corresponding method in turtle program.")))
;; 	  (symbol (when (null (keywordp op))
;; 		    (when (boundp op)
;; 		      (push (symbol-value op) stack))))
;; 	  (string (push op stack))
;; 	  (number (push op stack)))))))


(in-package :ioforms)

(error "This file is obsolete. Don't compile it.")

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
;; (define-method would-collide sprite (x0 y0)
;;   (with-field-values (grid-size grid sprite-grid) *world*
;;     (with-field-values (width height x y) self
;;       ;; determine squares sprite would intersect
;;       (let ((left (1- (floor (/ x0 grid-size))))
;; 	    (right (1+ (floor (/ (+ x0 width) grid-size))))
;; 	    (top (1- (floor (/ y0 grid-size))))
;; 	    (bottom (1+ (floor (/ (+ y0 height) grid-size)))))
;; 	;; search intersected squares for any obstacle
;; 	(or (block colliding
;; 	      (let (found)
;; 		(dotimes (i (max 0 (- bottom top)))
;; 		  (dotimes (j (max 0 (- right left)))
;; 		    (let ((i0 (+ i top))
;; 			  (j0 (+ j left)))
;; 		      (when (array-in-bounds-p grid i0 j0)
;; 			(when (collide-* self
;; 					 (* i0 grid-size) 
;; 					 (* j0 grid-size)
;; 					 grid-size grid-size)
;; 			  ;; save this intersection information
;; 			  (vector-push-extend self (aref sprite-grid i0 j0))
;; 			  ;; quit when obstacle found
;; 			  (let ((obstacle (obstacle-at-p *world* i0 j0)))
;; 			    (when obstacle
;; 			      (setf found obstacle))))))))
;; 		(return-from colliding found)))
;; 	    ;; scan for sprite intersections
;; 	    (block intersecting 
;; 	      (let (collision num-sprites ix)
;; 		(dotimes (i (max 0 (- bottom top)))
;; 		  (dotimes (j (max 0 (- right left)))
;; 		    (let ((i0 (+ i top))
;; 			  (j0 (+ j left)))
;; 		      (when (array-in-bounds-p grid i0 j0)
;; 			(setf collision (aref sprite-grid i0 j0))
;; 			(setf num-sprites (length collision))
;; 			(when (< 1 num-sprites)
;; 			  (dotimes (i (- num-sprites 1))
;; 			    (setf ix (1+ i))
;; 			    (loop do (let ((a (aref collision i))
;; 					   (b (aref collision ix)))
;; 				       (incf ix)
;; 				       (assert (and (object-p a) (object-p b)))
;; 				       (when (not (eq a b))
;; 					 (let ((bt (field-value :y b))
;; 					       (bl (field-value :x b))
;; 					       (bh (field-value :height b))
;; 					       (bw (field-value :width b)))
;; 					   (when (collide y0 x0 width height bt bl bw bh)
;; 					     (return-from intersecting t)))))
;; 				  while (< ix num-sprites)))))))))
;; 	      nil))))))

;; (define-method allocate-image block ()
;;   (with-fields (image height width) self
;;     (let ((oldimage image))
;;       (when oldimage
;; 	(sdl:free oldimage))
;;       (setf image (create-image width height)))))

;; (define-method resize-image block (&key height width)
;;   "Allocate an image buffer of HEIGHT by WIDTH pixels.
;; If there is no existing image, one of HEIGHT x WIDTH pixels is created
;; and stored in ^IMAGE. If there is an existing image, it is only
;; resized when the new dimensions differ from the existing image."
;;   (assert (and (integerp width) (integerp height)))
;;   (with-fields (image) self
;;     (if (null image)
;; 	(progn (setf ^width width
;; 		     ^height height)
;; 	       (allocate-image self))
;; 	(when (not (and (= ^width width)
;; 			(= ^height height)))
;; 	  (setf ^width width
;; 		^height height)
;; 	  (when image (allocate-image self))))))

