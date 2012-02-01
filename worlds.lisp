;; worlds.lisp --- places where gameplay happens

;; Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011  David O'Toole

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
;; along with this program.  If not, see %http://www.gnu.org/licenses/

(in-package :blocky)

(defparameter *default-grid-size* 16)
(defparameter *default-world-axis-size* 10)

(define-block world
  (name :initform nil :documentation "Name of the world.")
  (description :initform "Unknown area." :documentation "Brief description of area.")
  (window-x :initform 0)
  (window-y :initform 0)
  (window-x0 :initform nil)
  (window-y0 :initform nil)
  (window-speed :initform 2)
  (window-scale-x :initform 1)
  (window-scale-y :initform 1)
  ;; the invisible graph paper underlying our world of sprites
  (grid :documentation "A two-dimensional array of adjustable vectors of cells.")
  (grid-size :initform *default-grid-size* :documentation "Size of a grid tile in GL units; either height or width (they must be equal.)")
  (grid-width :initform 16 :documentation "The width of the world map, measured in tiles.")
  (grid-height :initform 16 :documentation "The height of the world map, measured in tiles.")
  ;; 
  ;; a world-local dictionary
  (variables :initform nil :documentation "Hash table mapping values to values, local to the current world.")
  ;; turtle graphics
  (grammar :initform '() :documentation "Context-free grammar for level generation.")
  (stack :initform '() :documentation "Stack for logo system.")
  ;;
  (player :documentation "The player cell (or sprite).")
  (background :initform nil)
  (background-color :initform "black")
  ;; sprite cells
  (sprites :initform nil :documentation "A hash table of sprites.")
  (quadtree :initform nil)
  ;; lighting 
  (automapped :initform nil :documentation "Show all previously lit squares.")
  (light-grid 
   :documentation 
   "A 2d array of integers giving the light level at that point in %grid.
At the moment, only 0=off and 1=on are supported.")
  (ambient-light :initform :total :documentation 
		 "Radius of ambient visibility. :total means that lighting is turned off.")
  ;; What happens when you hit the edge?
  (edge-condition :initform :exit
		  :documentation "Either :block the player, :exit the world, or :wrap around.")
  ;; serialization
  (serialized-grid :documentation "When non-nil, a serialized sexp version of the grid.")
  (excluded-fields :initform
  '(:grid :player)))

(defmacro define-world (name &body body)
  `(define-block (,name :super "BLOCKY:WORLD")
     ,@body))

(define-method layout world ())

(define-method window-bounding-box world ()
  (values %window-y 
	  %window-x
	  (+ %window-x *gl-screen-width*)
	  (+ %window-y *gl-screen-height*)))

(define-method move-window-to world (x y)
  (setf %window-x x 
	%window-y y))

(define-method move-window world (dx dy)
  (incf %window-x dx)
  (incf %window-y dy))

(define-method glide-window-to world (x y)
  (setf %window-x0 x)
  (setf %window-y0 y))

(define-method glide-window-to-object world (object)
  (multiple-value-bind (top left right bottom) 
      (bounding-box object)
    (glide-window-to 
     self 
     (max 0 (- left (/ *gl-screen-width* 2)))
     (max 0 (- top (/ *gl-screen-width* 2))))))

(define-method glide-follow world (object)
  (with-fields (window-x window-y) self
    (let ((margin-x (* 1/3 *gl-screen-width*))
	  (margin-y (* 1/3 *gl-screen-height*))
	  (world-width (* %grid-size %grid-width))
	  (world-height (* %grid-size %grid-height))
	  (object-x (field-value :x object))
	  (object-y (field-value :y object)))
    ;; are we outside the "comfort zone"?
    (if (or 
	 ;; too far left
	 (> (+ window-x margin-x) 
	    object-x)
	 ;; too far right
	 (> object-x
	    (- (+ window-x *gl-screen-width*)
	       margin-x))
	 ;; too far up
	 (> (+ window-y margin-y) 
	    object-y)
	 ;; too far down 
	 (> object-y 
	    (- (+ window-y *gl-screen-height*)
	       margin-y)))
	;; yes. recenter.
	(glide-window-to self
			 (max 0
			      (min (- world-width *gl-screen-width*)
				   (- object-x 
				      (truncate (/ *gl-screen-width* 2)))))
			 (max 0 
			      (min (- world-height *gl-screen-height*)
				   (- object-y 
				      (truncate (/ *gl-screen-height* 2))))))))))

(define-method update-window-glide world ()
  (with-fields (window-x window-x0 window-y window-y0 window-scrolling-speed) self
    (labels ((nearby (a b)
	       (> window-scrolling-speed (abs (- a b))))
	     (jump (a b)
	       (if (< a b) window-scrolling-speed (- window-scrolling-speed))))
      (when (and window-x0 window-y0)
	(if (nearby window-x window-x0)
	    (setf window-x0 nil)
	    (incf window-x (jump window-x window-x0)))
	(if (nearby window-y window-y0)
	    (setf window-y0 nil)
	    (incf window-y (jump window-y window-y0)))))))

(define-method scale-window world (&optional (window-scale-x 1.0) (window-scale-y 1.0))
  (setf %window-scale-x window-scale-x)
  (setf %window-scale-y window-scale-y))

(define-method project world ()
  (do-orthographic-projection)
  (do-window %window-x %window-y %window-scale-x %window-scale-y))

(define-method build world (&rest parameters))

(define-method initialize world (&key grid-size grid-height grid-width name)
  ;(setf %grid-size (or grid-size *default-grid-size*))
  ;; (setf %grid-height (or grid-height (truncate (/ *screen-height* %grid-size))))
  ;; (setf %grid-width (or grid-width (truncate (/ *screen-width* %grid-size))))
  (setf %variables (make-hash-table :test 'equal))
  (setf %sprites (make-hash-table :test 'equal))
  (let ((world-bounds (list 0 0
			    (* %grid-size %grid-width)
			    (* %grid-size %grid-height))))
    (setf %quadtree 
	  (build-quadtree (scale-bounding-box world-bounds 1.2)))
    (create-default-grid self)))
  
(define-method handle-event world (event)
  (or (handle-event%%block self event)
      (with-fields (player quadtree) self
	(when player 
	  (prog1 t
	    (let ((*quadtree* quadtree))
	      (handle-event player event)))))))

(define-method make world (&rest parameters)
  (apply #'initialize self parameters))

(define-method make-with-parameters world (parameters)
  (apply #'send self :make self parameters))

;;; The sprite layer. 

(define-method add-block world (sprite &optional x y append)
  (let ((*quadtree* %quadtree))
    (assert (not (gethash (find-uuid sprite) 
			  %sprites)))
    (setf (gethash (find-uuid sprite)
		   %sprites)
	  t)
    (when (and (numberp x) (numberp y))
      (setf (field-value :x sprite) x
	    (field-value :y sprite) y))
    (clear-saved-location sprite)
    ;; (when (field-value :image sprite)
    ;;   (resize-to-image sprite))
    (quadtree-insert %quadtree sprite)))
      
;      (move-to sprite x y))))

(define-method remove-block world (sprite)
  (remhash (find-uuid sprite) %sprites)
  (when (field-value :quadtree-node sprite)
    (quadtree-delete %quadtree sprite)))

(define-method discard-block world (sprite)
  (remhash (find-uuid sprite) %sprites))

;;; World-local variables

(define-method setvar world (var value)
  (setf (gethash var %variables) value))

(define-method getvar world (var)
  (gethash var %variables))

(defun world-variable (var-name)
  (getvar *world* var-name))

(defun set-world-variable (var-name value)
  (setvar *world* var-name value))

(defsetf world-variable set-world-variable)

(defmacro with-world-variables (vars &rest body)
  (labels ((make-clause (sym)
	     `(,sym (world-variable ,(make-keyword sym)))))
    (let* ((symbols (mapcar #'make-non-keyword vars))
	   (clauses (mapcar #'make-clause symbols)))
      `(symbol-macrolet ,clauses ,@body))))

;;; Working with the grid and its locations

(define-method grid-location world (row column)
  "Return the vector of cells at ROW, COLUMN in the world SELF."
  (when (array-in-bounds-p %grid row column)
    (aref %grid row column)))

(define-method grid-location-list world (row column)
  (coerce (grid-location self row column)
	  'list))
  
(defparameter *default-grid-depth* 4)

(define-method create-grid world (&key grid-width grid-height)
  "Initialize all the arrays for a world of GRID-WIDTH by GRID-HEIGHT cells."
  (let ((dims (list grid-height grid-width)))
    (let ((grid (make-array dims 
		 :element-type 'vector :adjustable nil))
	  (light-grid (make-array dims :element-type 'integer)))
      (dotimes (i grid-height)
	(dotimes (j grid-width)
	  ;; now put a vector in each square to represent the z-axis
	  (setf (aref grid i j)
		(make-array *default-grid-depth* 
			    :adjustable t
			    :fill-pointer 0))))
      (setf %grid grid
	    %grid-height grid-height
	    %grid-width grid-width))))

(define-method create-default-grid world ()
  "If grid-height and grid-width have been set in a world's definition,
initialize the arrays for a world of the size specified there."
  (if (and (integerp %grid-width)
	   (integerp %grid-height))
      (create-grid self :grid-width %grid-width :grid-height %grid-height)
      (error "Cannot create default grid without grid-height and grid-width set.")))

(define-method delete-tag-at world (row column tag)
  "Delete all cells in TAG at ROW, COLUMN in the grid.
The cells' :destroy method is invoked."
  (let* ((grid %grid))
    ;; (declare (type (simple-array vector (* *)) grid)
    ;; 	     (optimize (speed 3)))
    (when (array-in-bounds-p grid row column)
      (setf (aref grid row column)
	    (delete-if #'(lambda (c) (when (has-tag c tag)
				       (prog1 t (destroy c))))
		       (aref grid row column))))))

(define-method tag-at world (row column tag)
  "Returns the grid location at ROW,COLUMN if there is any cell with
TAG at ROW, COLUMN. TAG may be a list of keyword symbols or one
keyword symbol."
  (let ((catlist (etypecase tag
		   (keyword (list tag))
		   (list tag)))
	(grid %grid))
    (declare (type (simple-array vector (* *)) grid))
    (and (array-in-bounds-p grid row column)
	 (some #'(lambda (cell)
		   (when (intersection catlist
				       (field-value :tags cell))
		     cell))
	       (aref grid row column)))))

(define-method in-bounds-p world (row column)
  "Return non-nil if ROW and COLUMN are valid coordinates."
  (array-in-bounds-p %grid row column))

(define-method nth-cell world (n row column)
  (aref (aref %grid row column) n))

(define-method top-cell world (row column)
  (let ((cells (grid-location self row column)))
    (when (and cells (not (zerop (fill-pointer cells))))
      (aref cells (- (fill-pointer cells) 1)))))

(define-method delete-cell world (cell row column)
  "Delete CELL from the grid at ROW, COLUMN."
  (let* ((grid %grid)
	 (square (aref grid row column))
	 (start (position cell square :test #'eq)))
    ;; (declare (type (simple-array vector (* *)) grid) 
    ;; 	  (optimize (speed 3)))
    (when start
      (replace square square :start1 start :start2 (1+ start))
      (decf (fill-pointer square)))))

(define-method move-cell world (cell row column)
  "Move CELL to ROW, COLUMN."
  (let* ((old-row (field-value :row cell))
	 (old-column (field-value :column cell)))
    (delete-cell self cell old-row old-column)
    (drop-cell self cell row column)))

(defparameter *grid-layer-z* -100)

;; (define-method drop-cell world (cell row column)
  ;; (let ((size %grid-size))
  ;;   (when (array-in-bounds-p %grid row column)
  ;;     ;; (add-sprite self cell)
  ;;     (let ((*quadtree* %quadtree))
  ;; 	(move-to cell (* size column) (* size row))
  ;; 	(resize cell size size)
  ;; 	(setf (field-value :on-grid cell) t)))))

(define-method drop-block world (block x y)
  (add-block self block)
  (move-to block x y))

(define-method drop-cell world (block row column)
  (let ((*world* self))
    (let ((grid %grid))
      ;; (declare (optimize (speed 3)) 
      ;; 	     (type (simple-array vector (* *)) grid)
      ;; 	     (fixnum grid-size row column))
      (when (array-in-bounds-p grid row column)
	(setf (field-value :on-grid block) t)
;	(vector-push-extend block (aref grid row column))
	(setf (field-value :y block) (* %grid-size row))
	(setf (field-value :x block) (* %grid-size column))
	(quadtree-insert %quadtree block)
	(setf (field-value :row block) row)
	(setf (field-value :column block) row)))))

;;; Handling serialization
 
(define-method before-serialize world ()
  (with-field-values (grid-width grid-height) self
    (let ((grid %grid)
	  (sgrid (make-array (list grid-height grid-width) :initial-element nil :adjustable nil)))
      (dotimes (i grid-height)
	(dotimes (j grid-width)
	  (map nil #'(lambda (cell)
		       (when cell 
			 (push (serialize cell) 
			       (aref sgrid i j))))
	       (aref grid i j))))
      (setf %serialized-grid sgrid))))
    
(define-method after-deserialize world ()
    (create-default-grid self)
    (with-field-values (grid-width grid-height grid serialized-grid) self
      (dotimes (i grid-height)
	(dotimes (j grid-width)
	  (map nil #'(lambda (cell)
		       (when cell
			 (vector-push-extend (deserialize cell)
					     (aref grid i j))))
	       (reverse (aref serialized-grid i j)))))
      (setf %serialized-grid nil)))

;;; About the player
			      
(define-method drop-player-at-last-location world (player)
  (setf %player player)
  (drop-cell self player %player-exit-row %player-exit-column))
  
(define-method get-player world ()
  %player)

(defun player ()
  (get-player *world*))

(define-method player-row world ()
  "Return the grid row the player is on."
  (with-field-values (player grid-size) self
    (with-field-values (on-grid row y) player
      (if on-grid 
	  row
	  (truncate (/ y grid-size))))))

(define-method player-column world ()
  "Return the grid column the player is on."
  (with-field-values (player grid-size) self
    (with-field-values (on-grid column x) player
      (if on-grid 
	  column
	  (truncate (/ x grid-size))))))

(define-method obstacle-at world (row column)
  "Returns grid location at (ROW,COLUMN) if there's an obstacle
there."
  (or (not (array-in-bounds-p %grid row column))
      (some #'(lambda (cell)
		(when (has-tag cell :obstacle)
		  cell))
	    (aref %grid row column))))

(define-method enemy-at-p world (row column)
  (tag-at self row column :enemy))

(define-method adjacent-to-player world (row column)
  "Return non-nil when ROW, COLUMN is adjacent to the player."
  (<= (distance-to-player self row column) 1.5))

(define-method add-player world (player)
  "Set PLAYER as the player object to which the World will forward
most user command messages. (See also the method `forward'.)"
  (setf %player player)
  (add-block self player))

;;; Draw the world

(define-method draw world ()
  (project self) ;; set up camera
  (with-field-values (sprites grid-size grid grid-height grid-width
			      background background-color) self
    (declare (type (simple-array vector (* *)) grid))
    (if background
	(draw-image background 0 0)
	(when background-color
	  (draw-box 0 0
		    (* grid-size grid-width) 
		    (* grid-size grid-height)
		    :color background-color)))
    ;; draw the grid first
    (dotimes (i grid-height)
      (dotimes (j grid-width)
    	(let ((cells (aref grid i j)))
    	  (dotimes (z (fill-pointer cells))
    	    (draw (aref cells z))))))
    ;; draw the sprites
    (loop for sprite being the hash-keys in %sprites do
      (draw sprite))))
;    (quadtree-show %quadtree %player)))

;;; Simulation update

(define-method update world (&rest args)
  ;; (declare (optimize (speed 3)))
  (declare (ignore args))
  (with-field-values (grid sprites quadtree grid-height grid-width player) self
    (let ((*quadtree* quadtree))
      (assert (zerop *quadtree-depth*))
      ;; update the grid
      (dotimes (i grid-height)
	(dotimes (j grid-width)
	  (declare (type (simple-array vector (* *)) grid))
	  (let ((cells (aref grid i j)))
	    (dotimes (z (fill-pointer cells))
	      (update (aref cells z))))))
      ;; run the sprites
      (loop for sprite being the hash-keys in %sprites do
	(run-tasks sprite)
	(update sprite))
      ;; update window movement
      (glide-follow self %player)
      (update-window-glide self)
      ;; do collisions for both sprites and grid
      (loop for sprite being the hash-keys in %sprites do
	;; (grid-collide self sprite)
	(quadtree-collide quadtree sprite)))))
    
;;; Collision detection for the grid objects

(define-method map-grid world (sprite function)
  ;; figure out which grid squares we really need to scan
  (let ((grid-size %grid-size)
	(grid %grid))
    (declare (type (simple-array vector (* *)) grid) 
	     (optimize (speed 3)))
    (multiple-value-bind (x y width height)
	(bounding-box sprite)
      (let* ((left (1- (floor (/ x grid-size))))
	     (right (1+ (floor (/ (+ x width) grid-size))))
	     (top (1- (floor (/ y grid-size))))
	     (bottom (1+ (floor (/ (+ y height) grid-size)))))
	;; find out which scanned squares actually intersect the sprite
	(dotimes (i (max 0 (- bottom top)))
	  (dotimes (j (max 0 (- right left)))
	    (let ((i0 (+ i top))
		  (j0 (+ j left)))
	      (when (array-in-bounds-p grid i0 j0)
		(when (colliding-with-rectangle sprite 
						(* i0 grid-size) 
						(* j0 grid-size)
						grid-size grid-size)
		  (funcall function i0 j0))))))))))

(define-method map-grid-objects world (sprite function)
  (let ((grid %grid))
    (map-grid self sprite
	      #'(lambda (i j)
		  (map nil function (aref grid i j))))))

(define-method grid-collide world (sprite)
  (map-grid-objects
   self sprite 
   #'(lambda (object)
       (collide sprite object))))

;;; Lighting and line-of-sight

(defvar *lighting-hack-function* nil)
  
(define-method render-lighting world (cell)
  "When lighting is activated, calculate lit squares using light
sources and ray casting."
  (let* ((light-radius (field-value :light-radius cell))
	 (ambient %ambient-light)
	 (light-grid %light-grid)
	 (grid %grid)
	 (source-row (field-value :row cell))
	 (source-column (field-value :column cell))
	 (total (+ light-radius 
		   (if (numberp ambient) ambient 0)))
	 (octagon (make-array 100 :initial-element nil :adjustable t :fill-pointer 0))
	 (line (make-array 100 :initial-element nil :adjustable t :fill-pointer 0)))
;;    (declare (type (simple-array vector (* *)) grid) (optimize (speed 3)))
    ;; don't bother lighting if everything is lit.
    (when (not (eq :total ambient))
      ;; draw only odd-radius octagons that have a center pixel
      (when (evenp total)
	(incf total))
      (labels ((light-square (row column)
		 (when (array-in-bounds-p light-grid row column)
		   (setf (aref light-grid row column) 1) nil))
	       (collect-line-point (x y)
		 (prog1 nil (vector-push-extend (list x y) line)))
		 ;; (if (array-in-bounds-p light-grid x y)
		 ;;     (prog1 nil (vector-push-extend (list x y) line))
		 ;;     t))
	       (make-line (row column)
		 (setf (fill-pointer line) 0)
		 (let ((flipped (trace-line #'collect-line-point 
					    source-column source-row
					    column row)))
		   ;; Bresenham's swaps the input points around when x0 is to the
		   ;; right of x1. We need to reverse the list of points if this
		   ;; happens, otherwise shadows will be cast the wrong way.
		   (if flipped
		       (setf line (nreverse line))
		       ;; Furthermore, when a non-flipped line is drawn, the endpoint 
		       ;; isn't actually visited, so we append it to the line. (Maybe this 
		       ;; is a bug in my implementation?)
		       ;;
		       ;; Make sure endpoint of ray is traced.
		       (when (array-in-bounds-p grid row column)
			 (vector-push-extend (list row column) line)))))
	       (light-line (row column)
		 (make-line row column)
		 (block lighting 
		   (dotimes (i (fill-pointer line))
		     do (destructuring-bind (r c) (aref line i)
			  (when (array-in-bounds-p grid r c)
			    (light-square r c)
			    ;; HACK
			    (when *lighting-hack-function*
			      (funcall *lighting-hack-function* 
				       source-row source-column
				       r c))
			    ;; should we stop lighting?
			    (when (tag-at self r c :opaque) ;;'(:opaque :obstacle))
			      (return-from lighting t)))))))
	       (collect-octagon-point (r c)
		 (vector-push-extend (list r c) octagon) nil)
	       (light-rectangle (row column radius)
		 (trace-rectangle #'light-square 
				  (- row radius)
				  (- column radius) 
				  (* 2 radius)
				  (* 2 radius)
				  :fill))
	       (light-octagon (row column radius)
		 (setf (fill-pointer octagon) 0)
	       	 (trace-octagon #'collect-octagon-point 
	       			row column radius :thicken)
	       	 (dotimes (i (fill-pointer octagon))
	       	   (destructuring-bind (row column) (aref octagon i)
		     ;; HACK
		     ;; (when *lighting-hack-funtcion*
		     ;;   (funcall *lighting-hack-function* 
		     ;; 		source-row source-column
		     ;; 		row column "red"))
	       	     (light-line row column)))))
	(light-octagon source-row source-column total)
	(light-octagon source-row source-column (- total 2))))))

(define-method clear-light-grid world ()
  (unless %automapped
    (let ((light-grid %light-grid))
      (when (arrayp light-grid)
	(dotimes (i %grid-height)
	  (dotimes (j %grid-width)	
	    (setf (aref light-grid i j) 0)))))))
    
(define-method line-of-sight world (r1 c1 r2 c2 &optional (category :obstacle))
  "Return non-nil when there is a direct Bresenham's line of sight
along grid squares between R1,C1 and R2,C2."
  (let ((grid %grid))
    (when (and (array-in-bounds-p grid r1 c1) 
	       (array-in-bounds-p grid r2 c2))
      (let ((line (make-array 100 :initial-element nil :adjustable t :fill-pointer 0))
	    (num-points 0)
	    (r0 r1)
	    (c0 c1))
	(labels ((collect-point (&rest args)
		   (prog1 nil
		     (vector-push-extend args line)
		     (incf num-points))))
	  (let ((flipped (trace-line #'collect-point c1 r1 c2 r2)))
	    (if flipped 
		(setf line (nreverse line))
		(when (array-in-bounds-p grid r2 c2)
		  (incf num-points)
		  (vector-push-extend (list c2 r2) line)))
	    (let ((retval (block tracing
			    (let ((i 0))
			      (loop while (< i num-points) do
				(destructuring-bind (x y) (aref line i)
				  (setf r0 x c0 y)
				  (when *lighting-hack-function* 
				    (funcall *lighting-hack-function* r0 c0 r1 c1))
				  (if (and (= r0 r2)
					   (= c0 c2))
				      (return-from tracing t)
				      (when (tag-at self r0 c0 category)
					(return-from tracing nil))))
				(incf i)))
			    (return-from tracing t))))
	      (prog1 retval nil))))))))

;;; Pasting operations. This section needs work. 

(define-method paste-region world (other-world dest-row dest-column source-row source-column source-grid-height source-grid-width 
					       &optional deepcopy)
    (loop for row from 0 to source-grid-height
	  do (loop for column from 0 to source-grid-width
		   do (let* ((cells (grid-location other-world (+ row source-row) (+ column source-column)))
			     (n 0))
			(when (vectorp cells)
			  (loop while (< n (fill-pointer cells)) do
			    (let* ((cell (aref cells n))
				   (proto (object-parent cell))
				   (new-cell (if (or deepcopy (field-value :auto-deepcopy cell))
						 ;; create a distinct object with the same local field values.
						 (deserialize (serialize cell))
						 ;; create a similar object
						 (clone proto))))
			      (drop-cell self new-cell (+ row dest-row) (+ column dest-column) :exclusive nil))
			    (incf n)))))))

(define-method clone-onto world (other-world &optional deepcopy)
  (let ((other (etypecase other-world
		 (string (find-resource-object other-world))
		 (object other-world))))
    (with-fields (grid-height grid-width) other
      (create-grid self :grid-height grid-height :grid-width grid-width)
      (let ((*world* other))
	(paste-region self other 0 0 0 0 grid-height grid-width deepcopy)))))

(define-method begin-ambient-loop world ()
  "Begin looping your music for this world here."
  nil)

(define-method after-start-method world ()
  nil)

;; (defun generate-world-name (world)
;;   (concatenate 'string (get-some-object-name world) "-" (format nil "~S" (genseq))))

;; (defun create-blank-world (&key grid-height grid-width name)
;;   (let ((world (clone "BLOCKY:WORLD" :grid-height grid-height :grid-width grid-width)))
;;     (prog1 world
;;       (setf (field-value :name world)
;; 	    (or name (generate-world-name world))))))

;; (defun create-blank-object (arg1 arg2))

;;; Gateways and launchpads connect worlds together

;; (defcell gateway
;;   (tile :initform "gateway")
;;   (name :initform "Gateway")
;;   (categories :initform '(:gateway))
;;   (destination :initform nil))

;; (define-method initialize gateway (&key destination tile name)
;;   (when tile (setf %tile tile))
;;   (when name (setf %name name))
;;   (when destination (setf %destination destination)))

;; (define-method activate gateway ()
;;   (with-fields (destination) self
;;     (etypecase destination
;;       ;; it's an address.
;;       (list (play *universe* :address destination))
;;       ;; it's a mission name
;;       (symbol (begin (symbol-value destination) (get-player *world*))))))
	 
;; (define-prototype launchpad (:super "BLOCKY:GATEWAY")
;;   (tile :initform "launchpad")
;;   (categories :initform '(:gateway :player-entry-point))
;;   (description :initform "Press RETURN here to exit this area."))

;; (define-method activate launchpad ()
;;   (exit *universe* :player (get-player *world*)))

;; (define-method drop-entry-point world (row column)
;;   (replace-grid-location self row column (new launchpad)))

;; (defun find-object (object)
;;   (etypecase object
;;     (object 
;;        ;; check for name collision
;;        (let* ((old-name (or (field-value :name object)
;; 			    (generate-object-name object)))
;; 	      (new-name (if (find-resource old-name :noerror)
;; 			    (generate-object-name object)
;; 			    old-name)))
;; 	 (message "Indexing new object ~S as ~S" old-name new-name)
;; 	 (prog1 object 
;; 	   (make-object-resource new-name object)
;; 	   (setf (field-value :name object) new-name))))
;;     (list 
;;        ;; it's an address
;;        (destructuring-bind (prototype-name &rest parameters) object
;; 	 (let ((object (clone (symbol-value prototype-name))))
;; 	   (make-with-parameters object parameters)
;; 	   (find-object object))))
;;     (string (or (find-resource-object object :noerror)
;; 		(progn (make-object-resource object (create-blank-object :name object))
;; 		       (let ((object (find-resource-object object)))
;; 			 (prog1 object
;; 			   (setf (field-value :name object) object))))))))


;; (define-method initialize world (&key grid-height grid-width)
;;   (when grid-height (setf %grid-height grid-height))
;;   (when grid-width (setf %grid-width grid-width))
;;   (setf %variables (make-hash-table :test 'equal))
;;   (create-default-grid self))
    
;;; LOGO-like level generation capabilities 

;; Use turtle graphics to generate levels! One may write turtle
;; programs by hand, or use context-free grammars to generate the
;; turtle commands, or generate them programmatically in other ways.
;; See also logic.lisp.

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

;; (define-method is-generated world ()
;;   (if %grid t nil))

;; (define-method generate-with world (parameters)
;;   (apply #'send self :generate self parameters))

;; (define-method origin world ()
;;   "Move the turtle to its default location (0,0) and orientation (east)."
;;   (setf %row 0 %column 0 %direction :east))

;; (define-method color world ()
;;   "Set the color to =FOO= where FOO is the prototype symbol on top of
;; the stack."
;;   (let ((prototype (pop %stack)))
;;     (if (object-p prototype)
;; 	(setf %paint prototype)
;; 	(error "Must pass a =FOO= prototype symbol as a COLOR."))))

;; (define-method push-color world ()
;;   "Push the symbol name of the current %paint object onto the stack."
;;   (with-fields (paint stack) self
;;       (if (object-p paint)
;; 	  (prog1 (message "PUSHING PAINT ~S" (object-name paint))
;; 	    (push paint stack))
;; 	  (error "No paint to save on stack during PUSH-COLOR."))))

;; (define-method drop world ()
;;   "Clone the current %paint object and drop it at the current turtle
;; location."
;;   (with-field-values (paint row column) self
;;     (if (object-p paint)
;; 	(drop-cell self (clone paint) row column)
;; 	(error "Nothing to drop. Use =FOO= :COLOR to set the paint color."))))

;; (define-method jump world ()
;;   "Jump N squares forward where N is the integer on the top of the stack."
;;   (let ((distance (pop %stack)))
;;     (if (integerp distance)
;; 	(multiple-value-bind (row column) 
;; 	    (step-in-direction %row %column %direction distance)
;; 	  (setf %row row %column column)
;; 	  (when (not (array-in-bounds-p %grid row column))
;; 	    (message "Turtle left drawing area during MOVE.")))
;; 	(error "Must pass an integer as distance for MOVE."))))
      
;; ;; (define-method draw world ()
;; ;;   "Move N squares forward while painting cells. Clones N cells where N
;; ;; is the integer on the top of the stack."
;; ;;   (with-fields (paint stack) self
;; ;;     (if (not (object-p paint))
;; ;; 	(error "No paint set.")
;; ;; 	(let ((distance (pop stack)))
;; ;; 	  (if (integerp distance)
;; ;; 	      (dotimes (n distance)
;; ;; 		(drop-cell self (clone paint) %row %column)
;; ;; 		(multiple-value-bind (row column) 
;; ;; 		    (step-in-direction %row %column %direction)
;; ;; 		  (setf %row row %column column)
;; ;; 		  (when (array-in-bounds-p %grid row column)
;; ;; 		      (message "Turtle left drawing area during DRAW."))))
;; ;; 	      (error "Must pass an integer as distance for DRAW."))))))

;; (define-method pushloc world ()
;;   "Push the current row,col location (and direction) onto the stack."
;;   (push (list %row %column %direction) %stack))

;; (define-method poploc world ()
;;   "Jump to the location on the top of the stack, and pop the stack."
;;   (let ((loc (pop %stack)))
;;     (if (and (listp loc) (= 3 (length loc)))
;; 	(destructuring-bind (r c dir) loc
;; 	  (setf %row r %column c %direction dir))
;; 	(error "Invalid location argument for POPLOC. Must be a list of two integers plus a keyword."))))

;; (define-method right world ()
;;   "Turn N degrees clockwise, where N is 0, 45, or 90."
;;   (with-fields (direction stack) self
;;     (labels ((turn45 () (setf direction (getf *right-turn* direction))))
;;       (ecase (pop stack)
;; 	(0 nil)
;; 	(45 (turn45))
;; 	(90 (turn45) (turn45))))))

;; (define-method left world ()
;;   "Turn N degrees counter-clockwise, where N is 0, 45, or 90."
;;   (with-fields (direction stack) self
;;     (labels ((turn45 () (setf direction (getf *left-turn* direction))))
;;       (ecase (pop stack)
;; 	(0 nil)
;; 	(45 (turn45))
;; 	(90 (turn45) (turn45))))))

;; (define-method noop world ()
;;   nil)

;; (define-method set-narrator world (narrator)
;;   (setf %narrator narrator))

;; (define-method set-browser world (browser)
;;   (setf %browser browser))

;; (define-method random-place world (&optional &key avoiding distance)
;;   (with-field-values (grid-width grid-height) self
;;     (let ((limit 10000)
;; 	  (n 0)
;; 	  found r c)
;;       (loop do (progn (setf r (random grid-height))
;; 		      (setf c (random grid-width))
;; 		      (incf n)
;; 		      (unless 
;; 			  (or (and (numberp distance)
;; 				   (> distance (distance r c 0 0)))
;; 			      (tag-at self r c :exclusive))
;; 			(setf found t)))
;; 	    while (and (not found) 
;; 		       (< n limit)))
;;       (values r c found))))
		      
;; (define-method replace-cell world (cell new-cell row column
;; 					&optional &key loadout no-collisions)
;;   "Replace the CELL with NEW-CELL at ROW, COLUMN in this world."
;;   (let* ((cells (grid-location self row column))
;; 	 (pos (position cell cells)))
;;     (if (numberp pos)
;; 	(progn 
;; 	  (destroy (aref cells pos))
;; 	  (setf (aref cells pos) new-cell))
;; 	(error "Could not find cell to replace."))))

;; (define-method replace-cells-at world (row column data)
;;   "Destroy the cells at ROW, COLUMN, invoking DESTROY on each,
;; replacing them with the single cell (or vector of cells) DATA."
;;   (when (array-in-bounds-p %grid row column)
;;     (do-cells (cell (aref %grid row column))
;;       (destroy cell))
;;     (setf (aref %grid row column)
;; 	  (etypecase data
;; 	    (vector data)
;; 	    (object (let ((cells (make-array *default-grid-depth*
;; 						  :adjustable t
;; 						  :fill-pointer 0)))
;; 			   (prog1 cells
;; 			     (vector-push-extend data cells))))))
;;     (do-cells (cell (aref %grid row column))
;;       (set-location cell row column))))


;; (define-method exit world ()
;;   "Leave the current world."
;;   (setf %exited t) ;; see also `forward' method
;;   ;; record current location so we can exit back to it
;;   (setf %player-exit-row (field-value :row %player))
;;   (setf %player-exit-column (field-value :column %player))
;;   (exit %player)
;;   (delete-cell self %player %player-exit-row %player-exit-column))

;; (define-method drop-player-at-entry world (player)
;;   "Drop the PLAYER at the first entry point."
;;   (with-field-values (grid-width grid-height grid grid-size) self
;;     (multiple-value-bind (dest-row dest-column)
;; 	(block seeking
;; 	  (dotimes (i grid-height)
;; 	    (dotimes (j grid-width)
;; 	      (when (tag-at self i j :player-entry-point)
;; 		(return-from seeking (values i j)))))
;; 	  (return-from seeking (values 0 0)))
;;       (setf %player player)
;;       (ecase (field-value :type player)
;; 	(:cell (drop-cell self player dest-row dest-column :no-stepping t))
;; 	(:sprite (drop-sprite self player 
;; 			      (* dest-column grid-size)
;; 			      (* dest-row grid-size)))))))
	
;; (define-method obstacle-in-direction-p world (row column direction)
;;   "Return non-nil when there is an obstacle one step in DIRECTION from ROW, COLUMN."
;;   (multiple-value-bind (nrow ncol)
;;       (step-in-direction row column direction)
;;     (obstacle-at self nrow ncol)))

;; (define-method category-in-direction-p world (row column direction category)
;;   "Return non-nil when there is a cell in CATEGORY one step in
;; DIRECTION from ROW, COLUMN. CATEGORY may be a list as well."
;;   (multiple-value-bind (nrow ncol)
;;       (step-in-direction row column direction)
;;     (tag-at self nrow ncol category)))

;; (define-method target-in-direction-p world (row column direction)
;;   "Return non-nil when there is a target one step in DIRECTION from ROW, COLUMN."
;;   (multiple-value-bind (nrow ncol)
;;       (step-in-direction row column direction)
;;     (tag-at self nrow ncol :target)))

;;; worlds.lisp ends here
