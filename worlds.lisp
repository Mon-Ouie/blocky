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

(define-block world
  (variables :initform nil 
	     :documentation "Hash table mapping values to values, local to the current world.")
  (player :documentation "The player cell (or object).")
  (background :initform nil)
  (background-color :initform "black")
  (height :initform 1000)
  (width :initform 1000)
  ;; objects and collisions
  (objects :initform nil :documentation "A hash table with all the world's object.")
  (quadtree :initform nil)
  ;; viewing window
  (window-x :initform 0)
  (window-y :initform 0)
  (window-x0 :initform nil)
  (window-y0 :initform nil)
  (window-scrolling-speed :initform 2)
  (window-scale-x :initform 1)
  (window-scale-y :initform 1))

(defmacro define-world (name &body body)
  `(define-block (,name :super "BLOCKY:WORLD")
     ,@body))

(define-method get-objects world ()
  (loop for object being the hash-keys in %objects collect object))

(define-method layout world ())

;; Defining and scrolling the screen viewing window

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
  (with-fields (window-x window-y width height) self
    (let ((margin-x (* 1/3 *gl-screen-width*))
	  (margin-y (* 1/3 *gl-screen-height*))
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
			      (min (- width *gl-screen-width*)
				   (- object-x 
				      (truncate (/ *gl-screen-width* 2)))))
			 (max 0 
			      (min (- height *gl-screen-height*)
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

(define-method initialize world ()
  (setf %variables (make-hash-table :test 'equal))
  (setf %objects (make-hash-table :test 'equal)))
  
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

;;; The object layer. 

(define-method add-block world (object &optional x y append)
  (with-fields (quadtree) self
    (let ((*quadtree* quadtree))
      (assert (not (gethash (find-uuid object) 
			    %objects)))
      (setf (gethash (find-uuid object)
		     %objects)
	    t)
      (when (and (numberp x) (numberp y))
	(setf (field-value :x object) x
	      (field-value :y object) y))
      (clear-saved-location object)
      (when quadtree
	(quadtree-insert quadtree object)))))

(define-method drop-block world (block x y)
  (add-block self block)
  (move-to block x y))
      
(define-method remove-block world (object)
  (remhash (find-uuid object) %objects)
  (when (field-value :quadtree-node object)
    (quadtree-delete %quadtree object)))

(define-method discard-block world (object)
  (remhash (find-uuid object) %objects))

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

;;; About the player
			        
(define-method get-player world ()
  %player)

(defun player ()
  (get-player *world*))

(define-method add-player world (player)
  "Set PLAYER as the player object to which the World will forward
most user command messages. (See also the method `forward'.)"
  (setf %player player)
  (add-block self player))

;;; Configuring the world's space and its quadtrees

(defparameter *world-bounding-box-scale* 1.01)

;; see also quadtree.lisp 

(define-method resize world (new-height new-width)
  (assert (and (plusp new-height)
	       (plusp new-width)))
  (with-fields (height width quadtree objects) self
    (setf height new-height)
    (setf width new-width)
    (setf quadtree 
	  (build-quadtree 
	   (multiple-value-list 
	    (scale-bounding-box 
	     (multiple-value-list 
	      (bounding-box self))
	     *world-bounding-box-scale*))))
    (quadtree-fill quadtree objects)))

(define-method resize-automatically world ()
  (let ((objects (get-objects self)))
    (with-fields (quadtree) self
      (setf quadtree
	    (if (null objects)
		(build-quadtree (multiple-value-list (bounding-box self)))
		;; adjust bounding box so that all objects have positive coordinates
		(multiple-value-bind (top left right bottom)
		    (find-bounding-box objects)
		  ;; update world dimensions
		  (setf height (- bottom top))
		  (setf width (- right left))
		  ;; move all the objects
		  (dolist (object objects)
		    (with-fields (x y) object
		      (decf x left)
		      (decf y top)))
		  ;; make a quadtree with a one-percent margin on all sides
		  (build-quadtree 
		   (multiple-value-list 
		    (scale-bounding-box 
		     (multiple-value-list (find-bounding-box objects))
		     *world-bounding-box-scale*))))))
      (when objects
	(quadtree-fill quadtree objects)))))

;; Algebraic operations on worlds and their contents

(defun combine-worlds (&rest worlds)
  (when worlds
    (let ((world (new world)))
      (prog1 world 
	(dolist (object (mapcan #'get-objects worlds))
	  (add-block world object))
	(resize-automatically world)))))

(defun stack-worlds (&rest worlds)
  (when worlds
    (let ((new-world (new world)))
      (prog1 new-world
	(let ((y0 0))
	  (dolist (world worlds)
	    (let ((objects (get-objects world)))
	      (multiple-value-bind (top left right bottom)
		  (find-bounding-box objects)
		(dolist (object objects)
		  (with-fields (x y) object
		    (add-block new-world object x (+ y y0))))
		(incf y0 bottom)))))
	(resize-automatically new-world)))))

;;; Draw the world

(define-method draw world ()
;  (declare (optimize (speed 3)))
  (project self) ;; set up camera
  (with-field-values (objects width height background background-color) self
    ;; draw background image (or color)
    (if background
	(draw-image background 0 0)
	(when background-color
	  (draw-box 0 0 width height
		    :color background-color)))
    ;; draw the objects
    (loop for object being the hash-keys in %objects do
      (draw object))))
;    (quadtree-show %quadtree %player)))

;;; Simulation update

(define-method update world (&rest args)
  (declare (optimize (speed 3)))
  (declare (ignore args))
    (with-field-values (objects height width player) self
      (when (null %quadtree)
	(resize-automatically self))
      (let ((*quadtree* %quadtree))
	(assert (zerop *quadtree-depth*))
	;; run the objects
	(loop for object being the hash-keys in %objects do
	  (update object)
	  (run-tasks object))
	;; update window movement
	(glide-follow self player)
	(update-window-glide self)
	;; detect collisions
	(loop for object being the hash-keys in %objects do
	  (quadtree-collide *quadtree* object)))))

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
  
;; (defparameter *default-grid-depth* 4)

;; (define-method create-grid world (&key grid-width grid-height)
;;   "Initialize all the arrays for a world of GRID-WIDTH by GRID-HEIGHT cells."
;;   (let ((dims (list grid-height grid-width)))
;;     (let ((grid (make-array dims 
;; 		 :element-type 'vector :adjustable nil))
;; 	  (light-grid (make-array dims :element-type 'integer)))
;;       (dotimes (i grid-height)
;; 	(dotimes (j grid-width)
;; 	  ;; now put a vector in each square to represent the z-axis
;; 	  (setf (aref grid i j)
;; 		(make-array *default-grid-depth* 
;; 			    :adjustable t
;; 			    :fill-pointer 0))))
;;       (setf %grid grid
;; 	    %grid-height grid-height
;; 	    %grid-width grid-width))))

;;; Pasting operations. This section needs work. 

;; (define-method paste-region world (other-world dest-row dest-column source-row source-column source-grid-height source-grid-width 
;; 					       &optional deepcopy)
;;     (loop for row from 0 to source-grid-height
;; 	  do (loop for column from 0 to source-grid-width
;; 		   do (let* ((cells (grid-location other-world (+ row source-row) (+ column source-column)))
;; 			     (n 0))
;; 			(when (vectorp cells)
;; 			  (loop while (< n (fill-pointer cells)) do
;; 			    (let* ((cell (aref cells n))
;; 				   (proto (object-parent cell))
;; 				   (new-cell (if (or deepcopy (field-value :auto-deepcopy cell))
;; 						 ;; create a distinct object with the same local field values.
;; 						 (deserialize (serialize cell))
;; 						 ;; create a similar object
;; 						 (clone proto))))
;; 			      (drop-cell self new-cell (+ row dest-row) (+ column dest-column) :exclusive nil))
;; 			    (incf n)))))))

;; (define-method clone-onto world (other-world &optional deepcopy)
;;   (let ((other (etypecase other-world
;; 		 (string (find-resource-object other-world))
;; 		 (object other-world))))
;;     (with-fields (grid-height grid-width) other
;;       (create-grid self :grid-height grid-height :grid-width grid-width)
;;       (let ((*world* other))
;; 	(paste-region self other 0 0 0 0 grid-height grid-width deepcopy)))))

;; (define-method begin-ambient-loop world ()
;;   "Begin looping your music for this world here."
;;   nil)

;; (define-method after-start-method world ()
;;   nil)

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
;; 	(:object (drop-object self player 
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
