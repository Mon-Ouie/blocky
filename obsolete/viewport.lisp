;;; viewport.lisp --- world display widget

;; Copyright (C) 2009, 2010, 2011  David O'Toole

;; Author: David O'Toole ^dto@gnu.org
;; Keywords: games

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

;;; Commentary:

;; A world may have one or more viewport widgets. In a standard
;; viewport, cells are represented onscreen by uniformly-sized
;; graphical tiles; worlds are viewed from a bird's-eye
;; perspective. Other viewports may render schematic or compressed
;; views of the world (for example, an auto-map display.)

;;; Code:

(in-package :ioforms)

(defstruct overlay func parameters clock)

(defvar *default-grid-size* 16)

(defblock viewport 
  (world :documentation "The world object to be displayed.")
  (overlays :documentation "List of closures.")
  (use-overlays :initform t)
  (scrolling :initform t)
  (pending-draws :initform (make-array 100 :initial-element nil 
				       :adjustable t :fill-pointer 0))
  (margin :initform 15 :documentation "Scroll margin.")
  (window-x :initform 0 
	    :documentation "The world x-coordinate of the tile at the viewport's window.")
  (window-y :initform 0 
	    :documentation "The world y-coordinate of the tile at the viewport's window.")
  (window-width :initform 10 :documentation "The width in tiles of the viewport.")
  (window-height :initform 10 :documentation "The height in tiles of the viewport.")
  ;; pixel-perfect scrolling "drag"
  (drag-x :initform 0)
  (drag-y :initform 0)
  (grid-size :initform *default-grid-size* :documentation "Size in pixels of a tile. They must be square.")
  (excluded-fields :initform '(:world)))

(define-method initialize viewport (&key top left width height world grid-size)
;;  (parent/initialize self)
  (setf ^window-y top
	^window-x left 
	^width width
	^height height
	^world world
	^grid-size grid-size))

(define-method get-viewport-coordinates viewport (cell-row cell-column)
  (let ((size ^grid-size))
    (let ((x0 (* size cell-column))
	  (y0 (* size cell-row)))
      (values x0 y0))))

(define-method get-image-coordinates viewport (cell-row cell-column)
  (let ((size ^grid-size))
    (let ((x0 (* size (- cell-column ^window-x)))
	  (y0 (* size (- cell-row ^window-y))))
      (values x0 y0))))

(define-method get-screen-coordinates viewport (cell-row cell-column)
  (let ((size ^grid-size))
    (let ((x0 (+ (* size (- cell-column ^window-x)) ^x))
	  (y0 (+ (* size (- cell-row ^window-y)) ^y)))
      (values x0 y0))))

;; (define-method get-viewport-coordinates viewport (cell-row cell-column)
;;   (let ((size ^grid-size))
;;     (let ((x0 (* size (- cell-column ^window-x)))
;; 	  (y0 (* size (- cell-row ^window-y))))
;;       (values x0 y0))))

(define-method get-viewport-coordinates-* viewport (x y)
  (let* ((size ^grid-size)
	 (x0 (* ^window-x size))
	 (y0 (* ^window-y size)))
    (values (- x x0) (- y y0))))

(define-method add-overlay viewport (overlay)
  (pushnew overlay ^overlays))

(define-method draw-overlays viewport ()
  (when ^use-overlays
    ;; draw, removing any overlay that returns non-nil
    (let ((image ^image))
      (setf ^overlays 
	    (remove-if #'(lambda (ov)
			   (funcall ov image))
		       ^overlays)))))

(define-method set-world viewport (world)
  (setf ^world world))

(define-method set-scrolling viewport (flag)
  (setf ^scrolling flag))

(define-method set-grid-size viewport (&optional size)
  (with-fields (grid-size window-width window-height) self
    (setf grid-size (or size (field-value :grid-size ^world)))))
    
(define-method update-geometry viewport (&optional resize)    
  (with-field-values (grid-size window-width window-height width height) self
    (let ((new-width (* grid-size window-width))
	  (new-height (* grid-size window-height)))
      (unless (and (= new-width width)
		   (= new-height height))
	(when resize 
	  (setf ^window-height (truncate (/ new-height ^grid-size)))
	  (setf ^window-width (truncate (/ new-width ^grid-size)))
	  (resize-image self :height new-height :width new-width))))))

(define-method render viewport ()
;;(declare (optimize (speed 3)))
  (when ^visible
    (when ^scrolling (adjust self))
    (let* ((world (or ^world *world*))
           (window-width ^window-width)
           (window-height ^window-height)
           (window-x ^window-x)
           (window-y ^window-y)
	   (drag-x ^drag-x)
	   (drag-y ^drag-y)
           (pending-draws ^pending-draws)
           (image ^image)
           (tile nil)
           (grid-size ^grid-size)
           objects cell)
      (setf (fill-pointer pending-draws) 0)
      (with-field-values (grid light-grid environment-grid phase-number
                               height width sprites background
                               turn-number ambient-light) world
        ;; blank the display
        (clear self "white")
	;; draw any background
	(when (stringp background)
	  (let ((surface (find-resource-object background))
		(x-offset (* grid-size window-x))
		(y-offset (* grid-size window-y))
		(width (* grid-size window-width))
		(height (* grid-size window-height)))
	    (draw-resource-image background 0 0 
				 :render-cell (list (+ x-offset drag-x)
						    (+ y-offset drag-y)
						    width height) 
				 :destination image)))
        ;; draw the tiles
        (dotimes (i window-height)
          (dotimes (j window-width)
            ;; is this square lit? 
            (if (and (array-in-bounds-p grid (+ i window-y) (+ j window-x))
                     (or (eq :total ambient-light)
                         (= 1 (aref light-grid (+ i window-y) (+ j window-x)))))
                (progn 
                  (setf objects (aref grid 
                                      (+ i window-y)
                                      (+ j window-x)))
                  (dotimes (k (fill-pointer objects))
                    (setf cell (aref objects k))
                    (when (object-p cell)
                      (let ((j0 (+ (- drag-x) (* j grid-size)))
                            (i0 (+ (- drag-y) (* i grid-size))))
			(setf tile (field-value :tile cell))
			(if (or (member :drawn (field-value :categories cell))
				(null tile))
			    (vector-push-extend cell pending-draws)
			    (when tile 
			      (draw-resource-image tile j0 i0 
						   :render-cell (field-value :render-cell cell) 
						   :destination image)))))))
	      ;; not in bounds, or not lit; draw blackness
	      (draw-resource-image "blackness" (* j grid-size) (* i grid-size)
				   :destination image))))
        ;; draw the sprites
        (dolist (sprite sprites)
          ;; pull image and calculate screen coordinates
          (let* ((graphics (field-value :image sprite))
                 (x0 (field-value :x sprite))
                 (x1 (+ (- drag-x) (- x0 (* grid-size window-x))))
                 (y0 (field-value :y sprite))
                 (y1 (+ (- drag-y) (- y0 (* grid-size window-y)))))
            (when graphics (draw-resource-image graphics x1 y1 :destination image))))
        ;; draw the pending ops
        (map nil #'(lambda (cell)
                     (multiple-value-bind (x y) (image-coordinates cell)
                       (draw cell x y image)))
             pending-draws)
	(update-geometry self)
        ;; draw the overlays
        (draw-overlays self)))))

(define-method hit viewport (x y)
  (with-fields (window-x window-y grid-size world) self
    (when (send-parent :hit self x y)
      (let* ((x0 (- x ^x))
	     (y0 (- y ^y))
	     (r (truncate (/ y0 grid-size)))
	     (c (truncate (/ x0 grid-size)))
	     (r0 (+ window-y r))
	     (c0 (+ window-x c))
	     (y1 (* grid-size window-y))
	     (x1 (* grid-size window-x))
	     (grid (field-value :grid world))
	     (cells (when (array-in-bounds-p grid r0 c0)
		      (aref grid r0 c0))))
      (labels ((hit (sprite)
		 (multiple-value-bind (sx sy) (xy-coordinates sprite)
		   (let* ((im (field-value :image sprite))
			  (h (image-height im))
			  (w (image-width im)))
		     (when (and (<= sx (+ x0 x1) (+ sx w))
				(<= sy (+ y0 y1) (+ sy h)))
		       sprite)))))
	(or (some #'hit (field-value :sprites world))
	    (when (and cells (> (fill-pointer cells) 0))
		(aref cells (1- (fill-pointer cells))))))))))

(define-method set-window viewport (&key x y height width)
  (setf ^window-x x
	^window-y y
	^window-width width
	^window-height height))

(define-method adjust viewport (&optional snap)
  "Move the viewport's window if required to keep the player onscreen."
  (with-fields (drag-x drag-y grid-size) self
    (let* ((world (or ^world *world*))
	   (world-width (field-value :width world))
	   (world-height (field-value :height world))
	   (player (field-value :player world))
	   (player-x (player-column world))
	   (player-y (player-row world))
	   (margin ^margin))
      (with-fields (window-x window-y window-height window-width) self
	;; are we outside the "comfort zone"?
	(if (or 
	       ;; too far left
	       (> (+ window-x margin) 
		  player-x)
	       ;; too far right
	       (> player-x
		  (- (+ window-x window-width)
		     margin))
	       ;; too far up
	       (> (+ window-y margin) 
		  player-y)
	       ;; too far down 
	       (> player-y 
		  (- (+ window-y window-height)
		     margin)))
	    ;; yes. recenter.
	    (let ((new-x (max 0
			      (min (- world-width window-width)
				   (- player-x 
				      (truncate (/ window-width 2))))))
		  (new-y (max 0 
			      (min (- world-height window-height)
				   (- player-y 
				      (truncate (/ window-height 2)))))))
	      (if snap
		  ;; either abruptly
		  (setf window-x new-x window-y new-y)
		  ;; otherwise drag
		  (let ((dx (if (> new-x window-x) 1 -1))
			(dy (if (> new-y window-y) 1 -1)))
		    (when (not (= window-x new-x))
		      (incf drag-x (* 2 dx))
		      (when (= grid-size (abs drag-x))
			(setf drag-x 0))
		      (when (zerop drag-x)
			  (incf window-x dx)))
		    (when (not (= window-y new-y))
		      (incf drag-y (* 2 dy))
		      (when (= grid-size (abs drag-y))
			(setf drag-y 0))
		      (when (zerop drag-y)
			(incf window-y dy)))))))))))

;;; The minimap

(define-prototype minimap (:parent =viewport=)
  (category-map :initform '((:hidden "black")
			    (:player "white")
			    (:item "gold")
			    (:boss "yellow")
			    (:enemy "red")
			    (:target "blue")
			    (:friend "green")
			    (:obstacle "gray20"))) ;;; TODO ?
  (background-color :initform "black")
  (border-color :initform "gray20"))
		
(define-method render minimap ()
  (when ^visible
    (adjust self) ;; hehe
    (let* ((world (or ^world *world*))
           (window-width ^window-width)
           (window-height ^window-height)
           (window-x ^window-x)
           (window-y ^window-y)
           (grid-size ^grid-size)
           (category-map ^category-map)
           (grid (field-value :grid world))
           (image ^image)
           objects 
           cell
           categories)
      (with-field-values (grid light-grid environment-grid phase-number
                               height width 
                               turn-number ambient-light) world
        ;; blank the display
        (clear self)
        ;; ;; draw the border
        ;; (draw-rectangle 0 0 ^width ^height
        ;; 		      :color ^border-color
        ;; 		      :destination ^image)
        ;; draw the minimap
        (dotimes (i window-height)
          (dotimes (j window-width)
            (when (array-in-bounds-p grid i j)
              (setf objects (aref grid 
                                  (+ i window-y)
                                  (+ j window-x)))
              (block coloring
                (dolist (mapping category-map)
                  (destructuring-bind (category color) mapping
                    (dotimes (k (fill-pointer objects))
                      (setf cell (aref objects k))
                      (setf categories (field-value :categories cell))
                      ;; record location of player-entry-point if any
                      (when (member :player-entry-point categories)
                        (draw-circle (* grid-size (- j window-x))
                                     (* grid-size (- i window-y))
                                     4 
                                     :destination image 
                                     :color "yellow"))
                      (when (member category categories)
                        (ioforms:draw-box (* grid-size j) 
                                      (* grid-size i)
                                      grid-size grid-size
                                      :destination image 
                                      :stroke-color color
                                      :color color)
                        (return-from coloring)))))))))
        ;; draw player indicator
        (draw-circle (* grid-size (- (player-column world) window-x))
                     (* grid-size (- (player-row world) window-y))
                     4 
                     :destination image :color "white")))))

(defparameter *minimap-help-string* 
"This is the minimap, a form of radar.
Red dots represent enemies; the player's location is indicated in
white.")

(define-method select minimap ()
  (dolist (line (split-string-on-lines *minimap-help-string*))
    (>>narrateln :narrator line)))

(define-method hit minimap (x y)
  (when (send-parent :hit self x y)
    self))
  
	
		  


;;; viewport.lisp ends here
