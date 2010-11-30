;;; viewport.lisp --- tile engine display widget

;; Copyright (C) 2009  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A world may have one or more viewport widgets. In a standard
;; viewport, cells are represented onscreen by uniformly-sized
;; graphical tiles; worlds are viewed from a bird's-eye
;; perspective. Other viewports may render schematic or compressed
;; views of the world (for example, an auto-map display.)

;;; Code:

(in-package :gluon)

(defstruct overlay func parameters clock)

(define-prototype viewport 
    (:parent =widget= :documentation "A map display for GLUON worlds.")
  (world :documentation "The world object to be displayed.")
  (overlays :documentation "List of closures.")
  (use-overlays :initform t)
  (pending-draws :initform (make-array 100 :initial-element nil 
				       :adjustable t :fill-pointer 0))
  (margin :initform 15 :documentation "Scroll margin.")
  (origin-x :initform 0 
	    :documentation "The world x-coordinate of the tile at the viewport's origin.")
  (origin-y :initform 0 
	    :documentation "The world y-coordinate of the tile at the viewport's origin.")
  (origin-width :initform 10 :documentation "The width in tiles of the viewport.")
  (origin-height :initform 10 :documentation "The height in tiles of the viewport.")
  ;; pixel-perfect scrolling "drag"
  (drag-x :initform 0)
  (drag-y :initform 0)
  (tile-size :initform 16 :documentation "Size in pixels of a tile. They must be square.")
  (excluded-fields :initform '(:world)))

(define-method get-viewport-coordinates viewport (cell-row cell-column)
  (let ((size <tile-size>))
    (let ((x0 (* size cell-column))
	  (y0 (* size cell-row)))
      (values x0 y0))))

(define-method get-image-coordinates viewport (cell-row cell-column)
  (let ((size <tile-size>))
    (let ((x0 (* size (- cell-column <origin-x>)))
	  (y0 (* size (- cell-row <origin-y>))))
      (values x0 y0))))

(define-method get-screen-coordinates viewport (cell-row cell-column)
  (let ((size <tile-size>))
    (let ((x0 (+ (* size (- cell-column <origin-x>)) <x>))
	  (y0 (+ (* size (- cell-row <origin-y>)) <y>)))
      (values x0 y0))))

;; (define-method get-viewport-coordinates viewport (cell-row cell-column)
;;   (let ((size <tile-size>))
;;     (let ((x0 (* size (- cell-column <origin-x>)))
;; 	  (y0 (* size (- cell-row <origin-y>))))
;;       (values x0 y0))))

(define-method get-viewport-coordinates-* viewport (x y)
  (let* ((size <tile-size>)
	 (x0 (* <origin-x> size))
	 (y0 (* <origin-y> size)))
    (values (- x x0) (- y y0))))

(define-method add-overlay viewport (overlay)
  (pushnew overlay <overlays>))

(define-method draw-overlays viewport ()
  (when <use-overlays>
    ;; draw, removing any overlay that returns non-nil
    (let ((image <image>))
      (setf <overlays> 
	    (remove-if #'(lambda (ov)
			   (funcall ov image))
		       <overlays>)))))

(define-method set-world viewport (world)
  (setf <world> world))

(define-method set-tile-size viewport (&optional size)
  (with-fields (tile-size origin-width origin-height) self
    (setf tile-size (or size (field-value :tile-size <world>)))))
    
(define-method update-geometry viewport (&optional resize)    
  (with-field-values (tile-size origin-width origin-height width height) self
    (let ((new-width (* tile-size origin-width))
	  (new-height (* tile-size origin-height)))
      (unless (and (= new-width width)
		   (= new-height height))
	(when resize (/resize self :height new-height :width new-width))))))

(define-method render viewport ()
;;  (declare (optimize (speed 3)))
  (when <visible>
    (/adjust self) 
    (let* ((world (or <world> *world*))
           (origin-width <origin-width>)
           (origin-height <origin-height>)
           (origin-x <origin-x>)
           (origin-y <origin-y>)
	   (drag-x <drag-x>)
	   (drag-y <drag-y>)
           (pending-draws <pending-draws>)
           (image <image>)
           (tile nil)
           (tile-size <tile-size>)
           objects cell)
      (setf (fill-pointer pending-draws) 0)
      (with-field-values (grid light-grid environment-grid phase-number
                               height width sprites background
                               turn-number ambient-light) world
        ;; blank the display
        (/clear self)
	;; draw any background
	(when (stringp background)
	  (let ((surface (find-resource-object background))
		(x-offset (* tile-size origin-x))
		(y-offset (* tile-size origin-y))
		(width (* tile-size origin-width))
		(height (* tile-size origin-height)))
	    (draw-resource-image background 0 0 
				 :render-cell (list (+ x-offset drag-x)
						    (+ y-offset drag-y)
						    width height) 
				 :destination image)))
        ;; draw the tiles
        (dotimes (i origin-height)
          (dotimes (j origin-width)
            ;; is this square lit? 
            (if (and (array-in-bounds-p grid (+ i origin-y) (+ j origin-x))
                     (or (eq :total ambient-light)
                         (= 1 (aref light-grid (+ i origin-y) (+ j origin-x)))))
                (progn 
                  (setf objects (aref grid 
                                      (+ i origin-y)
                                      (+ j origin-x)))
                  (dotimes (k (fill-pointer objects))
                    (setf cell (aref objects k))
                    (when (object-p cell)
                      (let ((j0 (+ (- drag-x) (* j tile-size)))
                            (i0 (+ (- drag-y) (* i tile-size))))
			(setf tile (field-value :tile cell))
			(if (or (member :drawn (field-value :categories cell))
				(null tile))
			    (vector-push-extend cell pending-draws)
			    (when tile 
			      (draw-resource-image tile j0 i0 
						   :render-cell (field-value :render-cell cell) 
						   :destination image)))))))
	      ;; not in bounds, or not lit; draw blackness
	      (draw-resource-image ".blackness" (* j tile-size) (* i tile-size)
				   :destination image))))
        ;; draw the sprites
        (dolist (sprite sprites)
          ;; pull image and calculate screen coordinates
          (let* ((graphics (field-value :image sprite))
                 (x0 (field-value :x sprite))
                 (x1 (+ (- drag-x) (- x0 (* tile-size origin-x))))
                 (y0 (field-value :y sprite))
                 (y1 (+ (- drag-y) (- y0 (* tile-size origin-y)))))
            (when graphics (draw-resource-image graphics x1 y1 :destination image))))
        ;; draw the pending ops
        (map nil #'(lambda (cell)
                     (multiple-value-bind (x y) (/image-coordinates cell)
                       (/draw cell x y image)))
             pending-draws)
	(/update-geometry self)
        ;; draw the overlays
        (/draw-overlays self)))))

(define-method hit viewport (x y)
  (with-fields (origin-x origin-y tile-size) self
    (when (send-parent self :hit self x y)
      (let* ((x0 (- x <x>))
	     (y0 (- y <y>))
	     (r (truncate (/ y0 tile-size)))
	     (c (truncate (/ x0 tile-size)))
	     (r0 (+ origin-y r))
	     (c0 (+ origin-x c))
	     (y1 (* tile-size origin-y))
	     (x1 (* tile-size origin-x))
	     (grid (field-value :grid (or <world> *world*)))
	     (cells (when (array-in-bounds-p grid r0 c0)
		      (aref grid r0 c0))))
      (labels ((hit (sprite)
		 (multiple-value-bind (sx sy) (/xy-coordinates sprite)
		   (let* ((im (field-value :image sprite))
			  (h (image-height im))
			  (w (image-width im)))
		     (when (and (<= sx (+ x0 x1) (+ sx w))
				(<= sy (+ y0 y1) (+ sy h)))
		       sprite)))))
	(assert *world*)
	(or (some #'hit (field-value :sprites *world*))
	    (when cells (aref cells (1- (fill-pointer cells))))))))))

(define-method set-origin viewport (&key x y height width)
  (setf <origin-x> x
	<origin-y> y
	<origin-width> width
	<origin-height> height))

(define-method adjust viewport (&optional snap)
  "Move the viewport's origin if required to keep the player onscreen."
  (with-fields (drag-x drag-y tile-size) self
    (let* ((world (or <world> *world*))
	   (world-width (field-value :width world))
	   (world-height (field-value :height world))
	   (player (field-value :player world))
	   (player-x (/player-column world))
	   (player-y (/player-row world))
	   (margin <margin>))
      (with-fields (origin-x origin-y origin-height origin-width) self
	;; are we outside the "comfort zone"?
	(if (or 
	       ;; too far left
	       (> (+ origin-x margin) 
		  player-x)
	       ;; too far right
	       (> player-x
		  (- (+ origin-x origin-width)
		     margin))
	       ;; too far up
	       (> (+ origin-y margin) 
		  player-y)
	       ;; too far down 
	       (> player-y 
		  (- (+ origin-y origin-height)
		     margin)))
	    ;; yes. recenter.
	    (let ((new-x (max 0
			      (min (- world-width origin-width)
				   (- player-x 
				      (truncate (/ origin-width 2))))))
		  (new-y (max 0 
			      (min (- world-height origin-height)
				   (- player-y 
				      (truncate (/ origin-height 2)))))))
	      (if snap
		  ;; either abruptly
		  (setf origin-x new-x origin-y new-y)
		  ;; otherwise drag
		  (let ((dx (if (> new-x origin-x) 1 -1))
			(dy (if (> new-y origin-y) 1 -1)))
		    (when (not (= origin-x new-x))
		      (incf drag-x (* 2 dx))
		      (when (= tile-size (abs drag-x))
			(setf drag-x 0))
		      (when (zerop drag-x)
			  (incf origin-x dx)))
		    (when (not (= origin-y new-y))
		      (incf drag-y (* 2 dy))
		      (when (= tile-size (abs drag-y))
			(setf drag-y 0))
		      (when (zerop drag-y)
			(incf origin-y dy)))))))))))

;;; The minimap

(define-prototype minimap (:parent =viewport=)
  (category-map :initform '((:hidden ".black")
			    (:player ".white")
			    (:item ".gold")
			    (:boss ".yellow")
			    (:enemy ".red")
			    (:target ".blue")
			    (:friend ".green")
			    (:obstacle ".gray20"))) ;;; TODO ?
  (background-color :initform ".black")
  (border-color :initform ".gray20"))
		
(define-method render minimap ()
  (when <visible>
    (/adjust self) ;; hehe
    (let* ((world (or <world> *world*))
           (origin-width <origin-width>)
           (origin-height <origin-height>)
           (origin-x <origin-x>)
           (origin-y <origin-y>)
           (tile-size <tile-size>)
           (category-map <category-map>)
           (grid (field-value :grid world))
           (image <image>)
           objects 
           cell
           categories)
      (with-field-values (grid light-grid environment-grid phase-number
                               height width 
                               turn-number ambient-light) world
        ;; blank the display
        (/clear self)
        ;; ;; draw the border
        ;; (draw-rectangle 0 0 <width> <height>
        ;; 		      :color <border-color>
        ;; 		      :destination <image>)
        ;; draw the minimap
        (dotimes (i origin-height)
          (dotimes (j origin-width)
            (when (array-in-bounds-p grid i j)
              (setf objects (aref grid 
                                  (+ i origin-y)
                                  (+ j origin-x)))
              (block coloring
                (dolist (mapping category-map)
                  (destructuring-bind (category color) mapping
                    (dotimes (k (fill-pointer objects))
                      (setf cell (aref objects k))
                      (setf categories (field-value :categories cell))
                      ;; record location of player-entry-point if any
                      (when (member :player-entry-point categories)
                        (draw-circle (* tile-size (- j origin-x))
                                     (* tile-size (- i origin-y))
                                     4 
                                     :destination image 
                                     :color ".yellow"))
                      (when (member category categories)
                        (gluon:draw-box (* tile-size j) 
                                      (* tile-size i)
                                      tile-size tile-size
                                      :destination image 
                                      :stroke-color color
                                      :color color)
                        (return-from coloring)))))))))
        ;; draw player indicator
        (draw-circle (* tile-size (- (/player-column world) origin-x))
                     (* tile-size (- (/player-row world) origin-y))
                     4 
                     :destination image :color ".white")))))

(defparameter *minimap-help-string* 
"This is the minimap, a form of radar.
Red dots represent enemies; the player's location is indicated in
white.")

(define-method select minimap ()
  (dolist (line (split-string-on-lines *minimap-help-string*))
    (/>>narrateln :narrator line)))

(define-method hit minimap (x y)
  (when (send-parent self :hit self x y)
    self))
  
	
		  


;;; viewport.lisp ends here
