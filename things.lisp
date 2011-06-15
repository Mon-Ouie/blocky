;;; things.lisp --- defining game objects 

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

;;; Code:

(in-package :ioforms)

(defblock cell 
  (type :initform :cell)
  (row :documentation "When non-nil, the current row location of the cell.")
  (column :documentation "When non-nil, the current column of the cell.")
  (name :initform nil :documentation "The name of this cell.")
  (description :initform nil :documentation "A description of the cell.") 
  (categories :initform nil :documentation "List of category keyword symbols."))

;;; Cell categories

(define-method in-category cell (category)
  "Return non-nil if this cell is in the specified CATEGORY.

Cells may be placed into categories that influence their processing by
the engine. The field `^categories' is a set of keyword symbols; if a
symbol `:foo' is in the list, then the cell is in the category `:foo'.

Although a game built on IOFORMS can define whatever categories are
needed, certain base categories are built-in and have a fixed
interpretation:

 -    :obstacle --- Blocks movement and causes collisions
 -    :temporary --- This cell is not preserved when exiting a world.
 -    :light-source --- This object casts light. 
 -    :opaque --- Blocks line-of-sight, casts shadows. 
"
  (member category ^categories))

(define-method add-category cell (category)
  "Add this cell to the specified CATEGORY."
  (pushnew category ^categories))

(define-method delete-category cell (category)
  "Remove this cell from the specified CATEGORY."
  (setf ^categories (remove category ^categories)))

;;; Locating the cell in grid space

(define-method is-grid-located cell ()
  "Returns non-nil if this cell is located somewhere on the grid."
  (and (integerp ^row) (integerp ^column)))

(define-method grid-coordinates cell ()
  (values ^row ^column))

(define-method xy-coordinates cell ()
  (let ((size (field-value :grid-size *world*)))
    (values (* ^column size)
	    (* ^row size))))

(define-method coordinates cell ()
  (multiple-value-bind (x y) (xy-coordinates self)
    (values x y 0)))

(define-method draw cell ()
  (with-fields (image) self
    (when image
      (set-blending-mode ^blend)
      (multiple-value-bind (x y)
	  (xy-coordinates self)
	(draw-image image x y)))))

;;; Locating the player 

(define-method player-direction cell ()
  "Return the general compass direction of the player from X0,Y0."
  (with-fields (player) *world*
    (multiple-value-bind (x0 y0) (xy-coordinates self)
      (multiple-value-bind (x1 y1) (xy-coordinates player)
	(direction-to y0 x0 y1 x1)))))

(define-method player-distance cell ()
  "Return the straight line distance of the player from X0,Y0."
  (with-fields (player) *world*
    (multiple-value-bind (x0 y0) (xy-coordinates self)
      (multiple-value-bind (x1 y1) (xy-coordinates player)
	(distance x0 y0 x1 y1)))))

;;; Convenience macro for defining cells.

(defmacro defcell (name &body args)
  "Define a cell named NAME, with the fields ARGS as in a normal
prototype declaration. This is a convenience macro for defining new
cells."
  `(define-prototype ,name (:parent =cell=)
     ,@args))

;;; Cell death

(define-method destroy cell ())

(define-method die cell ()
  (destroy self)
  (delete-cell *world* self ^row ^column))

;;; Cell movement

(define-method move-to-grid cell (r c)
  (delete-cell *world* self ^row ^column)
  (drop-cell *world* self r c))

(define-method move-to cell (x y &optional z)
  (assert (and (numberp x) (numberp y)))
  (with-field-values (grid-size) *world*
    (let ((nearest-row (round y grid-size))
	  (nearest-column (round x grid-size)))
      (move-to-grid self nearest-row nearest-column))))

(define-method move cell (direction &optional (distance 1) ignore-obstacles)
  (error "This move method needs rewriting."))
;;   "Move this cell one step in DIRECTION on the grid. If
;; IGNORE-OBSTACLES is non-nil, the move will occur even if an obstacle
;; is in the way. Returns non-nil if a move occurred."
;;   (let ((world *world*))
;;     (multiple-value-bind (r c) 
;; 	(step-in-direction ^row ^column direction distance)
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
;; 		 (move-cell world self r c))))))))

(define-method bounding-box cell ()
  (multiple-value-bind (x y)
      (xy-coordinates self)
    (let ((size (field-value :grid-size *world*)))
      (values x y size size))))

(define-method collide cell (object)
  (declare (ignore object))
  "Respond to a collision detected with OBJECT."
  nil)

;;; Sprites

(defcell sprite 
  (type :initform :sprite)
  (height :initform nil :documentation "The cached width of the bounding box.")
  (width :initform nil :documentation "The cached height of the bounding box."))

;; Convenience macro for defining sprites

(defmacro defsprite (name &body args)
  `(define-prototype ,name (:parent =sprite=)
     ,@args))

(defun is-sprite (ob)
  (when (eq :sprite (field-value :type ob))))

(defun is-cell (ob)
  (when (eq :cell (field-value :type ob))))

(define-method update-image-dimensions sprite ()
  (with-fields (image height width) self
    (when image
      (setf width (image-width image))
      (setf height (image-height image)))))

;; (define-method initialize sprite ()
;;   (parent/initialize self))
 
(define-method draw sprite ()
  (set-blending-mode ^blend)
  (with-fields (image x y height) self
    (when image
      (when (null height)
	(update-image-dimensions self))
      (draw-image image x y))))

(define-method set-image sprite (image)
  (assert (stringp image))
  (setf ^image image)
  (update-image-dimensions self))

(define-method die sprite ()
  (remove-sprite *world* self))

(defparameter *sprite-context-menu*
  '((:label "Inspect" :action :inspect)
    (:label "Create reference" :action :create-reference)
    (:label "Destroy" :action :destroy)
    (:label "Make a copy" :action :copy)))

(define-method create-reference sprite ()
  (with-fields (x y) self
      (add *script* (new sprite-id self) x y)))

(define-method context-menu sprite ()
  (let ((label (format nil "Sprite menu for ~A" 
		       (get-some-object-name self))))
    (new menu :label label
	 :expanded t
	      :inputs (make-menu *sprite-context-menu* self))))
		    
;;; Sprite locations

(define-method grid-coordinates sprite ()
  (values (truncate (/ ^y (field-value :tile-size *world*)))
	  (truncate (/ ^x (field-value :tile-size *world*)))))

(define-method xy-coordinates sprite ()
  (values ^x ^y))

(define-method coordinates sprite ()
  (values ^x ^y ^z))

;;; Layout

(define-method layout sprite ()
  (with-fields (height width image) self
    (setf height (image-height image))
    (setf width (image-width image))))

(define-method draw-highlight sprite ())
(define-method draw-hover sprite ())
(define-method draw-border sprite ())

;;; Sprite movement

(define-method move-to sprite (x y &optional z)
  (assert (and (numberp x) (numberp y)))
  (setf ^x x ^y y)
  (when (numberp z)
    (assert (numberp z))
    (setf ^z z)))

(define-method move-to-grid sprite (row column)
  (with-field-values (grid-size) *world*
    (move-to self (* grid-size row) (* grid-size column))))

(define-method move sprite (direction &optional (distance 1) force)
  (assert (member direction *compass-directions*))
  (with-field-values (x y) self
    (multiple-value-bind (y0 x0) 
	(ioforms:step-in-direction y x direction distance)
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

(define-method bounding-box sprite ()
  (when (null ^height)
    (update-image-dimensions self))
  (values ^x ^y ^width ^height))

(define-method colliding-with-rectangle sprite (o-top o-left o-width o-height)
  ;; you must pass arguments in Y X order since this is TOP then LEFT
  (with-fields (x y width height) self
    (point-in-rectangle-p x y width height o-top o-left o-width o-height)))

;; (define-method colliding-with-point 

(define-method colliding-with sprite (thing)
  (multiple-value-bind (x y width height) 
      (bounding-box thing)
    (colliding-with-rectangle self y x width height)))

(define-method collide sprite (thing))

;;; Object dropping

(define-method drop sprite (thing &optional (delta-x 0) (delta-y 0))
;;  (assert (is-sprite thing))
  (with-field-values (x y) self
    (drop-sprite *world* thing (+ x delta-x) (+ y delta-y))))

;;; Playing a sound

(define-method play-sound cell (sample-name)
  (play-sample sample-name))

;;; Temporary text balloons

(defun seconds->frames (seconds)
  (truncate (* seconds ioforms:*frame-rate*)))

(defsprite balloon 
  :text "..."
  :clock (seconds->frames 5))

(define-method initialize balloon (string &optional (seconds 5.0))
  (with-fields (text clock) self
    (setf clock (seconds->frames seconds))
    (setf text string)
    (multiple-value-bind (width height) 
	(font-text-extents string *block-font*)
      (setf ^width width)
      (setf ^height height))))

(define-method draw balloon ()
  (with-fields (x y clock text) self
    (decf clock)
    (if (plusp clock)
	(ioforms:draw-string text x y :font *block-font* :color "black")
	(remove-sprite *world* self))))


;;; things.lisp ends here
