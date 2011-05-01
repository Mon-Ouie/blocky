;;; cells.lisp --- defining objects

;; Copyright (C) 2008, 2009, 2010, 2011  David O'Toole

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

;;; Code:

(in-package :ioforms)

;;; Base cell prototype

;; This is a base object for forms-browseable objects. See forms.lisp

(define-prototype cell ()
  (row :documentation "When non-nil, the current row location of the cell.")
  (column :documentation "When non-nil, the current column of the cell.")
  (type :initform :cell)
  (name :initform nil :documentation "The name of this cell.")
  (description :initform nil :documentation "A description of the cell.") 
  (categories :initform nil :documentation "List of category keyword symbols.") 
  (label :initform nil :documentation "Optional string or formatted line to display.")
  (widget :initform nil)
  (image :initform nil :documentation "Image to display. either a resource name string, or an IOFORMS image object.")
  (tile :initform ".asterisk" :documentation "Resource name of image. 
When nil, the method DRAW is invoked instead of using a tile.")
  (render-cell :initform nil :documentation "Subcell to render. See load-sprite-sheet-resource.")
  (auto-loadout :initform nil :documentation "When non-nil, the :loadout method is invoked upon entry into a world.")
  (auto-deepcopy :initform nil)
  (menu :initform nil :documentation "Menu objects."))
  
(define-method in-category cell (category)
  (member category <categories>))

(defparameter *default-cell-width* 16)

(define-method get cell ())

(define-method activate cell ())

(define-method set cell (data))

(define-method print cell () "")

(define-method width cell () 
  (with-field-values (widget image label) self
    (cond (widget (image-width (field-value :image widget)))
	  (image (image-width image))
	  (label (formatted-line-width label))
	  (t *default-cell-width*))))

(define-method height cell () 
  (with-field-values (widget image label) self
    (cond (widget (image-height (field-value :image widget)))
	  (image (image-height image))
	  (label (formatted-line-height label))
	  (t *default-cell-width*))))

(define-method render cell (dest x y width)
  (with-field-values (widget image) self
      (cond (widget 
	     (render widget)
	     (draw-image (field-value :image widget)
			 x y :destination dest))
	    ;; it's an image
	    (image 
	     (if (stringp image)
		 (draw-resource-image image x y :destination dest)
		 (draw-image image x y :destination dest)))
	    (<label>
	     ;; we have a formatted line
	     (let ((label <label>))
	       (when (listp label)
		 (let*
		     ((shortfall (- width (formatted-line-width label)))
		      (color (or (when (and (listp label)
					    (listp (last label)))
				   (getf (cdr (car (last label))) :background))
				 ".black"))
		      (spacer (when (plusp shortfall) 
				(list nil :width shortfall :background color)))
		      (line (if spacer (append label (list spacer))
				      label)))
		   (render-formatted-line line x y :destination dest))))))))

(defvar *default-cell-label* '((" DEF ")))

(define-method get-label cell ()
  (when label 
    (etypecase label
      (string (list (list label)))
      (list label))))

(define-method set-location cell (r c)
  "Set the row R and column C of the cell."
  (setf <row> r <column> c))

(define-method is-located cell ()
  "Returns non-nil if this cell is located somewhere on the grid."
  (or (and (integerp <row>) (integerp <column>))))

(define-method dislocate cell ()
  "Remove any location data from the cell."
  (when (integerp <row>)
    (setf <row> nil <column> nil))
  (when (integerp <x>)
    (setf <x> nil <y> nil)))

(define-method viewport-coordinates cell ()
  "Return as values X,Y the world coordinates of CELL."
  (assert (and <row> <column>))
  (get-viewport-coordinates (field-value :viewport *world*)
                            <row> <column>))

(define-method image-coordinates cell ()
  "Return as values X,Y the viewport image coordinates of CELL."
  (assert (and <row> <column>))
  (get-image-coordinates (field-value :viewport *world*)
                         <row> <column>))

(define-method screen-coordinates cell ()
  "Return as values X,Y the screen coordinates of CELL."
  (assert (and <row> <column>))
  (get-screen-coordinates (field-value :viewport *world*)
			  <row> <column>))

;;; Cell categories

(define-method in-category cell (category) 
  "Return non-nil if this cell is in the specified CATEGORY.

Cells may be placed into categories that influence their processing by
the engine. The field `<categories>' is a set of keyword symbols; if a
symbol `:foo' is in the list, then the cell is in the category `:foo'.

Although a game built on IOFORMS can define whatever categories are
needed, certain base categories are built-in and have a fixed
interpretation:

 -    :actor --- This cell is active and may be controlled by either the
      user or the CPU. Only actor cells receive `:run' messages
      every turn. Other cells are purely `reactive'. Actor
      cells participate in the Action Points system.
 -    :target --- This cell is susceptible to targeting.
 -    :proxy --- This cell is a proxy for another cell.
 -    :drawn --- This cell has a (draw) method used for custom drawing.
 -    :proxied  --- This cell is an occupant of a proxy.
 -    :dead --- This cell is no longer receiving run messages.
 -    :player --- Only one cell (your player avatar) has this category.
 -    :enemy --- This cell is playing against you.
 -    :exclusive --- Prevent some objects from stacking. See also the method `drop-cell' in worlds.lisp
 -    :obstacle --- Blocks movement and causes collisions
 -    :pushable --- Can be pushed by impacts.
 -    :ephemeral --- This cell is not preserved when exiting a world.
 -    :combining --- This cell automatically combines units with other cells in a container.
 -    :light-source --- This object casts light. 
 -    :opaque --- Blocks line-of-sight, casts shadows. 
 -    :container --- This cell contains other cells, and has an <inventory> field
 -    :contained ---  This cell is contained in another cell (i.e. not in open space on the map)
 -    :item --- A potential inventory item. 
 -    :equipper --- Uses equipment. 
 -    :equipped --- This item is currently equipped.
 -    :equipment --- This item can be equipped. 
"
  (member category <categories>))

(define-method add-category cell (category)
  "Add this cell to the specified CATEGORY."
  (pushnew category <categories>))

(define-method delete-category cell (category)
  "Remove this cell from the specified CATEGORY."
  (setf <categories> (remove category <categories>)))

;;; Player orientation

(define-method distance-to-player cell ()
  "Calculate the distance from the current location to the player."
  ;; todo fix for sprites
  (multiple-value-bind (row column) (grid-coordinates self)
    (distance-to-player *world* row column)))

(define-method direction-to-player cell ()
  "Calculate the general compass direction of the player."
  (direction-to-player *world* <row> <column>))

(define-method adjacent-to-player cell ()
  (adjacent-to-player *world* <row> <column>))

;;; Convenience macro for defining cells.

(defmacro defcell (name &body args)
  "Define a cell named NAME, with the fields ARGS as in a normal
prototype declaration. This is a convenience macro for defining new
cells."
  `(define-prototype ,name (:parent =cell=)
     ,@args))

;;; Cell movement

(define-method move cell (direction &optional (distance 1) unit)
  "Move this cell one step in DIRECTION on the grid. If
IGNORE-OBSTACLES is non-nil, the move will occur even if an obstacle
is in the way. Returns non-nil if a move occurred."
  (declare (ignore unit))
  (let ((world *world*))
    (multiple-value-bind (r c) 
	(step-in-direction <row> <column> direction)
      ;; 
      (cond ((null (grid-location world r c)) ;; are we at the edge?
	     ;; return nil because we didn't move
	     (prog1 nil
	     ;; edge conditions only affect player for now
	       (when (is-player self)
		 (ecase (field-value :edge-condition world)
		   (:block (say self "You cannot move in that direction."))
		   (:wrap nil) ;; TODO implement this for planet maps
		   (:exit (exit *universe*))))))
	    (t
	     (when (or ignore-obstacles 
		       (not (obstacle-at-p *world* r c)))
	       ;; return t because we moved
	       (prog1 t
;;		 (expend-action-points self (stat-value self :movement-cost))
		 (move world self r c))))))))
		 ;; (when <stepping>
		 ;;   (step-on-current-square self)))))))))

(define-method set-location cell (r c)
  "Set the row R and column C of the cell."
  (setf <row> r <column> c))

(define-method move-to cell (unit r c)
  (assert (member unit '(:space :spaces)))
  (delete-cell *world* self <row> <column>)
  (drop-cell *world* self r c))

;;; Adding items to the world 

(define-method drop cell (cell &key loadout (exclusive nil))
  "Add CELL to the world at the current location. By default,
EXCLUSIVE is nil; this allows one to drop objects on top of oneself.
When LOADOUT is non-nil, call the :loadout method."
  (drop-cell *world* cell <row> <column> :loadout loadout :exclusive exclusive))

(define-method drop-sprite cell (sprite &optional x y)
  "Add SPRITE to the world at location X,Y."
  (multiple-value-bind (x0 y0)
      (xy-coordinates self)
    (let ((x1 (or x x0))
	  (y1 (or y y0)))
      (add-sprite *world* sprite)
      (assert (and x1 y1))
      (move-to sprite x1 y1))))

(define-method is-player cell ()
  (in-category self :player))

;;; Finding and manipulating objects

(define-method find-object cell (&key (direction :here) (index :top) category)
  (let ((world *world*))
    (multiple-value-bind (nrow ncol)
	(step-in-direction <row> <column> direction)
      (if (in-bounds-p world nrow ncol)
	  (let (cell)
	    (let* ((cells (grid-location world nrow ncol))
		   (index2 (cond 
			     ((not (null category))
				(setf cell (category-at-p world nrow ncol category))
				(position cell cells :test 'eq))
			     ((and (eq :top index) (eq :here direction))
			      ;; skip yourself and instead get the item you're standing on
			      (- (fill-pointer cells) 2))
			     ((eq :top index)
			      (- (fill-pointer cells) 1))
			     ((numberp index) 
			      (when (array-in-bounds-p cells index)
				index)))))
	      (message "INDEX2: ~A" index2)
	      (setf cell (aref cells index2))
	      (values cell nrow ncol index2)))))))

(define-method clear-location cell ()
  (setf <row> nil <column> nil))

(define-method delete-from-world cell ()
  (delete-cell *world* self <row> <column>))

(define-method quit cell ()
  "Leave the gameworld."
  (delete-from-world self))

;;; Custom rendering

(define-method draw cell (x y image)
  "Use IOFORMS drawing commands to render a presentation of this cell at
X, Y to the offscreen image IMAGE.  This method is invoked to draw a
cell when its TILE field is nil, or when it is in the
category :drawn. See also viewport.lisp." 
  nil)

;;; Loadout

;; Automatic inventory and equipment loadout for new cells.
;; See how this is used in worlds.lisp.

(define-method loadout cell ()
  "This is called for cells after being dropped in a world, with a
non-nil :loadout argument. It can also be triggered manually.  Use
`loadout' for things that have to be done while in a world. (During
your cell's normal PROTON `initialize' method, the cell will not be in a
world or have a location."
  nil)

(define-method start cell ()
  "This method is invoked on cells whenever a new world map is visited."
  nil)

(define-method exit cell ()
  "This method is invoked on a player cell when it leaves a world."
  nil)

;;; Collision; see also gsprites.lisp

(define-method do-collision cell (object)
  "Respond to a collision detected with OBJECT."
  nil)

(define-method grid-coordinates cell ()
  (values <row> <column>))

(define-method xy-coordinates cell ()
  (values (* <column> (field-value :tile-size *world*))
	  (* <row> (field-value :tile-size *world*))))



;;; cells.lisp ends here
