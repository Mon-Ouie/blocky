;;; gcells.lisp --- defining objects

;; Copyright (C) 2008  David O'Toole

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

;;; Commentary:

;; "GCells" are CLON objects which represent all in-game entities;
;; player characters, enemies, weapons, items, walls and floors are
;; all different types of cells. Game play occurs in a
;; three-dimensional grid of cells called a "world" (see worlds.lisp).

;; Cells may be stacked along the z-axis, and may also contain other
;; cells. Cells interact by sending messages to one another (with
;; `send-queue'); these messages are queued and processed by the
;; world for delivery to their recipients. 

(in-package :gluon)

;;; "gCells" implement a 2D game engine.

(define-prototype gcell
    (:parent =cell=
     :documentation 
"`GCells' are interacting CLON game objects. Each cell represents some
in-game entity; player characters, enemies, weapons, items, walls and
floors are all different types of cells. Game play occurs in a
three-dimensional grid of cells called a World (see below).

Cells may be stacked along the z-axis, and may also contain other
cells. Cells interact by sending messages to one another and to other
objects in the environment; these messages are queued and processed by
the world for delivery to their recipients.

In cells.lisp you will find some basic roguelike logic built into
cells.

  - Basic features like name, description, and discovery.
  - Unified container, inventory, and equipment system.
  - Cells have an optional weight in kilograms, and the calculation
    recursively includes containers and equipment.
  - The `action points' system allocates game turns to different
    cells. 
  - Basic melee and ranged combat support.
  - Equipment slot system (i.e. `paper doll') not restricted to humanoid actors.
  - `Proxying', a feature used to implement drivable vehicles and/or demonic possession.
  - `Stats', for numeric-valued attributes susceptible to temporary
    and permanent effects (i.e. stat increases and drains, or
    encumbrance). Also supports setting minimum and maximum values,
    and keeping track of units (meters, kilograms.)
  - `Categories' allow arbitrary tagging of objects, with some
    categories having special interpretation by the engine.

These are in effect a basic set of mostly optional roleplaying
rules. By defining new prototypes based on cells, you can change the
rules and run the game the way you want.
Sprites are also based on cells. See `defsprite'.")
  (type :initform :cell)
  (auto-loadout :initform nil :documentation "When non-nil, the :loadout method is invoked upon entry into a world.")
  (auto-deepcopy :initform nil)
  (team :initform nil :documentation "Keyword symbol of team, if any.")
  (weight :documentation "Weight of the cell, in kilograms.")
  (widget :initform nil :documentation "GLUON widget object, if any.")
  (tile :initform ".asterisk" :documentation "Resource name of image. 
When nil, the method DRAW is invoked instead of using a tile.")
  (render-cell :initform nil :documentation "Subcell to render. See load-sprite-sheet-resource.")
  (row :documentation "When non-nil, the current row location of the cell.")
  (column :documentation "When non-nil, the current column of the cell.")
  ;; <: categories :>
  (categories :documentation "List of category keyword symbols") 
  ;; <: lighting :>
  (light-radius :initform 0 :documentation "Strength of light cast by this object.") 
  ;; <: action-points :>
  (menu :initform nil :documentation "Menu objects.")
  (speed :initform '(:base 10) :documentation "The number of action points alloted each phase.")
  (phase-number :initform 0
	       :documentation "An integer giving the last phase this cell has completed.")
  (action-points :initform 0
		 :documentation "An integer giving the ability of a cell to take turns on a given round.")
  (default-cost :initform '(:base 5 :min nil :max nil :delta nil)
    :documentation "Cost for basic actions.")
  (stepping :initform nil :documentation "Whether to generate step events where you walk.")
  (movement-cost :initform '(:base 10 :min nil :max nil :delta nil)
		 :documentation "Base cost of moving one square.")
  ;; <: knowledge :>
  (name :documentation "The name of this cell.")
  (description :documentation "A description of the cell.") 
  (unknown-name :documentation "The name of this cell, when it is unknown.")
  (unknown-description :documentation "A description of the cell, when it is unknown.")
  ;; <: equipment :>
  (equipment :documentation "Property list of :slot -> cell pairs.") 
  (equipment-slots :documentation "List of keyword symbols identifying available equipment slots."
		   :initform '(:head :neck :left-hand :right-hand :hands :feet :legs :torso :arms :pack :belt))
  (using-slot :documentation "Keyword symbol of the currently selected equipment slot.")
  (attacking-with :documentation "Keyword symbol of the currently selected weapon slot.")
  (firing-with :documentation "Keyword symbol of the currently selected firing weapon slot.")
  (equip-for :documentation "List of keyword symbols showing where this item may be equipped.")
  (equipper :documentation "When non-nil, the character currently equipping this item.")
  ;; <: containers :>
  (inventory :documentation "The contents (if any) of the cell.")
  (max-weight :documentation "Maximum weight this container can hold.")
  (max-items :documentation "Maximum number of items this container can hold.")
  (parent-container :documentation "Link to containing cell, if any.")
  ;; forms-related fields. see forms.lisp
  (label :initform nil :documentation "Label (string or formatted line) to be used as display in forms.")
  ;; proxying
  (occupant :documentation "Occupant cell, used to implement drivable vehicles.")
  (proxy :documentation "Link to the proxying cell for this occupant cell.")
  ;; other
  (combination-amount :initform 0 :documentation "Amount of item this cell represents.")
  (combination-key :initform nil :documentation "Only items matching this key will be combined.")
  ;; serialization
  (excluded-fields :initform '(:render-cell :equipper :parent-container :occupant :proxy)))

(define-method compute gcell () nil)
    
(define-method set gcell (data)
  nil)

(define-method get gcell ()
  (object-name (object-parent self)))

(define-method print gcell ()
  "")

(define-method read gcell (text)
  (read-from-string text))

(define-method is-located gcell ()
  "Returns non-nil if this cell is located somewhere on the grid."
  (or (and (integerp <row>) (integerp <column>))))

(define-method dislocate gcell ()
  "Remove any location data from the cell."
  (when (integerp <row>)
    (setf <row> nil <column> nil))
  (when (integerp <x>)
    (setf <x> nil <y> nil)))
    
;;; Convenience macro for defining gcells:

(defmacro defgcell (name &body args)
  "Define a gcell named NAME, with the fields ARGS as in a normal
prototype declaration. This is a convenience macro for defining new
gcells."
  `(define-prototype ,name (:parent =gcell=)
     ,@args))

;;; Names, knowledge, and descriptions

(define-method describe gcell (&optional description)
  "Narrate a description of the object. By default, uses
the :description field of the cell."
  (/emote self (get-some-object-name self) :timeout 5.0))
  ;; (setf description (or description <description>))
  ;; (when (stringp description)
  ;;   (dolist (line (split-string-on-lines description))
      

;;; Statistics 

(define-method stat-value gcell (stat-name &optional component (clamping t))
  "Compute the current value of the statistic in field STAT-NAME.
If a COMPONENT keyword is provided, return that component of the stat
instead of computing the value.

Characters and objects may have numeric-valued attributes like
Strength and Dexterity that have a minimum and maximum value
 (perhaps decided on the basis of class) as well as temporary and
permanent effects. In this case you want to store a base value,
minimum, maximum, and current delta, and compute the value at run
time.

Stats are just property lists with four different components: :base
:min :max and :delta."
  (let ((stat (field-value stat-name self)))
    (if (member component '(:base :min :max :delta :unit))
	(getf stat component)
	;; compute the value
	(destructuring-bind (&key base delta min max unit) stat
	  (let ((val (+ base (if (numberp delta) delta 0))))
	    (when clamping 
	      (when (and (numberp min) (< val min))
		(setf val min))
	      (when (and (numberp max) (> val max))
		(setf val max)))
	    (values val unit))))))

(define-method stat-effect gcell (stat-name val
					   &optional (component :base) (clamping t))
  "Add VAL, which may be negative, to the COMPONENT part of the stat
field named by STAT-NAME. The default is to change the :base value."
  (when (has-field stat-name self)
    (let* ((stat (field-value stat-name self))
	   (x (getf stat component)))
      (destructuring-bind (&key base min max &allow-other-keys) stat
	(incf x val)
	;; ensure base stays within bounds.
	(when (and clamping (eq :base component))
	  (when (numberp min)
	    (setf x (max min x)))
	  (when (numberp max)
	    (setf x (min max x))))
	;; update the stat
	(setf (getf stat component) x)
	(setf (field-value stat-name self) stat)))))
  
(defun make-stat (&key base min max delta unit)
  "Create a stat. Use this as an initialization form in cell declarations.
You must provide at least a :base value."
  (assert (numberp base))
  (list :base base :min min :max max :delta delta :unit unit))

;;; Pushing stuff; let the cell decide whether to move

(define-method push gcell (direction)
  nil)

;;; Custom rendering

(define-method draw gcell (x y image)
  "Use GLUON drawing commands to render a presentation of this cell at
X, Y to the offscreen image IMAGE.  This method is invoked to draw a
cell when its TILE field is nil, or when it is in the
category :drawn. See also viewport.lisp." 
  nil)

;;; Cell categories

(define-method in-category gcell (category) 
  "Return non-nil if this cell is in the specified CATEGORY.

Cells may be placed into categories that influence their processing by
the engine. The field `<categories>' is a set of keyword symbols; if a
symbol `:foo' is in the list, then the cell is in the category `:foo'.

Although a game built on GLUON can define whatever categories are
needed, certain base categories are built-in and have a fixed
interpretation:

 -    :actor --- This cell is active and may be controlled by either the
      user or the CPU. Only actor cells receive `:run' messages
      every turn. Other cells are purely `reactive'. Actor
      cells participate in the Action Points system.
 -    :target --- This cell is susceptible to targeting.
 -    :proxy --- This cell is a proxy for another cell.
 -    :drawn --- This cell has a (/draw) method used for custom drawing.
 -    :proxied  --- This cell is an occupant of a proxy.
 -    :dead --- This cell is no longer receiving run messages.
 -    :player --- Only one gcell (your player avatar) has this category.
 -    :enemy --- This cell is playing against you.
 -    :exclusive --- Prevent some objects from stacking. See also the method `drop-cell' in worlds.lisp
 -    :obstacle --- Blocks movement and causes collisions
 -    :pushable --- Can be pushed by impacts.
 -    :ephemeral --- This cell is not preserved when exiting a world.
 -    :combining --- This cell automatically combines units with other cells in a container.
 -    :light-source --- This object casts light. 
 -    :opaque --- Blocks line-of-sight, casts shadows. 
 -    :container --- This cell contains other cells, and has an <inventory> field
 -    :contained ---  This cell is contained in another gcell (i.e. not in open space on the map)
 -    :item --- A potential inventory item. 
 -    :equipper --- Uses equipment. 
 -    :equipped --- This item is currently equipped.
 -    :equipment --- This item can be equipped. 
"
  (member category <categories>))

(define-method add-category gcell (category)
  "Add this cell to the specified CATEGORY."
  (pushnew category <categories>))

(define-method delete-category gcell (category)
  "Remove this cell from the specified CATEGORY."
  (setf <categories> (remove category <categories>)))

;;; Run method

(define-method run gcell ()
  nil)

;;; Action Points

(define-method get-actions gcell ()
  <actions>)

(define-method is-actor gcell ()
  "Return non-nil if this cell is an actor. Actor cells receive a :run
message every frame."
  (member :actor <categories>))

(define-method is-player gcell ()
  "Return non-nil if this is the player."
  (member :player <categories>))

(defvar *action-points-over-p* nil 
  "When non-nil, ignore action points limit.")

;; The following functions calculate action points.

(define-method begin-phase gcell ()
  "Give the cell its allotment of action points to begin a phase.
If the last action of the previous turn brought the AP score into the
negative, then you'll come up that much short."
  (incf <action-points> (/stat-value self :speed)))

(define-method do-phase gcell ()
  "Invoked once at the beginning of each phase.")

(define-method poll-keys gcell ()
  nil)

(define-method can-act gcell (phase)
  "Determine whether the cell has enough action points to take some
action during PHASE.

The Action Points system is GLUON's model of roguelike time; Time is
divided into discrete episodes called phases.  Each phase consists
of one or more actions, each of which lasts a certain number of
action points' worth of time. During an action, the cell may modify
its own fields, invoke methods on itself, or send queued messages
to other cells in the environment. When a cell runs out of action
points, its phase ends and another cell's phase begins.

`Action points' (or `AP') control an actor cell's ability to take
actions during a phase. The AP score for a cell's phase starts at
 (/stat-value cell :speed). The AP cost of an action is determined by
the corresponding method's use of `expend-action-points'; see below. 

First your turn comes up, and GLUON waits for your input.  Once you
issue a command, some AP may be used up. When your AP is gone, the
computer's phase begins. The results are displayed, and if you're
still alive, the player phase begins again.

 (In realtime mode, GLUON does not wait for input.)

The queued messages' targets can be keywords like :world, :browser,
or :narrator instead of direct references to objects; the world
processes the messages before delivery and sends them to the right
place. (See also worlds.lisp)
"
  (when (and (not (/in-category self :dead))
	     (< <phase-number> phase)
	     (plusp <action-points>))
    (incf <phase-number>)))
  
(define-method expend-action-points gcell (points &optional min)
  "Expend POINTS action points, possibly going into the negative."
  (decf <action-points> points)
  (when (numberp min)
    (setf <action-points> (max min <action-points>))))

(define-method expend-default-action-points gcell ()
  (/expend-action-points self (/stat-value self :default-cost)))

(define-method end-phase gcell ()
  "End this cell's phase."
  (setf <phase-number> (/get-phase-number *world*)))

;;; Player orientation

(define-method distance-to-player gcell ()
  "Calculate the distance from the current location to the player."
  ;; todo fix for sprites
  (multiple-value-bind (row column) (/grid-coordinates self)
    (/distance-to-player *world* row column)))

(define-method direction-to-player gcell ()
  "Calculate the general compass direction of the player."
  (/direction-to-player *world* <row> <column>))

(define-method adjacent-to-player gcell ()
  (/adjacent-to-player *world* <row> <column>))

;;; Proxying and vehicles

(define-method proxy gcell (occupant)
  "Make this cell a proxy for OCCUPANT."
  (let ((world *world*))
    (when <occupant> 
      (error "Attempt to overwrite existing occupant cell in proxying."))
    (setf <occupant> occupant)
    ;; The cell should know when it is proxied, and who its proxy is.
    (/add-category occupant :proxied)
    (setf (field-value :proxy occupant) self)
    ;; Hide the proxy if it's in a world already.
    (when (numberp (field-value :row occupant))
      (/delete-cell world occupant <row> <column>))
    ;; Don't let anyone step on occupied vehicle.
    (/add-category self :obstacle)
    ;; Don't light up the map 
    (/add-category self :light-source)
    ;; If it's the player register self as player.
    (when (/is-player occupant)
      (message "OCCPU")
      (/add-category self :player)
      (/set-player world self)
      (setf <phase-number> (1- (/get-phase-number world))))))

(define-method unproxy gcell (&key dr dc dx dy)
  "Remove the occupant from this cell, dropping it on top."  
  (let ((world *world*)
	(occupant <occupant>))
    (when (null occupant)
      (error "Attempt to unproxy empty cell."))
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

(define-method do-post-unproxied gcell ()
  "This method is invoked on the unproxied former occupant cell after
unproxying. By default, it does nothing."
  nil)

(define-method forward gcell (method &rest args)
  "Attempt to deliver the failed message to the occupant, if any."
  (if (and (/is-player self)
	   (not (has-method method self))
	   (null <occupant>))
      (/say self (format nil "The ~S command is not applicable." method) )
      ;; otherwise maybe we're a vehicle
      (let ((occupant <occupant>))
	(when (null occupant)
	  (error "Cannot forward message ~S. No implementation found." method))
	(apply #'send self method occupant args))))
  
(define-method embark gcell (&optional v)
  "Enter a vehicle V."
  (let ((vehicle (or v (/category-at-p *world* <row> <column> :vehicle))))
    (if (null vehicle)
	(/>>say :narrator "No vehicle to embark.")
	(if (null (field-value :occupant vehicle))
	    (progn (/>>say :narrator "Entering vehicle.")
		   (/proxy vehicle self))
	    (/>>say :narrator "Already in vehicle.")))))

(define-method disembark gcell ()
  "Eject the occupant."
  (let ((occupant <occupant>))
    (when (and occupant (/in-category self :proxy))
	  (/unproxy self))))

;;; Cell movement

(define-method move gcell (direction &optional ignore-obstacles)
  "Move this cell one step in DIRECTION on the grid. If
IGNORE-OBSTACLES is non-nil, the move will occur even if an obstacle
is in the way. Returns non-nil if a move occurred."
  (let ((world *world*))
    (multiple-value-bind (r c) 
	(step-in-direction <row> <column> direction)
      ;; 
      (cond ((null (/cells-at world r c)) ;; are we at the edge?
	     ;; return nil because we didn't move
	     (prog1 nil
	     ;; edge conditions only affect player for now
	       (when (/is-player self)
		 (ecase (field-value :edge-condition world)
		   (:block (/say self "You cannot move in that direction."))
		   (:wrap nil) ;; TODO implement this for planet maps
		   (:exit (/exit *universe*))))))
	    (t
	     (when (or ignore-obstacles 
		       (not (/obstacle-at-p *world* r c)))
	       ;; return t because we moved
	       (prog1 t
		 (/expend-action-points self (/stat-value self :movement-cost))
		 (/move-cell world self r c))))))))
		 ;; (when <stepping>
		 ;;   (/step-on-current-square self)))))))))

(define-method set-location gcell (r c)
  "Set the row R and column C of the cell."
  (setf <row> r <column> c))

(define-method move-to gcell (r c)
  (/delete-cell *world* self <row> <column>)
  (/drop-cell *world* self r c))

(define-method exit gcell ()
  "This method is invoked on a player cell when it leaves a world."
  nil)

(define-method step-on-current-square gcell ()
  "Send :step events to all the cells on the current square."
  (when <stepping>
    (do-cells (gcell (/cells-at *world* <row> <column>))
      (unless (eq cell self) 
	(/step cell self)))))

(define-method drop gcell (cell &key loadout (exclusive nil))
  "Add CELL to the world at the current location. By default,
EXCLUSIVE is nil; this allows one to drop objects on top of oneself.
When LOADOUT is non-nil, call the :loadout method."
  (/drop-cell *world* cell <row> <column> :loadout loadout :exclusive exclusive))

(define-method drop-sprite gcell (sprite &optional x y)
  "Add SPRITE to the world at location X,Y."
  (multiple-value-bind (x0 y0)
      (/xy-coordinates self)
    (let ((x1 (or x x0))
	  (y1 (or y y0)))
      (/add-sprite *world* sprite)
      (assert (and x1 y1))
      (/update-position sprite x1 y1))))

(define-method step gcell (stepper)
  "Respond to being stepped on by the STEPPER."
  (declare (ignore stepper)))

(define-method is-light-source gcell ()
  "Returns non-nil if this cell is a light source."
  (/in-category self :light-source))

;;; Containers

;; An object's <inventory> field is a vector. Each position of the
;; vector holds either a cell object or nil. The number of available
;; slots is stored in the <max-items> field. When an item is added to
;; an inventory, the first open slot is used. 
;; TODO allow arbitrary placement

(define-method make-inventory gcell ()
  "Create an empty <inventory> of length <max-items>."
  (setf <inventory> (make-array (/get-max-items self)
				:initial-element nil
				:adjustable nil)))

(define-method make-equipment gcell ()
  "Create an empty equipment property list."
  (setf <equipment> (mapcon #'(lambda (slot)
				(list slot nil))
			    <equipment-slots>)))

(define-method get-max-items gcell ()
  "Return the maximum number of items this container can hold."
  (assert <max-items>)
  (/stat-value self :max-items))

(define-method set-container gcell (container)
  "Set the container pointer of this cell to CONTAINER.
All contained cells maintain a pointer to their containers."
  (setf <container> container))

(define-method is-container gcell ()
  "Returns non-nil if this cell is a container."
  (/in-category self :container))

(define-method is-item gcell ()
  "Returns non-nil if this cell is a potential inventory item."
  (/in-category self :item))

(define-method first-open-slot gcell ()
  "Return the integer position of the first open inventory slot, or
nil if none."
  (position nil <inventory>))

(define-method add-item gcell (item)
  "Add the ITEM to the cell's <inventory>.
Return the new integer position if successful, nil otherwise."
  ;; TODO check whether we can combine items
  (let ((pos (/first-open-slot self)))
    (when (and (numberp pos) (/in-category item :item))
      (prog1 pos
	(setf (aref <inventory> pos) item)
	(/add-category item :contained)
	(/set-container item self)))))
      
(define-method remove-item gcell (item)
  "Remove ITEM from the <inventory>.
Return ITEM if successful, nil otherwise."
  (let* ((pos (position item <inventory>)))
    (when pos
      (prog1 item
	(setf (aref <inventory> pos) nil)
	(/delete-category item :contained)
	(/set-container item nil)))))

(define-method item-at gcell (pos)
  "Return the item at inventory position POS."
  (assert <inventory>)
  (aref <inventory> pos))

(define-method replace-item-at gcell (item pos)
  "Replace the inventory item at position POS with ITEM."
  (setf (aref <inventory> pos) item))

(define-method weight gcell ()
  "Recursively calculate the weight of this cell."
  (let ((total 0)
	(inventory <inventory>)
	(cell nil))
    (if (/is-container self)
	;; recursively weigh the contents.
	(progn
	  (dotimes (n (length inventory))
	    (setf gcell (aref inventory n))
	    (when cell
	      (incf total (/weight cell))))
	  total)
	;; base case; just return the weight
	(or <weight> 0))))

(define-method drop-item gcell (pos)
  "Drop the item at inventory position POS."
  (let ((item (/item-at self pos)))
    (when item
      (/remove-item self item)
      (/drop self item))))

;;; Finding and manipulating objects

(define-method find gcell (&key (direction :here) (index :top) category)
  (let ((world *world*))
    (multiple-value-bind (nrow ncol)
	(step-in-direction <row> <column> direction)
      (if (/in-bounds-p world nrow ncol)
	  (let (cell)
	    (let* ((cells (/cells-at world nrow ncol))
		   (index2 (cond 
			     ((not (null category))
				(setf gcell (/category-at-p world nrow ncol category))
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
	      (setf gcell (aref cells index2))
	      (values cell nrow ncol index2)))))))

(define-method clear-location gcell ()
  (setf <row> nil <column> nil))

(define-method delete-from-world gcell ()
  (/delete-cell *world* self <row> <column>))
;;  (/clear-location self))
		       
(define-method take gcell (&key (direction :here) index category)
  "Take the item and return non-nil if successful."
  (multiple-value-bind (cell row column)
      (/find self :direction direction :index index :category category)
    (when (and (/in-category cell :item)
	       (/first-open-slot self))
      (prog1 t
	(/expend-default-action-points self)
	(/delete-from-world cell)
	(/add-item self cell)))))

(define-method use gcell (user)
  "Return non-nil if cell is used up and should disappear."
  (declare (ignore user))
  (prog1 nil
    (/say self "Nothing happens.")))
    
(define-method resolve gcell (reference &optional category)
  "Accept a REFERENCE to a cell, and try to get the real cell.
The REFERENCE may be an object, one of the `*compass-directions*', an
equipment slot keyword, or an integer denoting the nth inventory
slot."
  (etypecase reference
    (keyword (if (member reference *compass-directions*)
		 (/find self :direction reference :category category)
		 (/equipment-slot self reference)))
    (integer (/item-at self reference))
    (gluon:object reference)))

;;; Knowledge of objects

;; TODO port and document knowledge code

(defun some-name-of (ob)
  (let ((name (if (has-field :name ob)
		  (field-value :name ob)
		  nil)))
    (if (stringp name)
	name
	(progn
	  (setf name (symbol-name (object-name (object-parent ob))))
	  (subseq name (1+ (search "=" name))
		  (search "=" name :from-end t))))))

;;; Equipment

(define-method is-equipment gcell ()
  "Return non-nil if this cell is a piece of equipment."
  (/in-category self :equipment))

(define-method equipment-slot gcell (slot)
  "Return the equipment item (if any) in the slot named SLOT."
  (assert (member slot <equipment-slots>))
  (getf <equipment> slot))

(define-method equipment-match gcell (item)
  "Return a list of possible slots on which this cell could equip
ITEM. Returns nil if no such match is possible."
  (when (/is-equipment item)
    (intersection <equipment-slots> 
		  (field-value :equip-for item))))

(define-method add-equipment gcell (item &optional slot)
  (let ((match (/equipment-match self item)))
    (setf (getf <equipment> 
		(or slot (first match)))
	  item)))
  		
(define-method delete-equipment gcell (slot)
  (setf (getf <equipment> slot) nil))

;; todo rewrite this and decouple the messaging

(define-method equip gcell (&optional reference slot)
  (unless reference
    (error "Cannot resolve null reference during equipping."))
  ;; (unless (keywordp slot)
  ;;   (error "Cannot equip null slot---you must supply a keyword."))
  (let ((item (/resolve self reference)))
    (when item
      (let* ((match (/equipment-match self item))
	     (valid (/is-equipment item))
	     (slot2 (or slot (when match 
			       (first match))))
	     (open (when slot2
		     (null (/equipment-slot self slot2)))))
	(if (and valid match open)
	    (progn 
	      (/expend-default-action-points self)
	      (/add-equipment self item)
	      (/add-category item :equipped)
	      ;; remove from inventory
	      (/remove-item self item)
	      (setf (field-value :equipper item) self)
	      (when (and *message-queue* (/is-player self))
		(/>>newline :narrator )))
	    (progn
	      ;; explain failure
	      (when (and *message-queue* (/is-player self))
		(/>>say :narrator "You cannot equip ")
		(/>>print-object-tag :narrator item)
		(/>>newline :narrator)
		(cond
		  ((not valid) 
		   (/>>say :narrator "This item is not a piece of equipment."))
		  ((and match (not open))
		   (/>>say :narrator "You must un-equip the ~A first." slot2))
		  ((not match)
		   (/>>say :narrator "This can only be equipped in one of: ~A"
			  (field-value :equip-for item)))))))))))
    
(define-method dequip gcell (slot)
  ;; TODO document
  ;; TODO narration
  (let ((item (getf <equipment> slot)))
    (when item
      (/>>expend-default-action-points)
      (/>>delete-equipment self slot)
      (/>>add-item self item))))

;;; Loadout

;; Automatic inventory and equipment loadout for new cells.
;; See how this is used in worlds.lisp.

(define-method loadout gcell ()
  "This is called for cells after being dropped in a world, with a
non-nil :loadout argument. It can also be triggered manually.  Use
`loadout' for things that have to be done while in a world. (During
your cell's normal CLON `initialize' method, the cell will not be in a
world or have a location."
  nil)

;;;; Starting

(define-method start gcell ()
  "This method is invoked on cells whenever a new world map is visited."
  nil)

;;; Combat

(define-method attack gcell (target)
  (if (null <attacking-with>)
      (/>>say :narrator "No attack method specified.")
      (let* ((weapon (/equipment-slot self <attacking-with>))
	     (target-gcell (/resolve self target))
	     (target-name (field-value :name target-cell)))
	(if (null weapon)
	    (when (/is-player self)
	      (/>>say :narrator "Cannot attack without a weapon in ~A." 
		     <attacking-with>))
	    (let* ((attack-cost (/stat-value weapon :attack-cost))
		   (accuracy (/stat-value weapon :accuracy))
		   (dexterity (/stat-value self :dexterity))
		   (strength (/stat-value self :strength))
		   (to-hit (< (random 100)
			      (+ accuracy (* (random 10) (/ dexterity 2))))))
	      (if to-hit
		  ;; calculate and send damage
		  (let ((damage (+ (truncate (/ strength 3))
				   (/stat-value weapon :attack-power))))
		    (/>>expend-action-points self attack-cost)
		    (when (/is-player self)
		      (/>>say :narrator "You do ~D points of damage on the ~A."
			     damage
			     (get-some-object-name target-cell)))
		    (/>>damage target-cell damage))
		  (progn 
		    (/>>expend-default-action-points self)
		    (when (/is-player self)
		      (/>>narrateln :narrator "You missed.")))))))))
  
(define-method fire gcell (direction)
  (let ((weapon (/equipment-slot self <firing-with>)))
    (if weapon
	(progn 
	  (/expend-action-points self (/stat-value weapon :attack-cost))
	  (/fire weapon direction))
	(/>>narrateln :narrator "Nothing to fire with."))))

(define-method damage gcell (damage-points)
  (when (has-field :hit-points self)
    (progn 
      (/stat-effect self :hit-points (- damage-points))
      (when (zerop (/stat-value self :hit-points))
	(/die self)))))
      ;; (when (/is-player self)
      ;; 	(/>>say :narrator "You take ~D hit points of damage." damage-points)))))
	
(define-method die gcell ()
  "Abandon this cell to the garbage collector."
  (if (/in-category self :dead)
      (message "Warning: called die on dead cell!")
      (progn
	(setf <action-points> 0)
	(/add-category self :dead)
	(/delete-from-world self))))

(define-method cancel gcell ()
  "This cell was scheduled for drop and possible loadout in a world,
but this was canceled. A canceled cell should update any global state
to reflect its disappearance; this is different from a dying cell." nil)

(define-method expend-energy gcell (amount)
  (when (< amount (/stat-value self :energy))
    (prog1 t
      (/stat-effect self :energy (- amount)))))

(defparameter *default-sample-hearing-range* 15)

(define-method play-sample gcell (sample-name)
  "Play the sample SAMPLE-NAME.
May be affected by the player's :hearing-range stat, if any."
  (when (/get-player *world*)
    (let* ((player (/get-player *world*))
	   (range (if (proton:has-field :hearing-range player)
		      (proton:field-value :hearing-range player)
		      *default-sample-hearing-range*))
	   (dist (multiple-value-bind (row col) 
		     (/grid-coordinates self)
		   (distance col row
			     (/player-column *world*)
			     (/player-row *world*)))))
      (when (> range dist)
	(play-sample sample-name)))))

(define-method say gcell (format-string &rest args)
  "Print a string to the message narration window. Arguments
are as with `format'."
  (unless (/in-category self :dead)
    (let ((range (if (proton:has-field :hearing-range self)
		     <hearing-range>
		     *default-sample-hearing-range*))
	  (dist (distance (or <column> 0) (or <row> 0)
			  (/player-column *world*)
			  (/player-row *world*))))
      (when (> range dist)
	(apply #'send-queue self :say :narrator format-string args)))))

(define-method viewport-coordinates gcell ()
  "Return as values X,Y the world coordinates of CELL."
  (assert (and <row> <column>))
  (/get-viewport-coordinates (field-value :viewport *world*)
                            <row> <column>))

(define-method image-coordinates gcell ()
  "Return as values X,Y the viewport image coordinates of CELL."
  (assert (and <row> <column>))
  (/get-image-coordinates (field-value :viewport *world*)
                         <row> <column>))

(define-method screen-coordinates gcell ()
  "Return as values X,Y the screen coordinates of CELL."
  (assert (and <row> <column>))
  (/get-screen-coordinates (field-value :viewport *world*)
			  <row> <column>))

;; TODO is this needed?
(define-method deserialize gcell ()
  (with-field-values (equipment) self
    (when (listp equipment)
      (loop while equipment do
	(pop equipment) ;; get rid of keyword
	(let ((item (pop equipment)))
	  (when item
	    (setf (field-value :equipper item)
		  self)))))))

(define-method hit gcell (&optional other) nil)

(define-method can-see-player gcell ()
  (/line-of-sight *world* <row> <column> (/player-row *world*) (/player-column *world*)))

(define-method can-see gcell (cell &optional category)
  (/line-of-sight *world* <row> <column> (field-value :row cell) (field-value :column cell) category))

(define-method can-see-* gcell (r c &optional category)
  (/line-of-sight *world* <row> <column> r c category))

;;; User Interaction with keyboard and mouse

(define-method select gcell ()
  (/describe self))

;;; The asterisk cell is a wildcard

(define-prototype asterisk (:parent =cell=)
  (tile :initform ".asterisk")
  (name :initform "Command"))

(define-prototype gray-asterisk (:parent =cell=)
  (tile :initform ".gray-asterisk")
  (name :initform "System"))

;;; Sprites are not restricted to the grid

;; These sit in a different layer, the <sprite> layer in the world
;; object.

(define-prototype sprite (:parent =cell=
				  :documentation 
"Sprites are GLUON game objects derived from cells. Although most
behaviors are compatible, sprites can take any pixel location in the
world, and collision detection is performed between sprites and cells.")
  (x :initform nil :documentation "The world x-coordinate of the sprite.") 
  (y :initform nil :documentation "The world y-coordinate of the sprite.") 
  (saved-x :initform nil :documentation "Saved x-coordinate used to jump back from a collision.")
  (saved-y :initform nil :documentation "Saved y-coordinate used to jump back from a collision.")
  (image :initform nil :documentation "The arbitrarily sized image
  resource. This determines the bounding box.")
  (width :initform nil :documentation "The cached width of the bounding box.")
  (height :initform nil :documentation "The cached height of the bounding box.")
  (type :initform :sprite))

(define-method image-coordinates sprite ()
  (/get-viewport-coordinates-* (field-value :viewport *world*) <x> <y>))

;; Convenience macro for defining cells:

(defmacro defsprite (name &body args)
  `(define-prototype ,name (:parent =sprite=)
     ,@args))

(defun is-sprite (ob)
  (when (eq :sprite (field-value :type ob))))

(defun is-gcell (ob)
  (when (eq :gcell (field-value :type ob))))

(define-method update-dimensions sprite ()
  (proton:with-fields (image height width) self
    (when image
      (setf width (image-width image))
      (setf height (image-height image)))))
    
(define-method initialize sprite ()
  (when <image>
    (/update-image self <image>)))

(define-method die sprite ()
  (/remove-sprite *world* self))

(define-method update-image sprite (image)
  (setf <image> image)
  (/update-dimensions self))

(define-method update-position sprite (x y &optional ignore-obstacles)
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

(define-method move sprite (direction &optional movement-distance)
  (let ((dist (or movement-distance <movement-distance>)))
    (let ((y <y>)
	  (x <x>))
      (when (and y x)
	(multiple-value-bind (y0 x0) (gluon:step-in-direction y x direction dist)
	  (assert (and y0 x0))
	    (/update-position self x0 y0))))))

(define-method collide sprite (sprite)
  ;; (message "COLLIDING A=~S B=~S"
  ;; 	   (object-name (object-parent self))
  ;; 	   (object-name (object-parent sprite)))
  (let ((x0 (field-value :x sprite))
	(y0 (field-value :y sprite))
	(w (field-value :width sprite))
	(h (field-value :height sprite)))
    (/collide-* self y0 x0 w h)))
    
(define-method would-collide sprite (x0 y0)
  (proton:with-field-values (tile-size grid sprite-grid) *world*
    (proton:with-field-values (width height x y) self
      ;; determine squares sprite would intersect
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
	    ;; scan for sprite intersections
	    (block intersecting 
	      (let (collision num-sprites ix)
		(dotimes (i (max 0 (- bottom top)))
		  (dotimes (j (max 0 (- right left)))
		    (let ((i0 (+ i top))
			  (j0 (+ j left)))
		      (when (array-in-bounds-p grid i0 j0)
			(setf collision (aref sprite-grid i0 j0))
			(setf num-sprites (length collision))
			(when (< 1 num-sprites)
			  (dotimes (i (- num-sprites 1))
			    (setf ix (1+ i))
			    (loop do (let ((a (aref collision i))
					   (b (aref collision ix)))
				       (incf ix)
				       (assert (and (proton:object-p a) (proton:object-p b)))
				       (when (not (eq a b))
					 (let ((bt (field-value :y b))
					       (bl (field-value :x b))
					       (bh (field-value :height b))
					       (bw (field-value :width b)))
					   (when (collide y0 x0 width height bt bl bw bh)
					     (return-from intersecting t)))))
				  while (< ix num-sprites)))))))))
	      nil))))))
	    
(define-method collide-* sprite (o-top o-left o-width o-height)
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

(define-method do-collision gcell (object)
  "Respond to a collision detected with OBJECT."
  nil)

(define-method do-collision sprite (object)
  "Respond to a collision detected with OBJECT."
  nil)

(define-method save-excursion sprite ()
  (setf <saved-x> <x>)
  (setf <saved-y> <y>))

(define-method distance-to-player sprite ()
  "Calculate the distance from the current location to the player."
  (multiple-value-bind (r c) (/grid-coordinates self)
    (/distance-to-player *world* r c)))

(define-method undo-excursion sprite ()
  (/update-position self <saved-x> <saved-y>))

(define-method viewport-coordinates sprite ()
  (/get-viewport-coordinates-* (field-value :viewport *world*)
			      <x> <y>))

(define-method grid-coordinates gcell ()
  (values <row> <column>))

(define-method grid-coordinates sprite ()
  (values (truncate (/ <y> (field-value :tile-size *world*)))
	  (truncate (/ <x> (field-value :tile-size *world*)))))

(define-method xy-coordinates gcell ()
  (values (* <column> (field-value :tile-size *world*))
	  (* <row> (field-value :tile-size *world*))))

(define-method xy-coordinates sprite ()
  (values <x> <y>))

(define-method drop sprite (cell &optional (delta-row 0) (delta-column 0))
  (multiple-value-bind (r c)
      (/grid-coordinates self)
    (/drop-cell *world* gcell (+ r delta-row) (+ c delta-column))))
  
;;; Popup text balloons

(defcell balloon 
  (categories :initform '(:drawn :actor :balloon))
  (auto-loadout :initform t)
  text stroke-color background-color timeout following scale)

(define-method initialize balloon (&key text (stroke-color ".white") (background-color ".gray30")
					(style :balloon) (timeout nil) name tile description following
					(scale 1))
  (setf <text> text) 
  (when following (setf <following> following))
  (when tile (setf <tile> tile))
  (when name (setf <name> name))
  (when description (setf <description> description))
  (setf <stroke-color> stroke-color)
  (setf <background-color> background-color)
  (setf <style> style)
  (setf <scale> scale)
  (setf <timeout> (if (floatp timeout)
		      ;; specify in (roughly) seconds if floating
		      (truncate (/ (* timeout 1000)
				   gluon:*dt*))
		      ;; leave as frames if integer
		      timeout)))
  
(define-method draw balloon (x y image)
  (proton:with-field-values (text style scale) self
    (proton:with-field-values (tile-size) *world*
      (let* ((offset (ecase style
		       (:balloon tile-size)
		       (:clear 0)
		       (:flat 0)))
	     (x0 (+ x tile-size))
	     (y0 (+ y tile-size))
	     (x1 (+ x0 (* scale offset)))
	     (y1 (+ y0 (* scale offset)))
	     (margin 4)
	     (height (+ (* 2 margin) (apply #'+ (mapcar #'formatted-line-height text))))
	     (width (+ (* 2 margin) (apply #'max (mapcar #'formatted-line-width text)))))
	(when (eq style :balloon)
	  (draw-box x1 y1 width height 
		    :stroke-color <stroke-color>
		    :color <background-color>
		    :destination image)
	  (draw-line x0 y0 x1 y1 :destination image))
	(let ((x2 (+ margin x1))
	      (y2 (+ margin y1)))
	  (dolist (line text)
	    (render-formatted-line line x2 y2 :destination image)
	    (incf y2 (formatted-line-height line))))))))

(define-method run balloon ()
  (/expend-default-action-points self)
  (when <following>
    (multiple-value-bind (r c) (/grid-coordinates <following>)
      ;; follow emoter
      (/move-to self r c)))
  (when (integerp <timeout>)
    (when (minusp (decf <timeout>))
      (/die self))))

(define-method emote gcell (text &key (timeout 8.0) (style :clear))
  (let* ((ftext (if (stringp text) (list (list (list text)))
		    text))
	 (balloon (clone =balloon= :text ftext :timeout timeout 
			:following self :style style
			:scale 2)))
    (/play-sample self "talk")
    ;; get rid of any previous balloons first
    (/delete-category-at *world* <row> <column> :balloon)
    ;; drop it
    (/drop self balloon)))

;;; Sprite specials; editor placeholders for sprites.

;; This is a substitute for drop-sprite, mainly used in the editor.
;; These special objects replace themselves with a sprite upon
;; activation.

(defcell sprite-special
  (auto-loadout :initform t)
  (sprite-name :initform nil)
  (categories :initform '(:actor :drawn)))

(define-method set sprite-special (value)
  (assert (symbolp value))
  (setf <sprite-name> value))

(define-method draw sprite-special (x y image)
  (with-fields (sprite-name) self
    (let ((im (if (and sprite-name (has-field :image (symbol-value sprite-name)))
		  (field-value :image (symbol-value sprite-name))
		  ".asterisk")))
      (draw-resource-image im x y :destination image))))

(define-method run sprite-special ()
  (with-fields (sprite-name) self
    (when sprite-name
      (/drop-sprite self (clone (symbol-value sprite-name)))
      (/die self))))
		     
;; (define-method run sprite-special ()
;;   (/die self))

;;; gcells.lisp ends here
