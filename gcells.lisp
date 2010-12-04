;;; gcells.lisp --- defining objects

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

;;; Commentary:

;; "GCells" are CLON objects which represent all in-game entities;
;; player characters, enemies, weapons, items, walls and floors are
;; all different types of cells. Game play occurs in a
;; three-dimensional grid of cells called a "world" (see worlds.lisp).

;; Cells may be stacked along the z-axis, and may also contain other
;; cells. Cells interact by sending messages to one another (with
;; `send-queue'); these messages are queued and processed by the
;; world for delivery to their recipients. 

(in-package :iomacs)

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
  (team :initform nil :documentation "Keyword symbol of team, if any.")
  (weight :documentation "Weight of the cell, in kilograms.")
  (widget :initform nil :documentation "IOMACS widget object, if any.")
  (light-radius :initform 0 :documentation "Strength of light cast by this object.") 
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
  (name :documentation "The name of this cell.")
  (description :documentation "A description of the cell.") 
  (unknown-name :documentation "The name of this cell, when it is unknown.")
  (unknown-description :documentation "A description of the cell, when it is unknown.")
  (equipment :documentation "Property list of :slot -> cell pairs.") 
  (equipment-slots :documentation "List of keyword symbols identifying available equipment slots."
		   :initform '(:head :neck :left-hand :right-hand :hands :feet :legs :torso :arms :pack :belt))
  (using-slot :documentation "Keyword symbol of the currently selected equipment slot.")
  (attacking-with :documentation "Keyword symbol of the currently selected weapon slot.")
  (firing-with :documentation "Keyword symbol of the currently selected firing weapon slot.")
  (equip-for :documentation "List of keyword symbols showing where this item may be equipped.")
  (equipper :documentation "When non-nil, the character currently equipping this item.")
  (inventory :documentation "The contents (if any) of the cell.")
  (max-weight :documentation "Maximum weight this container can hold.")
  (max-items :documentation "Maximum number of items this container can hold.")
  (parent-container :documentation "Link to containing cell, if any.")
  ;; forms-related fields. see forms.lisp
  (label :initform nil :documentation "Label (string or formatted line) to be used as display in forms.")
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

The Action Points system is IOMACS's model of roguelike time; Time is
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

First your turn comes up, and IOMACS waits for your input.  Once you
issue a command, some AP may be used up. When your AP is gone, the
computer's phase begins. The results are displayed, and if you're
still alive, the player phase begins again.

 (In realtime mode, IOMACS does not wait for input.)

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

(define-method step-on-current-square gcell ()
  "Send :step events to all the cells on the current square."
  (when <stepping>
    (do-cells (gcell (/grid-location *world* <row> <column>))
      (unless (eq cell self) 
	(/step cell self)))))

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
    (iomacs:object reference)))

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
	   (range (if (has-field :hearing-range player)
		      (field-value :hearing-range player)
		      *default-sample-hearing-range*))
	   (dist (multiple-value-bind (row col) 
		     (/grid-coordinates self)
		   (distance col row
			     (/player-column *world*)
			     (/player-row *world*)))))
      (when (> range dist)
	(play-sample sample-name)))))

(define-method distance-to-player gcell ()
  (multiple-value-bind (r c) (/grid-coordinates self)
    (/distance-to-player *world* r c)))

(define-method say gcell (format-string &rest args)
  "Print a string to the message narration window. Arguments
are as with `format'."
  (unless (/in-category self :dead)
    (let ((range (if (has-field :hearing-range self)
		     <hearing-range>
		     *default-sample-hearing-range*))
	  (dist (/distance-to-player self)))
      (when (> range dist)
	(apply #'send-queue self :say :narrator format-string args)))))

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


;;; Popup text balloons

(defgcell balloon 
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
				   iomacs:*dt*))
		      ;; leave as frames if integer
		      timeout)))
  
(define-method draw balloon (x y image)
  (with-field-values (text style scale) self
    (with-field-values (tile-size) *world*
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
    (multiple-value-bind (row column)
	(/grid-coordinates self)
      (/delete-category-at *world* row column :balloon))
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
