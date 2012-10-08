;; worlds.lisp --- squeakish spaces 

;; Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011, 2012  David O'Toole

;; Author: David O'Toole dto@ioforms.org
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

(defvar *listener* nil)

(defvar *listener-open-p* nil)

(define-block world
  (variables :initform nil 
	     :documentation "Hash table mapping values to values, local to the current world.")
  (player :documentation "The player object, if any.")
  (followed-object :initform nil)
  (background-image :initform nil)
  (background-color :initform "white")
  (redraw-player :initform t)
  (category :initform :data)
  (x :initform 0)
  (y :initform 0)
  (paused :initform t)
  (heading :initform 0.0)
  (height :initform 256)
  (width :initform 256)
  (depth :initform *z-far*)
  (field-of-view :initform *field-of-view*)
  (was-key-repeat-p :initform nil)
  ;; objects and collisions
  (objects :initform nil :documentation "A hash table with all the world's objects.")
  (quadtree :initform nil)
  (quadtree-depth :initform nil)
  ;; viewing window 
  (window-x :initform 0)
  (window-y :initform 0)
  (window-z :initform 0)
  (window-x0 :initform nil)
  (window-y0 :initform nil)
  (window-z0 :initform nil)
  (horizontal-scrolling-margin :initform 1/4)
  (vertical-scrolling-margin :initform 1/4)
  (window-scrolling-speed :initform 5)
  (window-scale-x :initform 1)
  (window-scale-y :initform 1)
  (window-scale-z :initform 1)
  (projection-mode :initform :orthographic)
  (rewound-selection :initform nil)
  (future :initform nil)
  (future-steps :initform 32)
  (future-step-interval :initform 8)
  (default-events :initform
		  '(((:tab) :tab)
		    ((:tab :shift) :backtab)
		    ((:x :alt) :enter-listener)
		    ((:x :control) :cut)
		    ((:c :control) :copy)
		    ((:v :control) :paste)
		    ((:v :control :shift) :paste-here)
		    ((:g :control) :escape)
		    ((:escape) :toggle-listener)
		    ((:d :control) :drop-selection)
		    ((:m :alt) :add-message)
		    ((:s :alt) :add-statement)
		    ((:v :alt) :add-variable)
		    ((:l :alt) :add-self)
		    ((:f :alt) :add-field)
		    ((:e :alt) :add-expression)
		    ((:pause) :transport-toggle-play)
		    ((:f10) :toggle-listener)
		    ((:f12) :toggle-other-windows)
		    ))
  ;; prototype control
  (excluded-fields :initform
		   '(:events :quadtree :click-start :click-start-block :drag-origin :drag-start :drag-offset :focused-block :listener :drag :hover :highlight 
		     ;; shell objects are not saved:
		     :inputs)
		   :documentation "Don't serialize the menu bar.")
  (field-collection-type :initform :hash)
  ;; dragging info
  (drag :initform nil 
  	:documentation "Block being dragged, if any.")
  (drag-button :initform nil)
  (hover :initform nil
	 :documentation "Block being hovered over, if any.")
  (highlight :initform nil
	     :documentation "Block being highlighted, if any.")
  (ghost :initform nil
	 :documentation "Dummy block to hold original place of currently dragged block onscreen.")
  (focused-block :initform nil
		 :documentation "Block having current input focus, if any.")
  (last-focus :initform nil)
  (click-start :initform nil
	      :documentation "A cons (X . Y) of widget location at moment of click.")
  (click-start-block :initform nil
		     :documentation "The block indicated at the beginning of a drag.")
  (drag-origin :initform nil
	       :documentation "The parent block originally holding the dragged block.")
  (object-p :initform nil
		 :documentation "When non-nil, the dragged object is in the world.")
  (drag-start :initform nil
	      :documentation "A cons (X . Y) of widget location at start of dragging.")
  (drag-offset :initform nil
	       :documentation "A cons (X . Y) of relative mouse click location on dragged block.")
  ;; For buffer page worlds
  (prototype-name :initform nil)
  (method :initform nil)
  (modified :initform nil 
	  :documentation "Non-nil when modified since last save."))

(define-method toggle-other-windows world ()
  (glass-toggle))

(defun selection ()
  (get-selection (world)))

(defun selected-object ()
  (let ((sel (selection)))
    (assert (consp sel))
    (first sel)))

(defmacro with-world (world &rest body)
  `(let* ((*world* (find-uuid ,world)))
     ,@body))

(define-method transport-pause world ()
  (setf %paused t)
  (setf %rewound-selection
	(mapcar #'duplicate
		(get-selection self))))

(define-method transport-play world ()
  (setf %paused nil)
  (clear-future self)
  (mapc #'destroy (get-selection self))
  (dolist (each %rewound-selection)
    (add-object (world) each))
  (setf %rewound-selection nil))

(define-method transport-toggle-play world ()
  (if %paused 
      (transport-play self)
      (transport-pause self)))

(define-method show-future world ()
  (prog1 nil
    (let ((selection (get-selection self)))
      (let (future)
	(dolist (thing selection)
	  (remove-object self thing)
	  (let (trail)
	    (dotimes (i %future-steps)
	      (let ((ghost (duplicate thing)))
		(with-world self
		  (with-quadtree %quadtree
		    (add-object self ghost)
		    (assert (%quadtree-node ghost))
		    (dotimes (j (* i %future-step-interval))
		      (update ghost)
		      (run-tasks ghost)
		      (quadtree-collide ghost))))
		(remove-object self ghost)
		(push ghost trail)))
	    (push trail future))
	  (add-object self thing)
	  (make-halo thing))
	(setf %future future)))))

(define-method clear-future world ()
  (setf %future nil))

(define-method update-future world ()
  (when %future (show-future self)))

(defmacro define-world (name &body body)
  `(define-block (,name :super world)
     ,@body))

(define-method get-objects world ()
  (loop for object being the hash-values in %objects collect object))

(define-method has-object world (thing)
  (gethash (find-uuid thing) %objects))

;; Defining and scrolling the screen viewing window

(define-method window-bounding-box world ()
  (values %window-y 
	  %window-x
	  (+ %window-x *gl-screen-width*)
	  (+ %window-y *gl-screen-height*)))

(define-method move-window-to world (x y &optional z)
  (setf %window-x x 
	%window-y y)
  (when z (setf %window-z z)))

(define-method move-window-to-object world (object)
  (multiple-value-bind (top left right bottom) 
      (bounding-box object)
    (declare (ignore right bottom))
    (move-window-to 
     self 
     (max 0 (- left (/ *gl-screen-width* 2)))
     (max 0 (- top (/ *gl-screen-width* 2))))))

(define-method move-window-to-player world ()
  (when %player
    (move-window-to-object self %player)))

(define-method move-window world (dx dy &optional dz)
  (incf %window-x dx)
  (incf %window-y dy)
  (when dz (setf %window-dz dz)))

(define-method glide-window-to world (x y &optional z)
  (setf %window-x0 x)
  (setf %window-y0 y)
  (when z (setf %window-z z)))

(define-method glide-window-to-object world (object)
  (multiple-value-bind (top left right bottom) 
      (bounding-box object)
    (declare (ignore right bottom))
    (glide-window-to 
     self 
     (max 0 (- left (/ *gl-screen-width* 2)))
     (max 0 (- top (/ *gl-screen-width* 2))))))

(define-method glide-window-to-player world ()
  (when %player
    (glide-window-to-object self %player)))

(define-method follow-with-camera world (thing)
  (assert (or (null thing) (blockyp thing)))
  (setf %followed-object thing)
  (glide-window-to-object self %followed-object))

(define-method glide-follow world (object)
  (with-fields (window-x window-y width height) self
    (let ((margin-x (* %horizontal-scrolling-margin *gl-screen-width*))
	  (margin-y (* %vertical-scrolling-margin *gl-screen-height*))
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

(define-method project-window world ()
  (ecase %projection-mode 
    (:orthographic (project-orthographically))
    (:perspective (project-with-perspective :field-of-view %field-of-view :depth %depth)))
  (transform-window :x %window-x :y %window-y :z %window-z 
		    :scale-x %window-scale-x 
		    :scale-y %window-scale-y
		    :scale-z %window-scale-z))

(define-method emptyp world ()
  (or (null %objects)
      (zerop (hash-table-count %objects))))

(define-method initialize world (&key name)
  (initialize%super self)
  (setf %buffer-name name)
  (when name
    (setf %prototype-name (buffer-name-prototype name))
    (setf %method (buffer-name-method name))
    (add-buffer name self))
  (setf %ghost (new 'block))
  (setf %objects (make-hash-table :test 'equal)))

;;; The object layer. 

(defvar *object-placement-capture-hook*)

(define-method add-object world (object &optional x y (z 0))
  (with-world self
    (with-quadtree %quadtree
      (remove-thing-maybe self object)
      (assert (not (contains-object self object)))
      (setf (gethash (find-uuid object)
		     %objects)
	    (find-uuid object))
      (when (and (numberp x) (numberp y))
	(setf (%x object) x
	      (%y object) y))
      (when (numberp z)
	(setf (%z object) z))
      (clear-saved-location object)
      (quadtree-insert-maybe object)
      (after-place-hook object))))
      
(define-method remove-object world (object)
  (remhash (find-uuid object) %objects)
  (when (%quadtree-node object)
    (quadtree-delete object)
    (setf (%quadtree-node object) nil)))

(define-method remove-thing-maybe world (object)
  (with-world self
    (destroy-halo object)
    (when (gethash (find-uuid object) %objects)
      (remove-object self object))
    (when (%parent object)
      (unplug-from-parent object))))

(define-method add-block world (object &optional x y prepend)
  (remove-thing-maybe self object)
  (add-block%super self object x y))

(define-method drop-block world (object x y)
  (add-object self object)
  (move-to object x y))

(define-method drop-object world (object &optional x y)
  (add-object self object)
  (when (and (numberp x) (numberp y))
    (move-to object x y))
  (after-drop-hook object))

(define-method drop-selection world ()
  (dolist (each (get-selection self))
    (drop-object self each)))

(define-method add-at-pointer world (object)
  (add-block self object *pointer-x* *pointer-y* :prepend)
  (focus-on self object))

(define-method add-message world ()
  (add-at-pointer self (new 'message)))

(define-method add-statement world ()
  (add-at-pointer self (new 'statement)))

(define-method add-variable world ()
  (add-at-pointer self (new 'variable)))

(define-method add-expression world ()
  (add-at-pointer self (new 'expression)))

(define-method add-field world ()
  (add-at-pointer self (new 'field)))

(define-method add-self world ()
  (add-at-pointer self (new 'self)))

(define-method contains-object world (object)
  (gethash (find-uuid object) 
	   %objects))

(define-method destroy-block world (object)
  (remhash (find-uuid object) %objects))

;;; World-local variables

(define-method initialize-variables-maybe world () 
  (when (null %variables) 
    (setf %variables (make-hash-table :test 'equal))
    (setf (gethash "WORLD" %variables) self)))

(define-method set-variable world (var value)
  (initialize-variables-maybe self)
  (setf (gethash var %variables) value))

(define-method get-variable world (var)
  (initialize-variables-maybe self)
  (gethash var %variables))

(defun world-variable (var-name)
  (get-variable (world) var-name))

(defun set-world-variable (var-name value)
  (set-variable (world) var-name value))

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

(defun playerp (thing)
  (object-eq thing (player)))

(define-method set-player world (player)
  (setf %player (find-uuid player)))
  ;; (unless (contains-object self player)
  ;;   (add-object self player)))

;;; Configuring the world's space and its quadtree indexing

(defparameter *world-bounding-box-scale* 1.01
  "Actual size of bounding box used for quadtree. The world is bordered
around on all sides by a thin margin designed to prevent objects near
the edge of the universe piling up into the top quadrant and causing
slowdown. See also quadtree.lisp")

(define-method install-quadtree world ()
  ;; make a box with a one-percent margin on all sides.
  ;; this margin helps edge objects not pile up in quadrants
  (let ((box (multiple-value-list
	      (scale-bounding-box 
	       (multiple-value-list (bounding-box self))
	       *world-bounding-box-scale*))))
    (with-fields (quadtree) self
      (setf quadtree (build-quadtree 
		      box 
		      (or %quadtree-depth 
			  *default-quadtree-depth*)))
      (assert quadtree)
      (let ((objects (get-objects self)))
	(when objects
	  (quadtree-fill objects quadtree))))))

(define-method resize world (new-height new-width)
  (assert (and (plusp new-height)
	       (plusp new-width)))
  (with-fields (height width quadtree objects) self
    (setf height new-height)
    (setf width new-width)
    (when quadtree
      (install-quadtree self))))

(define-method trim world ()
  (prog1 self
    (let ((objects (get-objects self)))
      (when objects
	(with-fields (quadtree height width) self
	  ;; adjust bounding box so that all objects have positive coordinates
	  (multiple-value-bind (top left right bottom)
	      (find-bounding-box objects)
	    ;; resize the world so that everything just fits
	    (setf %x 0 %y 0)
	    (resize self (- bottom top) (- right left))
	    ;; move all the objects
	    (dolist (object objects)
	      (with-fields (x y) object
		(with-quadtree quadtree
		  (move-to object (- x left) (- y top)))))))))))

;;; Cut and paste

(define-method get-selection world ()
  (let ((all (append (get-objects self) %inputs)))
   (remove-if-not #'%halo all)))

(define-method copy world (&optional objects0)
  (let ((objects (or objects0 (get-selection self))))
    (clear-halos self)
    (when objects
      (setf *clipboard* (new 'world))
      (dolist (object objects)
	(let ((duplicate (duplicate object)))
	  ;; don't keep references to anything in the (world)
	  (clear-world-data duplicate)
	  (add-object *clipboard* duplicate))))))

(define-method cut world (&optional objects0)
  (with-world self
    (let ((objects (or objects0 (get-selection self))))
      (when objects
	(clear-halos self)
	(setf *clipboard* (new 'world))
	(dolist (object objects)
	  (with-quadtree %quadtree
	    (remove-thing-maybe self object))
	  (add-object *clipboard* object))))))

(define-method paste-from world ((source block) (dx number :default 0) (dy number :default 0))
  (dolist (object (mapcar #'duplicate (get-objects source)))
    (with-fields (x y) object
      (clear-world-data object)
      (with-world self
	(with-quadtree %quadtree
	  (add-object self object)
	  (move-to object (+ x dx) (+ y dy)))))))
  
(define-method paste world ((dx number :default 0) (dy number :default 0))
  (paste-from self *clipboard* dx dy))
  
(define-method paste-here world ()
  (let ((temp (new 'world)))
    (paste-from temp *clipboard*)
    (send :trim temp)
    (paste-from self temp
		(window-pointer-x)
		(window-pointer-y))))

;; (define-method paste-cut 

;;; Algebraic operations on worlds and their contents

(defvar *world-prototype* "BLOCKY:WORLD")

(defmacro with-world-prototype (world &rest body)
  `(let ((*world-prototype* (find-super ,world)))
     ,@body))

(define-method adjust-bounding-box-maybe world ()
  (if (emptyp self)
      self
      (let ((objects-bounding-box 
	      (multiple-value-list 
	       (find-bounding-box (get-objects self)))))
	(destructuring-bind (top left right bottom)
	    objects-bounding-box
	  ;; are all the objects inside the existing box?
	  (prog1 self
	    (unless (bounding-box-contains 
		     (multiple-value-list (bounding-box self))
		     objects-bounding-box)
	      (resize self bottom right)))))))

(defmacro with-new-world (&body body)
  `(with-world (clone *world-prototype*) 
     ,@body
     (adjust-bounding-box-maybe (world))))

(defun translate (world dx dy)
  (when world
    (assert (and (numberp dx) (numberp dy)))
    (with-new-world 
      (paste (world) world dx dy))))

(defun combine (world1 world2)
  (with-new-world 
    (when (and world1 world2)
      (dolist (object (nconc (get-objects world1)
			     (get-objects world2)))
	(add-object (world) object)))))

(define-method scale world (sx &optional sy)
  (let ((objects (get-objects self)))
    (dolist (object objects)
      (with-fields (x y width height) object
	(move-to object (* x sx) (* y (or sy sx)))
	(resize object (* width sx) (* height (or sy sx))))))
  (trim self))

(define-method destroy-region world (bounding-box))

(defun vertical-extent (world)
  (if (or (null world)
	  (emptyp world))
      0
      (multiple-value-bind (top left right bottom)
	  (bounding-box world)
	(declare (ignore left right))
	(- bottom top))))

(defun horizontal-extent (world)
  (if (or (null world)
	  (emptyp world))
      0
      (multiple-value-bind (top left right bottom)
	  (bounding-box world)
	(declare (ignore top bottom))
	(- right left))))
  
(defun arrange-below (&optional world1 world2)
  (when (and world1 world2)
    (combine world1
	     (translate world2
			0 
			(field-value :height world1)))))

(defun arrange-beside (&optional world1 world2)
  (when (and world1 world2)
    (combine world1 
	     (translate world2
			(field-value :width world1)
			0))))

(defun stack-vertically (&rest worlds)
  (reduce #'arrange-below worlds :initial-value (with-new-world)))

(defun stack-horizontally (&rest worlds)
  (reduce #'arrange-beside worlds :initial-value (with-new-world)))

(define-method flip-horizontally world ()
  (let ((objects (get-objects self)))
    (dolist (object objects)
      (with-fields (x y) object
	(move-to object (- x) y))))
  ;; get rid of negative coordinates
  (trim self))

(define-method flip-vertically world ()
  (let ((objects (get-objects self)))
    (dolist (object objects)
      (with-fields (x y) object
	(move-to object x (- y)))))
  (trim self))

(define-method mirror-horizontally world ()
  (stack-horizontally 
   self 
   (flip-horizontally (duplicate self))))

(define-method mirror-vertically world ()
  (stack-vertically 
   self 
   (flip-vertically (duplicate self))))

(defun with-border (border world)
  (with-fields (height width) world
    (with-new-world 
      (paste (world) world border border) 
      (resize (world)
	      (+ height (* border 2))
	      (+ width (* border 2))))))

;;; The Shell is an optional layer of objects on top of the world

;; Including a system menu, editor, and controls for switching worlds
;; and pages in the system. Maybe zooming out on a mega virtual desktop.

(define-method add-listener-maybe world (&optional force)
  (when (or force (null *listener*))
    (setf *listener* (new 'listener))))

(define-method enter-listener world ()
  (add-listener-maybe self)
  (setf %last-focus %focused-block)
  (focus-on self *listener* :clear-selection nil)
  (when (null *listener-open-p*) (setf %was-key-repeat-p (key-repeat-p)))
  (setf *listener-open-p* t)
  (enable-key-repeat))
  
(define-method exit-listener world ()
  (when *listener-open-p*
    (add-listener-maybe self)
    (setf *listener-open-p* nil)
    (focus-on self %last-focus)
    (setf %last-focus nil)
    (unless %was-key-repeat-p 
      (disable-key-repeat))
    (setf %was-key-repeat-p nil)))

(define-method toggle-listener world ()
  (if *listener-open-p* 
      (exit-listener self)
      (enter-listener self)))

(define-method grab-focus world ())

(define-method layout-shell-objects world ()
  (mapc #'layout %inputs))

(define-method update-shell-objects world ()
  (mapc #'update %inputs)
  (when *listener* (update *listener*)))

(define-method draw-shell-objects world ()
  (with-world self
    (with-fields (drag-start inputs drag focused-block
			 highlight inputs modified hover
			 ghost prompt) self
      ;; now start drawing the shell objects
      (mapc #'draw inputs)
      ;; draw any future
      (when %future
	(let ((*image-opacity* 0.2))
	  (dolist (trail %future)
	    (mapc #'draw trail))))
      ;; during dragging we draw the dragged block.
      (when drag 
	(layout drag)
	(when (field-value :parent drag)
	  (draw-ghost ghost))
	;; also draw any hover-over highlights 
	;; on objects you might drop stuff onto
	(when hover 
	  (draw-hover hover))
	(draw drag))
      (when *listener*
	(draw *listener*))
      ;; draw focus
      (when focused-block
	(assert (blockyp focused-block))
	(draw-focus focused-block))
      (when highlight
	(draw-highlight highlight)))))

(define-method draw-overlays world ())

(define-method draw world ()
  (with-world self
    (with-field-values (objects width height background-image background-color) self
      (unless %parent 
	(project-window self))
      ;; (when %parent 
      ;; 	(gl:push-matrix)
      ;; 	(gl:translate %x %y 0))
      ;; draw background 
      (if background-image
	  (draw-image background-image 0 0)
	  (when background-color
	    (draw-box 0 0 width height
		      :color background-color)))
      ;; now draw the object layer
      (let ((box (multiple-value-list (window-bounding-box self))))
	(loop for object being the hash-values in objects do
	  ;; only draw onscreen objects
	  (when (colliding-with-bounding-box object box)
	    (draw object))))
      ;; possibly redraw player to ensure visibility.
      (when (and %player %redraw-player)
	(draw %player))
      ;; (if %parent
      ;; 	  (gl:pop-matrix)
      ;; possibly draw shell
      (if *listener-open-p* 
	  (draw-shell-objects self)
	  (draw-overlays self)))))
  
;;; Simulation update

(define-method update world ()
  (setf *world* (find-uuid self))
  (with-field-values (objects drag player) self
    ;; build quadtree if needed
    (when (null %quadtree)
      (install-quadtree self))
    (assert %quadtree)
    (unless %paused
      (with-world self
	;; enable quadtree for collision detection
	(with-quadtree %quadtree
	  ;; possibly run the objects
	  (loop for object being the hash-values in %objects do
	    (when object
	      (update object)
	      (run-tasks object)))
	  ;; update window movement
	  (let ((thing (or 
			%followed-object
			(when (holding-shift) drag)
			player)))
	    (when thing
	      (glide-follow self thing)
	      (update-window-glide self)))
	  ;; detect collisions
	  (loop for object being the hash-values in objects do
	    (unless (eq :passive (field-value :collision-type object))
	      (quadtree-collide object))))))
    ;; now outside the quadtree,
    ;; possibly update the shell layer
    (with-world self
      (when *listener-open-p*
	(with-quadtree nil
	  (layout self)
	  (layout-shell-objects self)
	  (update-shell-objects self))))))



;;; Running a world as a script

(define-method evaluate world ()
  (prog1 self
    (with-world self
      (mapc #'evaluate %inputs))))

(define-method layout world ()
  ;; take over the entire GL window
  (with-world self
    ;; (setf %x 0 %y 0)
	  ;; %width *gl-screen-width* 
	  ;; %height *gl-screen-height*)
    (mapc #'layout %inputs)
    (when *listener*
      (layout *listener*))))
  
(define-method handle-event world (event)
  (with-field-values (player quadtree focused-block) self
    (with-world self
      (or (block%handle-event self event)
	  (let ((thing
		  (if *listener-open-p* 
		      focused-block
		      player)))
	      (prog1 t 
		(when thing 
		  (with-quadtree quadtree
		    (handle-event thing event)))))))))

;;; Hit testing

(define-method hit world (x y)
  ;; return self no matter where mouse is, so that we get to process
  ;; all the events.
  (declare (ignore x y))
  self)

(define-method hit-inputs world (x y)
  "Recursively search the blocks in this world for a block
intersecting the point X,Y. We have to search the top-level blocks
starting at the end of `%INPUTS' and going backward, because the
blocks are drawn in list order (i.e. the topmost blocks for
mousing-over are at the end of the list.) The return value is the
block found, or nil if none is found."
  (with-world self 
    (with-quadtree %quadtree
      (labels ((try (b)
		 (when b
		   (hit b x y))))
	;; check listener and inputs first
	(let* ((object-p nil)
	       (result 
		 (or 
		  (when *listener-open-p* 
		    (try *listener*))
		  (let ((parent 
			  (find-if #'try 
				   %inputs
				   :from-end t)))
		    (when parent
		      (try parent)))
		  ;; try world objects
		  (block trying
		    (loop for object being the hash-values of %objects
			  do (let ((result (try object)))
			       (when result 
				 (setf object-p t)
				 (return-from trying result))))))))
	  (values result object-p))))))
  
(defparameter *minimum-drag-distance* 6)
  
(define-method clear-halos world ()
  (mapc #'destroy-halo (get-objects self)))

(define-method focus-on world (block &key (clear-selection t))
  ;; possible to pass nil
  (with-fields (focused-block) self
    (with-world self
      (let ((last-focus focused-block))
	;; there's going to be a new focused block. 
	;; tell the current one it's no longer focused.
	(when (and clear-selection last-focus
		   ;; don't do this for same block
		   (not (object-eq last-focus block)))
	  (lose-focus last-focus))
	(when clear-selection
	  (when (not (holding-control))
	    (clear-halos self)))
	;; now set up the new focus (possibly nil)
	(setf focused-block (when block 
			      (find-uuid 
			       (pick-focus block))))
	;; sanity check
	(assert (or (null focused-block)
		    (blockyp focused-block)))
	;; now tell the block it has focus, but only if not the same
	(when (and focused-block
		   (not (object-eq last-focus focused-block)))
	  (focus block))))))

(define-method begin-drag world (mouse-x mouse-y block)
  (with-fields (drag drag-origin inputs drag-start ghost drag-offset) self
    (with-world self
      (setf drag (as-drag block mouse-x mouse-y))
      (setf drag-origin (find-parent drag))
      (when drag-origin
	  ;; parent might produce a new object
	(unplug-from-parent block))
      (let ((dx (field-value :x block))
	    (dy (field-value :y block))
	    (dw (field-value :width block))
	    (dh (field-value :height block)))
	(with-fields (x y width height) ghost
	  ;; remember the relative mouse coordinates from the time the
	  ;; user began dragging, so that the block being dragged is not
	  ;; simply anchored with its top left corner located exactly at
	  ;; the mouse pointer.
	  (let ((x-offset (- mouse-x dx))
		(y-offset (- mouse-y dy)))
	    (when (null drag-start)
	      (setf x dx y dy width dw height dh)
	      (setf drag-start (cons dx dy))
	      (setf drag-offset (cons x-offset y-offset)))))))))

(define-method drag-maybe world (x y)
  ;; require some actual mouse movement to initiate a drag
  (with-world self
    (with-fields (focused-block drag-button click-start click-start-block) self
      (when click-start
	(destructuring-bind (x1 . y1) click-start
	  (when (and focused-block click-start-block
		     (> (distance x y x1 y1)
			*minimum-drag-distance*)
		     (can-pick click-start-block))
	    (let ((drag 
		    (if (and drag-button (= 3 drag-button))
			;; right-drag means "grab whole thing"
			(topmost click-start-block) 
			(pick click-start-block))))
	      (when drag 
		(begin-drag self x y drag)
		;; clear click data
		(setf click-start nil)
		(setf click-start-block nil)))))))))

(define-method handle-point-motion world (mouse-x mouse-y)
  (with-fields (inputs hover highlight click-start drag-offset quadtree
		       drag-start drag) self
    (with-world self
      (with-quadtree quadtree
	(setf hover nil)
	(drag-maybe self mouse-x mouse-y)
	(if drag
	    ;; we're in a mouse drag.
	    (destructuring-bind (ox . oy) drag-offset
	      (let ((target-x (- mouse-x ox))
		    (target-y (- mouse-y oy)))
		(let ((candidate (hit-inputs self target-x target-y)))
		  ;; obviously we dont want to plug a block into itself.
		  (setf hover (if (object-eq drag candidate) nil
				  (find-uuid candidate)))
		  ;; keep moving along with the mouse
		  (drag drag target-x target-y))))
	    ;; not dragging, just moving
	    (progn
	      (setf highlight (find-uuid (hit-inputs self mouse-x mouse-y)))))))))
    ;; (when (null highlight)
  ;;   (when *listener*
  ;;     (with-world self (close-menus *listener*))))))))

(define-method press world (x y &optional button)
  (with-world self
    (with-fields (click-start drag-button click-start-block
			      focused-block) self
      ;; now find what we're touching
      (assert (or (null focused-block)
		  (blockyp focused-block)))
      (multiple-value-bind (block object-p)
	  (hit-inputs self x y)
	(setf %object-p object-p)
	(if (null block)
	    (focus-on self nil)
	    ;; (when *listener-open-p*
	    ;; 	(exit-listener self)))
	    (progn 
	      (setf click-start (cons x y))
	      (setf click-start-block (find-uuid block))
	      (setf drag-button button)
	      ;; now focus; this might cause another block to be
	      ;; focused, as in the case of the Listener
	      (focus-on self block)))))))

(define-method clear-drag-data world ()
  (setf %drag-start nil
	%drag-offset nil
	%object-p nil
	%drag-origin nil
	%drag-button nil
	%drag nil
	%hover nil
	%highlight nil
	%last-focus nil
	%click-start-block nil
	%click-start nil))
  
(define-method release world (x y &optional button)
  (with-world self
    (with-fields 
	(drag-offset drag-start hover drag click-start drag-button
		     click-start-block drag-origin focused-block modified) self
      (if drag
	  ;; we're dragging
	  (destructuring-bind (x0 . y0) drag-offset
	    (setf drag-button nil)
	    (let ((drag-parent (get-parent drag))
		  (drop-x (- x x0))
		  (drop-y (- y y0)))
	      (if (not (can-escape drag))
		  ;; put back in halo or wherever
		  (when drag-origin 
		    (add-block drag-origin drag drop-x drop-y))
		  ;; ok, drop. where are we dropping?
		  (progn 
		    (when drag-parent
		      (unplug-from-parent drag))
		    (if %object-p
			(move-to drag drop-x drop-y)
			(if (null hover)
			    ;; dropping on background. 
			    (drop-object self drag)
			    ;; dropping on another block
			    (when (not (accept hover drag))
			      ;; hovered block did not accept drag. 
			      ;; drop it back in the shell.
			      (add-block self drag drop-x drop-y))))))
	      ;; select the dropped block
	      (progn 
;		(select self drag)
;		(toggle-halo drag)
		(setf focused-block (find-uuid drag)))))
	  ;;
	  ;; we were clicking instead of dragging
	  (progn
	    (when focused-block
;	      (select self focused-block)
	      (with-world self 
		(cond
		  ;; right click and alt click are equivalent
		  ((or (= button 3)
		       (and (holding-alt) (= button 1)))
		   (alternate-tap focused-block x y))
		  ;; scroll wheel click and shift click are equivalent
		  ((or (= button 2)
		       (and (holding-shift) (= button 1)))
		   (scroll-tap focused-block x y))
		  ;; vertical scrolling
		  ((= button 4)
		   (scroll-up focused-block))
		  ((= button 5)
		   (scroll-down focused-block))
		  ;; hold shift for horizontal scrolling
		  ((and (= button 4)
		        (holding-shift))
		   (scroll-left focused-block))
		  ((and (= button 5)
		        (holding-shift))
		   (scroll-right focused-block))
		  ;; plain old click
		  (t 
		   (tap focused-block x y))))
		;;(select self focused-block))
	      (setf click-start nil))))
      ;; close any ephemeral menus
      (dolist (input %inputs)
	(when (and (menup input)
		   (not (object-eq focused-block input)))
	  (destroy input)))
      ;; clean up bookeeping
      (clear-drag-data self)
      (invalidate-layout self))))

(define-method tab world (&optional backward)
  (when %focused-block
    (with-world self
      (tab %focused-block backward))))

(define-method backtab world ()
  (tab self :backward))
  
(define-method escape world ()
  (with-world self
    (focus-on self nil)
    (setf %selection nil)))

(define-method start world ()
  (with-world self
    (unless (emptyp self)
      (trim self))
    (start-alone self)))

(defun on-screen-p (thing)
  (contained-in-bounding-box 
   thing
   (multiple-value-list (window-bounding-box (world)))))

;;; Serialization of worlds

(define-method before-serialize world ()
  (clear-halos self))

;; (define-method after-serialize world ()
;;   (loop for id being the hash-keys of %objects do
;;     (setf (gethash id %objects) (find-object id))))

(define-method after-deserialize world ()
  (after-deserialize%super self)
  (clear-drag-data self)
  (add-listener-maybe self :force))

;;; worlds.lisp ends here
