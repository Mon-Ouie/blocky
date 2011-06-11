;;; blocks.lisp --- A visual programming language inspired by MIT Scratch

;; Copyright (C) 2010, 2011 David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: oop, languages, mouse, lisp, multimedia, hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:

;; This file implements an interactive visual programming language
;; called Blocky, based on Common Lisp. The Blocky language is
;; influenced heavily by Smalltalk environments like Squeak Morphic
;; and MIT Scratch, in that programs are assembled by the user from
;; reusable, interchangeable pieces (or "blocks") represented by
;; colored shapes arranged on a page. The arrangement and connection
;; of the different blocks on the page determine how the pieces behave
;; (collectively) as a program.

;; BYOB (Build Your Own Blocks) is an advanced dialect of Scratch
;; developed at Berkeley; it makes Scratch into a more
;; industrial-strength object-oriented language by supporting
;; first-class procedures, easy abstraction, and a macro-like facility
;; enabling much more powerful blocks to be defined in terms of
;; simpler ones.

;; With my project Blocky I am making Lisp-based visual programming
;; language very similar to BYOB, but with a pervasive Lisp flavor. In
;; addition there are some improvements, such as native OpenGL support
;; throughout, and of course the advantage of compiling your block
;; diagrams to optimized machine code via SBCL.

;; New block types and behaviors can be defined with the macro
;; `defblock' and subsequently replacing default methods of the base
;; block prototype via `define-method'. With the macro `make-block'
;; you can convert lisp expressions into working block
;; diagrams. Diagrams can be saved with `serialize' and `deserialize'.

;; For more information on the design of Ioforms, see
;; http://ioforms.org/design.html

;; For more information on similar systems, see the following links:

;; http://scratch.mit.edu/
;; http://byob.berkeley.edu/
;; http://wiki.scratch.mit.edu/wiki/Category:Scratch_Modifications
;; http://en.wikipedia.org/wiki/Visual_programming_language

;;; Code:

(in-package :ioforms)

(defvar *target*)

(defmacro with-target (target &body body)
  `(let ((*target* ,target))
     ,@body))

(define-prototype block ()
  (uuid :initform (make-uuid))
  ;; general information
  (inputs :initform nil :documentation 
"List of input blocks. If ^SCHEMA is also present, its corresponding
alist entries give the names and input types of the inputs here.")
  (schema :initform nil :documentation 
"Association list whose nth entry is the (:NAME . :TYPE) of the nth input.")
  (results :initform nil :documentation
"Computed result values from the input blocks.")
  (category :initform :data :documentation "Category name of block. See also `*block-categories*'.")
  (parent :initform nil :documentation "Link to enclosing parent block, or nil if none.")
  (events :initform nil :documentation "Event bindings, if any.")
  (default-events :initform nil)
  (operation :initform :block :documentation "Keyword name of method to be invoked on target.")
  (excluded-fields :initform '(:events :input-widths :results :parent))
  ;; visual layout
  (x :initform 0 :documentation "Integer X coordinate of this block's position.")
  (y :initform 0 :documentation "Integer Y coordinate of this block's position.")
  (z :initform 0 :documentation "Integer Z coordinate of this block's position.")
  (blend :initform :alpha)
  (width :initform 32 :documentation "Cached width of block.")
  (height :initform 32 :documentation "Cached height of block.")
  (depth :initform 32 :documentation "Cached depth of block.")
  (pinned :initform nil :documentation "When non-nil, do not allow dragging.")
  (visible :initform t :documentation "When non-nil, block will be visible.")
  (image :initform nil :documentation "Texture to be displayed, if any.")
  (input-widths :initform nil))

(defmacro defblock (name &body args)
  "Define a new block prototype named =NAME=.
ARGS are field specifiers, as with `define-prototype'."
  `(define-prototype ,name (:parent =block=)
    ,@args))

(define-method update block ()
  "Update the simulation one step forward in time."
  nil)

(define-method pin block ()
  (setf ^pinned t))

(define-method unpin block ()
  (setf ^pinned nil))

(define-method is-pinned block ()
  ^pinned)

(defun named-input-position (self name)
  (with-fields (schema) self
    (let ((index (position name schema :key #'first)))
      (if (numberp index)
	  index
	  (error "No such input ~S" name)))))

(defun named-input-type (self name)
  (with-fields (schema) self
    (let ((index (position name schema :key #'first)))
      (if (numberp index)
	  (cdr (assoc name schema))
	  (error "No such input ~S" name)))))

(defun input (self name)
  (with-fields (inputs) self
    (nth (named-input-position self name) inputs)))

(defun (setf input) (self name value)
  (with-fields (inputs) self
    (setf (nth (named-input-position self name) 
	       inputs) 
	  value)))

(define-method initialize block (&rest blocks)
  "Prepare an empty block, or if BLOCKS is non-empty, a block
initialized with BLOCKS as inputs."
  (with-fields (inputs schema results input-widths) self
    (let ((arity (length schema)))
      (when (not (zerop arity))
	(setf inputs (make-list arity))
	(setf results (make-list arity))
	(setf input-widths (make-list arity))
	(dotimes (n (length blocks))
	  (setf (nth n inputs)
		(nth n blocks)))))))

;;; Defining keyboard/mouse/joystick events for blocks

(define-method initialize-events-table-maybe block (&optional force)
  (when (or force 
	    (null (has-local-value :events self)))
    (setf ^events (make-hash-table :test 'equal))))

(define-method bind-event-to-function block (event-name modifiers func)
  "Bind the described event to invoke FUNC.
EVENT-NAME is a string giving the key name; MODIFIERS is a list of
keywords like :control, :alt, and so on."
  (initialize-events-table-maybe self)
  (let ((event (normalize-event (cons event-name modifiers))))
    (setf (gethash event ^events)
	  func)))

(define-method unbind-event block (event-name modifiers)
  "Remove the described event binding."
  (remhash (normalize-event (cons event-name modifiers))
	   ^events))

(define-method handle-event block (event)
  "Look up and invoke the function (if any) bound to EVENT. Return t
if a binding was found, nil otherwise. The second value returned is
the return value of the function (if any)."
  (with-fields (events) self
    (when events
      (let ((func (gethash event events)))
	(if func
	    (values t (funcall func))
	    (values nil nil))))))

(defun bind-event-to-method (block event-name modifiers method-name)
  (bind-event-to-function block (string-upcase event-name) modifiers
			  #'(lambda ()
			      (send method-name block))))

(define-method bind-event block (event binding)
  (destructuring-bind (name &rest modifiers) event
    (etypecase binding
      (symbol (bind-event-to-method self name modifiers binding))
      (list 
       (flet ((do-it ()
		(apply #'send 
		       (make-keyword (first binding))
		       self
		       (rest binding))))
	 (bind-event-to-function self name modifiers #'do-it))))))

(define-method bind-any-default-events block ()
  (with-fields (default-events) self
    (when default-events
      (initialize-events-table-maybe self)
      (dolist (entry default-events)
	(apply #'bind-event self entry)))))

(define-method initialize block (&rest args)
  (declare (ignore args))
  (bind-any-default-events self))

;;; Creating blocks from S-expressions

(defvar *make-block-package* nil)

(defun make-block-ext (value)
  (if (listp value)
      (if (and (symbolp (first value))
	       (not (boundp (make-special-variable-name (first value)))))
	  (let ((entry (clone =symbol=)))
	    (prog1 entry (set-data entry (first value))))
	  (destructuring-bind (operation &rest args) value
	    (let ((block (apply #'clone
				(symbol-value
				 (make-special-variable-name
				  operation
				  *make-block-package*))
				(mapcar #'make-block-ext args))))
	      (prog1 block
		(with-fields (inputs) block
		  (setf inputs
			(mapcar #'(lambda (value)
				    (or value (clone (symbol-value '=null=))))
				inputs))
		  (dolist (input inputs)
		    (set-parent input block)))))))
      (let ((prototype
	     (symbol-value
	      (make-special-variable-name
	       (etypecase value
		 ;; see also `defentry' below.
		 (integer :integer)
		 (float :float)
		 (string :string)
		 (symbol :symbol))))))
	(let ((entry (clone prototype)))
	  (prog1 entry
	    (set-data entry value))))))

(defmacro make-block (expression)
  "Expand EXPRESSION specifying a block diagram into real blocks.
EXPRESSION is of the form:

  (BLOCK-NAME ARG1 ARG2 ... ARGN)

Where BLOCK-NAME is the name of a prototype defined with `defblock'
and ARG1-ARGN are numbers, symbols, strings, or nested
EXPRESSIONS.

 (The equals signs are not needed around BLOCK-NAME.)"
  (assert expression)
  `(make-block-ext ',expression))

(defparameter *block-categories*
  '(:system :motion :event :message :looks :sound :structure :data
    :hover :control :comment :sensing :operators :variables)
  "List of keywords used to group blocks into different functionality
areas.")

(defparameter *input-types*
  '(:block :anything :integer :float :number :string :symbol)
  "List of keywords identifying the type of a particular input.")

(define-method move block (x y)
  "Move the block to a new (X Y) location."
  (setf ^x x)
  (setf ^y y))

(define-method move-toward block (direction &optional (steps 1))
  (multiple-value-bind (y x)
      (step-in-direction ^y ^x direction steps)
    (setf ^y y 
	  ^x x)))

(define-method show block ()
  (setf ^visible t))

(define-method hide block ()
  (setf ^visible nil))

(define-method toggle-visible block ()
  (if ^visible
      (hide self)
      (show self)))

(define-method is-visible block ()
  ^visible)

(define-method set-parent block (parent)
  "Store a link to an enclosing PARENT block, if any."
  (setf ^parent parent))

(define-method get-parent block ()
  ^parent)

(define-method get-image block ()
  ^image)

(define-method draw block ()
  (with-fields (image x y width height blend) self
    (if image 
	(progn (set-blending-mode blend)
	       (draw-image image x y))
	(draw-patch self x y (+ x width) (+ y height) :depressed t))))

(define-method mouse-move block (x y))

(define-method mouse-down block (x y button))

(define-method mouse-up block (x y button))

(define-method input-position block (input)
  (with-fields (inputs) self
    (position input inputs)))

(define-method this-position block ()
  (with-fields (parent) self
    (when parent
      (input-position parent self))))

(define-method plug block (input n)
  "Connect the block INPUT as the value of the Nth input."
  (set (input self n) input)
  (set-parent input self))

(define-method unplug block (input)
  "Disconnect the block INPUT from this block."
  (let ((pos (position input ^inputs)))
    (plug self (null-block) pos)
    (set-parent input nil)))

(define-method unplug-from-parent block ()
  (with-fields (parent) self
    (when parent
      (unplug parent self))))

(define-method execute-inputs block ()
  "Execute all blocks in ^INPUTS from left-to-right. Results are
placed in corresponding positions of ^RESULTS. Override this method
when defining new blocks if you don't want to evaluate all the
inputs all the time."
  (with-fields (inputs results) self
    (dotimes (n (length inputs))
      (when (nth n inputs)
	(setf (nth n results)
	      (run (nth n inputs)))))))

(define-method execute block ()
  "Carry out the block's action by sending messages to the object `*target*'.
The *target* is a special variable bound in the execution
environment. Its value will be the IOFORMS object to send messages to.
The ^RESULTS field will be a list of results obtained by
executing/evaluating the blocks in ^INPUTS (see also
`BLOCK/EXECUTE-INPUTS'.) The default behavior of `EXECUTE' is to
send the ^OPERATION field's value as a message to the target, with
the inputs to the target's method being the current computed
^RESULTS, and return the result of the method call. This default
action is sufficient for many blocks whose main purpose is to send a
single message; other blocks can redefine this /EXECUTE method to do
something else. See also `defblock' and `send'."
  (with-fields (operation results) self
    (labels ((clean (item)
	       (if (symbolp item)
		   (make-keyword item)
		   item)))
      (assert *target*)
      (apply #'ioforms:send nil operation *target*
	     (mapcar #'clean results)))))

(define-method run block ()
  "Run input blocks to produce results, then run this block with
those results as input."
  (execute-inputs self)
  (execute self))

(define-method describe block ()
  "Show name and comprehensive help for this block.")

(define-method after-deserialize block ()
  "Make sure the block is ready after loading."
  (initialize self))

(define-method count block ()
  "Return the number of blocks enclosed in this block, including the
current block. Used for taking a census."
  (with-fields (inputs) self
    (+ 1 (length inputs))))

(defparameter *display-widgets*
   '(:integer =integer=
     :float =float=
     :string =textbox=
     :symbol =option=)
  "A property list mapping some input type keywords to
corresponding IOFORMS:=WIDGET= prototypes used for editing that kind
of value.")

(defparameter *background-color* "white"
  "The default background color of the IOFORMS user interface.")

(defparameter *socket-color* "gray80"
  "The default background color of block sockets.")

(defparameter *block-font* "sans-condensed-bold-12"
  "The font used in drawing block labels and input data.")

(defvar *dash* 3
  "Size in pseudo-pixels of (roughly) the size of the space between
two words. This is used as a unit for various layout operations.")

(defvar *pseudo-pixel-size* 1.0
  "Size in pixels of a pseudo-pixel.")

(defparameter *block-colors*
  '(:motion "cornflower blue"
    :system "gray50"
    :event "gray80"
    :hover "red"
    :socket "gray60"
    :data "gray70"
    :structure "gray60"
    :comment "grey70"
    :looks "purple"
    :sound "orchid"
    :message "sienna3"
    :control "orange1"
    :variables "DarkOrange2"
    :operators "OliveDrab3"
    :sensing "DeepSkyBlue3")
  "X11 color names of the different block categories.")

(defparameter *block-highlight-colors*
  '(:motion "sky blue"
    :system "gray80"
    :hover "dark orange"
    :event "gray90"
    :comment "grey90"
    :looks "medium orchid"
    :socket "gray80"
    :data "gray80"
    :structure "gray80"
    :sound "plum"
    :message "sienna2"
    :control "gold"
    :variables "DarkOrange1"
    :operators "OliveDrab1"
    :sensing "DeepSkyBlue2")
  "X11 color names of highlights on the different block categories.")

(defparameter *block-shadow-colors*
  '(:motion "steel blue"
    :system "gray50"
    :event "gray70"
    :socket "gray90"
    :data "gray55"
    :structure "gray45"
    :comment "grey40"
    :hover "orange red"
    :looks "dark orchid"
    :sound "violet red"
    :message "chocolate3"
    :control "dark orange"
    :variables "OrangeRed2"
    :operators "OliveDrab4"
    :sensing "turquoise3")
  "X11 color names of shadows on the different block categories.")

(defparameter *block-foreground-colors*
  '(:motion "white"
    :system "white"
    :event "gray40"
    :comment "gray30"
    :socket "gray20"
    :hover "yellow"
    :data "white"
    :structure "gray20"
    :message "white"
    :looks "white"
    :sound "white"
    :control "white"
    :variables "white"
    :operators "white"
    :sensing "white")
  "X11 color names of the text used for different block categories.")

(define-method find-color block (&optional (part :background))
  "Return the X11 color name of this block's category as a string.
If PART is provided, return the color for the corresponding
part (:BACKGROUND, :SHADOW, :FOREGROUND, or :HIGHLIGHT) of this category
of block."
  (let ((colors (ecase part
		  (:background *block-colors*)
		  (:highlight *block-highlight-colors*)
		  (:shadow *block-shadow-colors*)
		  (:foreground *block-foreground-colors*))))
    (getf colors ^category)))

(defparameter *selection-color* "red")

(defmacro with-block-drawing (&body body)
  "Run BODY forms with drawing primitives.
The primitives are CIRCLE, DISC, LINE, BOX, and TEXT. These are used
in subsequent functions as the basis of drawing nested diagrams of
blocks."
    `(let* ((foreground (find-color self :foreground))
	    (background (find-color self :background))
	    (highlight (find-color self :highlight))
	    (selection *selection-color*)
	    (shadow (find-color self :shadow))
	    (dash *dash*)
	    (radius (+ 2 *dash*))
	    (diameter (* 2 radius)))
       (labels ((circle (x y &optional color)
		  (draw-circle x y radius
			       :color (or color background)))
		(disc (x y &optional color)
		  (draw-solid-circle x y radius
				     :color (or color background)))
		(line (x0 y0 x1 y1 &optional color)
		  (draw-line x0 y0 x1 y1
			     :color (or color background)))
		(box (x y r b &optional color)
		  (draw-box x y (- r x) (- b y)
			    :color (or color background)))
		(text (x y string)
		  (draw-string string x y
			       :color foreground
			       :font *block-font*)))
	   ,@body)))

(define-method draw-patch block (x0 y0 x1 y1
				    &key depressed dark socket color)
  "Draw a standard IOFORMS block notation patch.
Top left corner at (X0 Y0), bottom right at (X1 Y1). If DEPRESSED is
non-nil, draw an indentation; otherwise a raised area is drawn. If
DARK is non-nil, paint a darker region. If SOCKET is non-nil, cut a hole
in the block where the background shows through. If COLOR is non-nil,
override all colors."
  (with-block-drawing 
    (let ((bevel (or color (if depressed shadow highlight)))
	  (chisel (or color (if depressed highlight shadow)))
	  (fill (or color (if socket
			      *socket-color*
			      (if dark shadow background)))))
      ;; top left
      (disc (+ x0 radius 1) (+ y0 radius) fill)
      (circle (+ x0 radius 1) (+ y0 radius) bevel)
      ;; top x1
      (disc (- x1 radius 1) (+ y0 radius 1) fill)
      (circle (- x1 radius 1) (+ y0 radius 1) chisel)
      ;; y1 x1
      (disc (- x1 radius 1) (- y1 radius 1) fill)
      (circle (- x1 radius 1) (- y1 radius 1) chisel)
      ;; y1 left
      (disc (+ x0 radius 1) (- y1 radius 1) fill)
      (circle (+ x0 radius 1) (- y1 radius 1))
      ;; y1
      (box (+ x0 radius) (- y1 diameter)
	   (- x1 radius 1) y1
	   fill)
      (line (+ x0 radius 1) y1
	    (- x1 radius 1) y1 chisel)
      ;; top
      (box (+ x0 radius) y0
	   (- x1 radius) (+ y0 diameter)
	   fill)
      (line (+ x0 radius 1) y0
	    (- x1 radius 1) y0 bevel)
      ;; left
      (box x0 (+ y0 radius)
	   (+ x0 diameter) (- y1 radius)
	   fill)
      (line x0 (+ y0 radius 1)
	    x0 (- y1 radius 1) bevel)
      ;; x1
      (box (- x1 diameter) (+ y0 radius)
	   x1 (- y1 radius)
	   fill)
      (line x1 (+ y0 radius 1)
	    x1 (- y1 radius 1) chisel)
      ;; content area
      (box (+ x0 radius) (+ y0 radius)
	   (- x1 radius) (- y1 radius)
	   fill))))

(define-method draw-socket block (x0 y0 x1 y1)
  (draw-patch self x0 y0 x1 y1 :depressed t :socket t))

(define-method draw-border block (&optional (color *selection-color*))
  (let ((dash *dash*))
    (with-fields (x y height width) self
      (draw-patch self (- x dash) (- y dash)
		   (+ x width dash)
		   (+ y height dash)
		   :color color))))

(define-method draw-background block ()
  (with-fields (x y width height) self
    (draw-patch self x y (+ x width) (+ y height))))

(define-method draw-ghost block ()
  (with-fields (x y width height) self
    (draw-patch self x y (+ x width) (+ y height)
		 :depressed t :socket t)))

(define-method handle-width block ()
  (+ (* 2 *dash*)
     (expression-width ^operation)))

(defparameter *socket-width* (* 18 *dash*))

(defun expression-width (expression &optional (font *block-font*))
  (if (ioforms:object-p expression)
      *socket-width*
      (font-text-extents (print-expression expression) font)))

(defun print-expression (expression)
  (string-downcase
   (typecase expression
     (symbol
	(substitute #\Space #\- (symbol-name expression)))
     (otherwise (format nil "~s" expression)))))

(define-method layout block ()
  (with-fields (input-widths height width) self
    (with-field-values (x y operation schema inputs) self
      (let* ((font *block-font*)
	     (dash *dash*)
	     (left (+ x (handle-width self)))
	     (max-height (font-height font)))
	(labels ((move-input (input)
		   (move input (+ left dash) y)
		   (layout input)
		   (setf max-height (max max-height (field-value :height input)))
		   (field-value :width input))
		 (layout-input (block category)
		   (let ((measurement
			  (+ dash (move-input block))))
		     (prog1 measurement
		       (incf left measurement)))))
	  (setf input-widths (mapcar #'layout-input inputs schema)))
	  (setf width (+ (- left x) (* 4 dash)))
	  (setf height (+ dash dash max-height))))))

(define-method draw-expression block (x0 y0 segment type)
  (with-fields (height input-widths) self
    (let ((dash *dash*)
	  (width *socket-width*))
      (if (eq type :block)
	  ;; draw a socket if there's no block; otherwise wait
	  ;; until later to draw.
	  (when (null segment)
	    (draw-socket self (+ x0 dash) (+ y0 dash)
			 (+ x0 *socket-width*)
			 (+ y0 (- height dash))))
	  (progn
	    (text x0 (+ y0 dash 1)
		  (print-expression segment))
		(setf width (expression-width segment))))
      width)))

(define-method after-deserialize block ()
  "Make sure the block is ready after loading."
  (initialize self))

(define-method count block ()
  "Return the number of blocks enclosed in this block, including the
current block."
  (with-fields (inputs) self
    (+ 1 (length inputs))))

(define-method draw-contents block ()
  (with-block-drawing 
    (with-field-values
	(x y operation inputs)
	self
      (let* ((dash *dash*)
	     (left (+ x (* 2 dash)))
	     (y0 (+ y dash 1)))
	(text left y0 (print-expression operation))
	(dolist (each inputs)
	  (draw each))))))

(defparameter *hover-color* "red")

(define-method draw-hover block ()
  (with-fields (x y width height) self
    (draw-box x y (+ *dash* width) (+ *dash* height)
	      :color *hover-color*)
    (draw-contents self)))

(define-method hit block (mouse-x mouse-y)
  "Return this block (or input block) if the coordinates MOUSE-X and
MOUSE-Y identify a point inside the block (or input block.)"
  (with-fields (x y width height inputs) self
    (when (within-extents mouse-x mouse-y x y
			  (+ x width) (+ y height))
      (message "HIT BLOCK? ~S" (list x y))
      (labels ((try (it)
		 (hit it mouse-x mouse-y)))
	(let ((result (some #'try inputs)))
	  (message "HIT2: RESULT: ~S" result)
	  (or result self))))))

(define-method accept block (other-block)
  (with-field-values (parent) self
    (when parent
      (prog1 t
	(let ((position (input-position parent self)))
	  (assert (integerp position))
	  (plug parent other-block position))))))

;;; Data entry blocks

(defblock entry
  (category :initform :data)
  (data :initform nil))

(define-method execute entry ()
  ^data)

(define-method set-data entry (data)
  (setf ^data data))

(define-method draw entry ()
  (with-block-drawing
    (with-fields (x y data parent) self
      (when (null parent) (draw-background self))
      (draw-contents self))))

(define-method draw-contents entry ()
  (with-block-drawing
    (with-fields (data x y) self
      (text (+ x (* 2 *dash*))
	    (+ y *dash* 1)
	    (print-expression data)))))

(define-method layout entry ()
  (with-fields (height width data) self
    (setf height (+ (* 2 *dash*) (font-height *block-font*)))
    (setf width (+ (* 4 *dash*) (expression-width data)))))

(defmacro defentry (name data)
  `(define-prototype ,name (:parent =entry=)
     (operation :initform ,(make-keyword name))
     (data :initform ,data)))

(defentry integer 0)
(defentry string "")
(defentry float 0.0)
(defentry symbol nil)

;;; Vertically stacked list of blocks

(defblock list
  (category :initform :structure))

(defparameter *null-display-string* "()")

(defun null-block () (clone =list=))

(define-method execute list ()
  ^results)

(define-method accept list (input &optional prepend)
  (with-fields (inputs) self
    (if inputs
	(if prepend
	    (setf inputs (nconc (list input) inputs))
	    (setf inputs (nconc inputs (list input))))
	(setf inputs (list input)))
    (when (get-parent input)
      (unplug-from-parent input))
    (set-parent input self)))

(define-method take-first list ()
  (with-fields (inputs) self
    (let ((block (first inputs)))
      (prog1 block
	(unplug self block)))))

(define-method length list ()
  (with-fields (inputs) self
    (length inputs)))

(define-method unplug list (input)
  (with-fields (inputs) self
    (setf inputs (delete input inputs))
    (set-parent input nil)))

(define-method layout-header list () 0)

(define-method layout-body-as-null list ()
  (with-fields (height width) self
    (setf width (+ (* 4 *dash*)
		   (font-text-extents *null-display-string*
				      *block-font*))
	  height (+ (* 4 *dash*)))))

(define-method layout-body-as-list list ()
  (with-fields (x y height width inputs) self
    (let* ((dash *dash*)
	   (header-height (+ dash (layout-header self)))
	   (y0 (+ y dash header-height))
	   (line-height (font-height *block-font*)))
      (setf height (+ (* 2 dash) line-height))
      (setf width (* 8 dash))
      (dolist (block inputs)
	(move block (+ x dash) y0)
	(layout block)
	(incf height (field-value :height block))
	(incf y0 (field-value :height block))
	(setf width (max width (field-value :width block))))
      (incf width (* 2 dash)))))

(define-method layout list ()
  (with-fields (inputs) self
    (if (null inputs)
	(layout-body-as-null self)
	(layout-body-as-list self))))

(define-method draw-header list () 0)

;;; blocks.lisp ends here
