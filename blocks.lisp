;;; blocks.lisp --- A visual programming language inspired by MIT Scratch

;; Copyright (C) 2010, 2011 David O'Toole

;; Author: David O'Toole ^dto@gnu.org
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
;; along with this program.  If not, see ^http://www.gnu.org/licenses/.

;;; Commentary:

;; This file implements a drag-and-drop visual programming language in
;; the style of Smalltalk environments such as Squeak Morphic and MIT
;; Scratch, but with a Lisp flavor. For more information see:

;; http://scratch.mit.edu/
;; http://byob.berkeley.edu/
;; http://wiki.scratch.mit.edu/wiki/Category:Scratch_Modifications
;; http://en.wikipedia.org/wiki/Visual_programming_language

;;; Code:

(in-package :ioforms)

;; The purpose of a block is to perform some action in response to a
;; number of input arguments and then return a value. Each argument is
;; itself a block and there are prebuilt block types for integers,
;; strings, symbols, and lists. To run a block diagram we proceed
;; depth-first to the leaves and execute those to obtain values, then
;; propagate results up the tree until the outermost block is executed
;; using the propagated results as its input values. See also
;; `BLOCK/RUN'.

;; New block types and behaviors can be defined with the macro
;; `defblock' and subsequently replacing default methods of the base
;; block prototype via `define-method'. With the macro `make-block'
;; you can convert lisp expressions into working block
;; diagrams. Diagrams can be saved with `serialize' and `deserialize'.

;; For more information, see http://ioforms.org/design.html

(defvar *target*)

(define-prototype block ()
  (name :initform nil)
  (pinned :initform nil :documentation "When non-nil, do not allow dragging.")
  (arguments :initform nil :documentation "List of block argument values.")
  (results :initform nil :documentation "Computed output values. See `BLOCK/EXECUTE'.")
  (schema :documentation 
	  "List of type keywords for corresponding expressions in ^arguments.
See also `*argument-types*'.")
  (operation :initform :block :documentation "Keyword name of method to be invoked on target.")
  (type :initform :data :documentation "Type name of block. See also `*block-types*'.")
  (x :initform 0 :documentation "Integer X coordinate of this block's position.")
  (y :initform 0 :documentation "Integer Y coordinate of this block's position.")
  (width :initform 32 :documentation "Cached width of block.")
  (height :initform 32 :documentation "Cached height of block.")
  (parent :initform nil :documentation "Link to enclosing parent block, or nil if none.")
  (data :initform nil :documentation "Data value for data entry blocks.")
  (image :initform nil :documentation "Offscreen buffer image, if any.")
  (visible :initform t :documentation "When non-nil, block will be visible.")
  (event-map :initform nil :documentation "Event bindings, if any.")  
  (child-widths :initform nil :documentation "List of widths of visual block segments. See `BLOCK/LAYOUT'.")
  (excluded-fields :initform '(:image :event-map :child-widths :results :parent)))

(defmacro defblock (name &body args)
  "Define a new block prototype named =NAME=.
ARGS are field specifiers, as with `define-prototype'."
  `(define-prototype ,name (:parent =block=)
     (operation :initform ,(make-keyword name))
     ,@args))

(define-method pin block ()
  (setf ^pinned t))

(define-method unpin block ()
  (setf ^pinned nil))

(define-method is-pinned block ()
  ^pinned)

;;; Defining input events for blocks

(define-method initialize-event-map-maybe block ()
  (with-fields (event-map) self
    (when (null event-map)
      (setf event-map (make-hash-table :test 'equal)))))

(define-method bind-event-to-function block (event-name modifiers func)
  "Bind the described event to invoke FUNC.
EVENT-NAME is a string giving the key name; MODIFIERS is a list of
keywords like :control, :alt, and so on."
  (initialize-event-map-maybe self)
  (setf (gethash (normalize-event (cons event-name modifiers))
		 ^event-map)
	func))

(define-method unbind-event block (event-name modifiers)
  "Remove the described event binding."
  (remhash (normalize-event (cons event-name modifiers))
	   ^event-map))

(define-method clear-event-map block ()
  (setf ^event-map (make-hash-table :test 'equal)))

(define-method handle-event block (event)
  "Look up and invoke the function (if any) bound to EVENT. Return t
if a binding was found, nil otherwise."
  (initialize-event-map-maybe self)
  (with-fields (event-map) self
      (when event-map 
	(let ((func (gethash event event-map)))
	  (when func
	    (prog1 t
	      (funcall func)))))))

(defun bind-event-to-prompt-insertion (prompt event-name modifiers &optional (insertion event-name))
  "For prompt PROMPT ensure that the event (EVENT-NAME MODIFIERS)
causes the text INSERTION to be inserted at point."
 (bind-event-to-function prompt (string-upcase event-name) modifiers
	      #'(lambda ()
		  (insert-string prompt insertion))))

(defun bind-event-to-method (block event-name modifiers method-name)
  (bind-event-to-function block (string-upcase event-name) modifiers
			  #'(lambda ()
			      (send method-name block))))

(defmacro bind-event (self event binding)
  (destructuring-bind (name &rest modifiers) event
    (etypecase binding
      (symbol `(bind-event-to-method ,self ,name ',modifiers ,binding))
      (list `(bind-event-to-function ,self ,name ',modifiers
				     #'(lambda ()
					 (,(first binding)
					   self
					   ,@(rest binding))))))))

(define-method generic-keybind block (binding) 
  (destructuring-bind (event modifiers data) binding
    (apply (etypecase data
	     (keyword #'bind-event-to-method)
	     (string #'bind-event-to-prompt-insertion))
	   self binding)))

;;; Creating blocks from textual lisp expressions

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
		(with-fields (arguments) block
		  (setf arguments 
			(mapcar #'(lambda (value)
				    (or value (clone (symbol-value '=null=))))
				arguments))
		  (dolist (child arguments)
		    (set-parent child block)))))))
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

(defparameter *block-types*
  '(:system :motion :event :message :looks :sound :structure :data
    :hover :control :comment :sensing :operators :variables)
  "List of keywords used to group blocks into different functionality
areas.")

(defparameter *argument-types*
  '(:block :anything :integer :float :number :string :symbol)
  "List of keywords identifying the type of a particular argument.")

(define-method move block (x y)
  "Move the block to a new (X Y) location."
  (setf ^x x)
  (setf ^y y))

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

(define-method get-argument block (n)
  "Return the value of the Nth block argument."
  (nth n ^arguments))

(define-method set-argument block (n value)
  "Set the Nth argument value to VALUE."
    (setf (nth n ^arguments) value))

(define-method child-position block (child)
  (with-fields (arguments) self
    (position child arguments)))

(define-method position block ()
  (with-fields (parent) self
    (when parent 
      (child-position parent self))))

(define-method plug block (child n)
  "Connect the block CHILD as the value of the Nth argument."
  (set-argument self n child)
  (set-parent child self))

(define-method unplug block (child)
  "Disconnect the block CHILD from this block."
  (let ((pos (position child ^arguments)))
    (plug self (null-block) pos)
    (set-parent child nil)))

(define-method unplug-from-parent block ()
  (with-fields (parent) self
    (when parent
      (unplug parent self))))

(define-method execute-arguments block ()
  "Execute all blocks in ^ARGUMENTS from left-to-right. Results are
placed in corresponding positions of ^RESULTS. Override this method
when defining new blocks if you don't want to evaluate all the
arguments all the time."
  (with-fields (arguments results) self
    (setf results (mapcar #'/run arguments))))

(define-method execute block ()
  "Carry out the block's action by sending messages to the object `*target*'.
The *target* is a special variable bound in the execution
environment. Its value will be the IOFORMS object to send messages to.
The ^RESULTS field will be a list of results obtained by
executing/evaluating the blocks in ^ARGUMENTS (see also
`BLOCK/EXECUTE-ARGUMENTS'.) The default behavior of `EXECUTE' is to
send the ^OPERATION field's value as a message to the target, with
the arguments to the target's method being the current computed
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

(defmacro with-target (target &body body)
  `(let ((*target* ,target))
     ,@body))

(define-method run block ()
  "Run child blocks to produce results, then run this block with
those results as input."
  (execute-arguments self)
  (execute self))

(define-method update block (&rest args)
  "Update the simulation one step forward in time.")
;;  (mapc #'update ^arguments)) ;; TODO not by default

(define-method describe block ()
  "Show name and comprehensive help for this block.")

(define-method initialize block (&rest args)
  "Prepare an empty block, or if ARGS is non-empty, a block
initialized with its values as arguments."
  (with-fields (arguments schema results) self
    (let ((arity (length schema)))
      (setf arguments (make-list arity))
      (setf results (make-list arity))
      (dotimes (n (length args))
	(setf (nth n arguments)
	      (nth n args))))))

(define-method deserialize block ()
  "Make sure the block is ready after loading."
  (initialize self))

(define-method count block ()
  "Return the number of blocks enclosed in this block, including the
current block." 
  (with-fields (arguments) self
    (+ 1 (length arguments))))

(defparameter *display-widgets*
   '(:integer =integer=
     :float =float=
     :string =textbox=
     :symbol =option=)
  "A property list mapping some argument type keywords to
corresponding IOFORMS:=WIDGET= prototypes used for editing that kind
of value.")

(defparameter *background-color* ".white" 
  "The default background color of the IOFORMS user interface.")

(defparameter *socket-color* ".gray80" 
  "The default background color of block sockets.")

(defparameter *block-font* "sans-condensed-bold-12"
  "The font used in drawing block labels and argument data.")

(defvar *dash* 3 
  "Size in pseudo-pixels of (roughly) the size of the space between
two words. This is used as a unit for various layout operations.")

(defvar *pseudo-pixel-size* 1.0
  "Size in pixels of a pseudo-pixel.")

(defparameter *block-colors* 
  '(:motion ".cornflower blue"
    :system ".gray50"
    :event ".gray80"
    :hover ".red"
    :socket ".gray60"
    :data ".gray70"
    :structure ".gray60"
    :comment ".grey70"
    :looks ".purple"
    :sound ".orchid"
    :message ".sienna3"
    :control ".orange1"
    :variables ".DarkOrange2"
    :operators ".OliveDrab3"
    :sensing ".DeepSkyBlue3")
  "X11 color names of the different block types.")

(defparameter *block-highlight-colors*
  '(:motion ".sky blue"
    :system ".gray80"
    :hover ".dark orange"
    :event ".gray90"
    :comment ".grey90"
    :looks ".medium orchid"
    :socket ".gray80"
    :data ".gray80"
    :structure ".gray80"
    :sound ".plum"
    :message ".sienna2"
    :control ".gold"
    :variables ".DarkOrange1"
    :operators ".OliveDrab1"
    :sensing ".DeepSkyBlue2")
  "X11 color names of highlights on the different block types.")

(defparameter *block-shadow-colors* 
  '(:motion ".steel blue"
    :system ".gray50"
    :event ".gray70"
    :socket ".gray90"
    :data ".gray55"
    :structure ".gray45"
    :comment ".grey40"
    :hover ".orange red"
    :looks ".dark orchid"
    :sound ".violet red"
    :message ".chocolate3"
    :control ".dark orange"
    :variables ".OrangeRed2"
    :operators ".OliveDrab4"
    :sensing ".turquoise3")
  "X11 color names of shadows on the different block types.")

(defparameter *block-foreground-colors* 
  '(:motion ".white"
    :system ".white"
    :event ".gray40"
    :comment ".gray30"
    :socket ".gray20"
    :hover ".yellow"
    :data ".white"
    :structure ".gray20"
    :message ".white"
    :looks ".white"
    :sound ".white"
    :control ".white"
    :variables ".white"
    :operators ".white"
    :sensing ".white")
  "X11 color names of the text used for different block types.")
 
(define-method find-color block (&optional (part :background))
  "Return the X11 color name of this block's type as a string.
If PART is provided, return the color for the corresponding 
:BACKGROUND, :SHADOW, :FOREGROUND, or :HIGHLIGHT parts of this type of
block."
  (let ((colors (ecase part
		  (:background *block-colors*)
		  (:highlight *block-highlight-colors*)
		  (:shadow *block-shadow-colors*)
		  (:foreground *block-foreground-colors*))))
    (getf colors ^type)))

(defparameter *selection-color* ".red")

(define-method create-image block ()
  (with-fields (image height width) self
    (let ((oldimage image))
      (when oldimage
	(sdl:free oldimage))
      (setf image (create-image width height)))))

(define-method resize block (&key height width)
  "Allocate an image buffer of HEIGHT by WIDTH pixels.
If there is no existing image, one of HEIGHT x WIDTH pixels is created
and stored in ^IMAGE. If there is an existing image, it is only
resized when the new dimensions differ from the existing image."
  (assert (and (integerp width) (integerp height)))
  (with-fields (image) self
    (if (null image) 
	(progn (setf ^width width 
		     ^height height)
	       (create-image self))
	(when (not (and (= ^width width) 
			(= ^height height)))
	  (setf ^width width 
		^height height) 
	  (when image (create-image self))))))

(defmacro with-block-drawing (image &body body)
  "Run BODY forms with drawing primitives set to draw on IMAGE.
The primitives are CIRCLE, DISC, LINE, BOX, and TEXT. These are used
in subsequent functions as the basis of drawing nested diagrams of
blocks."
  (let ((image-sym (gensym)))
    `(let* ((foreground (find-color self :foreground))
	    (background (find-color self :background))
	    (highlight (find-color self :highlight))
	    (selection *selection-color*)
	    (shadow (find-color self :shadow))
	    (dash *dash*)
	    (radius *dash*)
	    (diameter (* 2 radius))
	    (,image-sym ,image))
       (labels ((circle (x y &optional color)
		  (draw-aa-circle x y radius 
				  :color (or color background)
				  :destination ,image-sym))
		(disc (x y &optional color)
		  (draw-filled-circle x y radius
				      :color (or color background)
				      :destination ,image-sym))
		(line (x0 y0 x1 y1 &optional color)
		  (draw-line x0 y0 x1 y1 
			     :color (or color background)
			     :destination ,image-sym))
		(box (x y r b &optional color)
		  (draw-box x y (- r x) (- b y)
			    :color (or color background)
			    :stroke-color (or color background)
			    :destination ,image-sym))
		(text (x y string)
		  (draw-string-blended string x y
				       :foreground foreground
				       :destination ,image-sym
				       :font *block-font*)))
	   ,@body))))

(define-method draw-patch block (x0 y0 x1 y1 image 
				    &key depressed dark socket color)
  "Draw a standard IOFORMS block notation patch on IMAGE.
Top left corner at (X0 Y0), bottom right at (X1 Y1). If DEPRESSED is
non-nil, draw an indentation; otherwise a raised area is drawn. If
DARK is non-nil, paint a darker region. If SOCKET is non-nil, cut a hole
in the block where the background shows through. If COLOR is non-nil,
override all colors."
  (with-block-drawing image
    (let ((bevel (or color (if depressed shadow highlight)))
	  (chisel (or color (if depressed highlight shadow)))
	  (fill (or color (if socket
			      *socket-color* 
			      (if dark shadow background)))))
      ;; top left
      (disc (+ x0 radius) (+ y0 radius) fill)
      (circle (+ x0 radius) (+ y0 radius) bevel)
      ;; top x1
      (disc (- x1 radius 1) (+ y0 radius) fill)
      ;; y1 x1
      (disc (- x1 radius 1) (- y1 radius 1) fill)
      (circle (- x1 radius 1) (- y1 radius 1) chisel)
      ;; y1 left
      (disc (+ x0 radius) (- y1 radius 1) fill)
      (circle (+ x0 radius) (- y1 radius 1))
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

(define-method draw-socket block (x0 y0 x1 y1 image)
  (draw-patch self x0 y0 x1 y1 image :depressed t :socket t))
    
(define-method draw-border block (image &optional (color *selection-color*))
  (let ((dash *dash*))
    (with-fields (x y height width) self
      (draw-patch self (- x dash) (- y dash)
		   (+ x width dash)
		   (+ y height dash)
		   image :color color))))

(define-method draw-background block (image)
  (with-fields (x y width height) self
    (draw-patch self x y (+ x width) (+ y height) image)))

(define-method draw-ghost block (image)
  (with-fields (x y width height) self
    (draw-patch self x y (+ x width) (+ y height) image
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
  (with-fields (child-widths height width) self
    (with-field-values (x y operation schema arguments) self
      (let* ((font *block-font*)
	     (dash *dash*)
	     (left (+ x (handle-width self)))
	     (max-height (font-height font)))
	(labels ((move-child (child)
		   (move child (+ left dash) y)
		   (layout child)
		   (setf max-height (max max-height (field-value :height child)))
		   (field-value :width child))
		 (layout-child (block type)
		   (let ((measurement
			  (+ dash (move-child block))))
		     (prog1 measurement 
		       (incf left measurement)))))
	  (setf child-widths (mapcar #'layout-child arguments schema)))
	  (setf width (+ (- left x) (* 4 dash)))
	  (setf height (+ dash dash max-height))))))

(define-method draw-expression block (x0 y0 segment type image)
  (with-block-drawing image
      (with-fields (height child-widths) self
	(let ((dash *dash*)
	      (width *socket-width*))
	  (if (eq type :block)
	      ;; draw a socket if there's no block; otherwise wait
	      ;; until later to draw.
	      (when (null segment) 
		(draw-socket self (+ x0 dash) (+ y0 dash)
			      (+ x0 *socket-width*)
			      (+ y0 (- height dash))
			      image))
	      (progn 
		(text x0 (+ y0 dash 1)
		      (print-expression segment))
		(setf width (expression-width segment))))
	  width))))

(define-method render block ())

(define-method draw-contents block (image)
  (with-block-drawing image
    (with-field-values 
	(x y operation arguments)
	self
      (let* ((dash *dash*)
	     (left (+ x (* 2 dash)))
	     (y0 (+ y dash 1)))
	(if ^image
	    (progn 
	      (render self)
	      (draw-image ^image
			  left y0 :destination image))
	    (progn 
	      (text left y0 (print-expression operation))
	      (dolist (block arguments)
		(draw block image))))))))

(define-method draw block (output-image)
  (with-fields (image x y) self
    (if (null image)
	(progn
	  (draw-background self output-image)
	  (draw-contents self output-image))
	(progn
	  (render self)
	  (draw-image image x y 
		      :destination output-image)))))

(defparameter *hover-color* ".red")

(define-method draw-hover block (image)
  (with-fields (x y width height) self
    (draw-box x y (+ *dash* width) (+ *dash* height)
	      :stroke-color *hover-color* 
	      :color *hover-color*
	      :destination image))
  (draw-contents self image))
		    
(define-method hit block (mouse-x mouse-y)
  "Return this block (or child block) if the coordinates MOUSE-X and
MOUSE-Y identify a point inside the block (or child block.)"
  (with-fields (x y width height arguments) self
    (when (within-extents mouse-x mouse-y x y 
			  (+ x width) (+ y height))
      (labels ((hit (block)
		 (/hit block mouse-x mouse-y)))
	(let ((child (some #'hit arguments)))
	  (values (or child self) (when child (/position child))))))))
     	
(define-method accept block (other-block)
  (with-field-values (parent) self
    (when parent
      (prog1 t
	(let ((position (child-position parent self)))
	  (assert (integerp position))
	  (plug parent other-block position))))))

;;; Data entry blocks

(defblock entry 
  (type :initform :data)
  (schema :iniform nil)
  (data :initform nil))

(define-method execute entry ()
  ^data)

(define-method set-data entry (data)
  (setf ^data data))

(define-method draw entry (image)
  (with-block-drawing image
    (with-fields (x y data parent) self
      (when (null parent) (draw-background self image))
      (draw-contents self image))))

(define-method draw-contents entry (image)
  (with-block-drawing image
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
  (type :initform :structure))

(defparameter *null-display-string* "()")

(defun null-block () (clone =list=))

(define-method execute list ()
  ^results)

(define-method accept list (child &optional prepend)
  (with-fields (arguments) self
    (if arguments 
	(if prepend
	    (setf arguments (nconc (list child) arguments))
	    (setf arguments (nconc arguments (list child))))
	(setf arguments (list child)))
    (when (get-parent child)
      (unplug-from-parent child))
    (set-parent child self)))

(define-method take-first list ()
  (with-fields (arguments) self
    (let ((block (first arguments)))
      (prog1 block
	(unplug self block)))))

(define-method length list ()
  (with-fields (arguments) self
    (length arguments)))

(define-method unplug list (child)
  (with-fields (arguments) self
    (setf arguments (delete child arguments))
    (set-parent child nil)))

(define-method layout-header list () 0)

(define-method layout-body-as-null list ()
  (with-fields (height width) self
    (setf width (+ (* 4 *dash*)
		   (font-text-extents *null-display-string*
				      *block-font*))
	  height (+ (* 4 *dash*)))))

(define-method layout-body-as-list list ()
  (with-fields (x y height width arguments) self
    (let* ((dash *dash*)
	   (header-height (+ dash (layout-header self)))
	   (y0 (+ y dash header-height))
	   (line-height (font-height *block-font*)))
      (setf height (+ (* 2 dash) line-height))
      (setf width (* 8 dash))
      (dolist (block arguments)
	(move block (+ x dash) y0)
	(layout block)
	(incf height (field-value :height block))
	(incf y0 (field-value :height block))
	(setf width (max width (field-value :width block))))
      (incf width (* 2 dash)))))

(define-method layout list ()
  (with-fields (arguments) self
    (if (null arguments)
	(layout-body-as-null self)
	(layout-body-as-list self))))
    
(define-method draw-header list () 0)

;;; Composing blocks into larger programs, recursively.

(define-prototype script (:parent =list=)
  (arguments :iniform '(nil))
  (schema :initform '(:block))
  (target :initform nil)
  (variables :initform (make-hash-table :test 'eq)))

(define-method layout script ())

(define-method initialize script (&key blocks variables target)
  (setf ^arguments blocks)
  (when variables (setf ^variables variables))
  (when target (setf ^target target)))

(defvar *target* nil)

(define-method set-target script (target)
  (setf ^target target))

(define-method is-member script (block)
  (with-fields (arguments) self
    (find block arguments)))

(define-method add script (block &optional x y)
  (with-fields (arguments) self
    (assert (not (find block arguments)))
    (setf arguments (nconc arguments (list block)))
    (setf (field-value :parent block) nil) ;; TODO self?
    (when (and (integerp x)
	       (integerp y))
      (move block x y))))

(define-method layout-header script ()
  (with-fields (x y arguments) self
    (let ((name (first arguments))
	  (height (font-height *block-font*)))
      (prog1 height
	(move name 
	       (+ x (handle-width self))
	       (+ y height))))))

(define-method draw-header script (image)
  (prog1 (font-height *block-font*)
    (with-fields (x y) self
      (with-block-drawing image
	(text (+ x *dash* 1)
	      (+ y *dash* 1)
	      "script")))))
			   
(define-method run script ())
  ;; (with-fields (arguments target) self
  ;;   (with-target target
  ;;     (dolist (block arguments)
  ;; 	(run block)))))
	    
(define-method update script ())

(define-method bring-to-front script (block)
  (with-fields (arguments) self
    (when (find block arguments)
      (setf arguments (delete block arguments))
      (setf arguments (nconc arguments (list block))))))

(define-method delete-child script (block)
  (with-fields (arguments) self
    (assert (find block arguments))
    (setf arguments (delete block arguments))))

;; (define-method set script (var value)
;;   (setf (gethash var ^variables) value))

;; (define-method get script (var)
;;   (gethash var ^variables))

;; (defun block-variable (var-name)
;;   (get *block* var-name))

;; (defun (setf block-variable) (var-name value)
;;   (set *block* var-name value))

;; (defmacro with-block-variables (vars &rest body)
;;   (labels ((make-clause (sym)
;; 	     `(,sym (block-variable ,(make-keyword sym)))))
;;     (let* ((symbols (mapcar #'make-non-keyword vars))
;; 	   (clauses (mapcar #'make-clause symbols)))
;;       `(symbol-macrolet ,clauses ,@body))))
      
;;; blocks.lisp ends here
