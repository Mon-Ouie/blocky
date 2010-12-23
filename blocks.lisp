;;; blocks.lisp --- A visual programming language inspired by MIT Scratch

;; Copyright (C) 2010  David O'Toole

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

;; For the design of IOFORMS, I've followed a motto associated with
;; the visual programming language Pure Data: "The diagram is the
;; program."  Since the diagram is 2D, the program must therefore be
;; two-dimensional as well. That means every block in the program
;; (i.e. every expression) must have an X,Y position. The position
;; units are abstract "pseudo-pixels" which can be scaled
;; appropriately for display.

;; Unlike Pure Data and other visual languages that model themselves
;; after electronic components connected by wires, IOFORMS does away
;; with the explicitly drawn connections in favor of a tree structure
;; mapping more naturally to Lisp expressions. 

;; The purpose of a block is to perform some action in response to a
;; number of input arguments and then return a value. Each argument is
;; itself a block and there are prebuilt block types for integers,
;; strings, symbols, and lists. To run a block diagram we proceed
;; depth-first to the leaves and execute those to obtain values, then
;; propagate results up the tree until the outermost block is executed
;; using the propagated results as its input values. See also
;; `IOBLOCK/RUN'.

;; New block types and behaviors can be defined with the macro
;; `defblock' and subsequently replacing default methods of the base
;; block prototype via `define-method'. With the macro `make-block'
;; you can convert lisp expressions into working block
;; diagrams. Diagrams can be saved with `serialize' and `deserialize'.

(define-prototype ioblock ()
  (name :initform nil)
  (pinned :initform nil :documentation "When non-nil, do not allow dragging.")
  (arguments :initform nil :documentation "List of block argument values.")
  (results :initform nil :documentation "Computed output values. See `IOBLOCK/EXECUTE'.")
  (schema :documentation 
	  "List of type keywords for corresponding expressions in <arguments>.
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
  (keymap :initform nil :documentation "Keybindings, if any.")  
  (child-widths :initform nil :documentation "List of widths of visual block segments. See `BLOCK/LAYOUT'.")
  (excluded-fields :initform '(:widget :results :parent)))

(defmacro defblock (name &body args)
  "Define a new block prototype named =NAME=.
ARGS are field specifiers, as with `define-prototype'."
  `(define-prototype ,name (:parent =ioblock=)
     (operation :initform ,(make-keyword name))
     ,@args))

(define-method pin ioblock ()
  (setf <pinned> t))

(define-method unpin ioblock ()
  (setf <pinned> nil))

(define-method is-pinned ioblock ()
  <pinned>)

;;; Defining input events for blocks

(define-method initialize-keymap-maybe ioblock ()
  (with-fields (keymap) self
    (when (null keymap)
      (setf keymap (make-hash-table :test 'equal)))))

(define-method define-key ioblock (key-name modifiers func)
  "Bind the described keypress to invoke FUNC.
KEY-NAME is a string giving the key name; MODIFIERS is a list of
keywords like :control, :alt, and so on."
  (/initialize-keymap-maybe self)
  (setf (gethash (normalize-event (cons key-name modifiers))
		 <keymap>)
	func))

(define-method undefine-key ioblock (key-name modifiers)
  "Remove the described keybinding."
  (remhash (normalize-event (cons key-name modifiers))
	   <keymap>))

(define-method clear-keymap ioblock ()
  (setf <keymap> (make-hash-table :test 'equal)))

(define-method handle-key ioblock (keylist)
  "Look up and invoke the function (if any) bound to KEYLIST. Return t
if a binding was found, nil otherwise."
  (/initialize-keymap-maybe self)
  (with-fields (keymap) self
      (when keymap 
	(let ((func (gethash keylist keymap)))
	  (when func
	    (prog1 t
	      (funcall func)))))))

(defun bind-key-to-prompt-insertion (p key modifiers &optional (insertion key))
  "For prompt P ensure that the event (KEY MODIFIERS) causes the
text INSERTION to be inserted at point."
 (/define-key p (string-upcase key) modifiers
	      #'(lambda ()
		  (/insert p insertion))))

(defun bind-key-to-method (p key modifiers method-keyword)
  (/define-key p (string-upcase key) modifiers
	      #'(lambda ()
		  (send nil method-keyword p))))

(define-method generic-keybind ioblock (binding) 
  (destructuring-bind (key modifiers data) binding
    (apply (etypecase data
	     (keyword #'bind-key-to-method)
	     (string #'bind-key-to-prompt-insertion))
	   self binding)))

;;; Creating blocks from textual lisp expressions

(defvar *make-block-package* nil)

(defun make-block-ext (value)
  (if (listp value) 
      (if (and (symbolp (first value))
	       (not (boundp (make-special-variable-name (first value)))))
	  (let ((entry (clone =symbol=)))
	    (prog1 entry (/set-data entry (first value))))
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
		    (/set-parent child block)))))))
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
	    (/set-data entry value))))))

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

(define-method move ioblock (x y)
  "Move the block to a new (X Y) location."
  (setf <x> x)
  (setf <y> y))

(define-method show ioblock ()
  (setf <visible> t))

(define-method hide ioblock ()
  (setf <visible> nil))

(define-method toggle-visible ioblock ()
  (if <visible>
      (/hide self)
      (/show self)))

(define-method set-parent ioblock (parent)
  "Store a link to an enclosing PARENT block, if any."
  (setf <parent> parent))

(define-method get-parent ioblock ()
  <parent>)

(define-method get-widget ioblock ()
  <widget>)

(define-method get-argument ioblock (n)
  "Return the value of the Nth block argument."
  (nth n <arguments>))

(define-method set-argument ioblock (n value)
  "Set the Nth argument value to VALUE."
    (setf (nth n <arguments>) value))

(define-method child-position ioblock (child)
  (with-fields (arguments) self
    (position child arguments)))

(define-method position ioblock ()
  (with-fields (parent) self
    (when parent 
      (/child-position parent self))))

(define-method plug ioblock (child n)
  "Connect the block CHILD as the value of the Nth argument."
  (/set-argument self n child)
  (/set-parent child self))

(define-method unplug ioblock (child)
  "Disconnect the block CHILD from this block."
  (let ((pos (position child <arguments>)))
    (/plug self (null-block) pos)
    (/set-parent child nil)))

(define-method unplug-from-parent ioblock ()
  (with-fields (parent) self
    (when parent
      (/unplug parent self))))

(define-method execute-arguments ioblock (target)
  "Execute all blocks in <ARGUMENTS> from left-to-right. Results are
placed in corresponding positions of <RESULTS>. Override this method
when defining new blocks if you don't want to evaluate all the
arguments all the time."
  (with-fields (arguments results) self
    (setf results (mapcar #'(lambda (block)
			      (/run block target))
			  arguments))))
    ;; (when (and arguments results)
    ;;   (dotimes (n (length arguments))
    ;; 	(setf (nth n results)
    ;; 	      (/run (nth n arguments)
    ;; 		    target))))))

(define-method execute ioblock (target)
  "Carry out the block's action by sending messages to the object TARGET.
The TARGET argument is provided by the script executing the block,
and its value will be the IOFORMS object associated with the script.
The <RESULTS> field will be a list of results obtained by
executing/evaluating the blocks in <ARGUMENTS> (see also
`BLOCK/EXECUTE-ARGUMENTS'.) The default behavior of `EXECUTE' is to
send the <OPERATION> field's value as a message to the target, with
the arguments to the target's method being the current computed
<RESULTS>, and return the result of the method call. This default
action is sufficient for many blocks whose main purpose is to send a
single message; other blocks can redefine this /EXECUTE method to do
something else. See also `defblock' and `send'."
  (with-fields (operation results) self
    (labels ((clean (item)
	       (if (symbolp item)
		   (make-keyword item)
		   item)))
    (apply #'ioforms:send nil operation target 
	   (mapcar #'clean results)))))

(define-method run ioblock (target)
  "Run nested blocks on TARGET to produce results, then run this
block with those results as input."
  (/execute-arguments self target)
  (/execute self target))

(define-method describe ioblock ()
  "Show name and comprehensive help for this block.")

(define-method initialize ioblock (&rest args)
  "Prepare an empty block, or if ARGS is non-empty, a block
initialized with its values as arguments."
  (with-fields (arguments schema results) self
    (let ((arity (length schema)))
      (when args 
	(setf arguments (make-list arity))
	(dotimes (n (length args))
	  (setf (nth n arguments)
		(nth n args))))
      (setf results (make-list arity)))))

(define-method deserialize ioblock ()
  "Make sure the block is ready after loading."
  (/initialize self))

(define-method count ioblock ()
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

(defvar *dash-size* 3 
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
 
(define-method find-color ioblock (&optional (part :background))
  "Return the X11 color name of this block's type as a string.
If PART is provided, return the color for the corresponding 
:BACKGROUND, :SHADOW, :FOREGROUND, or :HIGHLIGHT parts of this type of
block."
  (let ((colors (ecase part
		  (:background *block-colors*)
		  (:highlight *block-highlight-colors*)
		  (:shadow *block-shadow-colors*)
		  (:foreground *block-foreground-colors*))))
    (getf colors <type>)))

(defparameter *selection-color* ".red")

(define-method resize ioblock (&key (height (* 8 *dash-size*))
				  (width (* 60 *dash-size*)))
  "Allocate an image buffer of HEIGHT by WIDTH pixels."
  (unless (and (= <width> width)
	       (= <height> height))
    (setf <width> width 
	  <height> height)  
    (let ((oldimage <image>))
      (setf <image> (create-image width height))
      (when oldimage (sdl:free oldimage)))))

(defmacro with-block-drawing (image &body body)
  "Run BODY forms with drawing primitives set to draw on IMAGE.
The primitives are CIRCLE, DISC, LINE, BOX, and TEXT. These are used
in subsequent functions as the basis of drawing nested diagrams of
blocks."
  (let ((image-sym (gensym)))
    `(let* ((foreground (/find-color self :foreground))
	    (background (/find-color self :background))
	    (highlight (/find-color self :highlight))
	    (selection *selection-color*)
	    (shadow (/find-color self :shadow))
	    (dash *dash-size*)
	    (radius *dash-size*)
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

(define-method draw-patch ioblock (x0 y0 x1 y1 image 
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
	  (fill (or color (if socket *socket-color* 
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

(define-method draw-socket ioblock (x0 y0 x1 y1 image)
  (/draw-patch self x0 y0 x1 y1 image :depressed t :socket t))
    
(define-method draw-border ioblock (image &optional (color *selection-color*))
  (let ((dash *dash-size*))
    (with-fields (x y height width) self
      (/draw-patch self (- x dash) (- y dash)
		   (+ x width dash)
		   (+ y height dash)
		   image :color color))))

(define-method draw-background ioblock (image)
  (with-fields (x y width height) self
    (/draw-patch self x y (+ x width) (+ y height) image)))

(define-method draw-ghost ioblock (image)
  (with-fields (x y width height) self
    (/draw-patch self x y (+ x width) (+ y height) image
		 :depressed t :socket t)))

(define-method handle-width ioblock ()
  (+ (* 2 *dash-size*)
     (expression-width <operation>)))

(defparameter *socket-width* (* 18 *dash-size*))

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

(define-method layout ioblock ()
  (with-fields (child-widths height width) self
    (with-field-values (x y operation schema arguments widget) self
      (let* ((font *block-font*)
	     (dash *dash-size*)
	     (left (+ x (/handle-width self)))
	     (max-height (font-height font)))
	(labels ((move-widget (widget)
		   (/move widget :x left :y y)
		   (incf left (field-value :width widget))
		   (setf max-height
			 (max max-height 
			      (field-value :height widget))))
		 (move-child (child)
		   (/move child (+ left dash) y)
		   (/layout child)
		   (setf max-height (max max-height (field-value :height child)))
		   (field-value :width child))
		 (layout-child (block type)
		   (let ((measurement
			  (+ dash (move-child block))))
		     (prog1 measurement 
		       (incf left measurement)))))
	  (if widget
	      (move-widget widget)
	      (setf child-widths (mapcar #'layout-child arguments schema)))
	  (setf width (+ (- left x) (* 4 dash)))
	  (setf height (+ dash dash max-height)))))))

(define-method draw-expression ioblock (x0 y0 segment type image)
  (with-block-drawing image
      (with-fields (height child-widths) self
	(let ((dash *dash-size*)
	      (width *socket-width*))
	  (if (eq type :block)
	      ;; draw a socket if there's no block; otherwise wait
	      ;; until later to draw.
	      (when (null segment) 
		(/draw-socket self (+ x0 dash) (+ y0 dash)
			      (+ x0 *socket-width*)
			      (+ y0 (- height dash))
			      image))
	      (progn 
		(text x0 (+ y0 dash 1)
		      (print-expression segment))
		(setf width (expression-width segment))))
	  width))))

(define-method draw-contents ioblock (image)
  (with-block-drawing image
    (with-field-values 
	(x y operation arguments schema widget)
	self
      (let* ((dash *dash-size*)
	     (left (+ x (* 2 dash)))
	     (y0 (+ y dash 1)))
	(if widget
	    (progn 
	      (/render widget)
	      (draw-image (field-value :image widget)
			  left y0 :destination image))
	    (progn 
	      (text left y0 (print-expression operation))
	      (dolist (block arguments)
		(/draw block image))))))))

(define-method draw ioblock (output-image)
  (with-fields (image x y) self
    (if (null image)
	(progn
	  (/draw-background self output-image)
	  (/draw-contents self output-image))
	(progn
	  (/render self)
	  (draw-image image x y 
		      :destination output-buffer)))))

(defparameter *hover-color* ".red")

(define-method draw-hover ioblock (image)
  (with-fields (x y width height) self
    (draw-box x y (+ *dash-size* width) (+ *dash-size* height)
	      :stroke-color *hover-color* 
	      :color *hover-color*
	      :destination image))
  (/draw-contents self image))
		    
(define-method hit ioblock (mouse-x mouse-y)
  "Return this block (or child block) if the coordinates MOUSE-X and
MOUSE-Y identify a point inside the block (or child block.)"
  (with-fields (x y width height arguments) self
    (when (within-extents mouse-x mouse-y x y 
			  (+ x width) (+ y height))
      (labels ((hit (block)
		 (/hit block mouse-x mouse-y)))
	(let ((child (some #'hit arguments)))
	  (values (or child self) (when child (/position child))))))))
     	
(define-method accept ioblock (other-block)
  (with-field-values (parent) self
    (when parent
      (prog1 t
	(let ((position (/child-position parent self)))
	  (assert (integerp position))
	  (/plug parent other-block position))))))

;;; Data entry blocks

(defblock entry 
  (type :initform :data)
  (schema :iniform nil)
  (data :initform nil))

(define-method execute entry (target)
  (declare (ignore target))
  <data>)

(define-method set-data entry (data)
  (setf <data> data))

(define-method draw entry (image)
  (with-block-drawing image
    (with-fields (x y data parent) self
      (when (null parent) (/draw-background self image))
      (/draw-contents self image))))

(define-method draw-contents entry (image)
  (with-block-drawing image
    (with-fields (data x y) self
      (text (+ x (* 2 *dash-size*))
	    (+ y *dash-size* 1)
	    (print-expression data)))))

(define-method layout entry ()
  (with-fields (height width data) self
    (setf height (+ (* 2 *dash-size*) (font-height *block-font*)))
    (setf width (+ (* 4 *dash-size*) (expression-width data)))))

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

(define-method execute list (target)
  (declare (ignore target))
  <results>)

(define-method initialize list (&rest args)
  (when args (setf <arguments> args)))

(define-method accept list (child &optional prepend)
  (with-fields (arguments) self
    (if arguments 
	(if prepend
	    (setf arguments (nconc (list child) arguments))
	    (setf arguments (nconc arguments (list child))))
	(setf arguments (list child)))
    (when (/get-parent child)
      (/unplug-from-parent child))
    (/set-parent child self)))

(define-method pop list ()
  (with-fields (arguments) self
    (let ((block (first arguments)))
      (prog1 block
	(/unplug self block)))))

(define-method length list ()
  (with-fields (arguments) self
    (length arguments)))

(define-method unplug list (child)
  (with-fields (arguments) self
    (setf arguments (delete child arguments))
    (/set-parent child nil)))

(define-method layout-header list () 0)

(define-method layout-body-as-null list ()
  (with-fields (height width) self
    (setf width (+ (* 4 *dash-size*)
		   (font-text-extents *null-display-string*
				      *block-font*))
	  height (+ (* 4 *dash-size*)))))

(define-method layout-body-as-list list ()
  (with-fields (x y height width) self
    (let* ((dash *dash-size*)
	   (header-height (+ dash (/layout-header self)))
	   (y0 (+ y dash header-height))
	   (line-height (font-height *block-font*)))
      (setf height (+ (* 2 dash) line-height))
      (setf width (* 8 dash))
      (dolist (block arguments)
	(/move block (+ x dash) y0)
	(/layout block)
	(incf height (field-value :height block))
	(incf y0 (field-value :height block))
	(setf width (max width (field-value :width block))))
      (incf width (* 2 dash)))))))
(font-height *block-font*)))))

(define-method layout list ()
  (with-fields (arguments) self
    (if (null arguments)
	(/layout-body-as-null self)
	(/layout-body-as-list self))))
    
(define-method draw-header list () 0)

;;; Composing blocks into larger programs, recursively.

(define-prototype block (:parent =list=)
  (arguments :iniform '(nil))
  (schema :initform '(:block))
  (blocks :initform '()
	  :documentation "List of blocks in the block.")
  (target :initform nil)
  (variables :initform (make-hash-table :test 'eq)))

(define-method initialize block (&key blocks variables target)
  (setf <blocks> blocks)
  (when variables (setf <variables> variables))
  (when target (setf <target> target)))

(defvar *block* nil)
(defvar *target* nil)

(define-method set-target block (target)
  (setf <target> target))

(define-method is-member block (block)
  (with-fields (blocks) self
    (find block blocks)))

(define-method add block (block &optional x y)
  (with-fields (blocks) self
    (assert (not (find block blocks)))
    (setf blocks (nconc blocks (list block)))
    (setf (field-value :parent block) nil)
    (when (and (integerp x)
	       (integerp y))
      (/move block x y))))

(define-method run block (block)
  (with-fields (blocks target) self
    (/run block target)))
	    
(define-method bring-to-front block (block)
  (with-fields (blocks) self
    (when (find block blocks)
      (setf blocks (delete block blocks))
      (setf blocks (nconc blocks (list block))))))

(define-method delete block (block)
  (with-fields (blocks) self
    (assert (find block blocks))
    (setf blocks (delete block blocks))))

(define-method set block (var value)
  (setf (gethash var <variables>) value))

(define-method get block (var)
  (gethash var <variables>))

(defun block-variable (var-name)
  (/get *block* var-name))

(defun (setf block-variable) (var-name value)
  (/set *block* var-name value))

(defmacro with-block-variables (vars &rest body)
  (labels ((make-clause (sym)
	     `(,sym (block-variable ,(make-keyword sym)))))
    (let* ((symbols (mapcar #'make-non-keyword vars))
	   (clauses (mapcar #'make-clause symbols)))
      `(symbol-macrolet ,clauses ,@body))))

;;; Block shell widget and command prompt

(define-prototype block-prompt (:parent =prompt=)
  output 
  (rows :initform 10))

(define-method initialize block-prompt (output)
  (/parent/initialize self)
  (setf <output> output))
  
(define-method do-sexp block-prompt (sexp)
  (with-fields (output rows) self
    (assert output)
    (let ((container (/get-parent output)))
      (when container
	(/accept container 
		 (let ((*make-block-package* (find-package :ioforms)))
		   (if (symbolp (first sexp))
		       (make-block-ext sexp)
		       (make-block-ext (first sexp)))))
	(when (> (/length container) rows)
	  (/pop container))))))

(defblock listener
  (type :initform :system))

(defparameter *minimum-listener-width* 200)

(define-method initialize listener ()
  (with-fields (widget) self
    (/parent/initialize self)
    (let ((prompt (clone =block-prompt= self)))
      (/resize prompt 
	       :width *minimum-listener-width*
	       :height (+ (* 2 *dash-size*) 
			  (font-height *default-font*)))
      (setf widget prompt))))

(defblock shell
  (block :initform nil 
	  :documentation "The IOFORMS:=BLOCK= object being edited.")
  (selection :initform ()
  	     :documentation "Subset of selected blocks.")
  (drag :initform nil 
  	:documentation "Block being dragged, if any.")
  (hover :initform nil
	 :documentation "Block being hovered over, if any.")
  (ghost :initform (clone =block=))
  (buffer :initform nil)
  (drag-start :initform nil
	      :documentation "A cons (X . Y) of widget location at start of dragging.")
  (drag-offset :initform nil
	       :documentation "A cons (X . Y) of mouse click location on dragged block.")
  (needs-redraw :initform t)
  (modified :initform nil 
	  :documentation "Non-nil when modified since last save."))

(define-method initialize shell ()
  (/parent/initialize self)
  (with-fields (block) self
    (setf block (clone =block=))))

(define-method select shell (block)
  (with-fields (selection blocks) self
    (pushnew block selection)))

(define-method select-if shell (predicate)
  (with-fields (selection blocks) self
    (setf selection (remove-if predicate blocks))))

(define-method unselect shell (block)
  (with-fields (selection) self
    (setf selection (delete block selection))))

(define-method handle-key shell (keys)
  (with-fields (selection needs-redraw) self
    (when (= 1 (length selection))
      (when (first selection)
	(/handle-key (first selection) keys)
	(setf needs-redraw t)))))

(define-method resize shell (&key width height)
  (with-fields (buffer prompt image) self
    (when (null buffer)
      (setf buffer (create-image width height)))
    (unless (and (= <width> width)
		 (= <height> height))
      (/parent/resize self :width width :height height)
      (when buffer
	(sdl:free buffer))
      (setf buffer (create-image width height)))))

(define-method redraw shell ()
  (with-fields (block buffer selection needs-redraw width height) self
    (with-fields (blocks) block
      (draw-box 0 0 width height 
		:color *background-color*
		:stroke-color *background-color*
		:destination buffer)
      (dolist (block blocks)
	(/layout block))
      (dolist (block blocks)
	(when (find block selection)
	  (/draw-border block buffer))
	(/draw block buffer))
      (setf needs-redraw nil))))

(define-method begin-drag shell (mouse-x mouse-y block)
  (with-fields (drag block drag-start ghost drag-offset) self
    (setf drag block)
    (when (/is-member block block)
      (/delete block block))
    (let ((dx (field-value :x block))
	  (dy (field-value :y block))
	  (dw (field-value :width block))
	  (dh (field-value :height block)))
      (with-fields (x y width height) ghost
	(let ((x-offset (- mouse-x dx))
	      (y-offset (- mouse-y dy)))
	  (when (null drag-start)
	    (setf x dx y dy width dw height dh)
	    (setf drag-start (cons dx dy))
	    (setf drag-offset (cons x-offset y-offset))))))))

(define-method hit-blocks shell (x y)
  (with-fields (block) self
    (when block 
      (with-fields (blocks) block
	(labels ((hit (b)
		   (/hit b x y)))
	  (let ((parent (find-if #'hit blocks :from-end t)))
	    (when parent
	      (/hit parent x y))))))))

(define-method render shell ()
  (with-fields 
      (block needs-redraw image buffer drag-start selection
      drag modified hover ghost prompt) self
    (dolist (block selection)
      (let ((widget (/get-widget block)))
	(when widget 
	  (/render widget)
	  (/draw block image))))
    (labels ((copy ()
	       (draw-image buffer 0 0 :destination image)))
      (when block
	(when needs-redraw 
	  (/redraw self)
	  (copy))
	(when drag 
	  (copy)
	  (/layout drag)
	  (/draw-ghost ghost image)
	  (/draw drag image)
	  (when hover 
	    (/draw-hover hover image)))))))

(define-method mouse-down shell (x y &optional button)
  (with-fields (block) self 
    (let ((block (/hit-blocks self x y)))
      (when block
	(case button
	  (1 (/begin-drag self x y block))
	  (3 (/run block block)))))))

(define-method mouse-move shell (mouse-x mouse-y)
  (with-fields (block hover drag-offset drag-start drag) self
    (setf hover nil)
    (when drag
      (destructuring-bind (ox . oy) drag-offset
	(let ((target-x (- mouse-x ox))
	      (target-y (- mouse-y oy)))
	  (setf hover (/hit-blocks self target-x target-y))
	  (/move drag target-x target-y))))))

(define-method mouse-up shell (x y &optional button)
  (with-fields 
      (block needs-redraw drag-offset drag-start hover
	      selection drag modified) 
      self
    (with-fields (blocks) block
      (when drag
	(let ((drag-parent (/get-parent drag)))
	  (when drag-parent
	    (/unplug-from-parent drag))
	  (let ((target hover))
	    (if target
		;; dropping on another block
		(unless (/accept target drag)
		  (/add block drag))
		;; dropping on background
		(/add block drag)))))
      (setf selection nil)
      (when drag (/select self drag))
      (setf drag-start nil
	    drag-offset nil
	    drag nil
	    needs-redraw t))))

;;; IF block

(defblock if 
  (type :initform :control)
  (result :initform nil)
  (schema :initform '(:block :block :block))
  (arguments :initform '(nil nil nil)))

(define-method execute if (target)
  <results>)

(define-method execute-arguments if (target)
  (with-fields (arguments results) self
    (destructuring-bind (predicate then else) arguments
      (if (/run predicate target)
	  (/run then target)
	  (/run else target)))))

;;; Get field value

(defblock my 
  (type :initform :variables)
  (schema :initform '(:block)))

(define-method execute my (target)
  (with-fields (results) self
    (field-value (make-keyword (car results))
		 target)))

;;; Set field value

(defblock set
  (type :initform :variables)
  (schema :initform '(:symbol :anything))
  (arguments :initform '(:counter 1)))

;;; Talking 

(defblock emote 
  (type :initform :looks)
  (schema :initform '(:string)))

(define-method execute emote (target)
  (/emote target 
	  (list (list (list (first <results>) :font *block-font*
			    :foreground ".black")))
	  :timeout 200 :style :clear))

;;; Other blocks

(defblock say 
  (type :initform :message)
  (schema :initform '(:string))
  (arguments :initform '("Hello!")))

(defblock move
  (type :initform :motion)
  (schema :initform '(:symbol :integer :symbol))
  (arguments :initform '(:north 10 :pixels)))

(defblock move-to
  (type :initform :motion)
  (schema :initform '(:unit :integer :integer))
  (arguments :initform '(:space 0 0)))

(defblock joystick-button
  (type :initform :sensing)
  (schema :initform '(:integer :symbol))
  (arguments :initform '(1 :down)))

(defblock visible?
  (type :initform :variables)
  (schema :initform nil)
  (arguments :initform nil))

(defblock set-variable 
  (type :initform :variables)
  (schema :initform '(:symbol :block))
  (arguments :initform '(:n nil)))

(defblock animate 
  (type :initform :looks)
  (schema :initform '(:string))
  (arguments :initform '(nil)))

(defblock play-music 
  (type :initform :sound)
  (schema :initform '(:string))
  (arguments :initform '("fanfare")))

(define-method execute play-music (target)
  (/play-music target (first <results>) :loop t))

(defblock play-sound 
  (type :initform :sound)
  (schema :initform '(:string))
  (arguments :initform '("boing")))

(defblock when 
  (type :initform :control)
  (schema :initform '(:block :block))
  (arguments :initform '(nil nil)))

(defblock unless
  (type :initform :control)
  (schema :initform '(:block :block))
  (arguments :initform '(nil nil)))

(defblock fire
  (type :initform :control)
  (schema :initform '(:block))
  (arguments :initform '(:south)))

(defblock see-player
  (type :initform :sensing)
  (schema :initform nil)
  (arguments :initform nil))

(defblock player-direction
  (type :initform :sensing)
  (schema :initform nil)
  (arguments :initform nil))

(defblock closer-than
  (type :initform :sensing)
  (schema :initform '(:block :block :block :block))
  (arguments :initform '(10 spaces to player)))
  
(defblock +
  (type :initform :operators)
  (schema :initform '(:number :number))
  (arguments :initform '(nil nil)))

(define-method execute + (target)
  (with-fields (results) self
    (when (every #'integerp results)
      (apply #'+ results))))

      
;;; blocks.lisp ends here
