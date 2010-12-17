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

(in-package :iosketch)

;; For the design of IOSKETCH, I've followed a motto associated with
;; the visual programming language Pure Data: "The diagram is the
;; program."  Since the diagram is 2D, the program must therefore be
;; two-dimensional as well. That means every block in the program
;; (i.e. every expression) must have an X,Y position. The position
;; units are abstract "pseudo-pixels" which can be scaled
;; appropriately for display.

;; Unlike Pure Data and other visual languages that model themselves
;; after electronic components connected by wires, IOSKETCH does away
;; with the explicitly drawn connections in favor of a tree structure
;; mapping to Lisp expressions. 

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

(define-prototype block ()
  (arguments :initform nil :documentation "List of block argument values.")
  (results :initform nil :documentation "Computed output values. See `BLOCK/EXECUTE'.")
  (schema :documentation 
	  "List of type keywords for corresponding expressions in <arguments>.
See also `*argument-types*'.")
  (operation :initform :block :documentation "Keyword name of method to be invoked on recipient.")
  (type :initform :data :documentation "Type name of block. See also `*block-types*'.")
  (x :initform 0 :documentation "Integer X coordinate of this block's position.")
  (y :initform 0 :documentation "Integer Y coordinate of this block's position.")
  (width :initform 32 :documentation "Cached width of block.")
  (height :initform 32 :documentation "Cached height of block.")
  (parent :initform nil :documentation "Link to enclosing parent block, or nil if none.")
  (data :initform nil :documentation "Data value for data entry blocks.")
  (widget :initform nil :documentation "Widget object for interacting with this block, if any.")
  (child-widths :initform nil :documentation "List of widths of visual block segments. See `BLOCK/LAYOUT'.")
  (excluded-fields :initform '(:widget :results :parent)))

(defmacro defblock (name &body args)
  "Define a new block prototype named =NAME=.
ARGS are field specifiers, as with `define-prototype'."
  `(define-prototype ,name (:parent =block=)
     (operation :initform ,(make-keyword name))
     ,@args))

(defun make-block-ext (value)
  (assert value)
  (if (listp value) 
      (destructuring-bind (operation &rest args) value
	(let ((block (apply #'clone 
			    (symbol-value 
			     (make-special-variable-name operation))
			    (mapcar #'make-block-ext args))))
	  (with-fields (arguments) block
	    (dolist (child arguments)
	      (/set-parent child block)))))
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
    :control :comment :sensing :operators :variables)
  "List of keywords used to group blocks into different functionality
areas.")

(defparameter *argument-types*
  '(:block :anything :integer :float :number :string :symbol)
  "List of keywords identifying the type of a particular argument.")

(define-method move block (x y)
  "Move the block to a new (X Y) location."
  (setf <x> x)
  (setf <y> y))

(define-method set-parent block (parent)
  "Store a link to an enclosing PARENT block, if any."
  (setf <parent> parent))

(define-method get-argument block (n)
  "Return the value of the Nth block argument."
  (nth n <arguments>))

(define-method set-argument block (n value)
  "Set the Nth argument value to VALUE."
    (setf (nth n <arguments>) value))

(define-method plug block (child n)
  "Connect the block CHILD as the value of the Nth argument."
  (/set-argument self n child)
  (/set-parent child self))

(define-method unplug block (child)
  "Disconnect the block CHILD from this block."
  (let ((pos (position child <arguments>)))
    (/set-argument self pos (null-block))
    (/set-parent child nil)))

(define-method execute-arguments block (recipient)
  "Execute all blocks in <ARGUMENTS> from left-to-right. Results are
placed in corresponding positions of <RESULTS>. Override this method
when defining new blocks if you don't want to evaluate all the
arguments all the time."
  (with-fields (arguments results) self
    (setf results (mapcar #'(lambda (block)
			      (/run block recipient))
			  arguments))))
    ;; (when (and arguments results)
    ;;   (dotimes (n (length arguments))
    ;; 	(setf (nth n results)
    ;; 	      (/run (nth n arguments)
    ;; 		    recipient))))))

(define-method execute block (recipient)
  "Carry out the block's action by sending messages to the object RECIPIENT.
The RECIPIENT argument is provided by the script executing the block,
and its value will be the IOSKETCH object associated with the script.
The <RESULTS> field will be a list of results obtained by
executing/evaluating the blocks in <ARGUMENTS> (see also
`BLOCK/EXECUTE-ARGUMENTS'.) The default behavior of `EXECUTE' is to
send the <OPERATION> field's value as a message to the recipient, with
the arguments to the recipient's method being the current computed
<RESULTS>, and return the result of the method call. This default
action is sufficient for many blocks whose main purpose is to send a
single message; other blocks can redefine this /EXECUTE method to do
something else. See also `defblock' and `send'."
  (with-fields (operation results) self
    (labels ((clean (item)
	       (if (symbolp item)
		   (make-keyword item)
		   item)))
    (apply #'iosketch:send nil operation recipient 
	   (mapcar #'clean results)))))

(define-method run block (recipient)
  "Run nested blocks on RECIPIENT to produce results, then run this
block with those results as input."
  (/execute-arguments self recipient)
  (/execute self recipient))

(define-method describe block ()
  "Show name and comprehensive help for this block.")

(define-method initialize block (&rest args)
  "Prepare an empty block, or if ARGS is non-empty, a block
initialized with its values as arguments."
  (with-fields (arguments schema results) self
    (let ((arity (length schema)))
      (when args 
	(assert (= (length args) arity))
	(setf arguments args))
      (setf results (make-list arity)))))

(define-method deserialize block ()
  "Make sure the block is ready after loading."
  (/initialize self))

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
corresponding IOSKETCH:=WIDGET= prototypes used for editing that kind
of value.")

(defparameter *background-color* ".white" 
  "The default background color of the IOSKETCH user interface.")

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
    :data ".gray45"
    :structure ".gray45"
    :comment ".grey40"
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
    (getf colors <type>)))

(defmacro with-block-drawing (image &body body)
  "Run BODY forms with drawing primitives set to draw on IMAGE.
The primitives are CIRCLE, DISC, LINE, BOX, and TEXT. These are used
in subsequent functions as the basis of drawing nested diagrams of
blocks."
  (let ((image-sym (gensym)))
    `(let* ((foreground (/find-color self :foreground))
	    (background (/find-color self :background))
	    (highlight (/find-color self :highlight))
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



(define-method draw-patch block (x0 y0 x1 y1 image 
				    &key depressed dark socket)
  "Draw a standard IOSKETCH block notation patch on IMAGE.
Top left corner at (X0 Y0), bottom right at (X1 Y1). If DEPRESSED is
non-nil, draw an indentation; otherwise a raised area is drawn. If
DARK is non-nil, paint a darker region. If SOCKET is non-nil, cut a hole
in the block where the background shows through."
  (with-block-drawing image
    (let ((bevel (if depressed shadow highlight))
	  (chisel (if depressed highlight shadow))
	  (fill (if socket *socket-color* 
		    (if dark shadow background))))
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
  (/draw-patch self x0 y0 x1 y1 image :depressed t :socket t))
    
(define-method draw-background block (image)
  (with-fields (x y width height) self
    (/draw-patch self x y (+ x width) (+ y height) image)))

(define-method draw-ghost block (image)
  (with-fields (x y width height) self
    (/draw-patch self x y (+ x width) (+ y height) image
		 :depressed t :socket t)))

(define-method handle-width block ()
  (+ (* 2 *dash-size*)
     (expression-width <operation>)))

(defparameter *socket-width* (* 18 *dash-size*))

(defun expression-width (expression &optional (font *block-font*))
  (if (iosketch:object-p expression)
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
	  (setf height (+ dash max-height)))))))

(define-method draw-expression block (x0 y0 segment type image)
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

(define-method draw-contents block (image)
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

(define-method draw block (image)
  (/draw-background self image)
  (/draw-contents self image))

(define-method hit block (mouse-x mouse-y)
  "Return this block (or child block) if the coordinates MOUSE-X and
MOUSE-Y identify a point inside the block (or child block.)"
  (with-fields (x y width height arguments) self
    (when (within-extents mouse-x mouse-y x y 
			  (+ x width) (+ y height))
      (labels ((hit (block)
		 (/hit block mouse-x mouse-y)))
	(let* ((child (some #'hit arguments))
	       (pos (position child arguments)))
	  (values (or child self) pos))))))
     		      
(define-method drop block (mouse-x mouse-y other-block)
  (multiple-value-bind (child position)
      (/hit self mouse-x mouse-y)
    (when (and position (/null child))
      (/plug other-block self position))))

;;; The null block

(define-method null block ()) 

(defblock null)

(define-method null null () t)

(defun null-block () (clone =null=))

(define-method run null (recipient)
  (declare (ignore recipient)))

(defparameter *null-display-string* "()")
;; (string #\LATIN_SMALL_LETTER_O_WITH_STROKE))

(define-method layout null ()
  (with-fields (height width) self
    (setf width (+ (* 4 *dash-size*)
		   (font-text-extents *null-display-string*
				      *block-font*))
	  height (+ (* 4 *dash-size*)
		    (font-height *block-font*)))))

(define-method draw-contents null (image)
  (with-block-drawing image
    (with-fields (x y) self
      (text (+ x (* 2 *dash-size*))
	    (+ y *dash-size* 1)
	    *null-display-string*))))

;;; Vertically stacked list of blocks

(defblock list
  (type :initform :structure))

(define-method execute list (recipient)
  (declare (ignore recipient))
  <results>)

(define-method initialize list (&rest args)
  (when args (setf <arguments> args)))

(define-method layout list ()
  (with-fields (x y arguments height width) self
    (let* ((dash *dash-size*)
	   (y0 (+ y dash))
	   (line-height (font-height *block-font*)))
      (setf height (+ (* 2 dash) line-height))
      (setf width (* 8 dash))
      (dolist (block arguments)
	(/move block (+ x dash) y0)
	(/layout block)
	(incf height (field-value :height block))
	(incf y0 (field-value :height block))
	(setf width (max width (field-value :width block))))
      (incf width (* 2 dash)))))

(define-method draw-contents list (image)
  (with-field-values (arguments) self
    (dolist (block arguments)
      (/draw block image))))

;;; Data entry blocks

(defblock entry 
  (type :initform :data)
  (schema :iniform nil)
  (data :initform nil))

(define-method execute entry (recipient)
  (declare (ignore recipient))
  <data>)

(define-method set-data entry (data)
  (setf <data> data))

(define-method draw entry (image)
  (with-block-drawing image
    (with-fields (x y data) self
      ;; (/draw-background self image)
      (text (+ x (* 2 dash))
	    (+ y dash 1)
	    (print-expression data)))))

(define-method layout entry ()
  (with-fields (height width data) self
    (setf height (+ (* 4 *dash-size*) (font-height *block-font*)))
    (setf width (+ (* 4 *dash-size*) (expression-width data)))))

(defmacro defentry (name data)
  `(define-prototype ,name (:parent =entry=)
     (operation :initform ,(make-keyword name))
     (data :initform ,data)))

(defentry integer 0)
(defentry string "")
(defentry float 0.0)
(defentry symbol nil)

;;; IF block

(defblock if 
  (type :initform :control)
  (result :initform nil)
  (schema :initform '(:block :block :block))
  (arguments :initform '(nil nil nil)))

(define-method execute if (recipient)
  <results>)

(define-method execute-arguments if (recipient)
  (with-fields (arguments results) self
    (destructuring-bind (predicate then else) arguments
      (if (/run predicate recipient)
	  (/run then recipient)
	  (/run else recipient)))))

;;; Get field value

(defblock my 
  (type :initform :variables)
  (schema :initform '(:block)))

(define-method execute my (recipient)
  (with-fields (results) self
    (field-value (make-keyword (car results))
		 recipient)))

;;; Set field value

(defblock set
  (type :initform :variables)
  (schema :initform '(:symbol :anything))
  (arguments :initform '(:counter 1)))

;;; Talking 

(defblock emote 
  (type :initform :looks)
  (schema :initform '(:string)))

(define-method execute emote (recipient)
  (/emote recipient 
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

(define-method execute play-music (recipient)
  (/play-music recipient (first <results>) :loop t))

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

(defblock start
  (type :initform :system) 
  (schema :initform nil)
  (arguments :initform nil))

(defblock stop
  (type :initform :system) 
  (schema :initform nil)
  (arguments :initform nil))
  
(defblock +
  (type :initform :operators)
  (schema :initform '(:number :number))
  (arguments :initform '(nil nil)))

(define-method execute + (recipient)
  (with-fields (results) self
    (when (every #'integerp results)
      (apply #'+ results))))
	  
(defun is-event-block (thing)
  (and (not (null thing))
       (iosketch:object-p thing)
       (has-field :operation thing)
       (eq :do (field-value :operation thing))))

;;; Composing blocks into larger programs

(define-prototype script ()
  (blocks :initform '()
	  :documentation "List of blocks in the script.")
  (recipient :initform nil)
  (variables :initform (make-hash-table :test 'eq)))

(define-method initialize script (&key blocks variables recipient)
  (setf <blocks> blocks)
  (when variables (setf <variables> variables))
  (when recipient (setf <recipient> recipient)))

(defvar *script*)

(define-method set-recipient script (recipient)
  (setf <recipient> recipient))

(define-method add script (block x y)
  (with-fields (blocks) self
    (setf blocks (adjoin block blocks))
    (/move block x y)))

(define-method run script (block)
  (with-fields (blocks recipient) self
    (assert (member block blocks))
    (/run block recipient)))
	    
(define-method bring-to-front script (block)
  (with-fields (blocks) self
    (when (find block blocks)
      (setf blocks (delete block blocks))
      (setf blocks (nconc blocks (list block))))))

(define-method delete script (block)
  (with-fields (blocks) self
    (setf blocks (delete block blocks))))

(define-method set script (var value)
  (setf (gethash var <variables>) value))

(define-method get script (var)
  (gethash var <variables>))

(defun script-variable (var-name)
  (/get *script* var-name))

(defun (setf script-variable) (var-name value)
  (/set *script* var-name value))

(defmacro with-script-variables (vars &rest body)
  (labels ((make-clause (sym)
	     `(,sym (script-variable ,(make-keyword sym)))))
    (let* ((symbols (mapcar #'make-non-keyword vars))
	   (clauses (mapcar #'make-clause symbols)))
      `(symbol-macrolet ,clauses ,@body))))

;;; Script editor widget

(defwidget editor
  (script :initform nil 
	  :documentation "The IOSKETCH:=SCRIPT= object being edited.")
  (selection :initform ()
  	     :documentation "Subset of selected blocks.")
  (focus :initform nil
  	 :documentation "Block with current focus.")
  (drag :initform nil 
  	:documentation "Block being dragged, if any.")
  (ghost :initform (clone =block=))
  (buffer :initform nil)
  (drag-start :initform nil
	      :documentation "A cons (X . Y) of widget location at start of dragging.")
  (drag-offset :initform nil
	       :documentation "A cons (X . Y) of mouse click location on dragged block.")
  (needs-redraw :initform t)
  (modified :initform nil 
	  :documentation "Non-nil when modified since last save."))

(define-method resize editor (&key width height)
  (with-fields (buffer image) self
    (when (null buffer)
      (setf buffer (create-image width height)))
    (unless (and (= <width> width)
		 (= <height> height))
      (/parent/resize self :width width :height height)
      (when buffer
	(sdl:free buffer))
      (setf buffer (create-image width height)))))

(define-method redraw editor ()
  (with-fields (script buffer needs-redraw width height) self
    (with-fields (blocks) script
      (draw-box 0 0 width height 
		:color *background-color*
		:stroke-color *background-color*
		:destination buffer)
      (dolist (block blocks)
	(/layout block))
      (dolist (block blocks)
	(/draw block buffer)))
    (setf needs-redraw nil)))

(define-method render editor ()
  (with-fields 
      (script needs-redraw image buffer drag-start selection focus
      drag modified ghost) self
    (labels ((copy ()
	       (draw-image buffer 0 0 :destination image)))
      (when script
	(when needs-redraw 
	  (/redraw self)
	  (copy))
	(when drag 
	  (copy)
	  (/layout drag)
	  (/draw-ghost ghost image)
	  (/draw drag image))))))

(define-method begin-drag editor (mouse-x mouse-y block)
  (with-fields (drag drag-start ghost drag-offset) self
    (setf drag block)
    (let ((parent (field-value :parent block)))
      (when parent
	(/unplug parent block)))
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

(define-method hit-blocks editor (x y)
  (with-fields (script) self
    (when script 
      (with-fields (blocks) script
	(labels ((hit (b)
		   (/hit b x y)))
	  (let ((parent (find-if #'hit blocks :from-end t)))
	    (when parent
	      (/hit parent x y))))))))

(define-method mouse-down editor (x y &optional button)
  (with-fields (script) self 
    (let ((block (/hit-blocks self x y)))
      (when block
	(case button
	  (1 (/begin-drag self x y block))
	  (3 (/run script block)))))))

(define-method mouse-move editor (mouse-x mouse-y)
  (with-fields (script drag-offset drag-start drag) self
    (when drag
      (destructuring-bind (ox . oy) drag-offset
	(/move drag (- mouse-x ox) (- mouse-y oy))))))

(define-method mouse-up editor (x y &optional button)
  (with-fields 
      (script needs-redraw drag-offset drag-start selection focus drag modified) 
      self
    (/bring-to-front script drag)
    (setf drag-start nil)
    (setf drag-offset nil)
    (setf drag nil)
    (setf needs-redraw t)))
      
;;; blocks.lisp ends here
