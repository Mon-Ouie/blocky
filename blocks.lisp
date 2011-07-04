;;; blocks.lisp --- A visual programming language inspired by MIT Scratch

;; Copyright (C) 2010, 2011 David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: oop, languages, mouse, lisp, multimedia, hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hopes that it will be useful,
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

;; With my project Blocky I am making a Lisp-based visual programming
;; language very similar to BYOB, but with a pervasive Lisp flavor. In
;; addition there are some improvements, such as native OpenGL support
;; throughout, and of course the advantage of compiling your block
;; diagrams to optimized machine code via SBCL.

;; New block types and behaviors can be defined with the macro
;; `defblock' and subsequently replacing default methods of the base
;; block prototype via `define-method'. With the macro `make-block'
;; you can convert lisp expressions into working block
;; diagrams. Diagrams can be saved with `serialize' and `deserialize'.
;; With the `recompile' function, certain blocks can be optimized away
;; into a simpler lisp expression that does the same job. For example,
;; a block that sums its arguments could compile down into a call to
;; the #'+ function, and so on with things like LOOP and COND.

;; For more information on the design of Ioforms, see
;; http://ioforms.org/design.html

;; For more information on similar systems, see the following links:

;; http://scratch.mit.edu/
;; http://byob.berkeley.edu/
;; http://wiki.scratch.mit.edu/wiki/Category:Scratch_Modifications
;; http://en.wikipedia.org/wiki/Visual_programming_language

;;; Code:

(in-package :ioforms)

(defvar *script* nil)

(defparameter *block-categories*
  '(:system :motion :event :message :looks :sound :structure :data
    :menu :hover :control :comment :sensing :operators :variables)
  "List of keywords used to group blocks into different functionality
areas.")

(defparameter *background-color* "white"
  "The default background color of the IOFORMS user interface.")

(defparameter *socket-color* "gray80"
  "The default background color of block sockets.")

(defparameter *block-font* "sans-condensed-bold-11"
  "The font used in drawing block labels and input data.")

(defvar *dash* 3
  "Size in pseudo-pixels of (roughly) the size of the space between
two words. This is used as a unit for various layout operations.")

(defun dash (n &rest terms)
  (apply #'+ (* n *dash*) terms))

(defvar *pseudo-pixel-size* 1.0
  "Size in pixels of a pseudo-pixel.")

(defvar *target* nil)

(defmacro with-target (target &body body)
  `(let ((*target* ,target))
     ,@body))

(defvar *text-base-y* nil)

(define-prototype block ()
  ;; general information
  (inputs :initform nil :documentation 
"List of input (or `child') blocks.")
  (results :initform nil :documentation
"Computed result values from the input blocks.")
  (category :initform :data :documentation "Category name of block. See also `*block-categories*'.")
  (temporary :initform nil)
  (methods :initform nil)
  (parent :initform nil :documentation "Link to enclosing parent block, or nil if none.")
  (events :initform nil :documentation "Event bindings, if any.")
  (default-events :initform nil)
  (operation :initform :block :documentation "Keyword name of method to be invoked on target.")
  (excluded-fields :initform '(:events :input-widths :results :parent))
  ;; visual layout
  (x :initform 0 :documentation "Integer X coordinate of this block's position.")
  (y :initform 0 :documentation "Integer Y coordinate of this block's position.")
  (z :initform 0 :documentation "Integer Z coordinate of this block's position.")
  (scale-x :initform 1)
  (scale-y :initform 1)
  (blend :initform :alpha)
  (opacity :initform 1.0)
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
  `(define-prototype ,name (:parent "IOFORMS:BLOCK")
    (operation :initform ,(make-keyword name))
    ,@(if (keywordp (first args))
	  (plist-to-descriptors args)
	  args)))

(define-method update block ()
  "Update the simulation one step forward in time.
By default, just update each child block."
  (mapc #'update %inputs))

(define-method change-image block (image)
  (setf %image image))

(define-method pin block ()
  (setf %pinned t))

(define-method unpin block ()
  (setf %pinned nil))

(define-method is-pinned block ()
  %pinned)

(define-method count-inputs block ()
  (length %inputs))

(define-method is-temporary block ()
  %temporary)

(define-method count-toplevel-blocks block ()
  (with-field-values (inputs) self
    (- (length inputs)
       (count-if #'is-temporary inputs))))

(define-method toplevel-blocks block ()
  (with-field-values (inputs) self
    (remove-if #'is-temporary inputs)))
  
(define-method contains block (block)
  (with-fields (inputs) self
    (find block inputs :test 'eq :key #'find-object)))

(define-method delete-input block (block)
  (with-fields (inputs) self
    (assert (contains self block))
    (setf inputs (delete (find-object block)
			 inputs
			 :key #'find-object
			 :test 'eq))))

(defun input-position (self input)
  (assert (not (null input)))
  (with-fields (schema inputs) self
    (etypecase input
      (ioforms:object (position input inputs :key #'find-object :test 'eq))
      (string (position (find-object input) inputs :key #'find-object :test 'eq))
      (integer
       (if (< input (length inputs))
	   input
	   (error "No such input index ~S" input)))
      (keyword 
       (let ((index (position input schema :key #'first)))
	 (if (numberp index)
	     index
	     (error "No such input named ~S" input)))))))

(defun input (self name)
  (with-fields (inputs) self
    (assert (not (null inputs)))
    (nth (input-position self name) inputs)))

(defun (setf input) (self name value)
  (with-fields (inputs) self
;    (assert (not (object-p name)))
    (assert (not (null inputs)))
    (setf (nth (input-position self name) inputs)
	  ;; store the real link
  	  (find-object value))))

(define-method position-within-parent block ()
  (input-position %parent self))

(defun named-input-type (self name)
  (with-fields (schema) self
    (let ((index (position name schema :key #'first)))
      (if (numberp index)
	  (cdr (assoc name schema))
	  (error "No such input ~S" name)))))

(define-method set-parent block (parent)
  "Store a UUID link to the enclosing block PARENT.
If PARENT is nil, then the existing parent link is cleared."
    (assert (is-valid-connection parent self))
    (setf %parent (when parent 
		    ;; always store uuid to prevent circularity
		    (find-uuid parent))))
	       
(define-method get-parent block ()
  %parent)

(define-method register-uuid block ()
  (add-object-to-database self))

(define-method update-result-lists block ()
  (let ((len (length %inputs)))
    (setf %input-widths (make-list len :initial-element 0))
    (setf %results (make-list len))))
		   
(define-method initialize block (&rest blocks)
  "Prepare an empty block, or if BLOCKS is non-empty, a block
initialized with BLOCKS as inputs."
  (when blocks
    (setf %inputs blocks)
    (dolist (child blocks)
      (set-parent child self)))
  (update-result-lists self)
  (bind-any-default-events self)
  (register-uuid self))

;;; Defining keyboard/mouse/joystick events for blocks

(define-method click block (mouse-x mouse-y)
  (declare (ignore mouse-x mouse-y)))

(define-method initialize-events-table-maybe block (&optional force)
  (when (or force 
	    (null (has-local-value :events self)))
    (setf %events (make-hash-table :test 'equal))))

(define-method bind-event-to-function block (event-name modifiers func)
  "Bind the described event to invoke FUNC.
EVENT-NAME is a string giving the key name; MODIFIERS is a list of
keywords like :control, :alt, and so on."
  (initialize-events-table-maybe self)
  (let ((event (normalize-event (cons event-name modifiers))))
    (setf (gethash event %events)
	  func)))

(define-method unbind-event block (event-name modifiers)
  "Remove the described event binding."
  (remhash (normalize-event (cons event-name modifiers))
	   %events))

(define-method handle-event block (event)
  "Look up and invoke the function (if any) bound to EVENT. Return t
if a binding was found, nil otherwise. The second value returned is
the return value of the function (if any)."
  (with-fields (events) self
    (when events
      (let ((func (gethash event events)))
	(if func
	    (prog1 (values t (funcall func))
	      (signal-layout-needed self))
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

;;; Creating blocks from S-expressions
 
(defun is-action-spec (spec)
  (and (listp spec)
       (symbolp (first spec))))

(defun is-list-spec (spec)
    (and (not (null spec))
	 (listp spec)))

(defun is-null-block-spec (spec)
  (and (not (null spec))
       (listp spec)
       (= 1 (length spec))
       (null (first spec))))

(defparameter *builtin-entry-types* 
  '(integer float string symbol number))
 
(defun data-block (datum)
  (let* ((data-type (type-of datum))
	 (head-type (if (listp data-type)
			(first data-type)
			data-type))
	 (type-specifier (if (member head-type *builtin-entry-types*)
			     head-type data-type)))
	(new entry :value datum 
		   :type-specifier type-specifier)))
		    
(defvar *make-block-package* nil)

(defun make-block-package ()
  (project-package-name))

(defun make-block (sexp)
    "Expand VALUE specifying a block diagram into real blocks.
SEXP is of the form:

  (BLOCK-NAME ARG1 ARG2 ... ARGN)

Where BLOCK-NAME is the name of a prototype defined with `defblock'
and ARG1-ARGN are numbers, symbols, strings, or nested SEXPS."
  ;; use labels because we need to call make-block from inside
  (labels ((action-block (spec)
	     (destructuring-bind (proto &rest arguments) spec
	       (let ((prototype 		       
		      (find-prototype 
		       (make-prototype-id proto 
					  ;; wait, is this wrong? wrong prototype?
					  (or (make-block-package)
					      (find-package "IOFORMS")))))
		     (arg-blocks (mapcar #'make-block arguments)))
		 (message "arg-blocks ~S" (list (length arg-blocks)
						(mapcar #'find-uuid arg-blocks)))
		 (apply #'clone prototype arg-blocks))))
	   (list-block (items)
	     (apply #'clone "IOFORMS:LIST" (mapcar #'make-block items))))
    (cond ((is-null-block-spec sexp)
	   (null-block))
	  ((is-action-spec sexp)
	   (action-block sexp))
	  ((is-list-spec sexp)
	   (list-block sexp))
	  ((not (null sexp)) (data-block sexp)))))

;;; Generic method invocation block. The bread and butter of doing stuff.

(defblock send prototype method schema target label)

(define-method execute send ()
  (apply #'send %method 
	 (or %target *target*)
	 (mapcar #'execute %inputs)))

(define-method click send (x y)
  (declare (ignore x y))
  (execute self))

(define-method accept send (block)
  ;; make these click-align instead
  (declare (ignore block)))

(defun pretty-symbol-string (thing)
  (let ((name (etypecase thing
		(symbol (symbol-name thing))
		(string thing))))
    (string-downcase 
     (substitute #\Space #\- name))))

(define-method initialize send (&key prototype method label target)
  (next%initialize self)
  (setf %target target)
  (let ((schema (method-schema (find-prototype prototype) method))
	(inputs nil))
    (dolist (entry schema)
      (push (new entry
		 :value (schema-option entry :default)
		 :parent self
		 :type-specifier (schema-type entry)
		 :options (schema-options entry)
		 :name (concatenate 'string
				    ":" ;; mimic the keyword arguments visually
				    (symbol-name (schema-name entry))))
	    inputs))
    (when inputs 
      (setf %inputs (nreverse inputs)))
    (let ((category (method-option (find-prototype prototype)
				   method :category)))
      (when category (setf %category category))
      (setf %schema schema
	    %prototype prototype
	    %method method
	    %label (or label (pretty-symbol-string method))))))

(define-method draw send ()
  (with-fields (x y width height label inputs) self
    (draw-patch self x y (+ x width) (+ y height))
    (let ((*text-base-y* (+ y (dash 1))))
      (draw-label-string self label "white")
      (dolist (each inputs)
	(draw each)))))

(define-method draw-hover send ()
  nil)

(define-method layout send ()
  (with-fields (input-widths height width label) self
    (with-field-values (x y inputs) self
      (let* ((font *block-font*)
	     (dash (dash 1))
	     (left (+ dash dash x (font-text-extents label font)))
	     (max-height (font-height font)))
	(labels ((move-input (input)
		   (move-to input (+ left dash) y)
		   (layout input)
		   (setf max-height (max max-height (field-value :height input)))
		   (field-value :width input))
		 (layout-input (input)
		   (let ((measurement
			  (+ dash dash (move-input input))))
		     (prog1 measurement
		       (incf left measurement)))))
	  (setf input-widths (mapcar #'layout-input inputs))
	  (setf width (+ (- left x) (* 4 dash)))
	  (setf height (+ dash (if (null inputs)
				   dash 0) max-height)))))))

;;; Verifying the tree structure of the blocks    

(defun is-bad-connection (sink source)
  ;; make sure source is not actually sink's parent somewhere
  (block checking
    (prog1 nil
      (let ((pointer sink))
	(loop while pointer do
	  (if (eq (find-object pointer)
		  (find-object source))
	      (return-from checking t)
	      (setf pointer (find-parent pointer))))))))

(defun is-valid-connection (sink source)
  (not (is-bad-connection sink source)))

(defun connect-parent-links (block)
  (dolist (child (field-value :inputs block))
    (when child
      (set-parent child block)
      (connect-parent-links child))))

;;; Block movement

(define-method move-to block (x y &optional z)
  "Move the block to a new (X Y) location."
  (setf %x x)
  (setf %y y)
  (when z 
    (setf %z z)))

(define-method move-toward block (direction &optional (steps 1))
  (with-field-values (y x) self
    (multiple-value-bind (y0 x0)
	(step-in-direction y x direction steps)
      (setf x x0)
      (setf y y0))))

(define-method show block ()
  (setf %visible t))

(define-method hide block ()
  (setf %visible nil))

(define-method toggle-visibility block ()
  (if %visible
      (hide self)
      (show self)))

(define-method is-visible block ()
  %visible)

(define-method get-image block ()
  %image)

(define-method mouse-move block (x y)
  (declare (ignore x y)))

(define-method mouse-down block (x y button)
  (declare (ignore x y button)))

(define-method mouse-up block (x y button)
  (declare (ignore x y button)))

(define-method this-position block ()
  (with-fields (parent) self
    (when parent
      (input-position parent self))))

(define-method plug block (thing n)
  "Connect the block THING as the value of the Nth input."
  (set-parent thing self)
  (setf (input self n) thing))

(define-method unplug block (input)
  "Disconnect the block INPUT from this block."
  (with-fields (inputs) self
    (prog1 input
      (setf inputs (delete input inputs :test 'eq :key #'find-object))
      (set-parent input nil))))

(define-method unplug-from-parent block ()
  (with-fields (parent) self
    (when parent
      (unplug parent self))))

(define-method method-menu block (method target)
  (assert (and (keywordp method) (not (null target))))
  (let ((method-string (pretty-symbol-string method)))
    (list :label method-string
	  :action #'(lambda ()
		      (add-block *script* 
				 (new send 
				      :prototype (find-parent-prototype-name self)
				      :method method
				      :target target
				      :label method-string)
				 (- *pointer-x* 10) 
				 (- *pointer-y* 10))))))

(define-method context-menu block ()
  (apply #'make-menu 
	 (list (list :label (concatenate 'string 
					 "Methods: "
					 (get-some-object-name self)
					 "(" (object-address-string self) ")")
		     :inputs (mapcar #'(lambda (method)
					 (method-menu self method self))
				     %methods)
		     :pinned nil
		     :expanded t))))

(define-method execute-inputs block ()
  "Execute all blocks in %INPUTS from left-to-right. Results are
placed in corresponding positions of %RESULTS. Override this method
when defining new blocks if you don't want to evaluate all the
inputs all the time."
  (with-fields (inputs results) self
    (let ((arity (length inputs)))
      (when (< (length results) arity)
	(setf results (make-list arity)))
      (dotimes (n arity)
	(when (nth n inputs)
	  (setf (nth n results)
		(run (nth n inputs))))))))

(define-method execute block ()
  "Carry out whatever action this block implements. The %RESULTS field
will be a list of results obtained by executing/evaluating the
corresponding blocks in %INPUTS (but this behavior can be overridden;
see also `BLOCK/EXECUTE-INPUTS' and `BLOCK/RUN').  The default is to
do nothing."
  nil)

(define-method run block ()
  "Run input blocks to produce results, then run this block with
those results as input. If you need argument blocks unevaluated, 
override this RUN method and evaluate just the ones you want."
  (execute-inputs self)
  (execute self))

;;; recompilation: compiling block diagrams into equivalent sexps

(define-method recompile-body block ()
  nil)

(define-method recompile-inputs block ()
  (mapc #'recompile %inputs))

(define-method recompile block ()
  (recompile-inputs self)
  (recompile-body self))
  
  ;; (with-fields (operation results) self
  ;;   (labels ((clean (item)
  ;; 	       (if (symbolp item)
  ;; 		   (make-keyword item)
  ;; 		   item)))
  ;;     (when *target*
  ;; 	(apply #'ioforms:send nil operation *target*
  ;; 	       (mapcar #'clean results))))))

(define-method describe block ()
  "Show name and comprehensive help for this block."
  nil)

;; (define-method after-deserialize block ()
;;   "Make sure the block is ready after loading."
;;   (initialize self))

(defun count-tree (tree)
  "Return the number of blocks enclosed in this block, including the
current block. Used for taking a census."
  (cond ((null tree) 0)
	;; without inputs, just count the root
	((null (field-value :inputs tree)) 1)
	;; otherwise, sum up the counts of the children (if any)
	(t (apply #'+ 1 
		  (mapcar #'count-tree 
			  (field-value :inputs tree))))))

(defparameter *block-colors*
  '(:motion "cornflower blue"
    :system "gray50"
    :event "gray80"
    :menu "gray95"
    :hover "red"
    :socket "gray60"
    :data "gray50"
    :structure "gray80"
    :comment "khaki"
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
    :menu "gray80"
    :comment "goldenrod"
    :looks "medium orchid"
    :socket "gray80"
    :data "gray80"
    :structure "gray92"
    :sound "plum"
    :message "sienna2"
    :control "gold"
    :variables "DarkOrange1"
    :operators "OliveDrab1"
    :sensing "DeepSkyBlue2")
  "X11 color names of highlights on the different block categories.")

(defparameter *block-shadow-colors*
  '(:motion "royal blue"
    :system "gray50"
    :event "gray70"
    :socket "gray90"
    :data "gray55"
    :menu "gray70"
    :structure "gray60"
    :comment "goldenrod"
    :hover "orange red"
    :looks "dark orchid"
    :sound "violet red"
    :message "chocolate3"
    :control "dark orange"
    :variables "OrangeRed2"
    :operators "OliveDrab4"
    :sensing "steel blue")
  "X11 color names of shadows on the different block categories.")

(defparameter *block-foreground-colors*
  '(:motion "white"
    :system "white"
    :event "gray40"
    :comment "black"
    :socket "gray20"
    :hover "yellow"
    :data "white"
    :menu "gray40"
    :structure "gray90"
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
    (let ((result (getf colors %category)))
      (or result (prog1 nil (message "WARNING: cannot find color ~S" part))))))

(defparameter *selection-color* "red")

(defmacro with-block-drawing (&body body)
  "Run BODY forms with drawing primitives.
The primitives are CIRCLE, DISC, LINE, BOX, and TEXT. These are used
in subsequent functions as the basis of drawing nested diagrams of
blocks."
  `(let* ((foreground (find-color self :foreground))
	  (background (find-color self :background))
	  (highlight (find-color self :highlight))
	  (shadow (find-color self :shadow))
	  (radius (+ 6 *dash*))
	  (diameter (* 2 radius)))
     (labels ((circle (x y &optional color)
		(draw-circle x y radius
			     :color (or color background)
			     :blend :alpha))
	      (disc (x y &optional color)
		(draw-solid-circle x y radius
				   :color (or color background)
				   :blend :alpha))
	      (line (x0 y0 x1 y1 &optional color)
		(draw-line x0 y0 x1 y1
			   :color (or color background)))
	      (box (x y r b &optional color)
		(draw-box x y (- r x) (- b y)
			  :color (or color background)))
	      (text (x y string &optional color2)
		(draw-string string x 
			     (or *text-base-y* y)
			     :color (or color2 foreground)
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
			      (if dark background background)))))
;      (disc (- x0 10) (- y0 10) fill) ;; a circle by itself
      ;; y1 x1
      (disc (- x1 radius ) (- y1 radius ) fill)
      (circle (- x1 radius ) (- y1 radius ) chisel) ;; chisel
      ;; y1 left
      (disc (+ x0 radius ) (- y1 radius ) fill)
      (circle (+ x0 radius ) (- y1 radius) chisel)
      ;; top left
      (disc (+ x0 radius ) (+ y0 radius) fill)
      (circle (+ x0 radius ) (+ y0 radius) bevel) ;;bevel
      ;; top x1
      (disc (- x1 radius ) (+ y0 radius ) fill)
      (circle (- x1 radius ) (+ y0 radius ) chisel) ;; chisel
      ;; y1
      (box (+ x0 radius) (- y1 diameter)
	   (- x1 radius 1) y1
	   fill)
      (line (+ x0 radius -2) y1
	    (- x1 radius 1) y1 chisel)
      ;; top
      (box (+ x0 radius) y0
	   (- x1 radius) (+ y0 diameter)
	   fill)
      (line (+ x0 radius) (+ y0 1)
	    (- x1 radius -4) (+ y0 1) bevel)
      ;; left
      (box x0 (+ y0 radius)
	   (+ x0 diameter) (- y1 radius)
	   fill)
      (line (+ x0 1) (+ y0 radius)
	    (+ x0 1) (- y1 radius -3) bevel)
      ;; x1
      (box (- x1 diameter) (+ y0 radius)
	   x1 (- y1 radius)
	   fill)
      (line x1 (+ y0 radius)
	    x1 (- y1 radius) chisel)
      ;; content area
      (box (+ x0 radius) (+ y0 radius)
	   (- x1 radius) (- y1 radius)
	   fill)
      ;; cover seams
      (disc (- x1 radius 1) (- y1 radius 1) fill) ;; y1 x1
      (disc (+ x0 radius 1) (- y1 radius 1) fill) ;; y1 left
      (disc (+ x0 radius 1) (+ y0 radius 1) fill) ;; top left
      (disc (- x1 radius 1) (+ y0 radius 1) fill) ;; top x1
      )))

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
     (expression-width %operation)))

(define-method header-height block () 0)

(define-method header-width block () %width)

(defparameter *socket-width* (* 18 *dash*))

(defun print-expression (expression)
  (assert (not (object-p expression)))
  (string-downcase
   (typecase expression
     (symbol
	(substitute #\Space #\- (symbol-name expression)))
     (otherwise (format nil "~s" expression)))))

(defun expression-width (expression &optional (font *block-font*))
  (if (ioforms:object-p expression)
      *socket-width*
      (font-text-extents (print-expression expression) font)))

(define-method layout block () nil)

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
	    (with-block-drawing 
	      (text x0 (+ y0 dash 1)
		  (print-expression segment))
	      (setf width (expression-width segment)))))
      width)))

(define-method after-deserialize block ()
  "Make sure the block is ready after loading."
  (initialize self))

(define-method draw-contents block ()
  (with-fields (operation inputs) self
    (draw-label self operation)
    (dolist (each inputs)
      (draw each))))

(define-method draw-label-string block (string &optional color)
  (with-block-drawing 
    (with-field-values
	(x y operation inputs)
	self
      (let* ((dash *dash*)
	     (left (+ x (* 2 dash)))
	     (y0 (+ y dash 1)))
	(text left y0 string color)))))

(define-method draw-label block (expression)
  (draw-label-string self (print-expression expression)))

(define-method draw block ()
  (with-fields (image x y width height blend opacity) self
    (if image 
	(progn (set-blending-mode blend)
	       (draw-image image x y))
	(progn (draw-patch self x y (+ x width) (+ y height))
	       (draw-contents self)))))

(defparameter *highlight-background-color* "gray80")
(defparameter *highlight-foreground-color* "gray20")

(define-method get-focus block () nil)

(define-method lose-focus block () nil)

(define-method draw-focus block () nil)

(define-method draw-highlight block () nil)
  ;; (with-fields (x y width height) self
  ;;   (draw-patch self x y (+ x *dash* width) (+ y *dash* height)
  ;; 	      :color *highlight-background-color*)
  ;;   (draw-contents self)))

(defparameter *hover-color* "red")

(define-method draw-hover block ()
  (with-fields (x y width height inputs) self
    (draw-patch self x y (+ x *dash* width) (+ y *dash* height)
	      :color *hover-color*)
    (dolist (input inputs)
      (draw input))))

(define-method hit block (mouse-x mouse-y)
  "Return this block (or input block) if the coordinates MOUSE-X and
MOUSE-Y identify a point inside the block (or input block.)"
  (with-fields (x y width height inputs) self
    (when (within-extents mouse-x mouse-y x y
			  (+ x width) (+ y height))
      (labels ((try (it)
		 (hit it mouse-x mouse-y)))
	(let ((result (some #'try inputs)))
	  (or result self))))))

(define-method accept block (other-block)
  (with-field-values (parent) self
    (when parent
      (prog1 t
	(let ((position (input-position parent self)))
	  (assert (integerp position))
	  (assert (is-valid-connection parent other-block))
	  (plug parent other-block position))))))

;;; Printing a block

(defun print-block (B)
  (let (fields)
    (flet ((add-field (field value)
	     (push (list field value) fields)))
      (typecase B
	(ioforms:object 
	 (let ((f2 (object-fields B)))
	   (etypecase f2
	     (hash-table (maphash #'add-field f2))
	     (list (setf fields f2)))
	   (cons (get-some-object-name B) fields)))
	(list (mapcar #'print-block B))
	(otherwise B)))))

;;; Vertically stacked list of blocks

(defblock list
  (dash :initform 2)
  (operation :initform :empty-list)
  (category :initform :structure))

(defparameter *null-display-string* "...")

(defun null-block () (clone "IOFORMS:LIST"))

(define-method click list (x y)
  (dolist (block %inputs)
    (run block)))

(define-method accept list (input &optional prepend)
  (with-fields (inputs) self
    (if inputs
	;; we've got inputs. add it to the list (prepending or not)
	(prog1 t
	  (assert (is-valid-connection self input))
	  ;; set parent if necessary 
	  (when (get-parent input)
	    (unplug-from-parent input))
	  (set-parent input self)
	  (setf inputs 
		(if prepend
		    (nconc (list input) inputs)
		    (nconc inputs (list input)))))
    	;; no inputs yet. make a single-element inputs list
	(prog1 t (setf inputs (list input))))))

(define-method take-first list ()
  (with-fields (inputs) self
    (let ((block (first inputs)))
      (prog1 block
	(unplug self block)))))

(define-method length list ()
  (length %inputs))

;; (define-method unplug list (input)
;;   (with-fields (inputs) self
;;     (delete-input input self)
;;     (set-parent input nil)))

(define-method header-height list () 0)

(define-method handle-width list ()
  (+ (* 2 *dash*)
     (expression-width *null-display-string*)))

(define-method layout-as-null list ()
  (with-fields (height width) self
    (setf width (+ (* 4 *dash*)
		   (font-text-extents *null-display-string*
				      *block-font*))
	  height (+ (font-height *block-font*) (* 4 *dash*)))))

(define-method layout-as-list list ()
  (with-fields (x y height width inputs dash) self
    (flet ((ldash (&rest args)
	     (apply #'dash dash args)))
    (let* ((header-height (ldash (header-height self)))
	   (y0 (ldash y header-height))
	   (line-height (font-height *block-font*)))
      (setf height (ldash line-height))
      (setf width (dash 8))
      (dolist (element inputs)
;	(message "layout: ~S" (list x y0 width))
	(move-to element (ldash x) y0)
	(layout element)
	(incf height (+ (ldash) (field-value :height element)))
	(incf y0 (field-value :height element))
	(setf width (max width (field-value :width element))))
      (incf width (dash 10))))))

(define-method layout list ()
  (with-fields (inputs) self
    (if (null inputs)
	(layout-as-null self)
	(layout-as-list self))))

(define-method draw-header list () 0)

(define-method draw list ()
  (with-fields (inputs) self
    (draw-background self)
    (if (null inputs)
	(draw-label-string self *null-display-string*)
	(dolist (each inputs)
	  (draw each)))))

(define-method initialize list (&rest blocks)
  (with-fields (inputs) self
    (next%initialize self)
    (setf inputs blocks)))

(defmacro with-script (script &rest body)
  `(let ((*script* ,script))
     ,@body))

;; Notice that this resize method resizes a BLOCK, not a script.
(define-method resize block (&key height width)
  (when (or (not (= height %height))
	    (not (= width %width)))
    (setf %height height)
    (setf %width width)
    (when *script* 
      (send :report-layout-change *script*))))

(define-method signal-layout-needed block ()
  (when *script*
    (send :report-layout-change *script*)))

(defblock script
  (menu :initform nil)
  (target :initform nil)
  (needs-layout :initform t)
  (variables :initform (make-hash-table :test 'eq)))

(define-method report-layout-change script ()
  (setf %needs-layout t))

(define-method bring-to-front script (block)
  (with-fields (inputs script) self
    (assert (contains script block))
    (delete-input self block)
    (append-input self block)))

(define-method delete-block script (block)
  (assert (contains self block))
  (delete-input self block))

(define-method update script ()
  (with-script self 
    (dolist (each %inputs)
      (update each))
    (update-layout self)))

(define-method update-layout script (&optional force)
  (with-fields (inputs needs-layout) self
    (when (or force needs-layout)
      (dolist (each inputs)
	(layout each))
      (setf needs-layout nil))))

(define-method initialize script (&key blocks variables target)
  (setf %blocks blocks)
  (when variables (setf %variables variables))
  (when target (setf %target target)))

(define-method set-target script (target)
  (setf %target target))

(define-method append-input script (block)
  (with-fields (inputs) self
    (assert (not (contains self block)))
    (set-parent block self)
    (setf inputs (nconc inputs (list block)))))

(define-method add-block script (block &optional x y)
  (with-fields (inputs) self
    (assert (not (contains self block)))
    (append-input self block)
    (when (and (integerp x)
	       (integerp y))
      (move-to block x y))
    (report-layout-change self)))

(define-method run script ())
  ;; (with-fields (inputs target) self
  ;;   (with-target target
  ;;     (dolist (block inputs)
  ;; 	(run block)))))

;; (define-method header-height script ()
;;   (with-fields (x y inputs) self
;;     (let ((name (first inputs))
;; 	  (height (font-height *block-font*)))
;;       (prog1 height
;; 	(move-to name
;; 	       (+ x (handle-width self))
;; 	       (+ y height))))))

;; (define-method draw-header script ()
;;   (prog1 (font-height *block-font*)
;;     (with-fields (x y) self
;;       (with-block-drawing 
;; 	(text (+ x *dash* 1)
;; 	      (+ y *dash* 1)
;; 	      "script")))))

;; (define-method set script (var value)
;;   (setf (gethash var %variables) value))

;; (define-method get script (var)
;;   (gethash var %variables))

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
