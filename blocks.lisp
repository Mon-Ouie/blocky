;;; blocks.lisp --- core visual language model for Blocky

;; Copyright (C) 2010, 2011 David O'Toole

;; Author: David O'Toole <dto@ioforms.org>
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

;; Please see the included file README.org for an overview.

;;; Code:

(in-package :blocky)

(defvar *buffer* nil
  "When non-nil, the UUID of the current buffer object.")

(defparameter *block-categories*
  '(:system :motion :event :message :looks :sound :structure :data
    :menu :hover :control :comment :sensing :operators :variables)
  "List of keywords used to group blocks into different functionality
areas.")

(defparameter *background-color* "white"
  "The default background color of the BLOCKY user interface.")

(defparameter *socket-color* "gray80"
  "The default background color of block sockets.")

(defparameter *block-font* "sans-11"
  "Name of the font used in drawing block labels and input data.")

(defparameter *font* *block-font*)

(defmacro with-font (font &rest body)
  `(let ((*font* ,font))
     ,@body))

(defparameter *sans* "sans-11"
  "Name of the default sans-serif font.")

(defparameter *serif* "serif-11"
  "Name of the default serif font.")

(defparameter *monospace* "sans-mono-10"
  "Name of the default monospace (fixed-width) font.")

(defvar *dash* 3
  "Size in pseudo-pixels of (roughly) the size of the space between
two words. This is used as a unit for various layout operations.
See also `*style'.")

(defun dash (&optional (n 1) &rest terms)
  "Return the number of pixels in N dashes. Add any remaining
arguments. Uses `*dash*' which may be configured by `*style*'."
  (apply #'+ (* n *dash*) terms))

(defvar *pseudo-pixel-size* 1.0
  "Size in pixels of a pseudo-pixel.")

(defvar *text-base-y* nil)

(defparameter *cursor-blink-time* 8 
  "The number of frames the cursor displays each color while blinking.")

(defparameter *cursor-color* "magenta" 
  "The color of the cursor when not blinking.")

(defparameter *cursor-blink-color* "cyan"
  "The color of the cursor when blinking.")

(define-prototype block 
    (:documentation
     "This is the base prototype for all objects in the Blocky system."
     )
  (cursor-clock :initform *cursor-blink-time*)
  ;; general information
  (inputs :initform nil :documentation 
	  "List of input (or `child') blocks.")
  (results :initform nil :documentation
	   "Computed result values from the input blocks.")
  (category :initform :data :documentation "Category name of block. See also `*block-categories*'.")
  (tags :initform nil)
  (temporary :initform nil)
  (methods :initform '(:make-reference :duplicate :make-sibling))
  (parent :initform nil :documentation "Link to enclosing parent block, or nil if none.")
  (events :initform nil :documentation "Event bindings, if any.")
  (default-events :initform nil)
  (description :initform nil :documentation "A description of the block.") 
  (operation :initform :block :documentation "Keyword name of method to be invoked on target.")
  (excluded-fields :initform nil) ;; Yay!
  ;; visual layout
  (x :initform 0 :documentation "Integer X coordinate of this block's position.")
  (y :initform 0 :documentation "Integer Y coordinate of this block's position.")
  (z :initform 0 :documentation "Integer Z coordinate of this block's position.")
  ;; possible grid location
  (row :documentation "When non-nil, the current row location of the block.")
  (column :documentation "When non-nil, the current column of the block.")
  ;; scaling/blending
  (scale-x :initform 1)
  (scale-y :initform 1)
  (blend :initform :alpha)
  (opacity :initform 1.0)
  ;; collisions
  (collision-type :initform :aabb)
  ;; dimensions
  (width :initform 32 :documentation "Cached width of block.")
  (height :initform 32 :documentation "Cached height of block.")
  (depth :initform 32 :documentation "Cached z-depth of block.")
  (pinned :initform nil :documentation "When non-nil, do not allow dragging.")
  (visible :initform t :documentation "When non-nil, block will be visible.")
  ;; morphic style halo
  (halo :initform nil)
  (mode :initform nil)
  (name :initform nil)
  (variables :initform (make-hash-table :test 'eq))
  (needs-layout :initform t)
  (label :initform nil)
  (image :initform nil :documentation "Texture to be displayed, if any."))

;;; Standard means of defining blocks

(defmacro define-block (spec &body args)
  (let ((name0 nil)
	(super0 "BLOCKY:BLOCK"))
    (etypecase spec
      (symbol (setf name0 spec))
      (list (destructuring-bind (name &key super) spec
	      (setf name0 name)
	      (when super (setf super0 super)))))
    `(define-prototype ,name0 (:super ,(make-prototype-id super0))
       (operation :initform ,(make-keyword name0))
       ,@(if (keywordp (first args))
	  (plist-to-descriptors args)
	  args))))

;;; Defining input events for blocks

;; Typical lambdas aren't serializable, so I use these blocks.

(define-block closure method arguments target)

(define-method initialize closure (method target &optional arguments)
  (assert (and method 
	       (listp arguments)
	       (find-uuid target)))
  (setf %method (make-keyword method)
	%arguments arguments
	%target (find-uuid target)))

(define-method evaluate closure ()
  (apply #'send %method %target %arguments))

;;; Using closures to respond to events

(define-method initialize-events-table-maybe block (&optional force)
  (when (or force 
	    (not (has-local-value :events self)))
    (setf %events (make-hash-table :test 'equal))))

(define-method bind-event-to-closure block (event-name modifiers closure)
  "Bind the described event to invoke the action of the CLOSURE.
EVENT-NAME is either a keyword symbol identifying the keyboard key, or
a string giving the Unicode character to be bound. MODIFIERS is a list
of keywords like :control, :alt, and so on."
  (assert (find-object closure))
  (initialize-events-table-maybe self)
  (let ((event (make-event event-name modifiers)))
    (setf (gethash event %events)
	  closure)))

(define-method unbind-event block (event-name modifiers)
  "Remove the described event binding."
  (remhash (normalize-event (cons event-name modifiers))
	   %events))

(define-method on-event block (event)
  "Look up and invoke the block closure (if any) bound to
EVENT. Return the closure if a binding was found, nil otherwise. The
second value returned is the return value of the evaluated closure (if
any)."
  (with-fields (events) self
    (when events
      (let ((closure 
	      ;; unpack event
	      (destructuring-bind (head &rest modifiers) event
		;; if head is a cons, check for symbol binding first,
		;; then for unicode binding. we do this because we'll
		;; often want to bind keys like ENTER or BACKSPACE
		;; regardless of their Unicode interpretation 
		(if (consp head)
		    (or (gethash (cons (car head) ;; try symbol
				       modifiers)
				 events)
			(gethash (cons (cdr head) ;; try unicode
				       modifiers)
				 events))
		    ;; it's not a cons. 
		    ;; just search event as-is
		    (gethash event events)))))
	(if closure
	    (prog1 (values closure (evaluate closure))
	      (invalidate-layout self))
	    (values nil nil))))))

(define-method on-text-event block (event)
  "Look up events as with `on-event', but insert unhandled keypresses
as Unicode characters via the `insert' function."
  (with-fields (events) self
    (destructuring-bind (key . unicode) (first event)
      (when (or (on-event%%block self (cons key (rest event)))
		;; treat Unicode characters as self-inserting
		(when unicode
		  (send :insert self unicode)))
	(invalidate-layout self)))))

(defun bind-event-to-method (block event-name modifiers method-name)
  "Arrange for METHOD-NAME to be sent as a message to this object
whenever the event (EVENT-NAME . MODIFIERS) is received."
  (destructuring-bind (key . mods) 
      (make-event event-name modifiers)
    (bind-event-to-closure block 
			   key
			   mods
			   (new closure method-name block))))

(define-method bind-event block (event binding)
  (destructuring-bind (name &rest modifiers) event
    (etypecase binding
      (symbol (bind-event-to-method self name modifiers binding))
      (list 
       ;; create a method call 
       (let ((closure (new closure
			   (make-keyword (first binding))
			   self
			   (rest binding))))
	 (bind-event-to-closure self name modifiers closure))))))

(define-method bind-any-default-events block ()
  (with-fields (default-events) self
    (when default-events
      (initialize-events-table-maybe self)
      (dolist (entry default-events)
	(apply #'bind-event self entry)))))

(defun bind-event-to-text-insertion (self key mods text)
  (bind-event-to-closure self key mods 
			 (new closure :insert self (list text))))
    
(define-method insert block (string)
  (declare (ignore string))
  nil)

(defvar *lowercase-alpha-characters* "abcdefghijklmnopqrstuvwxyz")
(defvar *uppercase-alpha-characters* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defvar *numeric-characters* "0123456789")
(defvar *graphic-characters* "`~!@#$%^&*()_-+={[}]|\:;\"'<,>.?/")

(defparameter *text-qwerty-keybindings*
  '(("a" (:control) :beginning-of-line)
    ("e" (:control) :end-of-line)
    ("f" (:control) :forward-char)
    ("b" (:control) :backward-char)
    (:home nil :beginning-of-line)
    (:end nil :end-of-line)
    (:right nil :forward-char)
    (:left nil :backward-char)
    ("k" (:control) :clear-line)
    (:backspace nil :backward-delete-char)
    (:delete nil :delete-char)
    ("d" (:control) :delete-char)
    (:return nil :enter)
    ("x" (:control) :exit)
    ("g" (:control) :exit)
    (:escape nil :exit)
    ("p" (:alt) :backward-history)
    ("n" (:alt) :forward-history)  
    (:up nil :backward-history)
    (:down nil :forward-history)))

(defparameter *arrow-key-text-navigation-keybindings*
  '((:up nil :previous-line)
    (:down nil :next-line)
    (:left nil :backward-char)
    (:right nil :forward-char))) 

(defun keybinding-event (binding)
  (cons (first binding)
	(second binding)))

(defun keybinding-action (binding)
  (nthcdr 2 binding))

(define-method install-keybindings block (keybindings)
  (dolist (binding keybindings)
    (bind-event self 
		(keybinding-event binding)
		(keybinding-action binding))))
        
(define-method install-text-keybindings block ()
  ;; install UI keys that will vary by locale
  (with-fields (events) self
    (setf events (make-hash-table :test 'equal))
    (dolist (binding *text-qwerty-keybindings*)
      (destructuring-bind (key mods result) binding
	(etypecase result
	  (keyword (bind-event-to-method self key mods result))
	  (string (bind-event-to-text-insertion self key mods result)))))))

;;; Each object has a Squeak-style pop-up halo

(define-method make-halo block ()
  (when (null %halo)
    (setf %halo (new halo self))
    (drop self %halo)))

(define-method discard-halo block ()
  (discard %halo)
  (setf %halo nil))

(define-method toggle-halo block ()
  (if %halo
      (discard-halo self)
      (make-halo self)))

(define-method on-drag block (x y)
  (move-to self x y))

(define-method can-escape block ()
  t)

(define-method discard block ()
  (mapc #'discard %inputs)
  (when %parent 
    (unplug-from-parent self))
  (push self (symbol-value '*trash*)))

;;; Serialization hooks

(define-method before-serialize block ())

(define-method after-deserialize block ()
  "Prepare a deserialized block for running."
  (register-uuid self))

(define-method on-update block ()
  "Update the simulation one step forward in time.
By default, just update each child block."
  (mapc #'on-update %inputs))

(define-method change-image block (image)
  (setf %image image))

(define-method pin block ()
  "Prevent dragging and moving of this block."
  (setf %pinned t))

(define-method unpin block () 
  "Allow dragging and moving of this block."
  (setf %pinned nil))

(define-method is-pinned block ()
  "When non-nil, dragging and moving are disallowed for this block."
  %pinned)

(define-method count-inputs block ()
  (length %inputs))

(defun is-temporary (thing)
  (and (has-field :temporary thing)
       (field-value :temporary thing)))

(define-method count-top-level-blocks block ()
  (with-field-values (inputs) self
    (- (length inputs)
       (count-if #'is-temporary inputs))))

(define-method top-level-blocks block ()
  (with-field-values (inputs) self
    (remove-if #'is-temporary inputs)))
  
(define-method contains block (block)
  (find (find-object block)
	%inputs
	:test 'eq
	:key #'find-object))

(defun input-position (self input)
  (assert (not (null input)))
  (with-fields (schema inputs) self
    (etypecase input
      (blocky:object (position input inputs :key #'find-object :test 'eq))
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

(defun (setf input) (self name block)
  (with-fields (inputs) self
;    (assert (not (object-p name)))
    (assert (not (null inputs)))
    (set-parent block self)
    (setf (nth (input-position self name) inputs)
	  ;; store the real link
  	  (find-object block))))

(define-method position-within-parent block ()
  (input-position %parent self))

(defun named-input-type (self name)
  (with-fields (schema) self
    (let ((index (position name schema :key #'first)))
      (if (numberp index)
	  (cdr (assoc name schema))
	  (error "No such input ~S" name)))))

(define-method set-parent block (parent)
  "Store a UUID link to the enclosing block PARENT."
  (assert (not (null parent)))
  (assert (is-valid-connection parent self))
  (setf %parent (when parent 
		  ;; always store uuid to prevent circularity
		  (find-uuid parent))))
	       
(define-method get-parent block ()
  %parent)

(define-method find-parent block ()
  (find-uuid %parent))

(defun is-valid-connection (sink source)
  (assert (or sink source))
  ;; make sure source is not actually sink's parent somewhere
  (block checking
    (prog1 t
      (let ((pointer sink))
	(loop while pointer do
	  (if (eq (find-object pointer)
		  (find-object source))
	      (return-from checking nil)
	      (setf pointer (find-parent pointer))))))))

(define-method register-uuid block ()
  (add-object-to-database self))

(define-method update-result-lists block ()
  (let ((len (length %inputs)))
    (setf %input-widths (make-list len :initial-element 0))
    (setf %results (make-list len))))

(define-method delete-input block (block)
  (with-fields (inputs) self
    (prog1 t
      (assert (contains self block))
      (setf inputs (remove block inputs
			   :key #'find-object
			   :test 'eq))
      (assert (not (contains self block))))))

(define-method default-inputs block ()
  nil)
 
(define-method deep-copy block () 
  nil) ;; not defined for generic blocks

(define-method duplicate block ()
  (clone (find-super self)))
  
(define-method initialize block (&rest blocks)
  "Prepare an empty block, or if BLOCKS is non-empty, a block
initialized with BLOCKS as inputs."
  (setf %inputs 
	(or blocks (default-inputs self)))
  (dolist (child blocks)
    (set-parent child self))
  (update-result-lists self)
  (bind-any-default-events self)
  (register-uuid self))

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
	 (type-specifier 
	   (if (member head-type *builtin-entry-types* :test 'equal)
			     head-type data-type)))
    ;; see also terminal.lisp for more on data entry blocks
    (typecase datum
      (string (new string :value datum))
      (symbol (new symbol :value datum))
      (otherwise (new entry :value datum :type-specifier type-specifier)))))
		    
(defvar *make-block-package* nil)

(defun make-block-package ()
  (or (project-package-name) (find-package :blocky)))

(defun make-block (sexp)
    "Expand VALUE specifying a block diagram into real blocks.
SEXP is of the form:

  (BLOCK-NAME ARG1 ARG2 ... ARGN)

Where BLOCK-NAME is the name of a prototype defined with `define-block'
and ARG1-ARGN are numbers, symbols, strings, or nested SEXPS."
  ;; use labels because we need to call make-block from inside
  (labels ((action-block (spec)
	     (destructuring-bind (proto &rest arguments) spec
	       (let ((prototype 		       
		      (find-prototype 
		       (make-prototype-id proto 
					  ;; wait, is this wrong? wrong prototype?
					  (or (make-block-package)
					      (find-package "BLOCKY")))))
		     (arg-blocks (mapcar #'make-block arguments)))
		 (message "arg-blocks ~S" (list (length arg-blocks)
		 				(mapcar #'find-uuid arg-blocks)))
		 (apply #'clone prototype arg-blocks))))
	   (list-block (items)
	     (apply #'clone "BLOCKY:LIST" (mapcar #'make-block items))))
    (cond ((is-null-block-spec sexp)
	   (null-block))
	  ((blockyp sexp) ;; catch UUIDs etc
	   sexp)
	  ((is-action-spec sexp)
	   (action-block sexp))
	  ((is-list-spec sexp)
	   (list-block sexp))
	  ((not (null sexp)) (data-block sexp)))))

(define-method resize block (&key height width)
  (setf %height height)
  (setf %width width)
  (invalidate-layout self))

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

(define-method on-select block () nil)

(define-method on-tap block (x y)
  (declare (ignore x y))
  nil)

(define-method on-alternate-tap block (x y)
  (toggle-halo self))

(define-method on-point block (x y)
  (declare (ignore x y)))

(define-method on-press block (x y button)
  (declare (ignore x y button)))

(define-method on-release block (x y button)
  (declare (ignore x y button)))

;;; Connecting blocks

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
  (with-fields (inputs parent) self
    (assert (contains self input))
    (prog1 input
      (setf inputs 
	    (delete input inputs 
		    :test 'eq :key #'find-object)))))

(define-method unplug-from-parent block ()
  (prog1 t
    (with-fields (parent) self
      (assert (not (null parent)))
      (assert (contains parent self))
      (unplug parent self)
      (assert (not (contains parent self)))
      (setf parent nil))))

(define-method make-method-menu-item block (method target)
  (assert (and target (keywordp method)))
  (let ((method-string (pretty-symbol-string method)))
    (list :label method-string
	  :method method
	  :target target
	  :action (new closure method target))))

(define-method drop block (other-block)
  (add-block *buffer* other-block %x %y))

(define-method context-menu block ()
  (let ((methods nil)
	(pointer self))
    ;; gather methods
    (loop do
      (when (has-local-value :methods pointer)
	(setf methods 
	      (union methods 
			    (field-value :methods pointer))))
      (setf pointer (object-super pointer))
      while pointer)
    ;; 
    (let (inputs)
      (dolist (method methods)
	(push (make-method-menu-item self method (find-uuid self)) inputs))
      (make-menu
       (list :label (concatenate 'string 
				 "Methods: "
				 (get-some-object-name self)
				 " " (object-address-string self))
	     :inputs (nreverse inputs)
	     :pinned nil
	     :expanded t
	     :locked t)
       :target (find-uuid self)))))

(define-method make-reference block ()
  (new reference self))

;;; evaluation and recompilation: compiling block diagrams into equivalent sexps

(define-method evaluate-inputs block ()
  "Evaluate all blocks in %INPUTS from left-to-right. Results are
placed in corresponding positions of %RESULTS. Override this method
when defining new blocks if you don't want to evaluate all the inputs
all the time."
  (with-fields (inputs results) self
    (let ((arity (length inputs)))
      (when (< (length results) arity)
	(setf results (make-list arity)))
      (dotimes (n arity)
	(when (nth n inputs)
	  (setf (nth n results)
		(evaluate (nth n inputs))))))
    results))

(define-method evaluate block () self)

;; (define-method evaluate block () 
;;   "Return the computed result of this block.  By default, all the
;; inputs are evaluated."
;;   (prog1 self
;;     (evaluate-inputs self)))

(define-method recompile block ()
  `(progn 
     ,@(mapcar #'recompile %inputs)))

;;; Context-sensitive user help

;; (define-method describe block ()
;;   "Show name and comprehensive help for this block."
;;   nil)

;; (define-method after-deserialize block ()
;;   "Make sure the block is ready after loading."
;;   nil)

(defun count-tree (tree)
  "Return the number of blocks enclosed in this block, including the
current block. Used for taking a count of all the nodes in a tree."
  (cond ((null tree) 0)
	;; without inputs, just count the root
	((null (field-value :inputs tree)) 1)
	;; otherwise, sum up the counts of the children (if any)
	(t (apply #'+ 1 
		  (mapcar #'count-tree 
			  (field-value :inputs tree))))))

;;; Drawing blocks

(defparameter *block-colors*
  '(:motion "cornflower blue"
    :system "gray50"
    :event "gray80"
    :menu "gray95"
    :hover "red"
    :socket "gray60"
    :data "gray50"
    :structure "gray50"
    :comment "gray83"
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
    :comment "gray88"
    :looks "medium orchid"
    :socket "gray80"
    :data "gray80"
    :structure "gray60"
    :sound "plum"
    :message "sienna2"
    :control "gold"
    :variables "DarkOrange1"
    :operators "OliveDrab1"
    :sensing "DeepSkyBlue2")
  "X11 color names of highlights on the different block categories.")

(defparameter *block-shadow-colors*
  '(:motion "royal blue"
    :system "gray40"
    :event "gray70"
    :socket "gray90"
    :data "gray55"
    :menu "gray70"
    :structure "gray35"
    :comment "gray70"
    :hover "orange red"
    :looks "dark magenta"
    :sound "violet red"
    :message "chocolate3"
    :control "dark orange"
    :variables "DarkOrange3"
    :operators "OliveDrab4"
    :sensing "steel blue")
  "X11 color names of shadows on the different block categories.")

(defparameter *block-foreground-colors*
  '(:motion "white"
    :system "white"
    :event "gray40"
    :comment "gray20"
    :socket "gray20"
    :hover "yellow"
    :data "white"
    :menu "gray40"
    :structure "white"
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
  (let* ((colors (ecase part
		  (:background *block-colors*)
		  (:highlight *block-highlight-colors*)
		  (:shadow *block-shadow-colors*)
		  (:foreground *block-foreground-colors*)))
	 (category (or %category :data))
	 (result (getf colors category)))
      (prog1 result 
	(assert category)
	(assert result))))

(defparameter *selection-color* "red")

(defparameter *styles* '((:rounded :dash 3)
		   (:flat :dash 1)))

(defvar *style* :flat)

(defmacro with-style (style &rest body)
  (let ((st (gensym)))
  `(let* ((,st ,style)
	  (*style* ,st)
	  (*dash* (or (getf *styles* ,st)
		      *dash*)))
     ,@body)))
     
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
			     :font *font*)))
       ,@body)))

(define-method draw-rounded-patch block (x0 y0 x1 y1
				    &key depressed dark socket color)
  "Draw a standard BLOCKY block notation patch.
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
      ;; y1 (bottom) 
      (box (+ x0 radius) (- y1 diameter)
	   (- x1 radius 1) y1
	   fill)
      (line (+ x0 radius -2) (1- y1)
	    (- x1 radius 1) y1 chisel)
      ;; top
      (box (+ x0 radius) y0
	   (- x1 radius) (+ y0 diameter)
	   fill)
      (line (+ x0 radius) (+ y0 0)
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

(define-method draw-flat-patch block (x0 y0 x1 y1
				    &key depressed dark socket color)
  "Draw a panel with top left corner at (X0 Y0), bottom right at (X1
Y1). If DEPRESSED is non-nil, draw an indentation; otherwise a raised
area is drawn. If DARK is non-nil, paint a darker region."
  (with-block-drawing 
    (let ((bevel (or color (if depressed shadow highlight)))
	  (chisel (or color (if depressed highlight shadow)))
	  (fill (or color (if socket
			      *socket-color*
			      (if dark background background)))))
      ;; content area
      (box x0 y0  
	   x1 y1
	   fill)
      ;; bottom
      (line x0 y1 
	    x1 y1 
	    chisel)
      ;; top
      (line x0 y0
	    x1 y0 
	    bevel)
      ;; left
      (line x0 y0
	    x0 y1 
	    bevel)
      ;; right
      (line x1 y0
	    x1 y1 
	    chisel)
      )))

(define-method draw-patch block (x0 y0 x1 y1 
				    &key depressed dark socket color)
  (let ((draw-function (ecase *style*
			 (:rounded #'draw-rounded-patch)
			 (:flat #'draw-flat-patch))))
    (funcall draw-function self
	     x0 y0 x1 y1 
	     :depressed depressed :dark dark 
	     :socket socket :color color)))

(define-method draw-socket block (x0 y0 x1 y1)
  (draw-patch self x0 y0 x1 y1 :depressed t :socket t))

;; (define-method draw-emblem block (&optional force)
;;   (with-fields (emblem x y) self
;;     (when (or force emblem)
;;       (draw-indicator emblem (- x (dash 3)) y 
;; 		      :color "red" :background "white"
;; 		      :scale 3))))

;;; Blinking cursor

(define-method update-cursor-clock block ()
  ;; keep the cursor blinking
  (with-fields (cursor-clock) self
    (decf cursor-clock)
    (when (> (- 0 *cursor-blink-time*) cursor-clock)
      (setf cursor-clock *cursor-blink-time*))))

(define-method draw-cursor-glyph block 
    (&optional (x 0) (y 0) (width 2) (height (font-height *font*))
	       &key color blink)
  (with-fields (cursor-clock) self
    (update-cursor-clock self)
    (let ((color2
	    (if blink
		(if (minusp cursor-clock)
		    *cursor-color*
		    *cursor-blink-color*)
		*cursor-color*)))
      (draw-box x y width height :color (or color color2)))))

(define-method draw-cursor block (&rest ignore) nil)

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

;; (define-method label-width block ()
;;   (+ (* 2 *dash*)
;;      (expression-width %operation)))

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

(defun expression-width (expression &optional (font *font*))
  (if (blocky:object-p expression)
      *socket-width*
      (font-text-width (print-expression expression) font)))

;; (define-method layout block () 
;;   (with-fields (x y width height inputs) self
;;     (setf width (dash 2))
;;     (setf height (dash 2))
;;     (let ((left (+ x (dash 1)))
;; 	  (top (+ y (dash 1))))
;;       (dolist (input inputs)
;; 	(layout input)
;; 	(move-to input left top)
;; 	(let ((width0 (field-value :width input)))
;; 	  (incf left (+ (dash 2) width0))
;; 	  (incf width (+ width0 (dash 2)))
;; 	  (setf height (max height (field-value :height input))))))
;;     (incf height (dash 2))
;;     (incf width (dash 2))))

(define-method layout-as-image block ()
  (with-fields (height width image) self
    (setf height (image-height image))
    (setf width (image-width image))))

(define-method layout block () 
  (if %image 
      (layout-as-image self)
      (with-fields (height width label) self
	(with-field-values (x y inputs) self
	  (let* ((left (+ x (label-width self)))
		 (max-height (font-height *font*))
		 (dash (dash 1)))
	    (dolist (input inputs)
	      (move-to input (+ left dash) y)
	      (layout input)
	      (setf max-height (max max-height (field-value :height input)))
	      (incf left (dash 1 (field-value :width input))))
	    ;; now update own dimensions
	    (setf width (dash 1 (- left x)))
	    (setf height (+ dash (if (null inputs)
				     dash 0) max-height)))))))
  
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

(define-method draw-inputs block ()
  (mapc #'draw %inputs))

(define-method draw-contents block ()
  (with-fields (operation inputs) self
    ;; (draw-label self operation)
    (dolist (each inputs)
      (draw each))))

(define-method update-parent-links block ()
  (dolist (each %inputs)
    (set-parent each self)))

;;; Labels for blocks

(define-method set-label-string block (label)
  (assert (stringp label))
  (setf %label label))

(define-method label-string block ()
  %label)

(define-method label-width block ()
  (if (null %label)
      0
      (+ (dash 2)
	 (font-text-width %label *font*))))
    
(define-method draw-label-string block (string &optional color)
  (with-block-drawing 
    (with-field-values (x y) self
      (let* ((dash *dash*)
	     (left (+ x (* 2 dash)))
	     (y0 (+ y dash 1)))
	(text left y0 string color)))))

(define-method draw-label block (expression)
  (draw-label-string self (print-expression expression)))

;;; Sound 

(define-method play-sound block (sample-name)
  (play-sample sample-name))

;;; General block drawing

(define-method draw block ()
  (with-fields (image x y width height blend opacity) self
    (if image 
	(progn (set-blending-mode blend)
	       (draw-image image x y))
	(progn (draw-patch self x y (+ x width) (+ y height))
	       (draw-contents self)))))

(defparameter *highlight-background-color* "gray80")
(defparameter *highlight-foreground-color* "gray20")

(define-method on-focus block () nil)

(define-method on-lose-focus block () nil)

(define-method grab-focus block () 
  (send :focus-on (symbol-value '*shell*) self))

(define-method draw-focus block () nil)

(define-method draw-highlight block () nil)

(defparameter *hover-color* "red")

;; (define-method draw-hover block ())

(define-method draw-hover block ()
  (with-fields (x y width height inputs) self
    (draw-patch self x y (+ x *dash* width) (+ y *dash* height)
	      :color *hover-color*)
    (dolist (input inputs)
      (draw input))))

(define-method update-image-dimensions block ()
  (with-fields (image height width scale-x scale-y) self
    (when image
      (setf width (* scale-x (image-width image)))
      (setf height (* scale-y (image-height image))))))

(define-method draw-as-sprite block ()
  (with-fields (image x y z height opacity blend scale-x scale-y) self
    (when image
      (when (null height)
	(update-image-dimensions self))
      (draw-image image x y :z z 
			    :opacity opacity 
			    :blend blend 
			    :scale-x scale-x
			    :scale-y scale-y))))

(define-method change-image block (image)
  (assert (stringp image))
  (setf %image image)
  (update-image-dimensions self))

(define-method adopt block (child)
  (when (get-parent child)
    (unplug-from-parent child))
  (set-parent child self))

(define-method can-pick block ()
  (not %pinned))

(define-method pick block ()
  self) ;; Possibly return a child, or a new object 

(define-method accept block (other-block)
  "Try to accept OTHER-BLOCK as a drag-and-dropped input. Return
non-nil to indicate that the block was accepted, nil otherwise."
  nil)

(define-method parent-is-buffer block ()
  (assert (not (null *buffer*)))
  (object-eq %parent *buffer*))

(define-method is-top-level block ()
  (object-eq %parent *buffer*))

;;; Block tags

(define-method has-tag block (tag)
  "Return non-nil if this block has the specified TAG.

Blocks may be marked with tags that influence their processing by the
engine. The field `%tags' is a set of keyword symbols; if a symbol
`:foo' is in the list, then the block is in the tag `:foo'.

Although a game built on BLOCKY can define whatever tags are
needed, certain base tags are built-in and have a fixed
interpretation:

 -    :obstacle --- Blocks movement and causes collisions
 -    :temporary --- This block is not preserved when exiting a world.
 -    :light-source --- This object casts light. 
 -    :opaque --- Blocks line-of-sight, casts shadows. 
"
  (member tag %tags))

(define-method add-tag block (tag)
  "Add the specified TAG to this block."
  (pushnew tag %tags))

(define-method remove-tag block (tag)
  "Remove the specified TAG  from this block."
  (setf %tags (remove tag %tags)))

;;; Collision detection

(define-method hit block (mouse-x mouse-y)
  "Return this block (or child input block) if the coordinates MOUSE-X
and MOUSE-Y identify a point inside the block (or input block.)"
  (with-fields (x y width height inputs) self
    (when (within-extents mouse-x mouse-y x y
			  (+ x width) (+ y height))
      (labels ((try (it)
		 (hit it mouse-x mouse-y)))
	(or (some #'try inputs) 
	    self)))))

(define-method bounding-box block ()
  (multiple-value-bind (x y)
      (xy-coordinates self)
    (let ((size (field-value :grid-size *world*)))
      (values x y size size))))

(define-method collide block (object)
  (declare (ignore object))
  "Respond to a collision detected with OBJECT."
  nil)

(define-method destroy block ()
  (remove-block *world* self)
  (discard self))

(define-method move-to-grid block (row column)
  (with-field-values (grid-size) *world*
    (move-to self (* grid-size row) (* grid-size column))))

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

(define-method bounding-box block ()
  (when (null %height)
    (update-image-dimensions self))
  (values %x %y %width %height))

(define-method colliding-with-rectangle block (o-top o-left o-width o-height)
  ;; you must pass arguments in Y X order since this is TOP then LEFT
  (with-fields (x y width height) self
    (point-in-rectangle-p x y width height o-top o-left o-width o-height)))

(define-method colliding-with block (thing)
  (multiple-value-bind (x y width height) 
      (bounding-box thing)
    (colliding-with-rectangle self y x width height)))

(define-method collide block (thing))

;;; Analog gamepad control

(define-method aim block (direction)
  (setf %direction direction))

(define-method stick-move block (&optional multiplier)
  (destructuring-bind (horizontal vertical) *joystick-motion-axes*
    (let* ((x (poll-joystick-axis horizontal))
	   (y (poll-joystick-axis vertical)))
      (let ((direction 
	      (cond 
		;; diagonal 
		((and (axis-pressed-p horizontal) (axis-pressed-p vertical))
		 (if (minusp y) 
		     (if (minusp x)
			 :northwest
			 :northeast)
		     (if (minusp x)
			 :southwest
			 :southeast)))
		;; horizontal 
		((axis-pressed-p horizontal)
		 (if (minusp x) :west :east))
		;; vertical 
		((axis-pressed-p vertical)
		 (if (minusp y) :north :south)))))
	(when direction 
	  ;; if the player pushed a direction, move in that direction.
	  (prog1 t 
	    (if multiplier
		(move-to self 
			 (+ %x (* multiplier (axis-as-float horizontal)))
			 (+ %y (* multiplier (axis-as-float vertical))))
		(move self direction multiplier))
	    ;; if the player is NOT pressing on the right stick, ALSO aim in this direction.
	    (destructuring-bind (aim-horz aim-vert) *joystick-aiming-axes*
	      (when (not (or (axis-pressed-p aim-horz)
			     (axis-pressed-p aim-vert)))
		(aim self direction)))))))))

(define-method stick-aim block ()
  (with-fields (direction) self
    (destructuring-bind (horizontal vertical) *joystick-aiming-axes*
      (let ((x (poll-joystick-axis horizontal))
	    (y (poll-joystick-axis vertical)))
	(cond ((and (axis-pressed-p horizontal) (axis-pressed-p vertical))
	       (aim self (if (minusp y) 
			     (if (minusp x)
				 :northwest
				 :northeast)
			     (if (minusp x)
				 :southwest
				 :southeast))))
	      ((axis-pressed-p horizontal)
	       (aim self (if (minusp x) :west :east)))
	      ((axis-pressed-p vertical)
	       (aim self (if (minusp y) :north :south))))))))

;;; Grouping blocks into buffers with buffer-local variables

(defun get-buffer (name)
  (gethash name *buffers*))

(defun make-buffer (&rest args)
  (let* ((buffer (apply #'clone :buffer args))
	 (name (field-value :name buffer))) ;; name may be uniqified
    (assert (not (gethash name *buffers*)))
    (prog1 buffer
      ;; add it to the table
      (setf (gethash name *buffers*)
	    (find-uuid buffer)))))
  
(defun uniquify-buffer-name (name)
  (let ((n 1)
	(name0 name))
    (block naming
      (loop while name0 do
	(if (get-buffer name0)
	    (setf name0 (format nil "~A.~S" name n)
		  n (1+ n))
	    (return-from naming name0))))))

(defmacro with-buffer (buffer &rest body)
  `(let ((*buffer* (find-uuid ,buffer)))
     (assert (blockyp *buffer*))
     ,@body))

(define-method setvar block (var value)
  (setf (gethash var %variables) value))

(define-method getvar block (var)
  (gethash var %variables))

(defun buffer-local-variable (var-name)
  (getvar *buffer* var-name))

(defun (setf buffer-local-variable) (var-name value)
  (setvar *buffer* var-name value))

(defmacro with-buffer-local-variables (vars &rest body)
  (labels ((make-clause (sym)
	     `(,sym (buffer-local-variable ,(make-keyword sym)))))
    (let* ((symbols (mapcar #'make-non-keyword vars))
	   (clauses (mapcar #'make-clause symbols)))
      `(symbol-macrolet ,clauses ,@body))))

(define-method queue-layout block ()
  (setf %needs-layout t))

(define-method invalidate-layout block ()
  (when *buffer*
    (queue-layout *buffer*)))

(define-method bring-to-front block (block)
  (with-fields (inputs block) self
    (assert (contains buffer block))
    (delete-input self block)
    (append-input self block)))

(define-method on-update block ()
  (with-buffer self 
    (dolist (each %inputs)
      (on-update each))
    (update-layout self)))

(define-method update-layout block (&optional force)
  (with-fields (inputs needs-layout) self
    (when (or force needs-layout)
      (dolist (each inputs)
	(layout each))
      (setf needs-layout nil))))

(define-method append-input block (block)
  (assert (blockyp block))
  (with-fields (inputs) self
    (assert (not (contains self block)))
    (set-parent block self)
    (setf inputs (nconc inputs (list block)))))

(define-method add-block block (block &optional x y)
  (assert (blockyp block))
  ;(assert (not (contains self block)))
  (append-input self block)
  (when (and (integerp x)
	     (integerp y))
    (move-to block x y))
  (invalidate-layout self))

(define-method delete-block block (block)
  (assert (blockyp block))
  (assert (contains self block))
  (delete-input self block))

;;; Vertically stacked list of blocks

(define-block list
  (dash :initform 2)
  (frozen :initform nil)
  (orientation :initform :vertical)
  (operation :initform :empty-list)
  (category :initform :structure))

(define-method is-frozen list () %frozen)

(define-method freeze list ()
  (setf %frozen t)
  (mapc #'pin %inputs))

(define-method unfreeze list ()
  (setf %frozen nil)
  (mapc #'unpin %inputs))

(define-method evaluate list () 
  "Return the computed result of this block.  By default, all the
inputs are evaluated."
  (prog1 self
    (evaluate-inputs self)))

(defparameter *null-display-string* "...")

(define-method set-orientation list (orientation)
  (assert (member orientation '(:horizontal :vertical)))
  (setf %orientation orientation))

(define-method on-tap list (x y)
  (dolist (block %inputs)
    (evaluate block)))

(define-method accept list (input &optional prepend)
  (when (not %frozen)
    (assert (blockyp input))
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
		      (append (list input) inputs)
		      (append inputs (list input)))))
	  ;; no inputs yet. make a single-element inputs list
	  (prog1 input 
	    (setf inputs (list input))
	    (set-parent input self))))))

(define-method take-first list ()
  (with-fields (inputs) self
    (let ((block (first inputs)))
      (prog1 block
	(unplug self block)))))

(define-method get-length list ()
  (length %inputs))

(define-method header-height list () 0)

(define-method label-width list ()
  (+ (* 2 *dash*)
     (expression-width *null-display-string*)))

(define-method layout-as-null list ()
  (with-fields (height width) self
    (setf width (+ (* 4 *dash*)
		   (font-text-width *null-display-string*
				      *font*))
	  height (+ (font-height *font*) (* 4 *dash*)))))

(define-method layout-vertically list ()
  (with-fields (x y height width inputs dash) self
    (flet ((ldash (&rest args)
	     (apply #'dash 1 args)))
    (let* ((header-height (ldash (header-height self)))
	   (y0 (ldash y header-height))
	   (line-height (font-height *font*)))
      (setf height (ldash line-height))
      (setf width (dash 8))
      (dolist (element inputs)
	(move-to element (ldash x) y0)
	(layout element)
	(incf height (field-value :height element))
	(incf y0 (field-value :height element))
	(setf width (max width (field-value :width element))))
      (incf height (dash 1))
      (incf width (dash 3))))))

(define-method layout-horizontally list ()
  (with-fields (x y height width inputs dash) self
    (flet ((ldash (&rest args)
	     (apply #'dash 1 args)))
    (let* ((header-height (ldash (header-height self)))
	   (x0 (ldash x))
	   (y0 (ldash y))
	   (line-height (font-height *font*)))
      (setf height (ldash line-height))
      (setf width (dash 8))
      (dolist (element inputs)
	(move-to element (ldash x0) y0)
	(layout element)
	(setf height (max height (+ (ldash) (field-value :height element))))
	(incf x0 (field-value :width element)))
      (incf height (dash 1))
      (incf width (dash 10))))))

(define-method layout list ()
  (with-fields (inputs) self
    (if (null inputs)
	(layout-as-null self)
	(ecase %orientation
	  (:horizontal (layout-horizontally self))
	  (:vertical (layout-vertically self))))))

(define-method draw-header list () 0)

(define-method draw list ()
  (with-fields (inputs) self
    (draw-background self)
    (if (null inputs)
	(draw-label-string self *null-display-string*)
	(dolist (each inputs)
	  (draw each)))))

(define-method initialize list (&rest blocks)
  (apply #'super%initialize self blocks)
  ;; allow them to be freely removed
  (unfreeze self))

(defmacro deflist (name &rest body)
  `(define-block (,name :super :list) ,@body))

(defun null-block () (new list))

(deflist empty-socket)

(define-method accept empty-socket (other-block)
  "Replace this empty socket with OTHER-BLOCK."
  (accept %parent other-block))

;;; Sending to a particular target

(defvar *target* nil)

(defmacro with-target (target &rest body)
  `(let ((*target* ,target))
     ,@body))

;;; Generic method invocation block. The bread and butter of doing stuff.

(define-block send prototype method schema target label)

(define-method evaluate send ()
  (apply #'send %method 
	 (or *target* %target) ;; with-target will override
	 (mapcar #'evaluate %inputs)))

(define-method on-tap send (x y)
  (declare (ignore x y))
  (evaluate self))

(define-method accept send (block)
  ;; make these click-align instead
  (assert (blockyp block))
  nil)

(defun-memo pretty-symbol-string (thing)
    (:key #'first :test 'equal :validator #'identity)
  (let ((name (etypecase thing
		(symbol (symbol-name thing))
		(string thing))))
    (string-downcase 
     (substitute #\Space #\- name))))

(define-method initialize send (&key prototype method label target)
  (super%initialize self)
  (setf %target target)
  (let ((schema (method-schema (find-prototype prototype) method))
	(inputs nil))
    (dolist (entry schema)
      (push (new entry
		 :value (schema-option entry :default)
		 :parent (find-uuid self)
		 :type-specifier (schema-type entry)
		 :options (schema-options entry)
		 :label (concatenate 'string
				    ":" ;; mimic the keyword arguments visually
				    (string-downcase (symbol-name (schema-name entry)))))
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

;;; A generic color swatch

(define-block color 
  :pinned nil
  :methods '(:set-color)
  :name "gray50"
  :width (dash 20) :height (dash 20))

(define-method (set-color :category :looks) color
    ((name string :default "gray50"))
  (setf %name name))

(define-method draw color ()
  (with-fields (x y width height red green blue) self
    (with-style :rounded
      (draw-patch self x y (+ x width) (+ y height)
		  :color %name))))

(define-method layout color ())

(define-method initialize color (&optional (name "gray50"))
  (super%initialize self)
  (setf %name name))

;;; Printing a block

(defun print-block (B)
  (let (fields)
    (flet ((add-field (field value)
	     (push (list field value) fields)))
      (typecase B
	(blocky:object 
	 (let ((f2 (object-fields B)))
	   (etypecase f2
	     (hash-table (maphash #'add-field f2))
	     (list (setf fields f2)))
	   (cons (get-some-object-name B) fields)))
	(list (mapcar #'print-block B))
	(otherwise B)))))

(defun split-string-on-lines (string)
  (with-input-from-string (stream string)
    (loop for line = (read-line stream nil)
	  while line collect line)))

;;; blocks.lisp ends here
