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
;; Scratch. For more information see:

;; http://scratch.mit.edu/
;; http://byob.berkeley.edu/
;; http://wiki.scratch.mit.edu/wiki/Category:Scratch_Modifications

;;; Code:

(in-package :iosketch)

;; For the design I've followed a motto associated with the visual
;; programming language Pure Data: "The diagram is the program."
;; Since the diagram is 2D, the program must therefore be
;; two-dimensional as well. That means every block in the program
;; (i.e. every expression) must have an X,Y position.

;; Besides a 2D position, each block has a (possibly empty) list of
;; arguments. Arguments are symbols like :move or :play-sound, or data
;; arguments such as numbers, strings, or symbols. Arguments may also be
;; objects and this involves nested blocks in the diagram.

(define-prototype block ()
  (arguments :documentation "List of block argument values.")
  (schema :documentation "List of CL type specifiers for corresponding expressions in <arguments>.")
  (operation :initform "block" :documentation "Symbol name of block's operation, i.e. message key.")
  (type :documentation "Type name of block. See also `*block-types*'.")
  (x :initform 0 :documentation "Integer X coordinate of this block's position.")
  (y :initform 0 :documentation "Integer Y coordinate of this block's position.")
  (width :initform 32 :documentation "Cached width of block.")
  (height :initform 32 :documentation "Cached height of block.")
  (parent :initform nil)
  (widgets :initform nil)
  (excluded-fields :initform '(:widgets :parent)))

(defmacro defblock (name &body args)
  `(define-prototype ,name (:parent =block=)
     (operation :initform ,(make-keyword name))
     ,@args))

(defparameter *block-types*
  '(:system :motion :event :message :looks :sound 
    :control :comment :sensing :operators :variables))

(defparameter *argument-types*
  '(:block :anything :body :integer :float :number :string :symbol))

(define-method move block (x y)
  (setf <x> x)
  (setf <y> y))

(define-method set-parent block (parent)
  (setf <parent> parent))

(define-method get-argument block (index)
  (nth index <arguments>))

(define-method set-argument block (index value)
    (setf (nth index <arguments>) value))

(define-method plug block (child index)
  (/set-argument self index child)
  (/set-parent child self))

(define-method unplug block (child)
  (let ((pos (position child <arguments>)))
    (/set-argument self pos nil)
    (/set-parent child nil)))

(define-method execute block (recipient)
  "Send the appropriate message to the RECIPIENT object."
  (apply #'iosketch:send nil <operation> recipient <arguments>))

(define-method describe block ()
  "Show name and comprehensive help for this block.")

(define-method initialize block (&rest arguments)
  (setf <widgets> (make-list (length <schema>)))
  (when arguments 
    (assert (= (length arguments)
	       (length <arguments>)))
    (setf <arguments> arguments)))

(define-method deserialize block ()
  (/initialize self))

(define-method count block () 1)

(defparameter *display-widgets*
   '(:integer =integer=
     :float =float=
     :number =number=
     :string =textbox=
     :symbol =option=))

(defparameter *background-color* ".white")

(defparameter *block-font* "sans-condensed-bold-12")

(defvar *dash-size* 3)

(defparameter *block-colors* 
  '(:motion ".cornflower blue"
    :system ".gray50"
    :event ".gray80"
    :socket ".gray60"
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
    :socket ".gray45"
    :comment ".grey40"
    :looks ".dark orchid"
    :sound ".violet red"
    :message ".chocolate3"
    :control ".dark orange"
    :variables ".OrangeRed2"
    :operators ".OliveDrab3"
    :sensing ".turquoise3")
  "X11 color names of shadows on the different block types.")

(defparameter *block-foreground-colors* 
  '(:motion ".white"
    :system ".white"
    :event ".gray40"
    :comment ".gray30"
    :socket ".gray20"
    :message ".white"
    :looks ".white"
    :sound ".white"
    :control ".white"
    :variables ".white"
    :operators ".white"
    :sensing ".white")
  "X11 color names of the text used for different block types.")

(define-method find-color block (&optional (part :background))
  (let ((colors (ecase part
		  (:background *block-colors*)
		  (:highlight *block-highlight-colors*)
		  (:shadow *block-shadow-colors*)
		  (:foreground *block-foreground-colors*))))
    (getf colors <type>)))

(define-method hit block (click-x click-y)
  (with-fields (x y width height) self
    (when (within-extents click-x click-y 
			  x y 
			  (+ x width)
			  (+ y height))
      self)))

(define-method line-string block ()
  (with-fields (operation arguments) self
    (labels ((clean (thing) 
      (let ((output (mapcar #'clean (cons operation arguments))))
	(string-downcase (format nil "~{~s~^ ~}" output))))))))

(defun print-segment (segment)
  (string-downcase 
   (typecase segment
     (keyword 
	(substitute #\Space #\- (symbol-name segment)))
     (otherwise (format nil "~s" segment)))))

(defparameter *socket-width* (* 18 *dash-size*))

(defun segment-width (segment &optional (font *block-font*))
  (if (iosketch:object-p segment)
      *socket-width*
      (font-text-extents (print-segment segment) font)))

(define-method handle-width block ()
  (+ (* 2 *dash-size*)
     (segment-width <operation>)))

(defmacro with-block-drawing (image &body body)
  (let ((image-sym (gensym)))
    `(with-field-values (height width) self
       (let* ((foreground (/find-color self :foreground))
	      (background (/find-color self :background))
	      (highlight (/find-color self :highlight))
	      (shadow (/find-color self :shadow))
	      (dash *dash-size*)
	      (label (/line-string self))
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
	   ,@body)))))

(define-method draw-patch block (x0 y0 x1 y1 image 
				    &key depressed dark)
    (with-block-drawing image
      (let ((bevel (if depressed shadow highlight))
	    (chisel (if depressed highlight shadow))
	    (fill (if dark shadow background)))
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
  (/draw-patch self x0 y0 x1 y1 image :depressed t :dark t))
    
(define-method draw-background block (image)
  (with-fields (x y width height) self
    (/draw-patch self x y (+ x width) (+ y height) image)))

(define-method draw-stub block (image)
  (with-fields (x y operation) self
    (let ((width (segment-width operation))
	  (height (+ (font-height *block-font*) (* 2 *dash-size*))))
      (/draw-patch self x y 
		   (+ x width (* 4 *dash-size*))
		   (+ y height (* 2 *dash-size*)) image)
      (/draw-segment self 
		     (+ x (* 2 *dash-size*))
		     (+ y 1)
		     operation :symbol image))))

(define-method layout block ()
  (with-field-values 
      (x y operation schema arguments height width widgets) 
      self
    (let* ((font *block-font*)
	   (dash *dash-size*)
	   (left (+ x (/handle-width self)))
	   (max-height (+ dash (font-height font))))
      (loop while arguments do
 	(let ((widget (pop widgets))
	      (argument (pop arguments))
	      (type (pop schema)))
	  (if (null widget)
	      (incf left 
		    (+ dash
		       (if (eq :block type)
			   (if (null argument)
			       *socket-width*
			       (if (object-p argument) 
				   (progn (/move argument (+ left dash dash) (+ y dash dash))
					  (/layout argument)
					  (field-value :width argument))
				   (segment-width argument)))
			   (segment-width argument))))
	      (progn (/move widget :x left :y (+ y dash))
		     (incf left (field-value :width widget))
		     (setf max-height 
			   (max max-height 
				(field-value :height widget)))))))
      (setf <width> (+ (- left x) (* 4 dash)))
      (setf <height> (+ max-height (* 2 dash))))))

(define-method draw-segment block (x0 y0 segment type image)
  (with-block-drawing image
    (let ((width *socket-width*))
      (with-fields (height) self
	(let ((dash *dash-size*))
	  (if (eq type :block)
	      (when (null segment) 
		(/draw-socket self (+ x0 dash) (+ y0 dash)
			      (+ x0 *socket-width*)
			      (+ y0 (- height dash))
			      image))
	      (progn 
		(text x0 (+ y0 dash 1)
		      (print-segment segment))
		(setf width (segment-width segment))))))
      width)))

(define-method draw-contents block (image)
  (with-block-drawing image
    (with-field-values 
	(x y operation arguments schema widgets)
	self
      (let* ((dash *dash-size*)
	     (left (+ x (* 2 dash))))
	(text left (+ y dash 1) (print-segment operation))
	(incf left (+ dash (segment-width operation)))
	(loop while arguments do
	  (let ((widget (pop widgets))
		(type (pop schema))
		(argument (pop arguments)))
	    (if (null widget)
		(incf left 
		      (+ dash (/draw-segment self 
					     left y 
					     argument type
					     image)))
		(incf left (+ dash (field-value :width widget))))))))))

(define-method hit-segment block (click-x click-y)
  (with-field-values 
      (x y width height operation arguments schema widgets)
    self
    (let* ((dash *dash-size*)
	   (left (+ x (* 2 dash)))
	   new-left n)
	(incf left (+ dash (segment-width operation)))
      (block testing
	(loop while arguments do
	  (let ((widget (pop widgets))
		(type (pop schema))
		(argument (pop arguments)))
	    (setf new-left 
		  (if (null widget)
		      (+ left
			 (if (eq :block type)
			     (if (null argument)
				 *socket-width*
				 (field-value :width argument))
			     (segment-width argument)))
		      (incf left (+ dash (field-value :width widget)))))
	    (when (and (< y click-y (+ y height))
		       (< left click-x new-left))
	      (return-from testing n))
	    (incf n)
	    (setf left new-left)))
	(return-from testing nil)))))
      		        
(define-method draw block (image)
  (/draw-background self image)
  (/draw-contents self image)
  (dolist (child <arguments>)
    (when (object-p child)
      (/draw child image))))
    
;;; Predefined blocks 

(defblock do
  (type :initform :event)
  (schema :initform '(:body))
  (arguments :initform '(nil)))

(defblock my 
  (type :initform :variables)
  (schema :initform '(:symbol))
  (arguments :initform '(:name)))

(defblock set
  (type :initform :variables)
  (schema :initform '(:symbol :anything))
  (arguments :initform '(:counter 1)))

(defblock say 
  (type :initform :message)
  (schema :initform '(:string))
  (arguments :initform '("Hello!")))

;; (define-method hit do (mouse-x mouse-y)
  
;;   )

;; (define-method drop do (x y block)
  
;;   ) 
 

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

(defblock if 
  (type :initform :control)
  (schema :initform '(:block :block :block))
  (arguments :initform '(nil nil nil)))

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

(defun is-event-block (thing)
  (and (not (null thing))
       (iosketch:object-p thing)
       (has-field :operation thing)
       (eq :do (field-value :operation thing))))

;;; Composing blocks into larger programs

(define-prototype script ()
  (blocks :initform '()
	  :documentation "List of blocks in the script.")
  (variables :initform (make-hash-table :test 'eq)))

(define-method initialize script (&key blocks variables)
  (when blocks (setf <blocks> blocks))
  (when variables (setf <variables> variables)))

(defvar *script*)

(define-method add script (block x y)
  (with-fields (blocks) self
    (setf blocks (adjoin block blocks))
    (/move block x y)))

(define-method bring-to-front script (block)
  (with-fields (blocks) self
    (when (member block blocks)
      (setf blocks (delete block blocks))
      (setf blocks (append blocks (list block))))))

(define-method delete script (block)
  (with-fields (blocks) self
    (setf blocks (delete block blocks))))

(define-method set script (var value)
  (setf (gethash var <variables>) value))

(define-method get script (var)
  (gethash var <variables>))

(defun script-variable (var-name)
  (/get *script* var-name))

(defun set-script-variable (var-name value)
  (/set *script* var-name value))

(defsetf script-variable set-script-variable)

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
      (script needs-redraw image buffer selection focus drag modified) self   
    (when script
      (when needs-redraw (/redraw self))
      (draw-image buffer 0 0 :destination image)
      (when drag (/draw-stub drag image)))))

(define-method mouse-down editor (x y &optional button)
  (with-fields (script selection focus drag modified) self
    (when script 
      (with-fields (blocks) script
	(labels ((hit (b)
		   (/hit b x y)))
	  (let ((block (find-if #'hit blocks :from-end t)))
	    (when block
	      (setf drag block)
	      (setf focus block))))))))

(define-method mouse-move editor (mouse-x mouse-y)
  (with-fields 
      (script drag-offset selection focus drag-start drag modified) 
      self
    (when drag
      (with-fields (x y) drag
	(when (null drag-start)
	  (setf drag-start (cons x y))
	  (setf drag-offset (cons (min (/handle-width drag)
				       (- mouse-x x))
				  (- mouse-y y))))
	(destructuring-bind (x0 . y0) drag-offset
	  (/move drag (- mouse-x x0) (- mouse-y y0)))))))

(define-method mouse-up editor (x y &optional button)
  (with-fields 
      (script needs-redraw drag-start selection focus drag modified) 
      self
    (/bring-to-front script drag)
    (setf drag-start nil)
    (setf drag-offset nil)
    (setf drag nil)
    (setf needs-redraw t)))

;; (define-method drag editor (x y))

;; (define-method drop editor (x y))
      
;;; blocks.lisp ends here
