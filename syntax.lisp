;;; syntax.lisp --- visual lisp macros for a blocky-in-blocky funfest

;; Copyright (C) 2011, 2012  David O'Toole

;; Author: David O'Toole <dto@ioforms.org>
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

;; This file implements a visual layer on top of prototypes.lisp, so
;; that OOP can occur visually. Understanding the terms used in
;; prototypes.lisp will help in reading the present file.

;;; Code:

(in-package :blocky)

;;; Widget for empty target slot

(deflist socket)

(define-method hit socket (x y)
  (if %inputs
      (hit%super self x y)
      (when (touching-point self x y)
	self)))

(defparameter *socket-size* 16)

(define-method layout socket ()
  (with-fields (label inputs height width) self
    (setf height *socket-size* width *socket-size*)
    (let ((target (first inputs))
	  (x %x)
	  (y %y)
	  (label-width 0))
      (when label
	(setf label-width 
	      (font-text-width label *font*))
	(incf x (+ (dash 1) label-width))
	(setf height (+ (max (font-height *font*)
			     *socket-size*)))
	(setf width (- x %x)))
      (cond 
	((and label (null target))
	 (incf width (+ (dash 1) *socket-size*)))
	((and target (null label))
	 (move-to target x y)
	 (layout target) 
	 (setf height (+ (%height target)))
	 (setf width (+ label-width (%width target))))))))

(define-method draw socket ()
  (with-fields (label inputs) self
    (let ((x %x)
	  (y %y))
      (when label 
	(draw-string label x y 
		     :color "white"
		     :font *font*)
	(incf x (+ (dash 1)
		   (font-text-width label *font*))))
      (if inputs 
	  (draw (first inputs))
	  (draw-image "socket" x y)))))

(define-method draw-hover socket ()
  (with-fields (x y width height) self
    (draw-box x y width height :color *hover-color* :alpha *hover-alpha*)))

(define-method accept socket (other-block)
  "Replace the child object with OTHER-BLOCK."
  (when other-block 
    (prog1 t
      (setf %inputs (list other-block))
      (set-parent other-block self))))

(define-method evaluate socket ()
  (when %inputs (first %inputs)))

(define-method can-pick socket () t)
  
(define-method pick socket ()
  ;; allow dragging of parent via empty socket
  (if (null %inputs)
      %parent
      ;; otherwise, pick object
      (first %inputs)))

(define-method initialize socket (&optional input label)
  (setf %input input %label label))

;;; Variables whose values are blocks

(defvar *parameters* nil)

(define-block (variable :super :list)
  (name :initform nil)
  (category :initform :variables))

(define-method initialize variable (&optional (name ""))
  (assert (stringp name))
  (setf %name name)
  (apply #'initialize%super self
	 (list (new 'string :value name)))
  (mapc #'pin %inputs))

(define-method accept variable (thing))

(define-method variable-name variable ()
  (make-keyword (evaluate (first %inputs))))

(define-method << variable ((target block))
  (setf (world-variable (variable-name self))
	target))

(define-method evaluate variable ()
  (world-variable (variable-name self)))

(define-method pick-target variable ()
  (or (evaluate self) self))

(define-method forward-message variable (method arguments)
  (apply #'send method 
	 (pick-target self)
	 arguments))

;;; Self reference

(defvar *self* nil)

(define-block-macro self
    (:super :list
     :fields
     ((category :initform :parameters))
     :inputs
     ((new 'string :value "self" :read-only t :locked t))))

(define-method accept self (thing))

(define-method evaluate self ()
  (or *self* *target*))

(define-method recompile self ()
  'self)
  
(define-method pick-target self ()
  (evaluate self))

(define-method forward-message self (method arguments)
  (apply #'send method 
	 (evaluate self)
	 arguments))

;;; Field references (to self or other objects)

(define-block-macro field 
  (:super :list
   :fields ((category :initform :fields)
	    (orientation :initform :horizontal))
   :inputs (:target (new 'socket)
	    :field (new 'string))))

(define-method accept field (thing)
  (declare (ignore thing))
  nil)

(define-method << field ((value block))
  (with-input-values (target field) self
    (let ((key (make-keyword field)))
      (setf (field-value key target) value))))

(define-method evaluate field ()
  (with-input-values (target field) self
    (field-value (make-keyword field) 
		 (pick-target target))))

;;; Parameter declarations (ordinary variables refer to them)

(define-block-macro parameter 
    (:super :variable
     :fields ((category :initform :parameters))))

;;; Closure for parameterizing a tree


;;; Inactive placeholder

(deflist blank)

(define-method accept blank ())
(define-method can-pick blank () nil)
(define-method draw blank ())
(define-method layout blank ()
  (setf %height 2 %width 2))
(define-method tap blank ())
(define-method alternate-tap blank ())

;;; Message argument GUI

(define-block arguments       
  (category :initform :message)
  prototype method schema target label button-p)

(define-method recompile arguments ()
  ;; grab argument values from all the input widgets
  (mapcar #'recompile %inputs))

(define-method evaluate arguments ()
  (mapcar #'evaluate %inputs))

(define-method tap arguments (x y)
  (declare (ignore x y))
  (when %button-p
    (evaluate self)))

(define-method accept arguments (block)
  ;; make these click-align instead
  (assert (blockyp block))
  nil)

(define-method initialize arguments (&key method label target)
  (let ((schema (find-schema method target))
	(inputs nil))
    (initialize%super self)
    (setf %no-background t)
    ;; create appropriate controls for the arguments in the schema
    (dolist (entry schema)
      (let ((thing 
	      (if (eq 'block (schema-type entry))
		  (new 'socket :label (pretty-string (schema-name entry)))
		  (clone (if (eq 'string (schema-type entry))
			     "BLOCKY:STRING" "BLOCKY:ENTRY")
			 :value (schema-option entry :default)
			 :parent (find-uuid self)
			 :type-specifier (schema-type entry)
			 :options (schema-options entry)
			 :label (pretty-string (schema-name entry))))))
	(push thing inputs)))
    (when inputs 
      (setf %inputs (nreverse inputs)))
    (setf %schema schema
	  %method method
	  %label label)))

(define-method draw arguments ()
  (with-fields (x y width height label inputs) self
    ;; (when %button-p
    ;;   (with-style :flat
    ;; 	(draw-patch self x y (+ x width) (+ y height))))
    (let ((*text-baseline* (+ y (dash 1))))
      (when label (draw-label-string self label "white"))
      (dolist (each inputs)
	(draw each)))))

(define-method draw-hover arguments ()
  nil)

(define-method pick-focus arguments ()
  (first %inputs))

;;; Lisp value printer block

(define-block-macro printer 
    (:super :list
     :fields 
     ((category :initform :data))
     :inputs 
     (:input (new 'socket)
      :output (new 'text))))

(define-method evaluate printer ()
  (let* ((*print-pretty* t)
	 (input (evaluate %%input))
	 (string (format nil "~S" 
			 (if (blockyp input)
			     (evaluate input) input))))
    (set-buffer %%output
		(split-string-on-lines string))))

(define-method accept printer (thing))
(define-method draw-hover printer () nil)

;;; Generic message block

(define-block-macro message 
    (:super :list
     :fields 
     ((orientation :initform :horizontal)
      (category :initform :message)
      (method :initform nil) 
      (updated :initform nil)
      (target :initform nil)
      (style :initform :flat))
     :inputs 
     (:method (new 'string)
      :arguments (new 'blank))))

(define-method get-target message ()
  (if (is-a 'statement-list %parent)
      (get-target %parent)
      ;; global overrides local
      (or *target* %target "BLOCKY:BLOCK")))

(define-method set-target message (target)
  (setf %target target))

(define-method evaluate message ()
  (with-input-values (arguments) self 
    (forward-message (get-target self)
		     %method
		     arguments)))

(define-method update-arguments-maybe message ()
  (with-input-values (method) self
    (when (plusp (length (string-trim " " method)))
      (let ((method-key (make-keyword (ugly-symbol method)))
	    (target (or (get-target self) (find-object "BLOCKY:BLOCK"))))
	(when (not (eq method-key %method))
	  ;; time to change args
	  (setf %method method-key)
	  (setf (second %inputs)
		(new 'arguments :method method-key
				:target target))
	  (setf %updated t))))))

(define-method update message ()
  (when %updated
    (grab-focus (first %inputs))
    (setf %updated nil)))

(define-method set-method message (method)
  (set-value %%method (pretty-string method))
  (update-arguments-maybe self))

(defun message-for-method (method target)
  (let ((message (new 'message)))
    (prog1 message
      (set-target message target)
      (set-method message method))))

(define-method child-updated message (child)
  (when (object-eq child %%method)
    (update-arguments-maybe self)))

(define-method draw-hover message ()
  nil)

(define-method accept message (thing)
  nil)

;;; Stacked messages to a particular receiver

(deflist statement-list
    (category :initform :message)
  (spacing :initform 0)
  (default-events :initform '(((:return :alt) :add-message)
			      ((:backspace :alt) :remove-message)))
  (no-background :initform t))

(define-method add-message statement-list ()
  (accept self (new 'message)))

(define-method remove-message statement-list ()
  (setf (cdr (last %inputs))
	nil))

(define-method get-target statement-list ()
  (assert %parent)
  (get-target %parent))

(define-method accept statement-list (thing)
  (when (is-a 'message thing)
    (accept%super self thing)))

(define-method can-pick statement-list () t)
      
(define-method pick statement-list () 
  (when (is-a 'statement %parent)
    %parent))

(define-block-macro statement 
    (:super :list
     :fields 
     ((orientation :initform :horizontal)
      (category :initform :message)
      (style :initform :flat))
     :inputs 
     (:target (new 'socket)
      :messages (new 'statement-list (new 'message)))))

(define-method evaluate statement ()
  (with-target %%target
    (dolist (message (%inputs %%messages))
      (evaluate message))))

(define-method get-target statement ()
  (first (%inputs %%target)))

(define-method accept statement (thing)
  (accept %%messages thing))

;;; Palettes to tear cloned objects off of 

(define-block (palette :super :list) 
    source
  (style :initform :rounded)
  (height :initform 100)
  (width :initform 100))

(define-method hit palette (x y)
  (when (within-extents x y %x %y (+ %x %width) (+ %y %height))
    self))

(define-method can-pick palette () t)

(define-method pick-drag palette (x y)
  (labels ((hit-it (ob)
	     (hit ob x y)))
    (setf %source (some #'hit-it %inputs))
    (if %source
	(make-clone %source)
	self)))

;;; syntax.lisp ends here
