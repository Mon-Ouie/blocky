;;; syntax.lisp --- visual lisp macros for a blocky-in-blocky funfest

;; Copyright (C) 2011  David O'Toole

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

;;; Dynamically binding the recipient of a message

(defvar *target* nil)

(defmacro with-target (target &rest body)
  `(let ((*target* ,target))
     ,@body))

;;; Widget for empty target slot

(deflist socket)

(define-method hit socket (x y)
  (if %inputs
      (hit%super self x y)
      (when (touching-point self x y)
	self)))

(define-method draw socket ()
  (if %inputs 
      (draw (first %inputs))
      (draw-image "socket" %x %y)))

(define-method draw-hover socket ()
  (with-fields (x y width height) self
    (draw-box x y width height :color "red" :alpha 0.5)))

(define-method accept socket (other-block)
  "Replace the child object with OTHER-BLOCK."
  (when other-block 
    (setf %inputs (list other-block))
    (set-parent other-block self)))

(define-method evaluate socket ()
  (when %inputs (first %inputs)))

(define-method can-pick socket () 
  (not (null %inputs)))

(define-method pick socket ()
  (first %inputs))

;;; Inactive placeholder

(deflist blank)

(define-method accept blank ())
(define-method can-pick blank () nil)
(define-method draw blank ())

;;; Message argument GUI

(define-block arguments prototype method schema target label button-p)

(define-method recompile arguments ()
  ;; grab argument values from all the input widgets
  (mapcar #'recompile %inputs))

(define-method evaluate arguments ()
  (mapcar #'evaluate %inputs))

(define-method tap arguments (x y)
  (declare (ignore x y))
  (when %button-p
    (evaluate self)))

(define-method can-pick arguments () t)

(define-method pick arguments ()
  ;; allow to move parent block by the labels of this arguments block
  (if %button-p self %parent))

(define-method accept arguments (block)
  ;; make these click-align instead
  (assert (blockyp block))
  nil)

(define-method initialize arguments (&key prototype schema method label target (button-p t))
  (initialize%super self)
  (setf %target target)
  (setf %button-p button-p)
  (let* ((proto0 (find-prototype (or prototype target "BLOCKY:BLOCK")))
	 (schema0
	   (or schema
	       (method-schema proto0) method))
	 (inputs nil)
	 (proto (or prototype (when target
				(object-name (find-super target))))))
    (dolist (entry schema0)
      (push (clone (if (eq 'string (schema-type entry))
		       "BLOCKY:STRING" "BLOCKY:ENTRY")
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
    (let ((category (when proto
		      (method-option (find-prototype proto)
				     method :category))))
      (when category (setf %category category))
      (setf %schema schema0
	    %prototype proto
	    %method method
	    %label label))))

(define-method draw arguments ()
  (with-fields (x y width height label inputs) self
    (when %button-p
      (with-style :flat
	(draw-patch self x y (+ x width) (+ y height))))
    (let ((*text-baseline* (+ y (dash 1))))
      (draw-label-string self label "white")
      (dolist (each inputs)
	(draw each)))))

(define-method draw-hover arguments ()
  nil)

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
	 (input-block (evaluate %%input))
	 (string (format nil "~S" (when input-block
				    (evaluate input-block)))))
    (set-buffer %%output
		(split-string-on-lines string))))

;;; Generic message block

(define-block-macro message 
    (:super :list
     :fields 
     ((orientation :initform :horizontal)
      (style :initform :flat))
     :inputs 
     (:target (new 'socket)
      :method (new 'string)
      :arguments (new 'blank))))

(define-method evaluate message ()
  (with-input-values (method target arguments) self 
    (apply #'send method target arguments)))

(define-method update-arguments message ()
  (with-input-values (method target) self
    (setf (third %inputs)
	  (new 'arguments :method method :target target))))

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
