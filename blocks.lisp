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
;; Scratch.

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
  (x :documentation "Integer X coordinate of this block's position.")
  (y :documentation "Integer Y coordinate of this block's position.")
  (width :documentation "Cached pixel width of block.")
  (height :documentation "Cached pixel height of block.")
  (arguments :documentation "List of block argument values.")
  (schema :documentation "List of CL type specifiers for corresponding expressions in <arguments>.")
  (operation :initform "block" :documentation "Symbol name of block's operation, i.e. message key.")
  (type :documentation "Type name of block. See also `*block-types*'."))

(defmacro defblock (name &body args)
  `(define-prototype ,name (:parent =block=)
     (operation :initform ,(make-keyword name))
     ,@args))

(defparameter *argument-types*
  '(:block :sprite :integer :float :number 
    :string :symbol :unit :direction :body))

(defparameter *block-types*
  '(:system :motion :event :message :looks :sound 
    :control :comment :sensing :operators :variables))

(defparameter *background-color* ".white")

(defparameter *block-font* "sans-condensed-bold-12")

(defvar *dash-size* 3)

(defvar *space-size* nil
  "Size of the basic spacer layout distance, in pixels.
  Should be just a bit more than the height of `*block-font*'")

(defparameter *block-colors* 
  '(:motion ".cornflower blue"
    :system ".gray50"
    :event ".white"
    :comment ".grey70"
    :looks ".purple"
    :sound ".orchid"
    :message ".sienna3"
    :control ".orange1"
    :variables ".DarkOrange2"
    :operators ".green1"
    :sensing ".DeepSkyBlue3")
  "X11 color names of the different block types.")

(defparameter *block-highlight-colors*
  '(:motion ".sky blue"
    :system ".gray80"
    :event ".white"
    :comment ".grey90"
    :looks ".medium orchid"
    :sound ".plum"
    :message ".sienna2"
    :control ".gold"
    :variables ".DarkOrange1"
    :operators ".chartreuse3"
    :sensing ".DeepSkyBlue2")
  "X11 color names of highlights on the different block types.")

(defparameter *block-shadow-colors* 
  '(:motion ".dark slate blue"
    :system ".gray50"
    :event ".white"
    :comment ".grey40"
    :looks ".dark orchid"
    :sound ".violet red"
    :message ".chocolate3"
    :control ".dark orange"
    :variables ".OrangeRed2"
    :operators ".green3"
    :sensing ".turquoise3")
  "X11 color names of shadows on the different block types.")

(defparameter *block-foreground-colors* 
  '(:motion ".white"
    :system ".white"
    :event ".gray50"
    :comment ".gray30"
    :message ".white"
    :looks ".white"
    :sound ".white"
    :control ".white"
    :variables ".white"
    :operators ".white"
    :sensing ".white")
  "X11 color names of the text used for different block types.")

(defun block-color (color &optional (part :background))
  (let ((colors (ecase part
		  (:background *block-colors*)
		  (:highlight *block-highlight-colors*)
		  (:shadow *block-shadow-colors*)
		  (:foreground *block-text-colors*))))
    (getf colors color)))

(defparameter *small-block-corners* '(:top-left ".top-left-corner-small"
				      :top-right ".top-right-corner-small"
				      :bottom-right ".bottom-right-corner-small"
				      :bottom-left ".bottom-left-corner-small"))

(defparameter *medium-block-corners* '(:top-left ".top-left-corner-medium"
				       :top-right ".top-right-corner-medium"
				       :bottom-right ".bottom-right-corner-medium"
				       :bottom-left ".bottom-left-corner-medium"))

(defparameter *default-block-corner-size* :small)

(defun block-corner (corner &optional (size *default-block-corner-size*))
  (let ((images (ecase size
		  (:medium *medium-block-corners*)
		  (:small *small-block-corners*))))
    (getf images corner)))

(defun block-corner-size (&optional (size *default-block-corner-size*))
  (ecase size
    (:small 5)
    (:medium 9)))

(define-method count block () 1)

(define-method move block (x y)
  (setf <x> x)
  (setf <y> y))

(define-method hit block (click-x click-y)
  (when (within-extents click-x click-y 
			x y 
			(+ x width)
			(+ y height))
    self))

(define-method arrange block ()
  (let ((font *block-font*)
	(line (format nil "~{~a~^ ~}" <arguments>)))
    (setf <width> (+ (* 2 *dash-size*) ;; spacing
		     (font-text-extents line font)))
    (setf <height> (font-height font))))

(define-method get-argument block (index)
  (nth index <arguments>))

(define-method set-argument block (index value)
    (setf (nth index <arguments>) value)
    (/arrange self))

(define-method execute block (recipient)
  "Send the appropriate message to the RECIPIENT object."
  (apply #'iosketch:send nil <operation> recipient <arguments>))

(define-method describe block ()
  "Show name and comprehensive help for this block.")

(define-method draw block (image)
  (with-field-values (x y type height width schema) self
    (let* ((foreground (block-color self :foreground))
	   (background (block-color self :background))
	   (highlight (block-color self :highlight))
	   (shadow (block-color self :shadow))
	   (space *space-size*)
	   (label (format nil "~{~a~^ ~}" <arguments>))
	   (box-width (+ space width space))
	   (box-height (+ space height space))
	   (corner-size (block-corner-size))
	   (bottom (+ y box-height))
	   (right (+ x box-width)))
      (draw-box x y box-width box-height
		:color background
		:stroke-color shadow
		:destination image)
      (draw-line 0 0 right 0 
		 :color highlight
		 :destination image)
      (draw-line 0 0 0 bottom
		 :color highlight
		 :destination image)
      (draw-string-blended label 
			   (+ x space)
			   (+ y space)
			   :foreground foreground
			   :background background
			   :destination image
			   :font *block-font*)
      (draw-image (block-corner :top-left)
		  x y :destination image)
      (draw-image (block-corner :top-right)
		  (- right corner-size) y 
		  :destination image)
      (draw-image (block-corner :bottom-right)
		  (- right corner-size)
		  (- bottom corner-size)
		  :destination image)
      (draw-image (block-corner :bottom-left)
		  x (- bottom corner-size)
		  :destination image))))

;;; Predefined blocks for sending various common messages 

(defblock move
  (type :initform :motion)
  (schema :initform '(:direction :integer :unit))
  (arguments :initform '(:north 10 :pixels)))

(defblock move-to
  (type :initform :motion)
  (schema :initform '(:unit :integer :integer))
  (arguments :initform '(:space 0 0)))

(defblock music 
  (type :initform :sound)
  (schema :initform '(:string :keyword :keyword))
  (arguments :initform '("fanfare" :loop :no)))

(defblock when 
  (type :initform :control)
  (schema :initform '(:predicate :block))
  (arguments :initform '(nil nil)))

(defblock if 
  (type :initform :control)
  (schema :initform '(:predicate :block :block))
  (arguments :initform '(nil nil nil)))

(defblock do
  (type :initform :event)
  (schema :initform '(:symbol :body))
  (body :initform nil)
  (arguments :initform nil))

(defun is-event-block (thing)
  (and (not (null thing))
       (iosketch:object-p thing)
       (has-field :operation thing)
       (eq :do (field-value :operation thing))))

;;; Composing blocks into larger programs

(define-prototype script ()
  (blocks :initform ()
	  :documentation "List of blocks in the script.")
  (variables :initform (make-hash-table :test 'eq)))

(defvar *script*)

(define-method add script (block x y)
  (with-fields (blocks) self
    (setf blocks (adjoin block blocks))
    (/move block x y)))

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
  (modified :initform nil 
  	    :documentation "Non-nil when modified since last save."))

(define-method render editor (image)
  (with-fields (script selection focus drag modified) self   
    (when script
      (with-fields (blocks) script
	(/clear self :color *background-color*)
	(map nil #'/arrange blocks)
	(map nil #'/draw blocks)))))
      
;;; blocks.lisp ends here
