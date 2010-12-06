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
;; objects.

;; Example block ideas, where [....] is a picture of a block:

;; [move forward 1 space]
;; [move west 10 pixels]
;; [play "beep"]
;; [music "piano" loop no]
;; [drop new bomb]

(define-prototype block ()
  (x :documentation "Integer X coordinate of this block's position.")
  (y :documentation "Integer Y coordinate of this block's position.")
  (arguments :documentation "List of block argument values.")
  (schema :documentation "List of CL type specifiers for corresponding expressions in <arguments>.")
  (operation :initform "block" :documentation "Symbol name of block's operation, i.e. message key.")
  (topic :documentation "Topic name of block. See also `*block-topics*'."))

(defparameter *argument-types*
  '(:block :sprite :integer :float :number 
    :string :symbol :unit :direction :body))

(defparameter *block-topics* '(:system :motion :event :message :looks :sound :control :comment :sensing :operators :variables))

(defparameter *background-color* ".white")

(defparameter *block-font* "sans-condensed-bold-12")

(defparameter *block-colors* '(:motion ".cornflower blue"
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
  "X11 color names of the different block topics.")

(defparameter *block-highlight-colors* '(:motion ".sky blue"
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
  "X11 color names of the different block topics.")

(defparameter *block-shadow-colors* '(:motion ".dark slate blue"
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
  "X11 color names of the different block topics.")

(defparameter *block-text-colors* '(:motion ".white"
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
  "X11 color names of the text used for different block topics.")

(define-method move block (x y)
  (setf <x> x)
  (setf <y> y))

(define-method hit block (click-x click-y)
  (when (within-extents click-x click-y 
			x y 
			(+ x width)
			(+ y height))
    self))

(define-method resize block (height width)
  (setf <height> height)
  (setf <width> width)) 

(define-method get-argument block (index)
  (nth index <arguments>))

(define-method set-argument block (index value)
    (setf (nth index <arguments>) value))

(define-method execute block (recipient)
  "Send the appropriate message to the RECIPIENT object."
  (apply #'iosketch:send nil <operation> recipient <arguments>))

(define-method describe block ()
  "Show name and comprehensive help for this block.")

(define-method draw block (x y image)) ;; themed blocks with AA sans fonts

(defmacro defblock (name &body args)
  `(define-prototype ,name (:parent =block=)
     (operation :initform ,(make-keyword name))
     ,@args))

;;; Predefined blocks for sending various common messages 

(defblock move
  (topic :initform :motion)
  (schema :initform '(:direction :integer :unit))
  (arguments :initform '(:north 10 :pixels)))

(defblock move-to
  (topic :initform :motion)
  (schema :initform '(:unit :integer :integer))
  (arguments :initform '(:space 0 0)))

(defblock play-music 
  (topic :initform :sound)
  (schema :initform '(:string :keyword :keyword))
  (arguments :initform '("fanfare" :loop :no)))

(defblock do
  (topic :initform :event)
  (schema :initform '(:symbol :body))
  (body :initform nil)
  (arguments :initform nil))

(defblock when 
  (topic :initform :control)
  (schema :initform '(:predicate :block))
  (arguments :initform '(nil nil)))

(defblock if 
  (topic :initform :control)
  (schema :initform '(:predicate :block :block))
  (arguments :initform '(nil nil nil)))

;; (defblock my)     ;; (my field) == <field>

;;; Composing blocks into larger programs

(define-prototype script ()
  blocks
  selection
  focus)

(defun is-event-block (thing)
  (and (not (null thing))
       (iosketch:object-p thing)
       (has-field :operation thing)
       (eq :do (field-value :operation thing))))

      
;;; blocks.lisp ends here
