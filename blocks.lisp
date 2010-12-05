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

(in-package :iomacs)

;; In terms of design I follow a motto associated with another visual
;; programming language, Pure Data. "The diagram is the program."
;; Since the diagram is 2D, the program must therefore be
;; two-dimensional as well. That means every block in the program
;; (i.e. every expression) must have an X,Y position and that
;; positioning of blocks---for example, indentation by
;; columns---influences the interpretation of the program. (This is
;; analogous how nesting is indicated in the Python language.)

;; Besides a 2D position, each block has a (possibly empty) list of
;; tokens. Tokens are symbols like :move or :play-sound, or data
;; arguments such as numbers, strings, or symbols. 

;; Example block ideas:

;; move forward 1 space
;; move west 10 pixels
;; play "beep"
;; music "piano" loop no
;; drop new bomb

(defparameter *default-block-color* "gray60")

(define-prototype block ()
  (row :documentation "Row number of this block in the enclosing program.")
  (column :documentation "Column number of this block in the enclosing program.")
  (tokens :documentation "List of expression data arguments.")
  (token-types :documentation "List of CL type specifiers for corresponding expressions in <tokens>.")
  (category :documentation "Category name of block. See also `*block-categories*'."
	    :initform *default-block-color*))

(defparameter *token-types* '(integer float number string keyword))
(defparameter *block-categories* '(:motion :looks :sound :control :sensing :operators :variables))
(defparameter *block-colors* '(:motion ".cornflower blue"
			       :looks ".purple"
			       :sound ".orchid"
			       :control ".orange1"
			       :variables ".DarkOrange2"
			       :operators ".green1"
			       :sensing ".DeepSkyBlue3")
  "X11 color names of the different block categories.")

(define-method move block (row column)
  (setf <row> row)
  (setf <column> column))

(define-method get-token block (index)
  (with-fields (tokens token-types) self
    (assert (not (null token-types)))
    (let ((value (nth index tokens)))
      (prog1 value
	(assert (typep value (nth index token-types)))))))

(define-method set-token block (index value)
  (with-fields (tokens token-types) self
    (assert (not (null token-types)))
    (assert (typep value (nth index token-types)))
    (setf (nth index tokens) value)))

(define-method execute block (recipient)
  (error "Cannot execute empty block. Try defining a non-empty block with `defblock'."))

(defmacro defblock)

(defblock send
    (category :initform :control)
    (token-types :initform ))

(defblock loop)

(define-prototype program ()
)



;;; blocks.lisp ends here
