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

;; For the design I've followed a motto associated with another visual
;; programming language, Pure Data: "The diagram is the program."
;; Since the diagram is 2D, the program must therefore be
;; two-dimensional as well. That means every block in the program
;; (i.e. every expression) must have an X,Y position and that
;; positioning of blocks---for example, indentation by
;; columns---influences the interpretation of the program. (This is
;; analogous how nesting is indicated in the Python language.)

;; Besides a 2D position, each block has a (possibly empty) list of
;; tokens. Tokens are symbols like :move or :play-sound, or data
;; arguments such as numbers, strings, or symbols. 

;; Example block ideas, where [....] is a picture of a block:

;; [move forward 1 space]
;; [move west 10 pixels]
;; [play "beep"]
;; [music "piano" loop no]
;; [drop new bomb]

(defcell block ()
  (row :documentation "Row number of this block in the enclosing program.")
  (column :documentation "Column number of this block in the enclosing program.")
  (tokens :documentation "List of expression data arguments.")
  (token-types :documentation "List of CL type specifiers for corresponding expressions in <tokens>.")
  (operation :initform "block" :documentation "Symbol name of block's operation, i.e. message key.")
  (topic :documentation "Topic name of block. See also `*block-topics*'."))

;; blocks can be arguments to blocks
(defparameter *token-types* '(iomacs:object integer float number string keyword))
(defparameter *block-topics* '(:system :motion :event :message :looks :sound :control :sensing :operators :variables))
(defparameter *block-colors* '(:motion ".cornflower blue"
			       :system ".gray50"
			       :event ".white"
			       :looks ".purple"
			       :sound ".orchid"
			       :message ".sienna3"
			       :control ".orange1"
			       :variables ".DarkOrange2"
			       :operators ".green1"
			       :sensing ".DeepSkyBlue3")
  "X11 color names of the different block topics.")
(defparameter *block-text-colors* '(:motion ".white"
				    :system ".white"
				    :event ".gray50"
				    :message ".white"
				    :looks ".white"
				    :sound ".white"
				    :control ".white"
				    :variables ".white"
				    :operators ".white"
				    :sensing ".white")
  "X11 color names of the text used for different block topics.")

(define-method move block (row column)
  (setf <row> row)
  (setf <column> column))

(define-method get-token block (index)
  (nth index <tokens>))

(define-method set-token block (index value)
    (setf (nth index <tokens>) value))

(define-method execute block (recipient)
  "Send the appropriate message to the RECIPIENT object."
  (apply #'iomacs:send nil <operation> recipient <tokens>))

(define-method describe block ()
  "Show name and comprehensive help for this block.")

(define-method draw block (x y image)) ;; themed blocks with AA sans fonts

(defmacro defblock (name &body args)
  `(define-prototype ,name (:parent =block=)
     (operation :initform ,(make-keyword name))
     ,@args))

;;; Predefined blocks for sending various common messages to cells

;; See also cells.lisp

(defblock move
  (topic :initform :motion)
  (token-types :initform '(:direction :distance :unit))
  (tokens :initform '(:north 10 :pixels)))

(defblock move-to
  (topic :initform :motion)
  (token-types :initform '(:unit :integer :integer))
  (tokens :initform '(:space 0 0)))

(defblock play-music 
  (topic :initform :sound)
  (token-types :initform '(:string :keyword :keyword))
  (tokens :initform '("fanfare" :loop :no)))

;;; Executing programs composed of blocks

;; Execution of a program begins with an event. If an event block's
;; name (for example "do mouse" or "do timer") matches a real event
;; (such as mouse movement, message, or button press), we begin
;; executing the program at that event block's (row, column)
;; location. Move downwards one space to the block immediately beneath
;; the triggered event and execute that, and so on.

;; Horizontal indentation by one space to the right indicates
;; expression nesting depth as is usual. Instead of textual whitespace
;; characters, here we indent with blank program spaces that have no
;; blocks in them. If we find a blank cell, look for something
;; indented one space; if so, that's interpreted as a BEGIN, etc.

;; A program may contain any number of event blocks with corresponding
;; subprograms, i.e. blocks arranged in a column (possibly with
;; indentation at parts) below named event blocks. The ability to
;; trigger events manually from a block means that local subroutines
;; within a program are easy to construct and is done in the same way
;; as responding to system events. Example:

;; [do congratulate]
;; [say "Congratulations, you clicked the object."]
;;
;; [do click]
;; [music "fanfare"]
;; [congratulate]

;; The explanation is that [do foo] expands into (define-method foo...

;; The final displayed dimensions of a block do not impact the program
;; itself in any way; the indentation is solely defined by a block's
;; grid location in the abstract, spreadsheet-like 2D space of the
;; program. (See forms.lisp) To prevent collisions, onscreen column
;; widths are dynamically adjusted according to block display result
;; widths.

;; An event "do" block with no name token is an anonymous subprogram.
;; It enables this syntax for conditionals:
;;
;;   [when <conditional>] [move west 10 pixels]
;;
;;   [when <conditional>] [explode]
;;
;;   [unless <conditional>] [do]
;;                          [play-sound "beep"]
;;                          [music "scary"]
;;
;;   [if <conditional>] [do]                        >>> [do]
;;                      [move north 1 space]            [move south 1 space]
;;                      [say "hello!" for 2 seconds]    [........
;;
;; NOTE: The ">>>" above indicates that more than one column may
;; intervene between the "then" and "else" clauses in that row.

(define-prototype program (:parent iomacs:=page=)
  ;; grid location of block being executed, if any
  (owner :documentation "Game object associated with script.")
  (row :initform 0) 
  (column :initform 0)
  positions
  )

(defun is-event-block (thing)
  (and (not (null thing))
       (iomacs:object-p thing)
       (has-field :operation thing)
       (eq :do (field-value :operation thing))))

(define-method begin program (start-row start-column)
  (with-fields (row column owner positions height width grid) self
    (push )))

(define-method end program)

(define-method do-event program (event-name)
  (with-fields (row column owner positions height width grid) self
    (let ((event-block 
	   (block searching
	     (dotimes (i height) 
	       (dotimes (j width)
		 (let ((it (/top-cell self i j)))
		   (when (and (is-event-block it)
			      ;; is it a DO block?
			      (eq :do (field-value :operation it)
				  (return-from searching it))))))))))
      (setf row (field-value :row event-block))
      (setf column (field-value :column event-block))
      ;; move down to first block  
      (incf row)
      ())))
      
;; when you reach a blank square moving down:
;;        reduce nesting by 1 level
;;        move left until non-blank is found
;;        begin executing 
      
      

;;; blocks.lisp ends here
