;;; cells.lisp --- defining objects

;; Copyright (C) 2008  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
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

;;; Code:

(in-package :xe2)

;;; Base cell prototype

;; This is a base object for forms-browseable objects. See forms.lisp

(define-prototype cell ()
  (name :initform nil :documentation "The name of this cell.")
  (description :initform nil :documentation "A description of the cell.") 
  (categories :initform nil :documentation "List of category keyword symbols")
  (label :initform nil :documentation "Optional string or formatted line to display.")
  row column
  (widget :initform nil)
  (image :initform nil :documentation "Image to display. either a resource name string, or an XE2 image object.")
  (excluded-fields :initform '(:widget)))
  
(define-method in-category cell (category)
  (member category <categories>))

(defparameter *default-cell-width* 16)

(define-method get cell ())

(define-method activate cell ())

(define-method set cell (data))

(define-method print cell () "")

(define-method width cell () 
  (with-field-values (widget image label) self
    (cond (widget (image-width (field-value :image widget)))
	  (image (image-width image))
	  (label (formatted-line-width label))
	  (t *default-cell-width*))))

(define-method height cell () 
  (with-field-values (widget image label) self
    (cond (widget (image-height (field-value :image widget)))
	  (image (image-height image))
	  (label (formatted-line-height label))
	  (t *default-cell-width*))))

(define-method render cell (dest x y width)
  (with-field-values (widget image) self
      (cond (widget 
	     (/render widget)
	     (draw-image (field-value :image widget)
			 x y :destination dest))
	    ;; it's an image
	    (image 
	     (if (stringp image)
		 (draw-resource-image image x y :destination dest)
		 (draw-image image x y :destination dest)))
	    (<label>
	     ;; we have a formatted line
	     (let ((label <label>))
	       (when (listp label)
		 (let*
		     ((shortfall (- width (formatted-line-width label)))
		      (color (or (when (and (listp label)
					    (listp (last label)))
				   (getf (cdr (car (last label))) :background))
				 ".black"))
		      (spacer (when (plusp shortfall) 
				(list nil :width shortfall :background color)))
		      (line (if spacer (append label (list spacer))
				      label)))
		   (render-formatted-line line x y :destination dest))))))))

(defvar *default-cell-label* '((" DEF ")))

(define-method get-label cell ()
  (when label 
    (etypecase label
      (string (list (list label)))
      (list label))))

(define-method set-location cell (r c)
  "Set the row R and column C of the cell."
  (setf <row> r <column> c))

;;; Convenience macro for defining cells.

(defmacro defcell (name &body args)
  "Define a cell named NAME, with the fields ARGS as in a normal
prototype declaration. This is a convenience macro for defining new
cells."
  `(define-prototype ,name (:parent =cell=)
     ,@args))

;;; cells.lisp ends here
