;;; text.lisp --- a text control

;; Copyright (C) 2008, 2009, 2010, 2011  David O'Toole

;; Author: David O'Toole <dto@blocky.org>
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
;; along with this program.  If not, see %http://www.gnu.org/licenses/.

;;; Code:

(in-package :blocky)

;;; Text display and edit control

(defparameter *text-margin* (dash 2) "Default onscreen margin (in pixels) of a text.")

(defparameter *text-minimum-width* 80) 

(define-block text
  (methods :initform '(:page-up :page-down :center :resize-to-fit :view-messages))
  (font :initform *monospace*)
  (buffer :initform nil)
  (category :initform :comment)
  (read-only :initform nil)
  (bordered :initform nil)
  (indicator :initform nil)
  (max-displayed-lines :initform 16 :documentation "An integer when scrolling is enabled.")
  (max-displayed-columns :initform nil)
  (background-color :initform "gray30")
  (foreground-color :initform "black")
  (cursor-color :initform "red")
  (point-row :initform 0)
  (point-column :initform 0)
  (auto-fit :initform t)
  (visible :initform t))

(define-method accept text (other))

(define-method enter text ()
  (newline self))

(define-method on-event text (event)
  (on-text-event self event))

(define-method set-buffer text (buffer)
  (setf %buffer buffer))

(define-method get-buffer-as-string text ()
  (apply #'concatenate 'string %buffer))

(defparameter *next-screen-context-lines* 3)

(define-method set-font text (font)
  (assert (stringp font))
  (assert (eq :font (resource-type (find-resource font))))
  (setf %font font))

(define-method page-up text ()
  "Scroll up one page, only when %max-displayed-lines is set."
  (with-field-values (max-displayed-lines) self
    (when (integerp max-displayed-lines)
      (setf %point-row (max 0
			   (- %point-row (- max-displayed-lines
					     *next-screen-context-lines*)))))))

(define-method page-down text ()
  "Scroll down one page, only when %max-displayed-lines is set."
  (with-field-values (max-displayed-lines) self
    (when (integerp max-displayed-lines)
      (setf %point-row (min (- (length %buffer) max-displayed-lines)
			     (+ %point-row (- max-displayed-lines
					     *next-screen-context-lines*)))))))

(define-method resize-to-scroll text (width height)
  "Resize the text to WIDTH * HEIGHT and enable scrolling of contents."
  (assert (and (numberp width) (numberp height)))
  (resize self width height)
  (setf %max-displayed-lines (truncate (/ height (font-height %font)))))

(define-method resize-to-fit text ()
  "Automatically resize the text to fit the text, and disable scrolling."
  ;; disable scrolling
  (setf %max-displayed-lines nil)
  ;; measure text
  (let* ((buffer %buffer)
	 (line-height (font-height %font))
	 (line-lengths (mapcar #'(lambda (s)
				   (font-text-width s %font))
			       buffer)))
    ;; update geometry
    (let ((width0 (max *text-minimum-width*
		       (+ (* 2 *text-margin*) 4
			  (if (null line-lengths)
			      0 
			      (apply #'max line-lengths)))))
	  (height0 (+ (* 2 *text-margin*)
		      (* line-height (max 1 (length buffer))))))
      (when (or (< %width width0)
		(< %height height0))
	(resize self width0 height0)))))

(define-method view-messages text ()
  (setf %auto-fit nil)
  (add-to-list '*message-hook-functions* 
	       #'(lambda (string)
		   (insert-string self string)
		   (newline self)))
  (setf %buffer (reverse *message-history*)))

(define-method end-of-line text ()
  (setf %point-column (length (nth %point-row %buffer))))

(define-method beginning-of-line text ()
  (setf %point-column 0))

(define-method initialize text (&optional buffer)
  (super%initialize self)
  (when (null buffer)
    (setf %buffer (list " ")))
  (when (stringp buffer)
    (setf %buffer (split-string-on-lines buffer)))
  (when (and buffer (listp buffer) (every #'stringp buffer))
    (setf %buffer buffer))
  (when (null (has-local-value :buffer self))
    (setf %buffer (list "")))
  (install-text-keybindings self)
  (install-keybindings self *arrow-key-text-navigation-keybindings*))

(define-method forward-char text ()
  (with-fields (buffer point-row point-column) self
    (setf point-column (min (1+ point-column)
			    (length (nth point-row buffer))))))

(define-method backward-char text ()
  (with-fields (buffer point-row point-column) self
    (setf point-column (max 0 (1- point-column)))))

(define-method next-line text ()
  (with-fields (buffer point-row point-column) self
    (setf point-row (min (1+ point-row)
			 (1- (length buffer))))
    (setf point-column (min point-column 
			    (length (nth point-row buffer))))))

(define-method previous-line text ()
  (with-fields (buffer point-row point-column) self
    (setf point-row (max 0 (1- point-row)))
    (setf point-column (min point-column
			    (length (nth point-row buffer))))))

(define-method newline text ()
  (with-fields (buffer point-row point-column) self
    (if (null buffer)
	(progn (push "" buffer)
	       (setf point-row 1))
	(if (and (= point-row (length buffer))
		 (= point-column (length (nth point-row buffer))))
	    (progn (setf buffer (append buffer (list "")))
		   (incf point-row)
		   (setf point-column 0))
	    ;;  insert line break
	    (let* ((line (nth point-row buffer))
		   (line-remainder (subseq line point-column))
		   (buffer-remainder (nthcdr (1+ point-row) buffer)))
	      ;; truncate current line
	      (setf (nth point-row buffer) 
		    (subseq line 0 point-column))
	      ;; insert new line
	      (if (= 0 point-row)
		  (setf (cdr buffer)
			(cons line-remainder (cdr buffer)))
		  (setf (cdr (nthcdr (- point-row 1) buffer))
			(cons (nth point-row buffer)
			      (cons line-remainder buffer-remainder))))
	      ;;
	      (incf point-row)			
	      (setf point-column 0))))))

(define-method backward-delete-char text ()
  (with-fields (buffer point-row point-column) self
    (if (and (= 0 point-column) 
	     (not (= 0 point-row)))
	(progn 
	  ;;
	  ;; we need to remove a line break.
	  (let ((line (nth (- point-row 1) buffer))
		(next-line (nth (+ point-row 1) buffer))
		(len (length buffer)))
	    (setf buffer (append (subseq buffer 0 (- point-row 1))
				 (list (concatenate 'string line (nth point-row buffer)))
				 (subseq buffer (min len (+ point-row 1)))))
	    ;; (setf (cdr (nthcdr (- point-row 1) buffer))
	    ;; 	  (nth (+ point-row 1) buffer))
	    ;;
	    ;; move cursor too
	    (decf point-row)
	    (setf point-column (length line))))
	;; otherwise, delete within current line.
	(when (not (= 0 point-column))
	  (let* ((line (nth point-row buffer))
		 (remainder (subseq line point-column)))
	    (setf (nth point-row buffer)
		  (concatenate 'string 
			       (subseq line 0 (- point-column 1))
			       remainder))
	    (decf point-column))))))
    
(define-method get-current-line text ()
  (nth %point-row %buffer))

(define-method end-of-line-p text ()
  (= %point-column
     (1- (length (get-current-line self)))))

(define-method beginning-of-line-p text ()
  (= %point-column 0))

(define-method top-of-buffer-p text ()
  (= %point-row 0)) 

(define-method bottom-of-buffer-p text ()
  (= %point-row
     (1- (length %buffer))))

(define-method beginning-of-buffer-p text ()
  (and (beginning-of-line-p self)
       (top-of-buffer-p self)))

(define-method end-of-buffer-p text ()
  (and (end-of-line-p self)
       (bottom-of-buffer-p self)))

(define-method delete-char text ()
  (with-fields (buffer point-row point-column) self
    (if (end-of-line-p self)
	;; just remove line break
	(unless (bottom-of-buffer-p self)
	  (next-line self)
	  (beginning-of-line self)
	  (backward-delete-char self))
	;; remove a character
	(progn 
	  (forward-char self)
	  (backward-delete-char self)))))

(define-method insert text (key)       
  (with-fields (buffer point-row point-column) self
    (if (null buffer)
	(progn
	  (push key buffer)
	  (incf point-column))
	(progn
	  (let* ((line (nth point-row buffer))
		 (remainder (subseq line point-column)))
	    (setf (nth point-row buffer)
		  (concatenate 'string
			       (subseq line 0 point-column)
			       key
			       remainder)))
	  (incf point-column)))))

(define-method insert-string text (string)
  (dolist (character (coerce string 'list))
    (insert self (string character))))

(define-method visible-lines text ()
  (with-fields (buffer max-displayed-lines) self
    (let ((end (length buffer)))
      (if %auto-fit 
	  buffer
	  (subseq buffer 
		  %point-row
		  (if max-displayed-lines
		      (min end max-displayed-lines)
		      end))))))

(define-method layout text ()
  (with-fields (height width font) self
    (when %auto-fit
      (resize-to-fit self))
    (setf width 0)
    (let* ((lines (visible-lines self))
	   (text-height (* (font-height %font) (length lines))))
      (setf height (dash 4 text-height))
      (dolist (line lines)
	(callf max width (dash 4 (font-text-width line font)))))))

(defparameter *text-cursor-width* 2)

(define-method draw text ()
  (with-fields (buffer width parent height) self
    (with-field-values (x y font point-row indicator) self
      ;; measure text
      (let ((line-height (font-height font)))
	  ;; draw background
	(draw-patch self x y 
		    (+ x width)
		    (+ y height)
		    :color (find-color self))
	;; draw text
	(let* ((x0 (+ x *text-margin*))
	       (y0 (+ y *text-margin*))
	       (lines (visible-lines self))
	       (text-height (* line-height (length lines))))
	  (dolist (line lines)
	    (when (plusp (length line))
	      (draw-string line x0 y0 
			   :font font :color %foreground-color))
	    (incf y0 line-height)))))))
      ;; ;; possibly draw emblem
      ;; (draw-emblem self))))

(define-method draw-focus text ()
  ;; draw cursor
  ;; TODO fix %point-row to be drawn relative pos in scrolling
  (with-fields (buffer width parent height) self
    (with-field-values (x y font point-row) self
      (when (null %read-only)
	(let* ((line-height (font-height font))
	       (current-line (nth point-row buffer))
	       (cursor-width *text-cursor-width*)
	       (x1 (+ x *text-margin*
		      (font-text-width (subseq current-line 0 %point-column)
				       font)))
	       (y1 (+ y *text-margin*
		      (* point-row (font-height font)))))
	  (draw-cursor-glyph self x1 y1 cursor-width line-height 
			     :blink t))))))

(define-method draw-hover text () nil)
	  
;;; text.lisp ends here
