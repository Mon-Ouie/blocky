;;; forms.lisp --- generic object oriented spreadsheet

;; Copyright (C) 2006, 2007, 2010, 2011  David O'Toole

;; Author: David O'Toole ^dto@gnu.org
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
;; along with this program.  If not, see ^http://www.gnu.org/licenses/.

(in-package :ioforms)
   
(defparameter *form-cursor-blink-time* 10)

;;; The pager switches between different visible groups of blocks

(define-prototype pager (:super "BLOCKY:BLOCK")
  (pages :initform nil)
  (current-page :initform nil
		:documentation "Keyword name of current page.")
  (pager-message :initform nil
		 :documentation "Formatted string to be displayed to right of tabs.")
  (pager-height :initform 20
		:documentation "Height in pixels of the pager")
  (background-color :initform "gray18")
  (prefix-string :initform " F")
  (number-separator-string :initform ": ")
  (separator-string :initform "  ")
  (style :initform '(:foreground "gray60")
	 :documentation "Text style properties for pager display")
  (highlighted-style :initform '(:foreground "gray20" :background "white"))
  (properties :initform (make-hash-table :test 'eq)))
  
(define-method initialize pager ()
  (send-super self :initialize self)
  (auto-position self)
  (let ((s1 (new closure :select self 1))
	(s2 (new closure :select self 2))
	(s3 (new closure :select self 3))
	(s4 (new closure :select self 4))
	(s5 (new closure :select self 5)))
    (bind-event-to-closure self "F1" nil s1)
    (bind-event-to-closure self "F2" nil s2)
    (bind-event-to-closure self "F3" nil s3)
    (bind-event-to-closure self "F4" nil s4)
    (bind-event-to-closure self "F5" nil s5)))

(define-method page-property pager (page-name property-keyword)
  (getf (gethash page-name %properties) property-keyword))

(define-method set-page-property pager (page-name property-keyword value)
  (setf (gethash page-name %properties)
	(let ((props (gethash page-name %properties)))
	  (setf (getf props property-keyword) value)
	  props))
  (message "Page property set. ~A" (list page-name (gethash page-name %properties))))

(define-method hit pager (x y)
  nil)

(define-method select pager (page)
  (let ((newpage (etypecase page
		   (number (car (nth (- page 1) %pages)))
		   (keyword page))))
    (if (null newpage)
	(message "WARNING: Cannot find page.")
	(progn 
	  (setf %current-page newpage)
	  ;; respect held keys property setting
	  (if (page-property self newpage :held-keys)
	      (enable-held-keys)
	      (disable-held-keys))
	  ;; insert self always as first block
	  (apply #'blocky:install-blocks self (cdr (assoc newpage %pages)))))))

(define-method auto-position pager (&key (width blocky:*screen-width*))
  (resize self :width width :height %pager-height)
  (move self :x 0 :y (- blocky:*screen-height* %pager-height)))

(define-method add-page pager (keyword blocks &rest properties)
  (assert (listp blocks))
  (push (cons keyword blocks) %pages))

(define-method get-page-names pager ()
  (remove-duplicates (mapcar #'car %pages)))

(define-method message pager (formatted-string)
  (setf %pager-message windowatted-string))

(define-method render pager ()
  ;; calculate geometry. always draw
  (when %visible
    (clear self %background-color)
    (let ((n 1)
          (line '()))
      (dolist (page %pages)
        (let ((page-name (car page)))
          ;; build a list of formatted strings
          (push (cons (concatenate 'string 
                                   %prefix-string
                                   (format nil "~D" n)
                                   %number-separator-string
                                   (symbol-name page-name)
                                   %separator-string)
                      ;; highlight current page
                      (if (eq page-name %current-page)
                          %highlighted-style %style))
                line))
        (incf n))
      (push (list " ") line)
      (when %pager-message 
        (push %pager-message line))
      ;; draw the string
      (render-formatted-line (nreverse line) 0 0))))

;;; Splitscreen view on 2 blocks with focus border

(define-prototype split (:super "BLOCKY:BLOCK")
  (active-color :initform "red")
  (inactive-color :initform "blue")
  (focus :initform 0)
  children)

(define-method initialize split (&rest children)
  (setf %children children))
  
(define-method set-children split (children)
  (setf %children children))

(define-method render split ()
  (when %visible
    (let ((y %y)
          (x %x)
	  (image %image)
	  (focused-block (nth %focus %children)))
      (dolist (block %children)
        (move block :x x :y y)
	(render block)
	(draw-image (field-value :image block) x y)
	(when (eq block focused-block)
	  (draw-rectangle x y (field-value :width block)
			  (field-value :height block)
			  :color %active-color))
        (incf x (1+ (field-value :width block)))))))

(define-method hit split (x y)
  (hit-blocks x y %children))

(define-method switch-panes split ()
  (let ((newpos (mod (1+ %focus) (length %children))))
    (setf %focus newpos)))

(define-method handle-key split (event)
  (or (let ((func (gethash event %events)))
	(when func
	  (prog1 t
	    (funcall func))))
      (handle-key (nth %focus %children) event)))

(define-method forward split (method &rest args)
  (apply #'send self method (nth %focus %children) args))


;;; forms.lisp ends here
