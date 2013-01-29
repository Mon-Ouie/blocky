;;; shell.lisp --- interactive multimedia forthlike repl/editor

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

(in-package :blocky)

;;; Modeline

(defun-memo modeline-position-string (x y)
    (:key #'identity :test 'equal :validator #'identity)
  (format nil "X:~S Y:~S" x y))

(define-block-macro modeline
    (:super list
     :fields 
     ((orientation :initform :horizontal)
      (no-background :initform t))
     :inputs (:project-id (new 'string :read-only t)
	      :buffer-id (new 'string :read-only t)
	      :position (new 'string :read-only t)
	      :mode (new 'string :read-only t))))

(define-method update modeline ()
  (set-value %%project-id *project*)
  (set-value %%buffer-id (%buffer-name (current-buffer)))
  (set-value %%position
	     (modeline-position-string
	      (%window-x (current-buffer))
	      (%window-y (current-buffer))))
  (set-value %%mode
	     (if (current-buffer)
		 (if (%paused (current-buffer))
		     "(paused)"
		     "(playing)")
		 "(empty)")))

;;; Lisp listener prompt that makes active Lisp blocks out of what you type.

(define-block (listener-prompt :super prompt)
  (operation :initform :prompt)
  (background :initform nil)
  (methods :initform '(:debug-on-error :print-on-error))
  output)

(define-method debug-on-error listener-prompt ()
  (setf *debug-on-error* t))

(define-method print-on-error listener-prompt ()
  (setf *debug-on-error* nil))

(define-method initialize listener-prompt (&optional output)
  (next-method self)
  (print-on-error self)
  (setf %output output))

(define-method set-output listener-prompt (output)
  (setf %output output))

(define-method can-pick listener-prompt () t)

(define-method pick listener-prompt ()
  %parent)

(define-method do-sexp listener-prompt (sexp)
  (with-fields (output) self
    (assert output)
    (let ((container (get-parent output)))
      (assert container)
      (let ((result (eval (first sexp))))
	(let ((new-block 
		;; is it a block?
		(if (blockyp result)
		    result
		    ;; no, make a new block from the data
		    (when result (make-block result)))))
	  ;; spit out result block, if any
	  (when new-block 
	    (accept container new-block)
	    (unpin new-block)))))))

(define-method label-width listener-prompt ()
  (dash 2 (font-text-width *default-prompt-string* *font*)))

(define-method do-after-evaluate listener-prompt ()
  ;; print any error output
  (when (and %parent (stringp %error-output)
	     (plusp (length %error-output)))
    (accept %parent (new 'text %error-output))))

;;; Putting it all together into a multiline blocks editor

(define-block shell
  (modified-p :initform nil)
  (point-row :initform 0) 
  (point-column :initform 0)
  (mark-row :initform nil)
  (mark-column :initform nil)
  (cursor-color :initform "yellow")
  (cursor-blink-color :initform "magenta")
  (cursor-blink-clock :initform 0)
  (origin-row :initform 0 :documentation "Row number of top-left displayed cell.") 
  (origin-column :initform 0 :documentation "Column number of top-left displayed cell.")
  (origin-height :initform nil)
  (origin-width :initform nil)

  (row-spacing :initform 1 :documentation "Number of pixels to add between rows.")

  (alignment :initform nil)
  (rows :initform 10)
  (columns :initform 10) 
  (column-widths :documentation "A vector of integers where v(x) is the pixel width of shell column x.")
  (row-heights :documentation "A vector of integers where v(x) is the pixel height of shell row x.")

  (row-styles :documentation "A vector of property lists used to customize the appearance of rows.")
  (column-styles :documentation "A vector of property lists used to customize the appearance of columns.")
  (zebra-stripes :documentation "When non-nil, zebra stripes are drawn.")
  (border-style :initform t :documentation "When non-nil, draw cell borders.")
  (draw-blanks :initform t :documentation "When non-nil, draw blank cells.")

  (header-style :initform t :documentation "When non-nil, draw row and column headers.")
  (header-line :initform nil :documentation "Formatted line to be displayed at top of shell above spreadsheet.")
  (status-line :initform nil :documentation "Formatted line to be displayed at bottom of shell below spreadsheet."))

(defparameter *default-buffer-name* "*scratch*")

(define-method set-modified-p shell (&optional (value t))
  (setf %modified-p value))

(defun buffer-modified-p (&optional (buffer (current-buffer)))
  (%modified-p buffer))
  
(define-method set-mark shell ()
  (setf %mark-row %point-row
	%mark-column %point-column))
   
(define-method clear-mark shell ()
  (setf %mark-row nil %mark-column nil))

(define-method mark-region shell ()
  (with-fields (mark-row mark-column point-row point-column) self
    (if (and (integerp mark-row) (integerp mark-column))
	(values (min mark-row point-row)
		(min mark-column point-column)
		(max mark-row point-row)
		(max mark-column point-column))
	(values nil nil nil nil))))

(define-method initialize shell (&optional (buffer-name *default-buffer-name*))
  (initialize%super self :name buffer)
  (install-keybindings self)
  (setf %point-row 0)
  (setf %point-column 0)
  (clear-mark self)
  (setf %point-column (min %columns %point-column))
  (setf %point-row (min %rows %point-row))
  (setf %point-column (min %columns %point-column))
  (setf %column-widths (make-array (+ 1 %columns) :initial-element 0)
	%row-heights (make-array (+ 1 %rows) :initial-element 0)
	%column-styles (make-array (+ 1 %columns))
	%row-styles (make-array (+ 1 %rows))))

(define-method layout-horizontally shell ()
  (with-fields (x y height spacing width inputs dash) self
    (flet ((ldash (&rest args) (apply #'+ %spacing args)))
      (let ((x0 (+ x spacing))
	    (y0 (ldash y))
	    (line-height (font-height *font*)))
	(setf height (ldash line-height))
	(setf width (dash 2))
	(dolist (element inputs)
	  (move-to element x0 y0)
	  (layout element)
	  (setf height (max height (+ (ldash) (field-value :height element))))
	  (incf x0 (field-value :width element))
	  (incf width (field-value :width element)))
;	  (incf width spacing))
	(incf height spacing)))))

(define-method layout shell ()
  (with-field-values (rows columns display-style buffer
			   column-widths row-heights) self
    (when buffer
      (with-field-values (grid) buffer
	(let ((height 0)
	      (width 0)
	      cell location)
	  (labels ((update-height (row pixels)
		     (setf (aref row-heights row)
			   (max (aref row-heights row) pixels)))
		   (update-width (column pixels)
		     (setf (aref column-widths column)
			   (max (aref column-widths column) pixels))))
	    ;; reset geometry
	    (dotimes (row rows)
	      (update-height row 0))
	    (dotimes (column columns)
	      (update-width column 0))
	    ;; now measure
	    (dotimes (row rows)
	      (dotimes (column columns)
		(setf location (aref grid row column))
		(when (and location (not (zerop (fill-pointer location))))
		  (setf cell (aref location (- (fill-pointer location) 1)))
		  (update-height row (height cell))
		  (update-width column (width cell)))))))))))

(defparameter *even-columns-format* '(:background "gray50" :foreground "gray10"))
(defparameter *odd-columns-format* '(:background "gray45" :foreground "gray10"))

(define-method draw shell ()
  (when %buffer
    (with-field-values (point-row point-column row-heights buffer buffer-name 
				   origin-row origin-column header-line status-line
				   mark-row mark-column width height
				   display-style header-style tool tool-methods entered focused
				   row-spacing rows columns draw-blanks column-widths) self
      (when %computing (compute self))
;;      (layout self)
      (let* ((image %image)
	     (widget-width %width)
	     (widget-height %height)
	     (rightmost-visible-column
	      (block searching
		(let ((width 0))
		  (loop for column from origin-column to columns 
			do (incf width (aref column-widths column))
			   (when (> width widget-width)
			     (return-from searching (- column 1))))
		  (return-from searching (- columns 1)))))
	     (bottom-visible-row
	      (block searching
		(let ((height (if (and header-line header-style)
				  (formatted-line-height header-line)
				  0)))
		  (loop for row from origin-row to rows 
			do (incf height (aref row-heights row))
			   (when (> height widget-height)
			     (return-from searching (- row 1))))
		  (return-from searching (- rows 1)))))
	     (x 0) 
	     (y 0)
	     (cursor-dimensions nil)
	     (mark-dimensions nil))
	;; store some geometry
	(setf %origin-width (- rightmost-visible-column origin-column))
	(setf %origin-height (- bottom-visible-row origin-row))
	;; see if current cell has a tooltip
	;; (let ((selected-cell (cell-at self point-row point-column)))
	;;   (when (object-p selected-cell)
	;;     (setf header-line (field-value :tooltip selected-cell))))
	;; draw header line with tooltip, if any
	(multiple-value-bind (top left bottom right) (mark-region self)
	  (let ((x0 0)
		(y0 0)
		pending-draws
		(x1 width)
		(y1 height))
	    (when (and header-line header-style)
	      (render-formatted-line header-line 0 y :destination image)
	      (incf y (formatted-line-height header-line)))
	    ;; TODO column header, if any
	    ;; (message "GEOMETRY: ~S" (list :origin-row origin-row
	    ;; 			      :origin-column origin-column
	    ;; 			      :right rightmost-visible-column
	    ;; 			      :bottom bottom-visible-row))
	    (loop for row from origin-row to bottom-visible-row do
	      (setf x 0)
	      (loop for column from origin-column to rightmost-visible-column do
		(let ((column-width (aref column-widths column))
		      (row-height (aref row-heights row))
		      (cell (cell-at self row column)))
		  ;; possibly set up region drawing info
		  (when (equal row top)
		    (setf y0 y))
		  (when (equal row bottom)
		    (setf y1 (+ y row-height)))
		  (when (equal column left)
		    (setf x0 x))
		  (when (equal column right)
		    (setf x1 (+ x column-width)))
		  ;; render the cell
		  (if (null cell)
		      (when draw-blanks
			(draw-box x y 
				  column-width 
				  row-height  
				  :stroke-color "gray30"
				  :color (if (evenp column) "gray50" "gray45")
				  :destination image))
		      ;; see also cells.lisp
		      (progn 
			(ecase display-style
			  (:label (render cell image x y column-width))
			  (:image (if (in-category cell :drawn)
				     (push (list cell x y) pending-draws)
				     (when (field-value :image cell)
				       (draw-image (find-resource-object 
						    (field-value :image cell)) x y :destination image)))))
			(when entered
			  (draw-rectangle x y 
					  column-width 
					  row-height
					  :color "red"
					  :destination image))))
		  ;; visually indicate edges of map with a yellow line
		  (let ((iwid 2))
		    (when (= rightmost-visible-column (- columns 1) column)
		      (draw-box (+ x column-width) y iwid row-height :stroke-color "yellow" :color "yellow"
				:destination image))
		    (when (= 0 column)
		      (draw-box 0 y iwid row-height :stroke-color "yellow" :color "yellow"
				:destination image))
		    (when (= bottom-visible-row row (- rows 1))
		      (draw-box x (+ y row-height) column-width iwid :stroke-color "yellow" :color "yellow"
				:destination image))
		    (when (= 0 row)
		      (draw-box x 0 column-width iwid :stroke-color "yellow" :color "yellow"
				:destination image)))
		  ;; possibly save cursor and mark drawing info for this cell
		  (when (and (= row point-row) (= column point-column))
		    (setf cursor-dimensions (list x y column-width row-height)))
		  (when (and (integerp mark-row) (integerp mark-column) (= row mark-row) (= column mark-column))
		    (setf mark-dimensions (list x y column-width row-height)))
		  ;; move to next column right
		  (incf x (aref column-widths column))))
	      ;; move to next row down ;; TODO fix row-spacing
	      (incf y (+ (if (eq :image display-style)
			     0 0) (aref row-heights row))))
	    ;; draw any pending drawn cells
	    (dolist (args pending-draws)
	      (destructuring-bind (cell x y) args
		(draw cell x y image)))
	    ;; create status line
	    ;; TODO break this formatting out into variables
	    (setf status-line
		  (list 
		   (list (format nil " ( ~A )     " buffer-name) :foreground (if focused "yellow" "white")
			 :background (if focused "red" "blue"))
		   (list (format nil "  ~A (~S, ~S) ~Sx~S "
				 tool point-row point-column rows columns)
			 :foreground "white"
			 :background "gray20")))
	    ;; draw status line
	    (when status-line 
	      (let* ((ht (formatted-line-height status-line))
		     (sy (- %height 1 ht)))
		(draw-box 0 sy %width ht :color "gray20" 
			  :stroke-color "gray20" :destination image)
		(render-formatted-line status-line 
				       0 sy 
				       :destination image)))
	    ;; render cursor and mark, if any 
	    (when cursor-dimensions
	      (destructuring-bind (x y w h) cursor-dimensions
	      (draw-cursor self x y w h)))
	    (when mark-dimensions
	      (destructuring-bind (x y w h) mark-dimensions
		(draw-mark self x y w h)))
	    (when (and (integerp mark-row) (integerp mark-column)
		       (notany #'null (list x0 y0 x1 y1)))
	      (draw-region self x0 y0 (- x1 x0) (- y1 y0)))))))))
  
(define-method scroll shell ()
  (with-fields (point-row point-column origin-row origin-column scroll-margin
			   origin-height origin-width buffer rows columns) self
    (when (or 
	   ;; too far left
	   (> (+ origin-column scroll-margin) 
	      point-column)
	   ;; too far right
	   (> point-column
	      (- (+ origin-column origin-width)
		 scroll-margin))
	   ;; too far up
	   (> (+ origin-row scroll-margin) 
	      point-row)
	   ;; too far down 
	   (> point-row 
	      (- (+ origin-row origin-height)
		 scroll-margin)))
      ;; yes. recenter.
      (setf origin-column
	    (max 0
		 (min (- columns origin-width)
		      (- point-column 
			 (truncate (/ origin-width 2))))))
      (setf origin-row
	    (max 0 
		 (min (- rows origin-height)
		      (- point-row
			 (truncate (/ origin-height 2)))))))))

(define-method draw-cursor shell (x y width height)
  (with-fields (cursor-color cursor-blink-color cursor-blink-clock focused) self
    (decf cursor-blink-clock)
    (when (minusp cursor-blink-clock)
      (setf cursor-blink-clock *form-cursor-blink-time*))
    (let ((color (if (or (null focused)
			 (< (truncate (/ *form-cursor-blink-time* 2))
			    cursor-blink-clock))
		     cursor-color
		     cursor-blink-color)))
      (draw-rectangle x y width height :color color :destination %image))))

(define-method draw-mark shell (x y width height)
  (draw-rectangle x y width height :color "white" :destination %image))

(define-method draw-region shell (x y width height)
  (draw-rectangle x y width height :color "cyan" :destination %image))
  
(define-method move-cursor shell (direction)
  "Move the cursor one step in DIRECTION. 
DIRECTION is one of :up :down :right :left."
  (unless %entered
    (with-field-values (point-row point-column rows columns) self
      (let ((cursor (list point-row point-column)))
	(setf cursor (ecase direction
		       (:up (if (= 0 point-row)
				(list (- point-row 1) point-column)
				cursor))
		       (:left (if (= 0 point-column)
				  (list point-row (- point-column 1))
				  cursor))
		       (:down (if (< point-row (- rows 1))
				  (list (+ point-row 1) point-column)
				  cursor))
		       (:right (if (< point-column (- columns 1))
				   (list point-row (+ point-column 1))
				   cursor))))
	(destructuring-bind (r c) cursor
	  (setf %point-row r %point-column c))
	;; possibly scroll
	(scroll self)))))
  
(define-method move-cursor-up shell ()
  (move-cursor self :up))

(define-method move-cursor-down shell ()
  (move-cursor self :down))

(define-method move-cursor-left shell ()
  (move-cursor self :left))

(define-method move-cursor-right shell ()
  (move-cursor self :right))

(define-method move-end-of-line shell ()
  (unless %entered
    (setf %point-column (1- %columns))
    (scroll self)))

(define-method move-beginning-of-line shell ()
  (unless %entered
    (setf %point-column 0)
    (scroll self)))

(define-method move-end-of-column shell ()
  (unless %entered
    (setf %point-row (1- %rows))
    (scroll self)))

(define-method move-beginning-of-column shell ()
  (unless %entered
    (setf %point-row 0)
    (scroll self)))

;;; shell.lisp ends here
