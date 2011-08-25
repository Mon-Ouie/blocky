;;; buffers.lisp --- collecting related blocks into groups

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

(define-block form
  (buffer :documentation "The buffer to be displayed.")
  rows columns
  (entered :initform nil :documentation "When non-nil, forward key events to the entry and/or any attached widget.")
  (cursor-row :initform 0) 
  (cursor-column :initform 0)
  (mark-row :initform nil)
  (mark-column :initform nil)
  (scroll-margin :initform 0)
  (display-style :initform :label)
  (cursor-color :initform "yellow")
  (focused :initform nil)
  (cursor-blink-color :initform "magenta")
  (cursor-blink-clock :initform 0)
  (origin-row :initform 0 :documentation "Row number of top-left displayed cell.") 
  (origin-column :initform 0 :documentation "Column number of top-left displayed cell.")
  (origin-height :initform nil)
  (origin-width :initform nil)
  (column-widths :documentation "A vector of integers where v(x) is the pixel width of form column x.")
  (row-heights :documentation "A vector of integers where v(x) is the pixel height of form row x.")
  (column-styles :documentation "A vector of property lists used to customize the appearance of columns.")
  (row-spacing :initform 1 :documentation "Number of pixels to add between rows.")
  (zebra-stripes :documentation "When non-nil, zebra stripes are drawn.")
  (row-styles :documentation "A vector of property lists used to customize the appearance of rows.")
  (border-style :initform t :documentation "When non-nil, draw cell borders.")
  (draw-blanks :initform t :documentation "When non-nil, draw blank cells.")
  (header-style :initform t :documentation "When non-nil, draw row and column headers.")
  (header-line :initform nil :documentation "Formatted line to be displayed at top of window above spreadsheet.")
  (status-line :initform nil :documentation "Formatted line to be displayed at bottom of window below spreadsheet.")
  (tool :initform :clone :documentation "Keyword symbol identifying the method to be applied.")
  (tool-methods :initform '(:clone :erase :inspect)))

(defparameter *default-buffer-name* "*scratch*")

(define-method initialize form (&optional (buffer *default-buffer-name*))
  (with-fields (entry) self
    (let ((buffer (find-buffer buffer)))
      (send-parent self :initialize self)
      (visit self buffer))))

(define-method blank form (&rest parameters)
  "Invoke the current buffer's default :make method, passing PARAMETER."
  (make-with-parameters %buffer parameters))

(define-method set-tool form (tool)
  "Set the current sheet's selected tool to TOOL."
  (assert (member tool %tool-methods))
  (setf %tool tool))

(define-method get-selected-cell-data form ()
  (let ((cell (selected-cell self)))
    (when cell
      (get cell))))

(define-method focus form ()
  (setf %focused t))

(define-method unfocus form ()
  (setf %focused nil))

(define-method next-tool form ()
  "Switch to the next available tool." 
  (with-fields (tool tool-methods) self
    (let ((pos (position tool tool-methods)))
      (assert pos)
      (setf tool (nth (mod (1+ pos) (length tool-methods))
		      tool-methods))
      (say self (format nil "Changing tool operation to ~S" tool)))))

(define-method set-modified form (&optional (value t))
  (with-fields (buffer) self
    (with-fields (name) buffer
      (set-resource-modified-p name value))))
  
(define-method apply-tool form (data)
  "Apply the current form's tool to the DATA."
  (set-modified self)
  (with-fields (tool tool-methods) self
    (send nil tool self data)))

(define-method clone form (data)
  "Clone the prototype named by the symbol DATA and drop the clone
at the current cursor location. See also APPLY-LEFT and APPLY-RIGHT."
  (if (and (symbolp data)
	   (boundp data)
	   (object-p (symbol-value data)))
      (drop-cell %buffer (clone (symbol-value data)) %cursor-row %cursor-column)
      (say self "Cannot clone.")))

(define-method inspect form ()
  nil)

(define-method erase form (&optional data)
  "Erase the top cell at the current location."
  (say self "Erasing top cell.")
  (let ((grid (field-value :grid %buffer)))
    (ignore-errors (vector-pop (aref grid %cursor-row %cursor-column)))))

(define-method set-mark form ()
  (setf %mark-row %cursor-row>
	<mark-column %cursor-column)
  (say self (format nil "Mark set at (~S, ~S)." %mark-row %mark-column)))
   
(define-method clear-mark form ()
  (setf %mark-row nil %mark-column nil)
  (say self "Mark cleared."))

(define-method mark-region form ()
  (with-fields (mark-row mark-column cursor-row cursor-column) self
    (if (and (integerp mark-row) (integerp mark-column))
	(values (min mark-row cursor-row)
		(min mark-column cursor-column)
		(max mark-row cursor-row)
		(max mark-column cursor-column))
	(values nil nil nil nil))))

(define-method visit form (&optional (buffer *default-buffer-name*))
  "Visit the buffer BUFFER with the current form. If BUFFER is a =buffer=
object, visit it and add the buffer to the buffer collection. If BUFFER is a
string, visit the named buffer. If the named buffer does not exist, a
default buffer is created. If BUFFER is a list, it is interpreted as a
buffer address, and a new buffer is generated according to that address.
See also CREATE-BUFFER."
  (let ((buffer (find-buffer buffer)))
    (assert (object-p buffer))
    (setf %buffer-name (field-value :name buffer))
    (say self (format nil "Visiting buffer ~S" %buffer-name))
    (set-resource-modified-p %buffer-name t)
    (setf %buffer buffer)
    (setf *buffer* buffer) ;; TODO suspicious
    (install-keybindings self)
    (setf %rows (field-value :height buffer))
    (setf %columns (field-value :width buffer))
    (assert (integerp %rows))
    (assert (integerp %columns))
    (setf %cursor-row 0)
    (setf %cursor-column 0)
    (clear-mark self)
    (setf %cursor-column (min %columns %cursor-column))
    (setf %cursor-row (min %rows %cursor-row))
    (setf %cursor-column (min %columns %cursor-column))
    (setf %column-widths (make-array (+ 1 %columns) :initial-element 0)
	  %row-heights (make-array (+ 1 %rows) :initial-element 0)
	  %column-styles (make-array (+ 1 %columns))
	  %row-styles (make-array (+ 1 %rows)))
    (layout self)))

(define-method set-prompt form (prompt)
  (setf %prompt prompt))

(define-method set-narrator form (narrator)
  (setf %narrator narrator))

(define-method install-keybindings form ()
  nil)

(define-method set-display-style form (style)
  "Set the rendering style of the current form to STYLE.
Must be one of (:image :label)."
  (setf %display-style style)
  (layout self))

(define-method image-view form ()
  "Switch to image view in the current form."
  (set-display-style self :image))

(define-method label-view form ()
  "Switch to label view in the current form."
  (set-display-style self :label))

(define-method goto-prompt form ()
  "Jump to the command prompt."
  (when %prompt
    (goto %prompt)))

(define-method activate form ()
  (let ((cell (selected-cell self)))
    (when cell
      (activate cell))))

;; (define-method eval form (&rest args)
;;   "Evaluate all the ARGS and print the result."
;;   (when %prompt 
;;     (print-data %prompt args :comment)))
 
;; (define-method say form (text)
;;   (when %prompt
;;     (say %prompt text)))

;; (define-method help form (&optional (command-name :commands))
;;   "Print documentation for the command COMMAND-NAME.
;; Type HELP :COMMANDS for a list of available commands."
;;   (let* ((command (make-keyword command-name))
;; 	 (docstring (method-documentation command))
;; 	 (arglist (method-arglist command)))
;;     (with-field-values (prompt) self
;;       (when prompt
;; 	(print-data prompt (format nil "Command name: ~A" command) :comment)
;; 	(print-data prompt (format nil "Arguments: ~a" (if (eq arglist :not-available)
;; 					       :none arglist))
;; 		    :comment)
;; 	(print-data prompt (format nil" ~A" docstring) :comment)))))

;; (define-method save-all form ()
;;   (say self "Saving objects...")
;;   (blocky:save-objects :force)
;;   (say self "Saving objects... Done."))

;; (define-method save form ()
;;   (say self "Saving objects...")
;;   (blocky:save-objects)
;;   (say self "Saving objects... Done."))
  
;; (define-method create-buffer form (&key height width name object)
;;   "Create and visit a blank buffer of height HEIGHT, width WIDTH, and name NAME.
;; If OBJECT is specified, use the NAME but ignore the HEIGHT and WIDTH."
;;   (let ((buffer (or object (create-blank-buffer :height height :width width :name name))))
;;     (when name (setf (field-value :name buffer) name))
;;     (make buffer)
;;     (visit self buffer)))

(define-method enter-or-exit form ()
  (if %entered
      (exit self)
      (enter self)))

(define-method enter form ()
  "Begin entering LISP data into the current cell."
  (unless %entered
    (say self "Now entering data. Press Control-ENTER to finish, or ESCAPE to cancel.")
    (let ((entry (clone =textbox=))
	  (cell (selected-cell self)))
      (resize entry :width 150 :height 30)
      (move entry :x 0 :y 0)
      (when (null cell)
	(setf cell (clone =data-cell=))
	(drop-cell %buffer cell %cursor-row %cursor-column))
      (let ((data (get cell)))
	(when data 
	  (let* ((output (print cell))
		 (lines (etypecase output
			  (string (list output))
			  (list output))))
	    (dolist (line lines)
	      (insert entry line)
	      (newline entry)))
	  (move-end-of-line entry)))
      (install-keybindings entry)
      (setf (field-value :auto-fit entry) t)
      (resize-to-fit entry)
      (setf %entered t)
      (setf (field-value :widget cell)
	    entry))))

(define-method exit form (&optional nosave)
  "Stop entering data into the current cell."
  (when %entered
    (when nosave (say self "Canceled data entry."))
    (with-fields (widget) (selected-cell self)
      (let* ((data (get-buffer-as-string widget)))
	(when data
	  (unless nosave
	    (let ((cell (selected-cell self)))
	      (handler-case 
		  (set cell (read cell data))
		(condition (c) 
		  (say self (format nil "Error reading data: ~S" c)))))))
	(setf widget nil)
	(setf %entered nil)
	(say self "Finished entering data.")))))

(defparameter *blank-cell-string* '(" ........ "))

;; (define-method row-height form (row)
;;   (let ((height 0) cell)
;;     (dotimes (column %columns)
;;       (setf cell (cell-at self row column))
;;       (when cell
;; 	(setf height (max height (height cell)))))
;;     (ecase %display-style
;;       (:label (max (formatted-string-height *blank-cell-string*) height))
;;       (:image height))))

;; (define-method column-width form (column)
;;   (let ((width 0) cell)
;;     (dotimes (row %rows)
;;       (setf cell (cell-at self row column))
;;       (when cell
;; 	(setf width (max width (width cell)))))
;;     (ecase %display-style 
;;       (:label (max width (formatted-string-width *blank-cell-string*)))
;;       (:image width))))

(define-method layout form ()
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

(define-method handle-key form (event)
  ;; possibly forward event to current cell. used for the event cell, see below.
  (prog1
      (if (or (and (equal "RETURN" (first event))
		   (equal :control (second event)))
	      (equal "ESCAPE" (first event)))
	  (send-parent self :initialize self)
	  (let* ((cell (selected-cell self))
		 (widget (when cell (field-value :widget cell))))
	    (cond ((and cell (has-method :handle-key cell))
		   (or (handle-key cell event)
		       (send-parent self :handle-key self event)))
		  ((and widget %entered)
		   (prog1 nil (handle-key widget event)))
		  (t (send-parent self :handle-key self event)))))
    (layout self)))

(define-method hit form (x0 y0) 
  (with-field-values (row-heights column-widths origin-row origin-column rows columns x y width height)
      self
    (when (within-extents x0 y0 x y (+ x width) (+ y height))
      (let* ((x %x)
	     (y %y)
	     (selected-column 
	      (loop for column from origin-column to columns
		    do (incf x (aref column-widths column))
		    when (> x x0) return column))
	     (selected-row 
	      (loop for row from origin-row to rows
		    do (incf y (aref row-heights row))
		    when (> y y0) return row)))
	(when (and (integerp selected-column) (integerp selected-row))
	  (when (array-in-bounds-p (field-value :grid %buffer)
				 selected-row selected-column)
	    (prog1 t
	      (setf %cursor-row selected-row
		    %cursor-column selected-column))))))))
  
(define-method compute form ())

;; TODO break up this method.

(define-method render form ()
  (clear self)
  (when %buffer
    (with-field-values (cursor-row cursor-column row-heights buffer buffer-name 
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
	;; (let ((selected-cell (cell-at self cursor-row cursor-column)))
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
		  (when (and (= row cursor-row) (= column cursor-column))
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
				 tool cursor-row cursor-column rows columns)
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
  
;;; Cursor
  
(define-method scroll form ()
  (with-fields (cursor-row cursor-column origin-row origin-column scroll-margin
			   origin-height origin-width buffer rows columns) self
    (when (or 
	   ;; too far left
	   (> (+ origin-column scroll-margin) 
	      cursor-column)
	   ;; too far right
	   (> cursor-column
	      (- (+ origin-column origin-width)
		 scroll-margin))
	   ;; too far up
	   (> (+ origin-row scroll-margin) 
	      cursor-row)
	   ;; too far down 
	   (> cursor-row 
	      (- (+ origin-row origin-height)
		 scroll-margin)))
      ;; yes. recenter.
      (setf origin-column
	    (max 0
		 (min (- columns origin-width)
		      (- cursor-column 
			 (truncate (/ origin-width 2))))))
      (setf origin-row
	    (max 0 
		 (min (- rows origin-height)
		      (- cursor-row
			 (truncate (/ origin-height 2)))))))))

(define-method draw-cursor form (x y width height)
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

(define-method draw-mark form (x y width height)
  (draw-rectangle x y width height :color "white" :destination %image))

(define-method draw-region form (x y width height)
  (draw-rectangle x y width height :color "cyan" :destination %image))
  
(define-method move-cursor form (direction)
  "Move the cursor one step in DIRECTION. 
DIRECTION is one of :up :down :right :left."
  (unless %entered
    (with-field-values (cursor-row cursor-column rows columns) self
      (let ((cursor (list cursor-row cursor-column)))
	(setf cursor (ecase direction
		       (:up (if (= 0 cursor-row)
				(list (- cursor-row 1) cursor-column)
				cursor))
		       (:left (if (= 0 cursor-column)
				  (list cursor-row (- cursor-column 1))
				  cursor))
		       (:down (if (< cursor-row (- rows 1))
				  (list (+ cursor-row 1) cursor-column)
				  cursor))
		       (:right (if (< cursor-column (- columns 1))
				   (list cursor-row (+ cursor-column 1))
				   cursor))))
	(destructuring-bind (r c) cursor
	  (setf %cursor-row r %cursor-column c))
	;; possibly scroll
	(scroll self)))))
  
(define-method move-cursor-up form ()
  (move-cursor self :up))

(define-method move-cursor-down form ()
  (move-cursor self :down))

(define-method move-cursor-left form ()
  (move-cursor self :left))

(define-method move-cursor-right form ()
  (move-cursor self :right))

(define-method move-end-of-line form ()
  (unless %entered
    (setf %cursor-column (1- %columns))
    (scroll self)))

(define-method move-beginning-of-line form ()
  (unless %entered
    (setf %cursor-column 0)
    (scroll self)))

(define-method move-end-of-column form ()
  (unless %entered
    (setf %cursor-row (1- %rows))
    (scroll self)))

(define-method move-beginning-of-column form ()
  (unless %entered
    (setf %cursor-row 0)
    (scroll self)))

;;; Grouping blocks into buffers with buffer-local variables

(defmacro with-buffer (buffer &rest body)
  `(let ((*buffer* (find-uuid ,buffer)))
     (assert (blockyp *buffer*))
     ,@body))

(define-block (buffer :super list)
  (target :initform nil)
  (needs-layout :initform t)
  (variables :initform (make-hash-table :test 'eq)))

(define-method invalidate-layout buffer ()
  (setf %needs-layout t))

(define-method delete-block buffer (block)
  (verify block)
  (assert (contains self block))
  (delete-input self block))

(define-method bring-to-front buffer (block)
  (with-fields (inputs buffer) self
    (assert (contains buffer block))
    (delete-input self block)
    (append-input self block)))

(define-method on-update buffer ()
  (with-buffer self 
    (dolist (each %inputs)
      (on-update each))
    (update-layout self)))

(define-method update-layout buffer (&optional force)
  (with-fields (inputs needs-layout) self
    (when (or force needs-layout)
      (dolist (each inputs)
	(layout each))
      (setf needs-layout nil))))

(define-method initialize buffer (&key blocks variables target 
				       (width (dash 120))
				       (height (dash 70)))
  (apply #'super%initialize self blocks)
  (message "Initializing BUFFER")
  (setf %width width
	%height height)
  (when variables (setf %variables variables))
  (when target (setf %target target)))

(define-method set-target buffer (target)
  (verify target)
  (setf %target target))

(define-method append-input buffer (block)
  (verify block)
  (with-fields (inputs) self
    (assert (not (contains self block)))
    (set-parent block self)
    (setf inputs (nconc inputs (list block)))))

(define-method add-block buffer (block &optional x y)
  (verify block)
  ;(assert (not (contains self block)))
  (append-input self block)
  (when (and (integerp x)
	     (integerp y))
    (move-to block x y))
  (invalidate-layout self))

(define-method setvar buffer (var value)
  (setf (gethash var %variables) value))

(define-method getvar buffer (var)
  (gethash var %variables))

(defun buffer-local-variable (var-name)
  (getvar *block* var-name))

(defun (setf buffer-local-variable) (var-name value)
  (setvar *block* var-name value))

(defmacro with-buffer-local-variables (vars &rest body)
  (labels ((make-clause (sym)
	     `(,sym (buffer-local-variable ,(make-keyword sym)))))
    (let* ((symbols (mapcar #'make-non-keyword vars))
	   (clauses (mapcar #'make-clause symbols)))
      `(symbol-macrolet ,clauses ,@body))))
	   
;;; Text display and edit control

(defparameter *textbox-margin* (dash 2) "Default onscreen margin (in pixels) of a textbox.")

(defparameter *textbox-minimum-width* 80) 

(define-block textbox
  (methods :initform '(:page-up :page-down :auto-center :resize-to-fit :view-messages))
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

(define-method accept textbox (other))

(define-method enter textbox ()
  (newline self))

(define-method on-event textbox (event)
  (on-text-event self event))

(define-method set-buffer textbox (buffer)
  (setf %buffer buffer))

(define-method get-buffer-as-string textbox ()
  (apply #'concatenate 'string %buffer))

(defparameter *next-screen-context-lines* 3)

(define-method set-font textbox (font)
  (assert (stringp font))
  (assert (eq :font (resource-type (find-resource font))))
  (setf %font font))

(define-method page-up textbox ()
  "Scroll up one page, only when %max-displayed-lines is set."
  (with-field-values (max-displayed-lines) self
    (when (integerp max-displayed-lines)
      (setf %point-row (max 0
			   (- %point-row (- max-displayed-lines
					     *next-screen-context-lines*)))))))

(define-method page-down textbox ()
  "Scroll down one page, only when %max-displayed-lines is set."
  (with-field-values (max-displayed-lines) self
    (when (integerp max-displayed-lines)
      (setf %point-row (min (- (length %buffer) max-displayed-lines)
			     (+ %point-row (- max-displayed-lines
					     *next-screen-context-lines*)))))))

(define-method auto-center textbox ()
  "Automatically center the textbox on the screen."
  (with-field-values (x y width height) self
    (let ((center-x (truncate (/ *screen-width* 2)))
	  (center-y (truncate (/ *screen-height* 2))))
      (setf %x (- center-x (truncate (/ width 2)))
	    %y (- center-y (truncate (/ height 2)))))))

(define-method resize-to-scroll textbox (&key width height)
  "Resize the textbox to WIDTH * HEIGHT and enable scrolling of contents."
  (assert (and (numberp width) (numberp height)))
  (resize self :height height :width width)
  (setf %max-displayed-lines (truncate (/ height (font-height %font)))))

(define-method resize-to-fit textbox ()
  "Automatically resize the textbox to fit the text, and disable scrolling."
  ;; disable scrolling
  (setf %max-displayed-lines nil)
  ;; measure text
  (let* ((buffer %buffer)
	 (line-height (font-height %font))
	 (line-lengths (mapcar #'(lambda (s)
				   (font-text-width s %font))
			       buffer)))
    ;; update geometry
    (let ((width0 (max *textbox-minimum-width*
		       (+ (* 2 *textbox-margin*) 4
			  (if (null line-lengths)
			      0 
			      (apply #'max line-lengths)))))
	  (height0 (+ (* 2 *textbox-margin*)
		      (* line-height (max 1 (length buffer))))))
      (when (or (< %width width0)
		(< %height height0))
	(resize self :height height0 :width width0)))))

(define-method view-messages textbox ()
  (setf %auto-fit nil)
  (add-to-list '*message-hook-functions* 
	       #'(lambda (string)
		   (insert-string self string)
		   (newline self)))
  (setf %buffer (reverse *message-history*)))

(define-method end-of-line textbox ()
  (setf %point-column (length (nth %point-row %buffer))))

(define-method beginning-of-line textbox ()
  (setf %point-column 0))

;; (defun bind-event-to-textbox-insertion (textbox key modifiers &optional (insertion key))
;;   "For textbox P ensure that the event (KEY MODIFIERS) causes the
;; text INSERTION to be inserted at point."
;;  (bind-event-to-closure 
;;   textbox 
;;   (string-upcase key)
;;   modifiers
;;   (new closure :insert textbox (list insertion))))

;; (define-method install-text-keybindings block ()
;;   ;; install basic keybindings
;;   (bind-event-to-method self "A" '(:control) :beginning-of-line)
;;   (bind-event-to-method self "E" '(:control) :end-of-line)
;;   (bind-event-to-method self "HOME nil :beginning-of-line)
;;   (bind-event-to-method self "END nil :end-of-line)
;;   (bind-event-to-method self "N" '(:control) :next-line)
;;   (bind-event-to-method self "P" '(:control) :previous-line)
;;   (bind-event-to-method self "F" '(:control) :forward-char)
;;   (bind-event-to-method self "B" '(:control) :backward-char)
;;   (bind-event-to-method self "DOWN nil :next-line)
;;   (bind-event-to-method self "UP" nil :previous-line)
;;   (bind-event-to-method self "RIGHT nil :forward-char)
;;   (bind-event-to-method self "LEFT" nil :backward-char)
;;   (bind-event-to-method self "K" '(:control) :clear)
;;   (bind-event-to-method self "BACKSPACE" nil :backward-delete-char)
;;   (bind-event-to-method self "DELETE" nil :delete-char)
;;   (bind-event-to-method self "RETURN" nil :newline)
;;   ;; install keybindings for self-inserting characters
;;   (map nil #'(lambda (char)
;; 	       (bind-event-to-text-insertion self (string char) nil
;; 					     (string-downcase char)))
;;        *lowercase-alpha-characters*)
;;   (map nil #'(lambda (char)
;; 	       (bind-event-to-text-insertion self (string char) '(:shift) (string char)))
;;        *uppercase-alpha-characters*)
;;   (map nil #'(lambda (char)
;; 	       (bind-event-to-text-insertion self (string char) nil (string char)))
;;        *numeric-characters*)
;;   ;; other characters
;;   (bind-event-to-text-insertion self "EQUALS" nil "=")
;;   (bind-event-to-text-insertion self "MINUS" nil "-")
;;   (bind-event-to-text-insertion self "EQUALS" '(:control) "+")
;;   (bind-event-to-text-insertion self "SEMICOLON" nil ";")
;;   (bind-event-to-text-insertion self "SEMICOLON" '(:shift) ":")
;;   (bind-event-to-text-insertion self "0" '(:shift) ")")
;;   (bind-event-to-text-insertion self "9" '(:shift) "(")
;;   (bind-event-to-text-insertion self "8" '(:shift) "*")
;;   (bind-event-to-text-insertion self "SPACE" nil " ")
;;   (bind-event-to-text-insertion self "QUOTE" nil "'")
;;   (bind-event-to-text-insertion self "QUOTE" '(:shift) "\""))

(define-method initialize textbox (&optional buffer)
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

(define-method forward-char textbox ()
  (with-fields (buffer point-row point-column) self
    (setf point-column (min (1+ point-column)
			    (length (nth point-row buffer))))))

(define-method backward-char textbox ()
  (with-fields (buffer point-row point-column) self
    (setf point-column (max 0 (1- point-column)))))

(define-method next-line textbox ()
  (with-fields (buffer point-row point-column) self
    (setf point-row (min (1+ point-row)
			 (1- (length buffer))))
    (setf point-column (min point-column 
			    (length (nth point-row buffer))))))

(define-method previous-line textbox ()
  (with-fields (buffer point-row point-column) self
    (setf point-row (max 0 (1- point-row)))
    (setf point-column (min point-column
			    (length (nth point-row buffer))))))

(define-method newline textbox ()
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

(define-method backward-delete-char textbox ()
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
    
(define-method get-current-line textbox ()
  (nth %point-row %buffer))

(define-method end-of-line-p textbox ()
  (= %point-column
     (1- (length (get-current-line self)))))

(define-method beginning-of-line-p textbox ()
  (= %point-column 0))

(define-method top-of-buffer-p textbox ()
  (= %point-row 0)) 

(define-method bottom-of-buffer-p textbox ()
  (= %point-row
     (1- (length %buffer))))

(define-method beginning-of-buffer-p textbox ()
  (and (beginning-of-line-p self)
       (top-of-buffer-p self)))

(define-method end-of-buffer-p textbox ()
  (and (end-of-line-p self)
       (bottom-of-buffer-p self)))

(define-method delete-char textbox ()
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

(define-method insert textbox (key)       
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

(define-method insert-string textbox (string)
  (dolist (character (coerce string 'list))
    (insert self (string character))))

(define-method visible-lines textbox ()
  (with-fields (buffer max-displayed-lines) self
    (let ((end (length buffer)))
      (if %auto-fit 
	  buffer
	  (subseq buffer 
		  %point-row
		  (if max-displayed-lines
		      (min end max-displayed-lines)
		      end))))))

(define-method layout textbox ()
  (with-fields (height width font) self
    (when %auto-fit
      (resize-to-fit self))
    (setf width 0)
    (let* ((lines (visible-lines self))
	   (text-height (* (font-height %font) (length lines))))
      (setf height (dash 4 text-height))
      (dolist (line lines)
	(callf max width (dash 4 (font-text-width line font)))))))

(defparameter *textbox-cursor-width* 2)

(define-method draw textbox ()
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
	(let* ((x0 (+ x *textbox-margin*))
	       (y0 (+ y *textbox-margin*))
	       (lines (visible-lines self))
	       (text-height (* line-height (length lines))))
	  (dolist (line lines)
	    (when (plusp (length line))
	      (draw-string line x0 y0 
			   :font font :color %foreground-color))
	    (incf y0 line-height))))
      ;; possibly draw emblem
      (draw-emblem self))))

(define-method draw-focus textbox ()
  ;; draw cursor
  ;; TODO fix %point-row to be drawn relative pos in scrolling
  (with-fields (buffer width parent height) self
    (with-field-values (x y font point-row) self
      (when (null %read-only)
	(let* ((line-height (font-height font))
	       (current-line (nth point-row buffer))
	       (cursor-width *textbox-cursor-width*)
	       (x1 (+ x *textbox-margin*
		      (font-text-width (subseq current-line 0 %point-column)
				       font)))
	       (y1 (+ y *textbox-margin*
		      (* point-row (font-height font)))))
	  (draw-cursor-glyph self x1 y1 cursor-width line-height 
			     :blink t))))))

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
  (setf %pager-message formatted-string))

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

;;; buffers.lisp ends here
