;;; windows.lisp --- an interactive block buffer editor

;; Copyright (C) 2007, 2008, 2009, 2010, 2011  David O'Toole

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

;; 

;;; Code:

(define-block window
  (buffer :initform nil :documentation "The buffer of objects to be displayed.")
  (rows :initform 10)
  (columns :initform 10) 
  (point-row :initform 0) 
  (point-column :initform 0)
  (mark-row :initform nil)
  (mark-column :initform nil)
  (cursor-blink-color :initform "magenta")
  (cursor-blink-clock :initform 0)
  (origin-row :initform 0 :documentation "Row number of top-left displayed cell.") 
  (origin-column :initform 0 :documentation "Column number of top-left displayed cell.")
  (origin-height :initform nil)
  (origin-width :initform nil)
  (column-widths :documentation "A vector of integers where v(x) is the pixel width of window column x.")
  (row-heights :documentation "A vector of integers where v(x) is the pixel height of window row x.")
  (column-styles :documentation "A vector of property lists used to customize the appearance of columns.")
  (row-spacing :initform 1 :documentation "Number of pixels to add between rows.")
  (zebra-stripes :documentation "When non-nil, zebra stripes are drawn.")
  (row-styles :documentation "A vector of property lists used to customize the appearance of rows.")
  (border-style :initform t :documentation "When non-nil, draw cell borders.")
  (draw-blanks :initform t :documentation "When non-nil, draw blank cells.")
  (header-style :initform t :documentation "When non-nil, draw row and column headers.")
  (header-line :initform nil :documentation "Formatted line to be displayed at top of window above spreadsheet.")
  (status-line :initform nil :documentation "Formatted line to be displayed at bottom of window below spreadsheet.")
  (scroll-margin :initform 0)
  (display-style :initform :label)
  (cursor-color :initform "yellow")
  (focused :initform nil)
  (tool :initform :clone :documentation "Keyword symbol identifying the method to be applied.")
  (tool-methods :initform '(:clone :erase :inspect)))

(defparameter *default-buffer-name* "*scratch*")

(define-method initialize window (&optional (buffer *default-buffer-name*))
  (with-fields (entry) self
    (let ((buffer (find-buffer buffer)))
      (super%initialize self)
      (visit self buffer))))

(define-method set-tool window (tool)
  "Set the current sheet's selected tool to TOOL."
  (assert (member tool %tool-methods))
  (setf %tool tool))

(define-method next-tool window ()
  "Switch to the next available tool." 
  (with-fields (tool tool-methods) self
    (let ((pos (position tool tool-methods)))
      (assert pos)
      (setf tool (nth (mod (1+ pos) (length tool-methods))
		      tool-methods))
      (say self (format nil "Changing tool operation to ~S" tool)))))

(define-method set-modified window (&optional (value t))
  (with-fields (buffer) self
    (with-fields (name) buffer
      (set-resource-modified-p name value))))
  
(define-method apply-tool window (data)
  "Apply the current window's tool to the DATA."
  (set-modified self)
  (with-fields (tool tool-methods) self
    (send nil tool self data)))

;; (define-method clone window (data)
;;   "Clone the prototype named by the symbol DATA and drop the clone
;; at the current cursor location. See also APPLY-LEFT and APPLY-RIGHT."
;;   (if (and (symbolp data)
;; 	   (boundp data)
;; 	   (object-p (symbol-value data)))
;;       (drop-cell %buffer (clone (symbol-value data)) %point-row %point-column)
;;       (say self "Cannot clone.")))

;; (define-method inspect window ()
;;   nil)

;; (define-method erase window (&optional data)
;;   "Erase the top cell at the current location."
;;   (say self "Erasing top cell.")
;;   (let ((grid (field-value :grid %buffer)))
;;     (ignore-errors (vector-pop (aref grid %point-row %point-column)))))

(define-method set-mark window ()
  (setf %mark-row %point-row>
	<mark-column %point-column)
  (say self (format nil "Mark set at (~S, ~S)." %mark-row %mark-column)))
   
(define-method clear-mark window ()
  (setf %mark-row nil %mark-column nil)
  (say self "Mark cleared."))

(define-method mark-region window ()
  (with-fields (mark-row mark-column point-row point-column) self
    (if (and (integerp mark-row) (integerp mark-column))
	(values (min mark-row point-row)
		(min mark-column point-column)
		(max mark-row point-row)
		(max mark-column point-column))
	(values nil nil nil nil))))

(define-method visit window (&optional (buffer *default-buffer-name*))
  "Visit the buffer BUFFER with the current window. If BUFFER is a =buffer=
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
    (setf %point-row 0)
    (setf %point-column 0)
    (clear-mark self)
    (setf %point-column (min %columns %point-column))
    (setf %point-row (min %rows %point-row))
    (setf %point-column (min %columns %point-column))
    (setf %column-widths (make-array (+ 1 %columns) :initial-element 0)
	  %row-heights (make-array (+ 1 %rows) :initial-element 0)
	  %column-styles (make-array (+ 1 %columns))
	  %row-styles (make-array (+ 1 %rows)))
    (layout self)))

(define-method cell-at window (row column)
  (assert (and (integerp row) (integerp column)))
  (top-cell %buffer row column))

(define-method set-prompt window (prompt)
  (setf %prompt prompt))

(define-method set-narrator window (narrator)
  (setf %narrator narrator))

(define-method install-keybindings window ()
  nil)

(define-method set-display-style window (style)
  "Set the rendering style of the current window to STYLE.
Must be one of (:image :label)."
  (setf %display-style style)
  (layout self))

(define-method image-view window ()
  "Switch to image view in the current window."
  (set-display-style self :image))

(define-method label-view window ()
  "Switch to label view in the current window."
  (set-display-style self :label))

(define-method goto-prompt window ()
  "Jump to the command prompt."
  (when %prompt
    (goto %prompt)))

(define-method selected-cell window ()
  (cell-at self %point-row %point-column))

(define-method activate window ()
  (let ((cell (selected-cell self)))
    (when cell
      (activate cell))))

(define-method eval window (&rest args)
  "Evaluate all the ARGS and print the result."
  (when %prompt 
    (print-data %prompt args :comment)))
 
(define-method say window (text)
  (when %prompt
    (say %prompt text)))

(define-method help window (&optional (command-name :commands))
  "Print documentation for the command COMMAND-NAME.
Type HELP :COMMANDS for a list of available commands."
  (let* ((command (make-keyword command-name))
	 (docstring (method-documentation command))
	 (arglist (method-arglist command)))
    (with-field-values (prompt) self
      (when prompt
	(print-data prompt (format nil "Command name: ~A" command) :comment)
	(print-data prompt (format nil "Arguments: ~a" (if (eq arglist :not-available)
					       :none arglist))
		    :comment)
	(print-data prompt (format nil" ~A" docstring) :comment)))))

(define-method save-all window ()
  (say self "Saving objects...")
  (ioforms:save-objects :force)
  (say self "Saving objects... Done."))

(define-method save window ()
  (say self "Saving objects...")
  (ioforms:save-objects)
  (say self "Saving objects... Done."))
  
(define-method create-buffer window (&key height width name object)
  "Create and visit a blank buffer of height HEIGHT, width WIDTH, and name NAME.
If OBJECT is specified, use the NAME but ignore the HEIGHT and WIDTH."
  (let ((buffer (or object (create-blank-buffer :height height :width width :name name))))
    (when name (setf (field-value :name buffer) name))
    (make buffer)
    (visit self buffer)))

(define-method enter-or-exit window ()
  (if %entered
      (exit self)
      (enter self)))

(define-method enter window ()
  "Begin entering LISP data into the current cell."
  (unless %entered
    (say self "Now entering data. Press Control-ENTER to finish, or ESCAPE to cancel.")
    (let ((entry (clone =textbox=))
	  (cell (selected-cell self)))
      (resize entry :width 150 :height 30)
      (move entry :x 0 :y 0)
      (when (null cell)
	(setf cell (clone =data-cell=))
	(drop-cell %buffer cell %point-row %point-column))
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

(define-method exit window (&optional nosave)
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
    
(define-method open-project window (name)
  "Load the IOFORMS project named NAME for development."
  (say self (format nil "Loading module ~S" name))
  (ioforms:open-project name))

(define-method quit window ()
  "Quit XIOFORMS."
  (ioforms:quit t))

(define-method cancel window ()
  (clear-mark self)
  (exit self :nosave))

(defparameter *blank-cell-string* '(" ........ "))

;; (define-method row-height window (row)
;;   (let ((height 0) cell)
;;     (dotimes (column %columns)
;;       (setf cell (cell-at self row column))
;;       (when cell
;; 	(setf height (max height (height cell)))))
;;     (ecase %display-style
;;       (:label (max (formatted-string-height *blank-cell-string*) height))
;;       (:image height))))

;; (define-method column-width window (column)
;;   (let ((width 0) cell)
;;     (dotimes (row %rows)
;;       (setf cell (cell-at self row column))
;;       (when cell
;; 	(setf width (max width (width cell)))))
;;     (ecase %display-style 
;;       (:label (max width (formatted-string-width *blank-cell-string*)))
;;       (:image width))))

(define-method layout window ()
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

(define-method handle-key window (event)
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

(define-method hit window (x0 y0) 
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
	      (setf %point-row selected-row
		    %point-column selected-column))))))))
  
(define-method compute window ())

;; TODO break up this method.

(define-method render window ()
  (clear self)
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
  
;;; Cursor
  
(define-method scroll window ()
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

(define-method draw-cursor window (x y width height)
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

(define-method draw-mark window (x y width height)
  (draw-rectangle x y width height :color "white" :destination %image))

(define-method draw-region window (x y width height)
  (draw-rectangle x y width height :color "cyan" :destination %image))
  
(define-method move-cursor window (direction)
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
  
(define-method move-cursor-up window ()
  (move-cursor self :up))

(define-method move-cursor-down window ()
  (move-cursor self :down))

(define-method move-cursor-left window ()
  (move-cursor self :left))

(define-method move-cursor-right window ()
  (move-cursor self :right))

(define-method move-end-of-line window ()
  (unless %entered
    (setf %point-column (1- %columns))
    (scroll self)))

(define-method move-beginning-of-line window ()
  (unless %entered
    (setf %point-column 0)
    (scroll self)))

(define-method move-end-of-column window ()
  (unless %entered
    (setf %point-row (1- %rows))
    (scroll self)))

(define-method move-beginning-of-column window ()
  (unless %entered
    (setf %point-row 0)
    (scroll self)))

;;; windows.lisp ends here
