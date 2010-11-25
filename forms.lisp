;;; forms.lisp --- generic object oriented spreadsheet

;; Copyright (C) 2006, 2007, 2010  David O'Toole

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

(in-package :gluon)

;;; Pages are grids of cells.
;;; Pages also ARE cells!

(defparameter *default-page-z-size* 8)

(defcell page 
  (name :initform nil :documentation "Name of the page.")
  (width :initform 16) 
  (height :initform 16)
  (modified :initform nil)
  ;; cells 
  (grid :documentation "A two-dimensional array of adjustable vectors of cells.")
  (serialized-grid :documentation "A serialized sexp version.")
  ;; forms processing
  (variables :initform nil :documentation "Hash table mapping values to values, local to the form.")
  ;; queueing 
  (message-queue :initform (make-queue))
  (excluded-fields :initform '(:grid)))

(define-method initialize page (&key height width name)
    (when height (setf <height> height))
    (when width (setf <width> width))
    (when name (setf <name> name))
    (setf <variables> (make-hash-table :test 'equal))
    (/create-default-grid self))
  
(define-method make page (&rest parameters)
  (apply #'/initialize self parameters))

(define-method make-with-parameters page (parameters)
  (apply #'send self :make self parameters))

(define-method set page (var value)
  (setf (gethash var <variables>) value))

(define-method get page (var)
  (gethash var <variables>))

(defun page-variable (var-name)
  (/get *page* var-name))

(defun set-page-variable (var-name value)
  (/set *page* var-name value))

(defsetf page-variable set-page-variable)

(defmacro with-page-variables (vars &rest body)
  (labels ((make-clause (sym)
	     `(,sym (page-variable ,(make-keyword sym)))))
    (let* ((symbols (mapcar #'make-non-keyword vars))
	   (clauses (mapcar #'make-clause symbols)))
      `(symbol-macrolet ,clauses ,@body))))

(define-method grid-location page (row column)
  "Return the vector of cells at ROW, COLUMN in the page SELF."
  (when (array-in-bounds-p <grid> row column)
    (aref <grid> row column)))

(define-method grid-location-list page (row column)
  (coerce (/grid-location self row column)
	  'list))

(define-method category-at-p page (row column category)
  "Returns non-nil if there is any cell in CATEGORY at ROW, COLUMN.
CATEGORY may be a list of keyword symbols or one keyword symbol."
  (declare (optimize (speed 3)))
  (let ((catlist (etypecase category
		   (keyword (list category))
		   (list category)))
	(grid <grid>))
    (declare (type (simple-array vector (* *)) grid))
    (and (array-in-bounds-p grid row column)
	 (some #'(lambda (cell)
		   (when (intersection catlist
				       (field-value :categories cell))
		     cell))
	       (aref grid row column)))))

(define-method in-bounds-p page (row column)
  "Return non-nil if ROW and COLUMN are valid coordinates."
  (array-in-bounds-p <grid> row column))

(define-method nth-cell page (n row column)
  (aref (aref <grid> row column) n))

(define-method top-cell page (row column)
  (let ((cells (/grid-location self row column)))
    (when (and cells (not (zerop (fill-pointer cells))))
      (aref cells (- (fill-pointer cells) 1)))))

(define-method drop-cell page (cell row column)
  (vector-push-extend cell (aref <grid> row column)))

(define-method replace-cell page (cell new-cell row column
					&optional &key loadout no-collisions)
  "Replace the CELL with NEW-CELL at ROW, COLUMN in this page."
  (let* ((cells (/grid-location self row column))
	 (pos (position cell cells)))
    (if (numberp pos)
	(setf (aref cells pos) new-cell)
	(error "Could not find cell to replace."))))

(define-method replace-cells-at page (row column data)
  "Destroy the cells at ROW, COLUMN, invoking CANCEL on each,
replacing them with the single cell (or vector of cells) DATA."
  (when (array-in-bounds-p <grid> row column)
    (do-cells (cell (aref <grid> row column))
      (/cancel cell))
    (setf (aref <grid> row column)
	  (etypecase data
	    (vector data)
	    (proton:object (let ((cells (make-array *default-page-z-size* 
						  :adjustable t
						  :fill-pointer 0)))
			   (prog1 cells
			     (vector-push-extend data cells))))))
    (do-cells (cell (aref <grid> row column))
      (/set-location cell row column))))

(define-method delete-cell page (cell row column)
  "Delete CELL from the grid at ROW, COLUMN."
  (ecase (field-value :type cell)
    (:cell
       (let* ((grid <grid>)
	      (square (aref grid row column))
	      (start (position cell square :test #'eq)))
	 (declare (type (simple-array vector (* *)) grid) 
		  (optimize (speed 3)))
	 (when start
	   (replace square square :start1 start :start2 (1+ start))
	   (decf (fill-pointer square)))))
    (:sprite
       (/remove-sprite self cell))))
    
(define-method move page (cell row column)
  "Move CELL to ROW, COLUMN."
  (let* ((old-row (field-value :row cell))
	 (old-column (field-value :column cell)))
    (/delete-cell self cell old-row old-column)
    (/drop-cell self cell row column)))

(define-method delete-category-at page (row column category)
  "Delete all cells in CATEGORY at ROW, COLUMN in the grid.
The cells' :cancel method is invoked."
  (let* ((grid <grid>))
    (declare (type (simple-array vector (* *)) grid)
	     (optimize (speed 3)))
    (when (array-in-bounds-p grid row column)
      (setf (aref grid row column)
	    (delete-if #'(lambda (c) (when (/in-category c category)
				       (prog1 t (/cancel c))))
		       (aref grid row column))))))
 
(define-method serialize page ()
  (proton:with-field-values (width height) self
    (let ((grid <grid>)
	  (sgrid (make-array (list height width) :initial-element nil :adjustable nil)))
      (dotimes (i height)
	(dotimes (j width)
	  (map nil #'(lambda (cell)
		       (when cell 
			 (push (proton:serialize cell) 
			       (aref sgrid i j))))
	       (aref grid i j))))
      (setf <serialized-grid> sgrid))))
    
(define-method deserialize page ()
    (/create-default-grid self)
    (proton:with-field-values (width height grid serialized-grid) self
      (dotimes (i height)
	(dotimes (j width)
	  (map nil #'(lambda (cell)
		       (when cell
			 (vector-push-extend (proton:deserialize cell)
					     (aref grid i j))))
	       (reverse (aref serialized-grid i j)))))
      (setf <serialized-grid> nil)))
  
(define-method create-grid page (&key width height)
  "Initialize all the arrays for a page of WIDTH by HEIGHT cells."
  (let ((dims (list height width)))
    (let ((grid (make-array dims 
		 :element-type 'vector :adjustable nil)))
      ;; now put a vector in each square to represent the z-axis
      (dotimes (i height)
	(dotimes (j width)
	  (setf (aref grid i j)
		(make-array *default-page-z-size* 
			    :adjustable t
			    :fill-pointer 0))))
      (setf <grid> grid
	    <height> height
	    <width> width))))

(define-method create-default-grid page ()
  "If height and width have been set in a page's definition,
initialize the arrays for a page of the size specified there."
  (if (and (numberp <width>)
	   (numberp <height>))
      (/create-grid self :width <width> :height <height>)
      (error "Cannot create default grid without height and width set.")))

(define-method paste-region page (other-page dest-row dest-column source-row source-column source-height source-width 
					       &optional deepcopy)
    (loop for row from 0 to source-height
	  do (loop for column from 0 to source-width
		   do (let* ((cells (/grid-location other-page (+ row source-row) (+ column source-column)))
			     (n 0))
			(when (vectorp cells)
			  (loop while (< n (fill-pointer cells)) do
			    (let* ((cell (aref cells n))
				   (proto (object-parent cell))
				   (new-cell (if (or deepcopy (field-value :auto-deepcopy cell))
						 ;; create a distinct object with the same local field values.
						 (deserialize (serialize cell))
						 ;; create a similar object
						 (clone proto))))
			      (/drop-cell self new-cell (+ row dest-row) (+ column dest-column) :exclusive nil))
			    (incf n)))))))

(define-method clone-onto page (other-page &optional deepcopy)
  (let ((other (etypecase other-page
		 (string (find-resource-object other-page))
		 (proton:object other-page))))
    (with-fields (height width) other
      (/create-grid self :height height :width width)
      (let ((*page* other))
	(/paste-region self other 0 0 0 0 height width deepcopy)))))

(defun generate-page-name (page)
  (concatenate 'string (get-some-object-name page) "-" (format nil "~S" (genseq))))

(defun create-blank-page (&key height width name)
  (let ((page (clone =page= :height height :width width)))
    (prog1 page
      (setf (field-value :name page)
	    (or name (generate-page-name page))))))

(defun find-page (page)
  (etypecase page
    (proton:object 
       ;; check for name collision
       (let* ((old-name (or (field-value :name page)
			    (generate-page-name page)))
	      (new-name (if (find-resource old-name :noerror)
			    (generate-page-name page)
			    old-name)))
	 (message "Indexing new page ~S as ~S" old-name new-name)
	 (prog1 page 
	   (make-object-resource new-name page)
	   (setf (field-value :name page) new-name))))
    (list 
       ;; it's an address
       (destructuring-bind (prototype-name &rest parameters) page
	 (let ((page (clone (symbol-value prototype-name))))
	   [make-with-parameters page parameters]
	   (find-page page))))
    (string (or (find-resource-object page :noerror)
		(progn (make-object-resource page (create-blank-page :name page))
		       (let ((object (find-resource-object page)))
			 (prog1 object
			   (setf (field-value :name object) page))))))))

;; (maphash #'(lambda (k v) (when (resource-p v)
;; 			   (when (eq :object (resource-type v))
;; 			     (message "XXobject ~S" (resource-object v)))))
;; 	 *resource-table*)

;;; A generic data cell just prints out the stored value.

(defparameter *data-cell-style* '(:foreground ".gray40" :background ".white"))

(defcell data-cell data)

(define-method set data-cell (data)
  (setf <data> data))

(define-method get data-cell ()
  <data>)

(define-method print data-cell ()
  (write-to-string <data> :circle t :pretty t :escape nil :lines 1))

(define-method read data-cell (text)
  (read-from-string text))

(define-method compute data-cell ()
  ;; update the label
  (setf <label> (list (cons (format nil " ~S  " <data>) *data-cell-style*))))

;;; A var cell stores a value into a variable, and reads it.

(defparameter *var-cell-style* '(:foreground ".white" :background ".blue"))

(defcell var-cell variable)

(define-method initialize var-cell (variable)
  (setf <variable> variable))

(define-method set var-cell (value)
  (/set-variable *page* <variable> value))

(define-method get var-cell ()
  (/get-variable *page* <variable>))

(define-method compute var-cell ()
  (setf <label> (list (cons (format nil ">> ~A  " <variable>) *var-cell-style*))))

;;; Event cell picks up next event when clicked

(defparameter *event-cell-style* '(:foreground ".yellow" :background ".forest green"))

(defcell event-cell event capturing)

(define-method set event-cell (event)
  (setf <event> event))

(define-method get event-cell ()
  <event>)

(define-method handle-key event-cell (event)
  (when <capturing> 
    ;; signal that we handled this event
    (prog1 t
      (setf <event> event)
      (setf <capturing> nil))))
  
(define-method compute event-cell () 
  (setf <label> 
	(list (cons (if <capturing>
			" CAPTURING... "
			(destructuring-bind (key &rest modifiers) <event>
			  (if modifiers
			      (let ((mod-string (format nil " ~A" 
							(apply #'concatenate 'string 
							       (mapcar #'(lambda (mod)
									   (format nil "~A " mod))
								       modifiers)))))
				(if (string= "JOYSTICK" key)
				    (concatenate 'string key mod-string)
				    (concatenate 'string mod-string key " ")))
			      (concatenate 'string " " key " "))))
		    *event-cell-style*))))

(define-method activate event-cell ()
  ;; capture next event
  (setf <capturing> t))

;;; Comment cell just displays text.

(defparameter *comment-cell-style* '(:foreground ".white" :background ".gray20"))

(defcell comment-cell comment)

(define-method set comment-cell (comment)
  (setf <comment> comment))

(define-method get comment-cell ()
  <comment>)

(define-method compute comment-cell () 
  (setf <label> (list (cons (format nil " ~A " <comment>) *comment-cell-style*))))

;;; Image cell just displays an image.

(defcell image-cell image)

(define-method set image-cell (image) 
  (setf <image> image))

(define-method get image-cell ()
  <image>)

(define-method compute image-cell () 
  (when <image> (setf <label> (list (list nil :image <image>)))))

;;; Button cell executes some lambda

(defparameter *button-cell-style* '(:foreground ".yellow" :background ".red"))

(defparameter *button-cell-highlight-style* '(:foreground ".red" :background ".white"))

(defparameter *button-cell-highlight-time* 15)

(defcell button-cell button clock)

(define-method initialize button-cell (&key closure text)
  (setf <closure> closure)
  (setf <clock> 0)
  (setf <label> (list (cons text *button-cell-style*))))
  
(define-method set button-cell (button) nil)

(define-method get button-cell () <closure>)

(define-method compute button-cell () 
  (with-fields (label clock) self
    (unless (zerop clock)
      (decf clock))
    (setf (cdr (first label))
	  (if (plusp clock)
	      *button-cell-highlight-style*
	      *button-cell-style*))))

(define-method activate button-cell ()
  (funcall <closure>)
  (setf <clock> *button-cell-highlight-time*))

;;; Command cell executes some prompt command

(defparameter *command-cell-style* '(:foreground ".white" :background ".gray20"))

(defparameter *command-cell-highlight-style* '(:foreground ".yellow" :background ".red"))

(defparameter *command-cell-highlight-time* 15)

(defvar *form-command-handler-function* nil)

(defcell command-cell command (clock :initform 0))
  
(define-method initialize command-cell ()
  (setf <label> (list (list "                    "))))

(define-method set command-cell (command)
  (setf <command> command)
  (setf <label> (mapcar #'list command)))

(define-method get command-cell () 
  <command>)

(define-method command-string command-cell ()
  (when <command> 
    (apply #'concatenate 'string <command>)))

(define-method compute command-cell () 
  (with-fields (label clock) self
    (unless (zerop clock)
      (decf clock))
    (when label (setf (cdr (first label))
		      (if (plusp clock)
			  *command-cell-highlight-style*
			  *command-cell-style*)))))

(define-method activate command-cell ()
  (when (and <command> *form-command-handler-function*)
    (let ((command (apply #'concatenate 'string <command>)))
      (funcall *form-command-handler-function* command)
      (setf <clock> *command-cell-highlight-time*))))

(define-method print command-cell ()
  <command>)

(define-method read command-cell (newdata)
  (let ((lines (etypecase newdata
		 (list newdata)
		 (string (list newdata)))))
    (setf <command> lines)))

;;; Plain text buffer widget 

(defcell buffer-cell buffer)

(define-method set buffer-cell (buffer) 
  (setf <buffer> buffer))

(define-method get buffer-cell ()
  <buffer>)

(define-method print buffer-cell ()
  <buffer>)

(define-method read buffer-cell (newdata)
  (let ((lines (etypecase newdata
		 (list newdata)
		 (string (list newdata)))))
    (setf <buffer> lines)))
     
;;; The form GUI widget browses workbook pages composed of cells

(defparameter *form-cursor-blink-time* 10)

(define-prototype form 
    (:parent =widget= :documentation  "An interactive graphical spreadsheet.")
  prompt narrator computing
  (page-name :initform nil)
  (page :documentation "The gluon:=page= of objects to be displayed.")
  rows columns
  (entered :initform nil :documentation "When non-nil, forward key events to the entry and/or any attached widget.")
  (cursor-row :initform 0) 
  (cursor-column :initform 0)
  (mark-row :initform nil)
  (mark-column :initform nil)
  (scroll-margin :initform 0)
  (display-style :initform :label)
  (cursor-color :initform ".yellow")
  (focused :initform nil)
  (cursor-blink-color :initform ".magenta")
  (cursor-blink-clock :initform 0)
  (origin-row :initform 0 :documentation "Row number of top-left displayed cell.") 
  (origin-column :initform 0 :documentation "Column number of top-left displayed cell.")
  (origin-height :initform nil)
  (origin-width :initform nil)
  (column-widths :documentation "A vector of integers where v(/x) is the pixel width of form column x.")
  (row-heights :documentation "A vector of integers where v(/x) is the pixel height of form row x.")
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

(defparameter *default-page-name* "*scratch*")

(define-method initialize form (&optional (page *default-page-name*))
  (with-fields (entry) self
    (let ((page (find-page page)))
      (send-parent self :initialize self)
      (/visit self page))))

(define-method blank form (&rest parameters)
  "Invoke the current page's default :make method, passing PARAMETER."
  (/make-with-parameters <page> parameters))

(define-method set-tool form (tool)
  "Set the current sheet's selected tool to TOOL."
  (assert (member tool <tool-methods>))
  (setf <tool> tool))

(define-method get-selected-cell-data form ()
  (let ((cell (/selected-cell self)))
    (when cell
      (/get cell))))

(define-method focus form ()
  (setf <focused> t))

(define-method unfocus form ()
  (setf <focused> nil))

(define-method next-tool form ()
  "Switch to the next available tool." 
  (with-fields (tool tool-methods) self
    (let ((pos (position tool tool-methods)))
      (assert pos)
      (setf tool (nth (mod (1+ pos) (length tool-methods))
		      tool-methods))
      (/say self (format nil "Changing tool operation to ~S" tool)))))

(define-method set-modified form (&optional (value t))
  (with-fields (page) self
    (with-fields (name) page
      (set-resource-modified-p name value))))
  
(define-method apply-tool form (data)
  "Apply the current form's tool to the DATA."
  (/set-modified self)
  (with-fields (tool tool-methods) self
    (send nil tool self data)))

(define-method clone form (data)
  "Clone the prototype named by the symbol DATA and drop the clone
at the current cursor location. See also APPLY-LEFT and APPLY-RIGHT."
  (if (and (symbolp data)
	   (boundp data)
	   (proton:object-p (symbol-value data)))
      (/drop-cell <page> (clone (symbol-value data)) <cursor-row> <cursor-column>)
      (/say self "Cannot clone.")))

(define-method inspect form ()
  nil)

(define-method erase form (&optional data)
  "Erase the top cell at the current location."
  (/say self "Erasing top cell.")
  (let ((grid (field-value :grid <page>)))
    (ignore-errors (vector-pop (aref grid <cursor-row> <cursor-column>)))))

(define-method set-mark form ()
  (setf <mark-row> <cursor-row>
	<mark-column> <cursor-column>)
  (/say self (format nil "Mark set at (~S, ~S)." <mark-row> <mark-column>)))
   
(define-method clear-mark form ()
  (setf <mark-row> nil <mark-column> nil)
  (/say self "Mark cleared."))

(define-method mark-region form ()
  (with-fields (mark-row mark-column cursor-row cursor-column) self
    (if (and (integerp mark-row) (integerp mark-column))
	(values (min mark-row cursor-row)
		(min mark-column cursor-column)
		(max mark-row cursor-row)
		(max mark-column cursor-column))
	(values nil nil nil nil))))

(define-method visit form (&optional (page *default-page-name*))
  "Visit the page PAGE with the current form. If PAGE is a =page=
object, visit it and add the page to the page collection. If PAGE is a
string, visit the named page. If the named page does not exist, a
default page is created. If PAGE is a list, it is interpreted as a
page address, and a new page is generated according to that address.
See also CREATE-PAGE."
  (let ((page (find-page page)))
    (assert (object-p page))
    (setf <page-name> (field-value :name page))
    (/say self (format nil "Visiting page ~S" <page-name>))
    (set-resource-modified-p <page-name> t)
    (setf <page> page)
    (setf *page* page) ;; TODO suspicious
    (/install-keybindings self)
    (setf <rows> (field-value :height page))
    (setf <columns> (field-value :width page))
    (assert (integerp <rows>))
    (assert (integerp <columns>))
    (setf <cursor-row> 0)
    (setf <cursor-column> 0)
    (/clear-mark self)
    (setf <cursor-column> (min <columns> <cursor-column>))
    (setf <cursor-row> (min <rows> <cursor-row>))
    (setf <cursor-column> (min <columns> <cursor-column>))
    (setf <column-widths> (make-array (+ 1 <columns>) :initial-element 0)
	  <row-heights> (make-array (+ 1 <rows>) :initial-element 0)
	  <column-styles> (make-array (+ 1 <columns>))
	  <row-styles> (make-array (+ 1 <rows>)))
    (/compute-geometry self)))

(define-method cell-at form (row column)
  (assert (and (integerp row) (integerp column)))
  (/top-cell <page> row column))

(define-method set-prompt form (prompt)
  (setf <prompt> prompt))

(define-method set-narrator form (narrator)
  (setf <narrator> narrator))

(define-method install-keybindings form ()
  nil)

(define-method set-display-style form (style)
  "Set the rendering style of the current form to STYLE.
Must be one of (:image :label)."
  (setf <display-style> style)
  (/compute-geometry self))

(define-method image-view form ()
  "Switch to image view in the current form."
  (/set-display-style self :image))

(define-method label-view form ()
  "Switch to label view in the current form."
  (/set-display-style self :label))

(define-method goto-prompt form ()
  "Jump to the command prompt."
  (when <prompt>
    (/goto <prompt>)))

(define-method selected-cell form ()
  (/cell-at self <cursor-row> <cursor-column>))

(define-method activate form ()
  (let ((cell (/selected-cell self)))
    (when cell
      (/activate cell))))

(define-method eval form (&rest args)
  "Evaluate all the ARGS and print the result."
  (when <prompt> 
    (/print-data <prompt> args :comment)))
 
(define-method say form (text)
  (when <prompt>
    (/say <prompt> text)))

(define-method help form (&optional (command-name :commands))
  "Print documentation for the command COMMAND-NAME.
Type HELP :COMMANDS for a list of available commands."
  (let* ((command (make-keyword command-name))
	 (docstring (method-documentation command))
	 (arglist (method-arglist command)))
    (with-field-values (prompt) self
      (when prompt
	(/print-data prompt (format nil "Command name: ~A" command) :comment)
	(/print-data prompt (format nil "Arguments: ~a" (if (eq arglist :not-available)
					       :none arglist))
		    :comment)
	(/print-data prompt (format nil" ~A" docstring) :comment)))))

(define-method save-all form ()
  (/say self "Saving objects...")
  (gluon:save-modified-objects t)
  (/say self "Saving objects... Done."))

(define-method save-modified form ()
  (/say self "Saving objects...")
  (gluon:save-modified-objects)
  (/say self "Saving objects... Done."))
  
(define-method create-page form (&key height width name object)
  "Create and visit a blank page of height HEIGHT, width WIDTH, and name NAME.
If OBJECT is specified, use the NAME but ignore the HEIGHT and WIDTH."
  (let ((page (or object (create-blank-page :height height :width width :name name))))
    (when name (setf (field-value :name page) name))
    (/make page)
    (/visit self page)))

(define-method enter-or-exit form ()
  (if <entered>
      (/exit self)
      (/enter self)))

(define-method enter form ()
  "Begin entering LISP data into the current cell."
  (unless <entered>
    (/say self "Now entering data. Press Control-ENTER to finish, or ESCAPE to cancel.")
    (let ((entry (clone =textbox=))
	  (cell (/selected-cell self)))
      (/resize entry :width 150 :height 30)
      (/move entry :x 0 :y 0)
      (when (null cell)
	(setf cell (clone =data-cell=))
	(/drop-cell <page> cell <cursor-row> <cursor-column>))
      (let ((data (/get cell)))
	(when data 
	  (let* ((output (/print cell))
		 (lines (etypecase output
			  (string (list output))
			  (list output))))
	    (dolist (line lines)
	      (/insert entry line)
	      (/newline entry)))
	  (/move-end-of-line entry)))
      (/install-keybindings entry)
      (setf (field-value :auto-fit entry) t)
      (/resize-to-fit entry)
      (setf <entered> t)
      (setf (field-value :widget cell)
	    entry))))

(define-method exit form (&optional nosave)
  "Stop entering data into the current cell."
  (when <entered>
    (when nosave (/say self "Canceled data entry."))
    (with-fields (widget) (/selected-cell self)
      (let* ((data (/get-buffer-as-string widget)))
	(when data
	  (unless nosave
	    (let ((cell (/selected-cell self)))
	      (handler-case 
		  (/set cell (/read cell data))
		(condition (c) 
		  (/say self (format nil "Error reading data: ~S" c)))))))
	(setf widget nil)
	(setf <entered> nil)
	(/say self "Finished entering data.")))))
    
(define-method load-module form (name)
  "Load the GLUON module NAME for development."
  (/say self (format nil "Loading module ~S" name))
  (gluon:load-module name))

(define-method quit form ()
  "Quit XIOMACS."
  (gluon:quit t))

(define-method cancel form ()
  (/clear-mark self)
  (/exit self :nosave))

(defparameter *blank-cell-string* '(" ........ "))

;; (define-method row-height form (row)
;;   (let ((height 0) cell)
;;     (dotimes (column <columns>)
;;       (setf cell (/cell-at self row column))
;;       (when cell
;; 	(setf height (max height (/height cell)))))
;;     (ecase <display-style>
;;       (:label (max (formatted-string-height *blank-cell-string*) height))
;;       (:image height))))

;; (define-method column-width form (column)
;;   (let ((width 0) cell)
;;     (dotimes (row <rows>)
;;       (setf cell (/cell-at self row column))
;;       (when cell
;; 	(setf width (max width (/width cell)))))
;;     (ecase <display-style> 
;;       (:label (max width (formatted-string-width *blank-cell-string*)))
;;       (:image width))))

(define-method compute-geometry form ()
  (with-field-values (rows columns display-style page
			   column-widths row-heights) self
    (when page
      (with-field-values (grid) page
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
		  (update-height row (/height cell))
		  (update-width column (/width cell)))))))))))

(defparameter *even-columns-format* '(:background ".gray50" :foreground ".gray10"))
(defparameter *odd-columns-format* '(:background ".gray45" :foreground ".gray10"))

(define-method handle-key form (event)
  ;; possibly forward event to current cell. used for the event cell, see below.
  (prog1
      (if (or (and (equal "RETURN" (first event))
		   (equal :control (second event)))
	      (equal "ESCAPE" (first event)))
	  (send-parent self :initialize self)
	  (let* ((cell (/selected-cell self))
		 (widget (when cell (field-value :widget cell))))
	    (cond ((and cell (has-method :handle-key cell))
		   (or (/handle-key cell event)
		       (send-parent self :handle-key self event)))
		  ((and widget <entered>)
		   (prog1 nil (/handle-key widget event)))
		  (t (send-parent self :handle-key self event)))))
    (/compute-geometry self)))

(define-method hit form (x0 y0) 
  (with-field-values (row-heights column-widths origin-row origin-column rows columns x y width height)
      self
    (when (within-extents x0 y0 x y (+ x width) (+ y height))
      (let* ((x <x>)
	     (y <y>)
	     (selected-column 
	      (loop for column from origin-column to columns
		    do (incf x (aref column-widths column))
		    when (> x x0) return column))
	     (selected-row 
	      (loop for row from origin-row to rows
		    do (incf y (aref row-heights row))
		    when (> y y0) return row)))
	(when (and (integerp selected-column) (integerp selected-row))
	  (when (array-in-bounds-p (field-value :grid <page>)
				 selected-row selected-column)
	    (prog1 t
	      (setf <cursor-row> selected-row
		    <cursor-column> selected-column))))))))
  
(define-method compute form ()
  (with-fields (rows columns) self
    (let (data cell)
      (dotimes (row rows)
	(setf data nil)
	(dotimes (column columns)
	  (setf cell (/cell-at self row column))
	  (if (null cell)
	      (setf data nil)
	      (progn 
		(when data (/set cell data))
		(/compute cell)
		(setf data (/get cell)))))))))

;; TODO break up this method.

(define-method render form ()
  (/clear self)
  (when <page>
    (with-field-values (cursor-row cursor-column row-heights page page-name 
				   origin-row origin-column header-line status-line
				   mark-row mark-column width height
				   display-style header-style tool tool-methods entered focused
				   row-spacing rows columns draw-blanks column-widths) self
      (when <computing> (/compute self))
;;      (/compute-geometry self)
      (let* ((image <image>)
	     (widget-width <width>)
	     (widget-height <height>)
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
	(setf <origin-width> (- rightmost-visible-column origin-column))
	(setf <origin-height> (- bottom-visible-row origin-row))
	;; see if current cell has a tooltip
	;; (let ((selected-cell (/cell-at self cursor-row cursor-column)))
	;;   (when (object-p selected-cell)
	;;     (setf header-line (field-value :tooltip selected-cell))))
	;; draw header line with tooltip, if any
	(multiple-value-bind (top left bottom right) (/mark-region self)
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
		      (cell (/cell-at self row column)))
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
				  :stroke-color ".gray30"
				  :color (if (evenp column) ".gray50" ".gray45")
				  :destination image))
		      ;; see also cells.lisp
		      (progn 
			(ecase display-style
			  (:label (/render cell image x y column-width))
			  (:image (if (/in-category cell :drawn)
				     (push (list cell x y) pending-draws)
				     (when (field-value :image cell)
				       (draw-image (find-resource-object 
						    (field-value :image cell)) x y :destination image)))))
			(when entered
			  (draw-rectangle x y 
					  column-width 
					  row-height
					  :color ".red"
					  :destination image))))
		  ;; visually indicate edges of map with a yellow line
		  (let ((iwid 2))
		    (when (= rightmost-visible-column (- columns 1) column)
		      (draw-box (+ x column-width) y iwid row-height :stroke-color ".yellow" :color ".yellow"
				:destination image))
		    (when (= 0 column)
		      (draw-box 0 y iwid row-height :stroke-color ".yellow" :color ".yellow"
				:destination image))
		    (when (= bottom-visible-row row (- rows 1))
		      (draw-box x (+ y row-height) column-width iwid :stroke-color ".yellow" :color ".yellow"
				:destination image))
		    (when (= 0 row)
		      (draw-box x 0 column-width iwid :stroke-color ".yellow" :color ".yellow"
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
		(/draw cell x y image)))
	    ;; create status line
	    ;; TODO break this formatting out into variables
	    (setf status-line
		  (list 
		   (list (format nil " (/ ~A )     " page-name) :foreground (if focused ".yellow" ".white")
			 :background (if focused ".red" ".blue"))
		   (list (format nil "  ~A (~S, ~S) ~Sx~S "
				 tool cursor-row cursor-column rows columns)
			 :foreground ".white"
			 :background ".gray20")))
	    ;; draw status line
	    (when status-line 
	      (let* ((ht (formatted-line-height status-line))
		     (sy (- <height> 1 ht)))
		(draw-box 0 sy <width> ht :color ".gray20" 
			  :stroke-color ".gray20" :destination image)
		(render-formatted-line status-line 
				       0 sy 
				       :destination image)))
	    ;; render cursor and mark, if any 
	    (when cursor-dimensions
	      (destructuring-bind (x y w h) cursor-dimensions
	      (/draw-cursor self x y w h)))
	    (when mark-dimensions
	      (destructuring-bind (x y w h) mark-dimensions
		(/draw-mark self x y w h)))
	    (when (and (integerp mark-row) (integerp mark-column)
		       (notany #'null (list x0 y0 x1 y1)))
	      (/draw-region self x0 y0 (- x1 x0) (- y1 y0)))))))))
  
;;; Cursor
  
(define-method scroll form ()
  (with-fields (cursor-row cursor-column origin-row origin-column scroll-margin
			   origin-height origin-width page rows columns) self
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
      (draw-rectangle x y width height :color color :destination <image>))))

(define-method draw-mark form (x y width height)
  (draw-rectangle x y width height :color ".white" :destination <image>))

(define-method draw-region form (x y width height)
  (draw-rectangle x y width height :color ".cyan" :destination <image>))
  
(define-method move-cursor form (direction)
  "Move the cursor one step in DIRECTION. 
DIRECTION is one of :up :down :right :left."
  (unless <entered>
    (with-field-values (cursor-row cursor-column rows columns) self
      (let ((cursor (list cursor-row cursor-column)))
	(setf cursor (ecase direction
		       (:up (if (/= 0 cursor-row)
				(list (- cursor-row 1) cursor-column)
				cursor))
		       (:left (if (/= 0 cursor-column)
				  (list cursor-row (- cursor-column 1))
				  cursor))
		       (:down (if (< cursor-row (- rows 1))
				  (list (+ cursor-row 1) cursor-column)
				  cursor))
		       (:right (if (< cursor-column (- columns 1))
				   (list cursor-row (+ cursor-column 1))
				   cursor))))
	(destructuring-bind (r c) cursor
	  (setf <cursor-row> r <cursor-column> c))
	;; possibly scroll
	(/scroll self)))))
  
(define-method move-cursor-up form ()
  (/move-cursor self :up))

(define-method move-cursor-down form ()
  (/move-cursor self :down))

(define-method move-cursor-left form ()
  (/move-cursor self :left))

(define-method move-cursor-right form ()
  (/move-cursor self :right))

(define-method move-end-of-line form ()
  (unless <entered>
    (setf <cursor-column> (1- <columns>))
    (/scroll self)))

(define-method move-beginning-of-line form ()
  (unless <entered>
    (setf <cursor-column> 0)
    (/scroll self)))

(define-method move-end-of-column form ()
  (unless <entered>
    (setf <cursor-row> (1- <rows>))
    (/scroll self)))

(define-method move-beginning-of-column form ()
  (unless <entered>
    (setf <cursor-row> 0)
    (/scroll self)))

;;; Creating commonly used cells

(define-method drop-at-cursor form (type)
  (/drop-cell <page> (clone type) <cursor-row> <cursor-column>)) 

(define-method drop-data-cell form ()
  (/drop-at-cursor self =data-cell=))

(define-method drop-command-cell form ()
  (/drop-at-cursor self =command-cell=))

(define-method drop-button-cell form ()
  (/drop-at-cursor self =button-cell=))

;;; forms.lisp ends here
