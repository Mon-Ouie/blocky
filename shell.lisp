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

;;; Command prompt block

(defparameter *logo-height* 26)

(defparameter *socket-size* 16)
(defparameter *active-prompt-color* "red")
(defparameter *inactive-prompt-color* "gray10")
(defparameter *prompt-cursor-inactive-color* "gray50")

(defparameter *default-prompt-text-color* "white")
(defparameter *default-prompt-outside-text-color* "gray20")
(defparameter *default-prompt-label-color* "gray20")

(defparameter *default-entry-text-color* "white")
(defparameter *default-entry-label-color* "white")
(defparameter *default-prompt-string* "     ")

(defparameter *default-prompt-margin* 4)

(defparameter *default-prompt-history-size* 100)
(defparameter *default-cursor-width* 1)
 
(define-block prompt
  (text-color :initform "gray20")
  (visible :documentation "When non-nil, the prompt is drawn." :initform t)
  (receiver :documentation "The object to send command messages to.")
  (point :initform 0 :documentation "Integer index of cursor within prompt line.")
  (line :initform "" :documentation "Currently edited command line.")
  (background :initform t)
  (error-output :initform "")
  (minimum-width :initform 100)
  (text-color :initform *default-prompt-text-color*)
  (label-color :initform *default-prompt-label-color*)
  options label 
  (pinned :initform nil)
  (prompt-string :initform *default-prompt-string*)
  (category :initform :data)
  (history :documentation "A queue of strings containing the command history.")
  (history-position :initform 0))

(define-method accept prompt (&rest args)
  nil)

(define-method exit prompt ()
  (clear-line self))

(define-method goto prompt ()
  (say self "Enter command below at the >> prompt. Press ENTER when finished, or CONTROL-X to cancel."))

(define-method say prompt (&rest args)
  (apply #'message args))

(define-method initialize prompt ()
  (block%initialize self)
  (when (not (has-local-value :history self))
    (setf %history (make-queue :max *default-prompt-history-size* :count 0)))
  (install-text-keybindings self))

(define-method handle-event prompt (event)
  (unless %read-only
    (handle-text-event self event)))

(define-method forward-char prompt ()
  (setf %point (min (1+ %point)
		     (length %line))))

(define-method backward-char prompt ()
  (setf %point (max 0 (1- %point))))

(define-method insert prompt (string)
  (setf %line (concatenate 'string
			    (subseq %line 0 %point)
			    string
			    (subseq %line %point)))
  (incf %point (length string)))
  ;; ;; if the insertion ends with a period, also execute the command
  ;; ;; line.
  ;; (when (string= "." (subseq string (1- (length string))))
  ;;   (setf %line (subseq string 0 (1- (length string))))
  ;;   (execute self)))

(define-method backward-delete-char prompt ()
  (when (< 0 %point) 
    (setf %line (concatenate 'string
			      (subseq %line 0 (1- %point))
			      (subseq %line %point)))
    (decf %point)))

(define-method delete-char prompt ()
  (with-fields (point line) self
    (when (<= 0 point (1- (length line)))
      (setf line (concatenate 'string
			      (subseq line 0 point)
			      (subseq line (1+ point)))))))

(define-method print-data prompt (data &optional comment)
  (dolist (line (split-string-on-lines (write-to-string data :circle t :pretty t :escape nil :lines 5)))
    (say self (if comment ";; ~A"
		  " ~A") line)))

(define-method do-sexp prompt (sexp)
  (with-fields (receiver) self
    (destructuring-bind (operation &rest arguments) sexp
      (apply #'send (make-keyword operation) 
	     receiver (mapcar #'eval arguments)))))

(define-method read-expression prompt (input-string)
  (let* ((*package* (or (find-package (make-block-package))
			(find-package :blocky)))
	 (*make-prototype-id-package* *package*))
    (handler-case 
	(read-from-string (concatenate 'string "(" input-string ")"))
      (condition (c)
	(format *error-output* "~S" c)))))

(define-method print-expression prompt (sexp)
  (format nil "~S" sexp))

(define-method enter prompt (&optional no-clear)
  (labels ((print-it (c) 
	     (message "~A" c)))
    (let* ((*read-eval* nil)
	   (line %line)
	   (sexp (read-expression self line)))
      (unless no-clear (clear-line self))
      (setf %error-output
	    (with-output-to-string (*standard-output*)
	      (when sexp 
		(if *debug-on-error*
		    (do-sexp self sexp)
		    (handler-case
			(handler-bind (((not serious-condition)
					 (lambda (c) 
					   (print-it c)
					   ;; If there's a muffle-warning
					   ;; restart associated, use it to
					   ;; avoid double-printing.
					   (let ((r (find-restart 'muffle-warning c)))
					     (when r (invoke-restart r))))))
			  (do-sexp self sexp))
		      (serious-condition (c)
			(print-it c))))
		(queue line %history))))
      (do-after-evaluate self))))

(define-method newline prompt ()
  (enter self))

(define-method do-after-evaluate prompt ()
  nil)

(define-method history-item prompt (n)
  (assert (integerp n))
  (assert (not (minusp n)))
  (nth (- (queue-count %history) n)
       (queue-head %history)))

(define-method forward-history prompt ()
  (when (> %history-position 0)
    (setf %line (history-item self (progn (decf %history-position)
					   %history-position)))
    (when (null %line) (setf %line ""))
    (setf %point (length %line))))
 
(define-method backward-history prompt ()
  (when %history 
    (when (numberp %history-position)
      (when (< %history-position (queue-count %history))
	(setf %line (history-item self (progn (incf %history-position)
					      %history-position)))
	(setf %point (length %line))))))

(define-method previous-line prompt ()
  (backward-history self))

(define-method next-line prompt ()
  (forward-history self))

(define-method set-receiver prompt (receiver)
  (setf %receiver receiver))

(define-method clear-line prompt ()
  (setf %line "")
  (setf %point 0)
  (setf %history-position 0))

(define-method end-of-line prompt ()
  (setf %point (length %line)))

(define-method beginning-of-line prompt ()
  (setf %point 0))

(define-method draw-cursor prompt 
    (&key (x-offset 0) (y-offset 0)
	  color blink)
  (with-fields (x y width height clock point parent background
		  prompt-string line) self
    (draw-cursor-glyph self
     ;;
     (+ x (or x-offset 0)
	(font-text-width (if (<= point (length line))
			     (subseq line 0 point)
			     " ")
			 *font*)
	(if x-offset 0 (font-text-width prompt-string *font*)))
     ;;
     (+ y (or y-offset 0) *default-prompt-margin*)
     *default-cursor-width*
     ;; (font-text-width 
     ;;  (string (if (< point (length line))
     ;; 		   (aref line 
     ;; 			 (max (max 0 
     ;; 				   (1- (length line)))
     ;; 			      point))
     ;; 		   #\Space))
     (* (font-height *font*) 0.8)
     :color color
     :blink blink)))

(define-method label-width prompt () 
  (font-text-width %prompt-string *font*))

(define-method label-string prompt () %prompt-string)

(define-method draw-border prompt ())

(define-method draw-hover prompt ())

(define-method tap prompt (mouse-x mouse-y)
  ;(declare (ignore mouse-y))
  (if %read-only
      (tap%super self mouse-x mouse-y)
      (with-fields (x y width height clock point parent background
		      line) self
	;; find the left edge of the data area
	(let* ((left (+ x (label-width self) (dash 4)))
	       (tx (- mouse-x left)))
	  ;; which character was clicked?
	  (let ((click-index 
		  (block measuring
		    (dotimes (ix (length line))
		      (when (< tx (font-text-width 
				   (subseq line 0 ix)
				   *font*))
			(return-from measuring ix))))))
	    (if (numberp click-index)
		(setf point click-index)
		(setf point (length line))))))))

(define-method layout prompt ())

(define-method update-layout-maybe prompt ()
  (with-fields (line) self
    (resize self 
	    (+ 12 (* 5 *dash*)
	       (font-text-width line *font*)
	       (font-text-width *default-prompt-string* *font*))
	    (+ (* 2 *default-prompt-margin*) (font-height *font*)))))

(define-method draw-input-area prompt (state)
  ;; draw shaded area for data entry.
  ;; makes the cursor show up a bit better too.
  (with-fields (x y parent label line) self
    (assert (not (null line)))
    (let ((label-width (label-width self))
	  (line-width (font-text-width line *font*)))
      (draw-box (dash 0.5 x label-width)
		(dash 1 y)
		(dash 2 line-width)
		(+ 1 (font-height *font*))
		:color (ecase state
			 (:active *active-prompt-color*)
			 (:inactive 
			  (find-color 
			   (or 
			    (unless (is-a 'buffer %parent)
			      %parent)
			    self) :shadow)))))))

(define-method draw-indicators prompt (state)
  (with-fields (x y options text-color width parent height line) self
    (let ((label-width (label-width self))
	  (line-width (font-text-width line *font*))
	  (fh (font-height *font*)))
      ;; (draw-indicator :top-left-triangle
      ;; 		      (dash 1 x 1 label-width)
      ;; 		      (dash 1 y)
      ;; 		      :state state)
      (draw-indicator :bottom-right-triangle
		      (dash 1 x -2 label-width line-width)
		      (+ y -2 fh)
		      :state state))))

(define-method draw-focus prompt () 
  (unless %read-only
    (with-fields (cursor-clock x y width line parent) self
      (let* ((label (label-string self))
	     (label-width (label-width self))
	     (line-width (font-text-width line *font*)))
	;; draw shaded area for input
	(draw-input-area self :active)
	;; draw cursor.
	(update-cursor-clock self)
	(draw-cursor self 
		     :x-offset
		     (dash 3 (font-text-width label *font*))
		     :blink t)
	;; draw highlighted indicators
	(draw-indicators self :active)
	;; redraw content (but not label)
	(draw self :nolabel)))))

(define-method draw prompt (&optional nolabel)
  (with-fields (x y width height point parent background
		  line prompt-string) self
    (when (null line) (setf line ""))
    (let ((strings-y *default-prompt-margin*))
      (unless nolabel
	;; draw prompt string
	(assert (stringp %text-color))
	(draw-string prompt-string
		     (+ x *default-prompt-margin*)
		     (+ y strings-y)
		     :color (if (treep parent)
				%text-color
				*default-prompt-outside-text-color*)
		     :font *font*)
 	(update-layout-maybe self)
	;; draw background for input
	(unless %read-only
	  (draw-input-area self :inactive)
	  (draw-indicators self :inactive)))
      ;; draw current command line text
      (when (null line) (setf line ""))
      (unless (zerop (length line))
	(draw-string line
		     (dash 1 x (label-width self))
		     (+ y strings-y)
		     :color %text-color
		     :font *font*)))))

;;; General-purpose data entry block based on the prompt block.

(define-block (entry :super prompt)
  (category :initform :data)
  (methods :initform '(:toggle-read-only))
  (locked :initform nil)
  (pinned :initform nil)
  (minimum-width :initform 10)
  (text-color :initform *default-entry-text-color*)
  (label-color :initform *default-entry-label-color*)
  type-specifier value)

(define-method initialize entry 
    (&key value type-specifier options label label-color parent locked
    read-only)
  (initialize%super self)
  ;(assert (and value type-specifier))
  (when parent (setf %parent parent))
  (setf %type-specifier type-specifier
	%options options
	%locked locked
	%read-only read-only
	%value value)
  ;; fill in the input box with the value
  (setf %line (if (null value)
		  ""
		  (if (stringp value)
		      ;; no extraneous quotes unless it's a general sexp entry
		      value
		      (format nil "~S" value))))
  (setf %label 
	(or label 
	    (getf options :label)))
  (when label-color (setf %label-color label-color)))

(define-method evaluate entry ()
  %value)

(define-method set-value entry (value)
  (message "VALUE: ~S" value)
  (setf %value value)
  (setf %line (prin1-to-string value)))

(define-method get-value entry ()
  %value)

(define-method recompile entry ()
  %value)

(define-method label-string entry ()
  (or %label 
      (getf %options :label)
      ""))

(define-method can-pick entry () 
  (not %pinned))
  
(define-method pick entry ()
  (if %pinned %parent self))

;; (define-method can-pick entry () 
;;   (if (is-a 'arguments %parent)
;;       t
;;       (can-pick%super self)))

;; (define-method can-pick entry () 
;; (define-method pick entry ()
;;   (if (is-a 'arguments %parent)
;;       %parent
;;       (pick%super self)))
      
(define-method toggle-read-only entry ()
  (unless %locked
    (setf %read-only (if %read-only nil t))))

(define-method label-width entry ()
  (dash 1 (font-text-width (label-string self) *font*)))

(define-method draw-label entry ()
  (when %label
    (draw-string %label
		 (+ (dash 1 %x))
		    *text-baseline*
		 :color (find-color self :foreground)
		 :font *font*)))
		 
  ;; (draw-string (label-string self)
  ;; 	       (dash 1 %x)
  ;; 	       (+ %y (dash 1))
  ;; 	       :color %label-color
  ;; 	       :font *font*))

(define-method draw entry (&optional nolabel)
  (with-fields (x y options read-only 
		  text-color width 
		  parent height line) self
    (let ((label-width (label-width self))
	  (line-width (font-text-width line *font*)))
      ;; draw the label string 
      (let ((*text-baseline* (+ y (dash 1))))
	(unless nolabel 
	  (when (plusp (length %label))
	    (draw-label self))
	  ;; draw shaded area for input
	  (unless read-only
	    (draw-input-area self :inactive)
	    ;; draw indicators
	    (draw-indicators self :inactive)))
	;; draw current input string
	(when (null line) (setf line ""))
	(unless (zerop (length line))
	  (draw-string line
		       (+ (dash 1 x) label-width)
		       *text-baseline*
		       :color (find-color self :foreground)
		       :font *font*))))))
		 
(define-method draw-focus entry ()
  (unless %read-only 
    (with-fields (x y line) self
      (draw-input-area self :active)
      (let ((*text-baseline* (+ y (dash 1))))
	(unless (zerop (length line))
	  (draw-string line
		       (+ (dash 1 x) (label-width self))
		       *text-baseline*
		       :color *default-prompt-text-color*
		       :font *font*))
	(draw-indicators self :active)
	(update-cursor-clock self)
	(draw-cursor self 
		     :x-offset
		     (dash 2 (font-text-width (label-string self) *font*))
		     :blink t)))))
  
(define-method do-sexp entry (sexp)
  (with-fields (value type-specifier parent) self
    (assert (and (listp sexp) (= 1 (length sexp))))
    (let ((datum (first sexp)))
      (if (or (null type-specifier)
	      (type-check self datum))
	  (setf value datum)
	  (message "Warning: value entered does not match type ~S. Not storing value."
		   type-specifier))
      (when parent (child-updated parent self)))))
 
(define-method enter entry ()
  (unless %read-only
    (enter%super self :no-clear)))

(define-method layout entry ()
  (with-fields (height width value line) self
    (setf height (+ (* 1 *dash*) (font-height *font*)))
    (setf width (+ (* 2 *dash*)
		   (label-width self)
		   (max %minimum-width
			(font-text-width line *font*))))))

(define-method lose-focus entry ()
  ;; update the entry value if the user mouses away
  (enter self))

;;; Dropping expressions onto argument inputs

(define-method accept entry (thing)
  (with-fields (parent) self
    (when (is-a 'arguments parent)
      (prog1 t
	(let ((index (position-within-parent self)))
	  (send :replace-widget parent index
		(send :schema-widget parent 
		      (nth index (%schema parent))
		      :input thing
		      :force-socket t)))))))
		      
;;; Allow dragging the parent block more easily

(define-method hit entry (x y)
  (when (hit%super self x y)
    ;; always allow clicking data area
    (if (< x (+ %x (label-width self)))
	%parent
	self)))

(define-method type-check entry (datum)
  (typep datum %type-specifier))
  ;; (with-fields (type-specifier) self
  ;;   (etypecase type-specifier
  ;;     (symbol (funcall type-specifier datum))
  ;;     (list (typep datum type-specifier)))))

(define-method do-after-evaluate entry ()
  ;; print any error output
  (when (and (stringp %error-output)
	     (plusp (length %error-output)))
    (add-block (current-buffer) (new 'text %error-output) *pointer-x* *pointer-y*)))

;;; Easily defining new entry blocks

(defmacro defentry (name type value &rest specs)
  `(define-block (,name :super entry)
     (type-specifier :initform ',type)
     (value :initform ',value)
     ,@specs))

(defentry integer integerp 0)
(defentry number numberp 0)
(defentry non-negative-number (number 0 *) 0)
(defentry float floatp 0.0)
(defentry symbol symbolp nil 
  (category :initform :data))
(defentry positive-integer (integer 1 *) 1)
(defentry non-negative-integer (integer 0 *) 0)

(defentry expression t nil 
  (category :initform :expression))

(define-method evaluate expression ()
  (eval (get-value self)))

;;; Plain text entry, as a string

(defentry string stringp "")

(define-method read-expression string (input-string)
  ;; pass-through; don't read string at all.
  input-string)

(define-method do-sexp string (sexp)
  (assert (stringp sexp))
  (setf %value sexp)
  (when %parent (child-updated %parent self)))
 
(define-method set-value string (value)
  (when (stringp value)
    (setf %value value)
    (setf %line value)))

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
  (prompt%initialize self)
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
    ;; (dolist (line (split-string-on-lines %error-output))
    ;;   (accept %parent (new 'string :value line)))))

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
