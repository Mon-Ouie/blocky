;;; shell.lisp --- interactive multimedia forthlike repl/editor

;; Copyright (C) 2011, 2012, 2013  David O'Toole

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

(defparameter *logo-height* 26)
(defparameter *form-cursor-blink-time* 10)

(define-block shell
  ;; a list of lists of blocks
  (lines :initform nil)
  ;; "point" is the location of the selected block.
  (point-row :initform 0) 
  (point-column :initform 0)
  ;; what block type to use for new blanks
  (blank :initform 'string)
  ;; the cursor shows where point is
  (cursor-color :initform "yellow")
  (cursor-blink-color :initform "magenta")
  (cursor-blink-clock :initform 0)
  ;;
  (row-spacing :initform 1 :documentation "Number of pixels to add between rows.")
  (mark-row :initform nil)
  (mark-column :initform nil)
  (alignment :initform nil))

  ;; (rows :initform 10)
  ;; (columns :initform 10) 
  ;; (column-widths :documentation "A vector of integers where v(x) is the pixel width of shell column x.")
  ;; (row-heights :documentation "A vector of integers where v(x) is the pixel height of shell row x.")
  ;; (row-styles :documentation "A vector of property lists used to customize the appearance of rows.")
  ;; (column-styles :documentation "A vector of property lists used to customize the appearance of columns.")
  ;; (zebra-stripes :documentation "When non-nil, zebra stripes are drawn.")
  ;; (border-style :initform t :documentation "When non-nil, draw cell borders.")
  ;; (draw-blanks :initform t :documentation "When non-nil, draw blank cells.")
  ;; (header-style :initform t :documentation "When non-nil, draw row and column headers.")
  ;; (header-line :initform nil :documentation "Formatted line to be displayed at top of shell above spreadsheet.")
  ;; (status-line :initform nil :documentation "Formatted line to be displayed at bottom of shell below spreadsheet."))

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

(define-method initialize shell ()
  (initialize%super self)
  (setf %lines (list (new %blank)))
  ;; see command implementations below
  (install-text-keybindings self)
  (setf %point-row 0)
  (setf %point-column 0)
  (clear-mark self))

(define-method pick shell () self)

;;; Emulate the feel of emacs text properties buffers

;; Here we move over complete blocks, not characters.
;; Individual character movement is accomplished by the focused block methods.

(define-method end-of-line shell ()
  (setf %point-column (length (nth %point-row %lines))))

(define-method beginning-of-line shell ()
  (setf %point-column 0))

(define-method beginning-of-buffer shell ()
  (setf %point-row 0 %point-column 0))

(define-method end-of-buffer shell ()
  (setf %point-row (1- (length %lines)))
  (end-of-line self))

(define-method current-line shell ()
  (nth %point-row %lines))

(define-method thing-at-point shell ()
  (nth %point-column (current-line self)))

(define-method forward-char shell ()
  (with-fields (lines point-row point-column) self
    (setf point-column (min (1+ point-column)
			    (length (nth point-row lines))))))

(define-method backward-char shell ()
  (with-fields (lines point-row point-column) self
    (setf point-column (max 0 (1- point-column)))))

(define-method next-line shell ()
  (with-fields (lines point-row point-column) self
    (setf point-row (min (1+ point-row)
			 (1- (length lines))))
    (setf point-column (min point-column 
			    (length (nth point-row lines))))))

(define-method previous-line shell ()
  (with-fields (lines point-row point-column) self
    (setf point-row (max 0 (1- point-row)))
    (setf point-column (min point-column
			    (length (nth point-row lines))))))

(define-method newline shell ()
  (with-fields (lines blank point-row point-column) self
    (if (null lines)
	(push (list (new blank)) lines)
	(if (and (= point-row (length lines))
		 (= point-column (length (nth point-row lines))))
	    ;; at end of content
	    (progn (setf lines (append lines (list (new blank))))
		   (incf point-row)
		   (setf point-column 0))
	    ;; insert line break
	    (let* ((line (nth point-row lines))
		   (line-remainder (subseq line point-column))
		   (lines-remainder (nthcdr (1+ point-row) lines)))
	      ;; truncate current line
	      (setf (nth point-row lines) 
		    (subseq line 0 point-column))
	      ;; insert new line
	      (if (= 0 point-row)
		  (setf (cdr lines)
			(cons line-remainder (cdr lines)))
		  (setf (cdr (nthcdr (- point-row 1) lines))
			(cons (nth point-row lines)
			      (cons line-remainder lines-remainder))))
	      ;; move to new line
	      (incf point-row)			
	      (setf point-column 0))))))

(define-method backward-delete-char shell ()
  (with-fields (lines point-row point-column) self
    (if (and (= 0 point-column) 
	     (not (= 0 point-row)))
	(progn 
	  ;;
	  ;; we need to remove a line break.
	  (let ((line (nth (- point-row 1) lines))
		(next-line (nth (+ point-row 1) lines))
		(len (length lines)))
	    (setf lines (append (subseq lines 0 (- point-row 1))
				 (list (concatenate 'string line (nth point-row lines)))
				 (subseq lines (min len (+ point-row 1)))))
	    ;; (setf (cdr (nthcdr (- point-row 1) lines))
	    ;; 	  (nth (+ point-row 1) lines))
	    ;;
	    ;; move cursor too
	    (decf point-row)
	    (setf point-column (length line))))
	;; otherwise, delete within current line.
	(when (not (= 0 point-column))
	  (let* ((line (nth point-row lines))
		 (remainder (subseq line point-column)))
	    (setf (nth point-row lines)
		  (concatenate 'string 
			       (subseq line 0 (- point-column 1))
			       remainder))
	    (decf point-column))))))
    
(define-method end-of-line-p shell ()
  (= %point-column
     (1- (length (current-line self)))))

(define-method beginning-of-line-p shell ()
  (= %point-column 0))

(define-method top-of-lines-p shell ()
  (= %point-row 0)) 

(define-method bottom-of-lines-p shell ()
  (= %point-row
     (1- (length %lines))))

(define-method beginning-of-lines-p shell ()
  (and (beginning-of-line-p self)
       (top-of-lines-p self)))

(define-method end-of-lines-p shell ()
  (and (end-of-line-p self)
       (bottom-of-lines-p self)))

(define-method delete-char shell ()
  (with-fields (lines point-row point-column) self
    (if (end-of-line-p self)
	;; just remove line break
	(unless (bottom-of-lines-p self)
	  (next-line self)
	  (beginning-of-line self)
	  (backward-delete-char self))
	;; remove a character
	(progn 
	  (forward-char self)
	  (backward-delete-char self)))))

(define-method handle-event shell (event)
  (let ((thing (thing-at-point self)))
    (or (and thing (handle-event thing event))
	(next-method self event))))

;;; Drawing the shell

(define-method layout shell ()
  (with-fields (width height x y lines spacing) self
    (setf width 0 height 0)
    (let ((y0 y)
	  (x0 x)
	  (h0 0))
      (dolist (line lines)
	(dolist (thing line)
	  (move-to thing x0 y0)
	  (layout thing)
	  (incf x0 (+ (%width thing) spacing))
	  (setf h0 (max h0 (%height thing))))
	(setf width (max width (- x0 x)))
	(incf height h0)
	(incf y0 h0)
	(setf x0 x)
	(setf h0 0)))))

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

(define-method draw shell ()
  (dolist (line %lines)
    (mapc #'draw line)))
  
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

;;; Custom data entry for Listener. See also basic.lisp 

(define-block (listener-prompt :super prompt)
  (operation :initform :prompt)
  (background :initform nil)
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

;;; The Listener is a pop-up Lisp command prompt.

(define-block (listener :super list)
  (temporary :initform t)
  (display-lines :initform 12))

(defparameter *minimum-listener-width* 200)

(define-method initialize listener ()
  (with-fields (image inputs) self
    (let ((prompt (new 'listener-prompt self))
	  (modeline (new 'modeline)))
      (list%initialize self)
      (set-output prompt prompt)
      (setf inputs (list modeline prompt))
      (set-parent prompt self)
      (set-parent modeline self)
      (pin prompt)
      (pin modeline))))

(define-method layout listener ()
  (with-fields (height width parent inputs) self
    ;; start by calculating current height
    (setf height (font-height *font*))
    (setf width 0)
    ;; update all child dimensions
    (dolist (element inputs)
      (layout element)
      (incf height (field-value :height element))
      (callf max width (dash 2 (field-value :width element))))
    ;; now compute proper positions and re-layout
    (let* ((x (%window-x (current-buffer)))
	   (y0 (+ (%window-y (current-buffer))
		 *gl-screen-height*))
	   (y (- y0 height (dash 3))))
      (dolist (element inputs)
	(decf y0 (field-value :height element))
	(move-to element x y0)
	(layout element))
      (setf %y y)
      (setf %x x)
      ;;  a little extra room at the top and sides
      (incf height (dash 3)))))
      ;; ;; move to the right spot to keep the bottom on the bottom.
      ;; (setf y (- y0 (dash 1))))))

(define-method get-prompt listener ()
  (second %inputs))
 
(define-method evaluate listener ()
  (evaluate (get-prompt self)))

(define-method focus listener ()
  (grab-focus (get-prompt self)))

(define-method debug-on-error listener ()
  (debug-on-error (get-prompt self)))

(define-method print-on-error listener ()
  (print-on-error (get-prompt self)))

(define-method accept listener (input &optional prepend)
  (declare (ignore prepend))
  (with-fields (inputs scrollback-length) self
    (assert (not (null inputs))) ;; we always have a prompt
    (prog1 t
      (assert (valid-connection-p self input))
      (let ((len (length inputs)))
	;; (when (> len scrollback-length)
	;;   ;; drop last item in scrollback
	;;   (setf inputs (subseq inputs 0 (1- len))))
	;; set parent if necessary 
	(adopt self input)
	(setf inputs 
	      (nconc (list (first inputs) 
			   (second inputs)
			   input)
		     (nthcdr 2 inputs)))))))

(define-method draw listener ()
  (with-fields (inputs x y height width) self
    (draw-box x y *gl-screen-width* height :color "black" :alpha 0.3)
;    (draw-patch self x y (+ x width) (+ y height))
    (if (null inputs)
	(draw-label-string self *null-display-string*)
	(mapc #'draw inputs))
    (draw-image "blocky" 
		x
		(- (+ y height)
		   -2
		   (+ *logo-height*)
		   (%height (first inputs)))
		:height *logo-height* :width *logo-height*)))

;;; shell.lisp ends here
