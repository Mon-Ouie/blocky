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

;;; Grouping blocks into buffers with buffer-local variables

(defmacro with-buffer (buffer &rest body)
  `(let ((*buffer* (find-uuid ,buffer)))
     (assert (blockyp *buffer*))
     ,@body))

(define-block (buffer :super list)
  (needs-layout :initform t)
  (variables :initform (make-hash-table :test 'eq)))

(define-method invalidate-layout buffer ()
  (setf %needs-layout t))

(define-method delete-block buffer (block)
  (assert (blockyp block))
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

(define-method initialize buffer (&key blocks variables  
				       (width (dash 120))
				       (height (dash 70)))
  (apply #'super%initialize self blocks)
  (setf %width width
	%height height)
  (when variables (setf %variables variables)))

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

;;; buffers.lisp ends here
