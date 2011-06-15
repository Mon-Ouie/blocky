;;; widgets.lisp --- interactive graphical element blocks

;; Copyright (C) 2008, 2009, 2010, 2011  David O'Toole

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
;; along with this program.  If not, see ^http://www.gnu.org/licenses/.

;;; Code:

(in-package :ioforms)

;;; Formatted display block

(defvar *default-formatter-scrollback-size* 1000)

(defun formatted-string-height (S)
  (destructuring-bind (string &key image (font *block-font*) &allow-other-keys) S
    (declare (ignore string))
    (if image
	(image-height image)
	(font-height font))))

(defun formatted-string-width (S)
  (destructuring-bind (string &key image width (font *block-font*) &allow-other-keys) S
    (or width
	(if image 
	    (image-width image)
	    (* (font-text-extents string font))))))

(defun formatted-line-height (line)
  (apply #'max (mapcar #'formatted-string-height line)))

(defun formatted-line-width (line)
  (apply #'+ (mapcar #'formatted-string-width line)))

(defun render-formatted-string (formatted-string x y 
				&key (text-offset 0))
  "Render the FORMATTED-STRING to position X,Y.  
If an integer TEXT-OFFSET is provided, add that many pixels to the Y
coordinate for rendered text in the line. (This is used to make text
align with inline images that are larger than the text height---see
also `render-formatted-line')."
  (destructuring-bind (string &key (foreground "white") 
		       width
		       (font *block-font*)
		       background image)
      formatted-string
    ;; if :width is specified, draw a background square of that width
    (when (integerp width)
      (draw-box x y width (formatted-string-height formatted-string)
		:color background))
    ;; now draw foreground image or text
    (if image
	(draw-image (typecase image
		      (string (find-resource-object image))
		      (otherwise image))
		    x y)
	(if (null string)
	    (message "Warning: no string to render.")
	    (draw-string string x (+ text-offset y)
			 :font font :color foreground)))))
	    ;; (if background
	    ;; 	(draw-string-shaded string x (+ text-offset y)
	    ;; 				foreground background
	    ;; 				:font font)
	    ;; 	    (draw-string-solid string x (+ text-offset y) :font font
	    ;; 			       :color foreground)))))))

(defun render-formatted-line (line x y &key (font *block-font*))
  "Render the formatted LINE at position X,Y.
Return the height of the rendered line."
  (let* ((line-height (formatted-line-height line))
	 (default-font-height (font-height font))
	 (text-offset (if (> line-height default-font-height)
			  (truncate (/ (- line-height 
					  default-font-height)
				       2))
			  0))
	 (current-x x))
    (dolist (string line)
      (when string
	(render-formatted-string string current-x y :text-offset text-offset )
	(incf current-x (formatted-string-width string)))
      line-height)))
  
;; Next comes the formatted-text output block, which uses the utility
;; functions just defined.

(define-prototype formatter 
    (:parent block :documentation 
"=FORMATTER= is a simple output formatting block for the
presentation of messages and other in-game data. Foreground and
background colors are supported, as well as displaying images
in-line with text of different fonts.

A formatted line is a list of formatted strings. A formatted 
string is a cons of (STRING . PROPERTIES), where the keys in
PROPERTIES are chosen from:

  - :FOREGROUND --- Foreground color. A color resource name.
  - :BACKGROUND --- Background color. A color resource name.
  - :IMAGE --- Image to be displayed instead of STRING.
      If this is a string, the corresponding resource image is 
      found and displayed. If this is an image object, the image 
      itself is displayed.
  - :WIDTH --- Occupy this pixel width if set to an integer.
  - :FONT ---  Font name. Defaults to *block-font*.
")
  (lines :documentation "Vector of lines.")
  (display-current-line :initform nil)
  (current-line :documentation "Formatted line currently being composed."))

(define-method printf formatter (string &rest keys &key image foreground background font)
  "Add a formatted STRING to the end of the current line.
Example: (printf my-formatter \"hello\" :foreground \"red\")"
  (vector-push-extend (cons string keys) ^current-line))

(define-method print-formatted-string formatter (formatted-string)
  (vector-push-extend formatted-string ^current-line))	       

(define-method print-image formatter (image)
  (printf self nil :image image))

(define-method println formatter (&rest args)
  "Print the ARGS as a formatted string, following up with a newline."
  (apply #'ioforms:send self :print self args)
  (newline self))

(define-method insert-space formatter ()
  (printf self " "))

(define-method clear-line formatter ()
  (setf ^current-line (make-array 10 :adjustable t :fill-pointer 0)))

(define-method newline formatter ()
  "Add the current line to the display, and start a fresh offscreen
line."
  (when (and (vectorp ^current-line)
	     (> (fill-pointer ^current-line) 0))
    (vector-push-extend (coerce ^current-line 'list) ^lines))
  (setf ^current-line (make-array 10 :adjustable t :fill-pointer 0)))

(define-method reset-lines formatter ()
  (setf ^lines (make-array 10 :adjustable t :fill-pointer 0)))

(define-method delete-line formatter (&optional (num-lines 1))
  (when (>= (length ^lines) num-lines)
    (dotimes (n num-lines)
      (vector-pop ^lines))))

(define-method delete-all-lines formatter ()
  (delete-line self (fill-pointer ^lines)))

(define-method initialize formatter ()
  (parent/initialize self)
  (reset-lines self)
  (newline self))

(define-method update formatter ()
  "Invoked before each render. Replace this method for custom
auto-updated displays."  
  nil)

(define-method print-object-tag formatter (ob)
  (print-image self (or (field-value :tile ob) (field-value :image ob)))
  (insert-space self)
  (printf self (get-some-object-name ob))
  (insert-space self))

(define-method print-separator formatter ()
  (printf self "  :  " :foreground "gray20"))

(define-method draw formatter ()
  (let* ((current-line (coerce ^current-line 'list))
	 (y-offset (if current-line
		       (formatted-line-height current-line)
		       0)))
    (let ((y (- ^height (if ^display-current-line
			    y-offset 0)))
	  (n 0)
	  line
	  (lines ^lines))
      (when (and current-line ^display-current-line)
	(render-formatted-line current-line 0 (- ^height y-offset)))
      (setf n (fill-pointer lines))
      (when (plusp n)
	(loop do
	  (progn 
	    (setf line (aref lines (- n 1)))
	    (decf y (formatted-line-height line))
	    (render-formatted-line line 0 y)
	    (decf n))
	  ;; reached top of output image?
	      while (and (plusp y) 
			 ;; ran out of lines to display?
			 (not (zerop n))))))))

(defun split-string-on-lines (string)
  (with-input-from-string (stream string)
    (loop for line = (read-line stream nil)
	  while line collect line)))
	   
;;; Command prompt block

(defparameter *prompt-blink-time* 24)
(defparameter *prompt-cursor-color* "magenta")
(defparameter *prompt-cursor-blink-color* "red")

(defparameter *direct-prompt-string* "> ")
(defparameter *forward-prompt-string* "Press CONTROL-X to enter the prompt.")

(defparameter *default-prompt-margin* 4)

(defparameter *default-prompt-history-size* 100)
(defparameter *default-cursor-width* 8)

(defvar *lowercase-alpha-characters* "abcdefghijklmnopqrstuvwxyz")
(defvar *uppercase-alpha-characters* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defvar *numeric-characters* "0123456789")

(define-prototype prompt
    (:parent "IOFORMS:BLOCK" :documentation 
"The command prompt block is a text input area with Emacs-like
keybindings. It is used to send messages to objects. (For ease of
use, prompt commands may also be bound to single keystrokes.)

 The command syntax is:

:  command-name arg1 arg2 ...

All tokens must be Lisp-readable symbols, strings, or numbers.

The command prompt will change its commands into message sends, and
send them to a designated command receiver:

:  yes             -->   (yes ^receiver)
:  move :north     -->   (move ^receiver :north)
:  attack :west :with :left-hand  --> (attack ^receiver :west 
:                                             :with :left-hand)

So the commands are just the receiver's methods. The command
line's HELP system is just a method documentation browser
 (i.e. SLOT-DESCRIPTORS.) 

The prompt can bind single keystrokes (i.e. one or more modifiers
and a keypress code) to the insertion of an arbitrary string at
point in the prompt. A string that ends in a period is a
\"terminating\" keybinding; a terminating keybinding also completes
the command input, causing the resulting command to be executed.

Examples: 

:   ^up      -->    move :north .
:  shift-^up -->    push :north .
:    C-q      -->    quaff         ;; also shows potion list as output
:    M-1      -->    choose 1 .    ;; choose option 1 from output

The prompt has two input modes; direct mode and forward mode. In
direct mode, the prompt block's own keymap is used. In forward
mode, all keypresses (except for the mode escape key) are rejected
by returning `nil' from `handle-key'.

In the typical setup, the first block to receive the keypress
would be the default command prompt; a customized prompt, with
game-specific keybindings, would come second. During play, the
command prompt would reject all keypresses, which would pass on to
the next block in the frame (the customized prompt.) To 'escape'
this and enter commands, hit ESCAPE (and again to return to forward
mode.)

The modes can be toggled with CONTROL-X.
")
  (mode :documentation "Either :direct or :forward." :initform :direct)
  (clock :initform *prompt-blink-time*)
  (visible :documentation "When non-nil, the prompt is drawn." :initform t)
  (receiver :documentation "The object to send command messages to when in :forward mode.")
  (point :initform 0 :documentation "Integer index of cursor within prompt line.")
  (line :initform "" :documentation "Currently edited command line.")
  (category :initform :data)
  (history :initform (make-queue :max *default-prompt-history-size*)
	   :documentation "A queue of strings containing the command history.")
  (history-position :initform 0)
  (debug-on-error :initform nil))

;; (define-method handle-event prompt (event)
;;   "Reject all keypresses when in :forward mode; otherwise handle them
;; normally."
;;   (ecase ^mode
;;     ;; returning t stops the frame from trying other blocks
;;     (:direct (prog1 t (let ((func (gethash event ^keymap)))
;; 			(when func
;; 			  (funcall func)))))
;;     (:forward (when (equal (normalize-event '("X" :control))
;; 			   event)
;; 		(prog1 t (goto self))))))

(define-method accept prompt (&rest args)
  nil)

(define-method exit prompt ()
  (clear-line self)
  (setf ^mode :forward))

(define-method goto prompt ()
  (say self "Enter command below at the >> prompt. Press ENTER when finished, or CONTROL-X to cancel.")
  (setf ^mode :direct))

(define-method set-mode prompt (mode)
  (setf ^mode mode))

(defun bind-event-to-prompt-insertion (self key mods text)
  (bind-event-to-function self key mods 
			  #'(lambda ()
			      (insert self text))))

(define-method install-keybindings prompt ()
  ;; install varying keybindings
  (with-fields (keybindings) self
    (setf keybindings (make-hash-table :test 'equal))
    (dolist (binding (ecase *user-keyboard-layout*
    		       (:qwerty *prompt-qwerty-keybindings*)
    		       (:sweden *prompt-sweden-keybindings*)))
      (destructuring-bind (key mods result) binding
	(etypecase result
	  (keyword (bind-event-to-method self key mods result))
	  (string (bind-event-to-prompt-insertion self key mods result)))))
    ;; install keybindings for self-inserting characters
    (map nil #'(lambda (char)
  		 (bind-event-to-prompt-insertion self (string char) nil
  					       (string-downcase char)))
  	 *lowercase-alpha-characters*)
    (map nil #'(lambda (char)
  		 (bind-event-to-prompt-insertion 
		  self (string char) '(:shift) (string char)))
  	 *uppercase-alpha-characters*)
    (map nil #'(lambda (char)
  		 (bind-event-to-prompt-insertion self (string char) 
						 nil (string char)))
  	 *numeric-characters*)))

(defparameter *prompt-qwerty-keybindings*
  '(("A" (:control) :move-beginning-of-line)
    ("E" (:control) :move-end-of-line)
    ("F" (:control) :forward-char)
    ("B" (:control) :backward-char)
    ("HOME" nil :move-beginning-of-line)
    ("END" nil :move-end-of-line)
    ("RIGHT" nil :forward-char)
    ("LEFT" nil :backward-char)
    ("K" (:control) :clear-line)
    ("BACKSPACE" nil :backward-delete-char)
    ("RETURN" nil :enter)
    ("X" (:control) :exit)
    ("G" (:control) :exit)
    ("ESCAPE" nil :exit)
    ("P" (:alt) :backward-history)
    ("N" (:alt) :forward-history)  
    ("UP" nil :backward-history)
    ("DOWN" nil :forward-history)  
    ("MINUS" nil "-")
    ("HASH" nil "#")
    ("SLASH" nil "/")
    ("BACKSLASH" nil "\\")
    ("BACKQUOTE" nil "`")
    ("EXCLAIM" nil "!")
    ("PERIOD" nil ".")
    ("PERIOD" (:shift) ">")
    ("COMMA" nil ",")
    ("COMMA" (:shift) "<")
    ("EQUALS" nil "=")
    ("EQUALS" (:shift) "+")
    ("SEMICOLON" nil ";")
    ("SEMICOLON" (:shift) ":")
    ("0" (:shift) ")") 
    ("9" (:shift) "(")
    ("8" (:shift) "*")
    ("SPACE" nil " ")
    ("SLASH" (:shift) "?")
    ("QUOTE" nil "'")
    ("QUOTE" (:shift) "\"")))

(defparameter *prompt-sweden-keybindings*
  '(("A" (:CONTROL) :MOVE-BEGINNING-OF-LINE) 
    ("E" (:CONTROL) :MOVE-END-OF-LINE)
    ("F" (:CONTROL) :FORWARD-CHAR) 
    ("B" (:CONTROL) :BACKWARD-CHAR)
    ("HOME" NIL :MOVE-BEGINNING-OF-LINE)
    ("END" NIL :MOVE-END-OF-LINE)
    ("RIGHT" NIL :FORWARD-CHAR)
    ("LEFT" NIL :BACKWARD-CHAR)
    ("K" (:CONTROL) :CLEAR-LINE) 
    ("BACKSPACE" NIL :BACKWARD-DELETE-CHAR)
    ("RETURN" NIL :ENTER)
    ("X" (:CONTROL) :EXIT)
    ("ESCAPE" NIL :EXIT)
    ("P" (:ALT) :BACKWARD-HISTORY)
    ("N" (:ALT) :FORWARD-HISTORY)
    ("UP" NIL :BACKWARD-HISTORY)
    ("DOWN" NIL :FORWARD-HISTORY)
    ("MINUS" NIL "-")
    ("0" (:SHIFT) "=")
    ("EQUALS" (:SHIFT) "+")
    ("COMMA" (:SHIFT) ";")
    ("PERIOD" (:SHIFT) ":")
    ("9" (:SHIFT) ")")
    ("8" (:SHIFT) "(")
    ("QUOTE" (:SHIFT) "*")
    ("SPACE" NIL " ")
    ("QUOTE" NIL "'") 
    ("2" (:SHIFT) "\"")))

(define-method say prompt (&rest args)
  (apply #'message args))

(define-method initialize prompt ()
  (parent/initialize self)
  (install-keybindings self))

(define-method forward-char prompt ()
  (setf ^point (min (1+ ^point)
		     (length ^line))))

(define-method backward-char prompt ()
  (setf ^point (max 0 (1- ^point))))

(define-method insert prompt (string)
  (setf ^line (concatenate 'string
			    (subseq ^line 0 ^point)
			    string
			    (subseq ^line ^point)))
  (incf ^point (length string))
  ;; if the insertion ends with a period, also execute the command
  ;; line.
  (when (string= "." (subseq string (1- (length string))))
    (setf ^line (subseq string 0 (1- (length string))))
    (execute self)))

(define-method backward-delete-char prompt ()
  (when (< 0 ^point) 
    (setf ^line (concatenate 'string
			      (subseq ^line 0 (1- ^point))
			      (subseq ^line ^point)))
    (decf ^point)))

(define-method print-data prompt (data &optional comment)
  (dolist (line (split-string-on-lines (write-to-string data :circle t :pretty t :escape nil :lines 5)))
    (say self (if comment ";; ~A"
		  " ~A") line)))

(defparameter *prompt-debug-on-error* t)

(define-method do-sexp prompt (sexp)
  (with-fields (receiver) self
    (destructuring-bind (operation &rest arguments) sexp
      (apply #'send (make-keyword operation) 
	     receiver (mapcar #'eval arguments)))))

(define-method enter prompt ()
  (labels ((print-it (c) 
	     (print-data self c :comment)))
    (let* ((*read-eval* nil)
	   (line ^line)
	   (sexp (handler-case 
		     (read-from-string (concatenate 'string "(" line ")"))
		   (condition (c)
		     (print-it c)))))
      (clear-line self)
      (when sexp 
	(say self "~A" line)
	(if *prompt-debug-on-error*
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
	(queue line ^history))))
  (do-after-execute self))

(define-method do-after-execute prompt ()
  nil)

(define-method history-item prompt (n)
  (assert (integerp n))
  (nth (- (queue-count ^history) n)
       (queue-head ^history)))

(define-method forward-history prompt ()
  (when (> ^history-position 0)
    (setf ^line (history-item self (progn (decf ^history-position)
					   ^history-position)))
    (setf ^point (length ^line))))
 
(define-method backward-history prompt ()
  (when (< ^history-position (queue-count ^history))
    (setf ^line (history-item self (progn (incf ^history-position)
					   ^history-position)))
    (setf ^point (length ^line))))

(define-method set-receiver prompt (receiver)
  (setf ^receiver receiver))

(define-method clear-line prompt ()
  (setf ^line "")
  (setf ^point 0)
  (setf ^history-position 0))

(define-method move-end-of-line prompt ()
  (setf ^point (length ^line)))

(define-method move-beginning-of-line prompt ()
  (setf ^point 0))

(define-method draw prompt ()
  (with-fields (x y width height clock mode point parent line) self
    (draw-background self)
    ;; keep the cursor blinking
    (decf clock)
    (when (> (- 0 *prompt-blink-time*) clock)
      (setf clock *prompt-blink-time*))
    
    (let* ((font-height (font-height *block-font*))
	   (prompt-height (+ (* 2 *default-prompt-margin*)
			     font-height))
	   (strings-y *default-prompt-margin*)
	   (prompt-string (ecase mode
			    (:direct *direct-prompt-string*)
			    (:forward *forward-prompt-string*))))
    ;; draw cursor
    (when (eq :direct mode)
      (let ((color (if (minusp clock)
		       *prompt-cursor-color*
		       *prompt-cursor-blink-color*)))
	(when (> point (length line))
	  (setf point (1- (length line))))
	(draw-box (+ x (font-text-extents (if (<= point (length line))
					    (subseq line 0 point)
					    " ")
					*block-font*)
		     (font-text-extents prompt-string *block-font*))
		  (+ y strings-y)
		  (font-text-extents 
		   (string (if (< point (length line))
			       (aref line 
				     (max (max 0 
					       (1- (length line)))
					  point))
			       #\Space))
		   *block-font*)
		  font-height
		  :color color))
      ;; draw prompt string
      (draw-string prompt-string
		   (+ x *default-prompt-margin*)
		   (+ y strings-y)
		   :color "white"
		   :font *block-font*))
    (update-layout-maybe self)
    ;; draw current command line text
    (when (null line) (setf line ""))
    (unless (zerop (length line))
      (draw-string line
		   (+ x
		      (font-text-extents prompt-string *block-font*))
		   (+ y strings-y)
		   :color "white"
		   :font *block-font*)))))


(define-method layout prompt ())

(define-method update-layout-maybe prompt ()
  (with-fields (line) self
    (resize self 
	    :width  
	    (+ 12 (* 5 *dash*)
	       (font-text-extents line *block-font*)
	       (font-text-extents *direct-prompt-string* *block-font*))
	    :height 
	    (+ (* 2 *default-prompt-margin*) (font-height *block-font*)))))

;;; Text display and edit control

;; No fancy formatting, but editable and supports scrolling.

(defparameter *textbox-margin* 4 "Default onscreen margin (in pixels) of a textbox.")

(defparameter *textbox-minimum-width* 10) 

(define-prototype textbox (:parent "IOFORMS:BLOCK")
  (font :initform "default-font")
  (buffer :initform nil)
  (read-only :initform nil)
  (bordered :initform nil)
  (max-displayed-rows :initform nil :documentation "An integer when scrolling is enabled.")
  (max-displayed-columns :initform nil)
  (background-color :initform "gray30")
  (foreground-color :initform "white")
  (cursor-color :initform "yellow")
  (point-row :initform 0)
  (point-column :initform 0)
  (auto-fit :initform nil)
  (visible :initform t))

(define-method handle-event textbox (event)
  (unless ^read-only
    (let ((func (gethash event ^keymap)))
      (when func
	(prog1 t
	  (funcall func))))))

(define-method set-buffer textbox (buffer)
  (setf ^buffer buffer))

(define-method get-buffer-as-string textbox ()
  (apply #'concatenate 'string ^buffer))

(defparameter *next-screen-context-lines* 3)

(define-method page-up textbox ()
  "Scroll up one page, only when ^max-displayed-rows is set."
  (with-field-values (max-displayed-rows) self
    (when (integerp max-displayed-rows)
      (setf ^point-row (max 0
			   (- ^point-row (- max-displayed-rows
					     *next-screen-context-lines*)))))))

(define-method page-down textbox ()
  "Scroll down one page, only when ^max-displayed-rows is set."
  (with-field-values (max-displayed-rows) self
    (when (integerp max-displayed-rows)
      (setf ^point-row (min (- (length ^buffer) max-displayed-rows)
			     (+ ^point-row (- max-displayed-rows
					     *next-screen-context-lines*)))))))

(define-method auto-center textbox ()
  "Automatically center the textbox on the screen."
  (with-field-values (x y width height) self
    (let ((center-x (truncate (/ *screen-width* 2)))
	  (center-y (truncate (/ *screen-height* 2))))
      (setf ^x (- center-x (truncate (/ width 2)))
	    ^y (- center-y (truncate (/ height 2)))))))

(define-method resize-to-scroll textbox (&key width height)
  "Resize the textbox to WIDTH * HEIGHT and enable scrolling of contents.
This method allocates a new SDL surface."
  (assert (and (numberp width) (numberp height)))
  (resize self :height height :width width)
  (setf ^max-displayed-rows (truncate (/ height (font-height ^font)))))

(define-method resize-to-fit textbox ()
  "Automatically resize the textbox to fit the text, and disable scrolling.
This method allocates a new SDL surface when necessary."
  ;; disable scrolling
  (setf ^max-displayed-rows nil)
  ;; measure text
  (let* ((buffer ^buffer)
	 (line-height (font-height ^font))
	 (line-lengths (mapcar #'(lambda (s)
				   (font-text-extents s ^font))
			       buffer)))
    ;; update geometry
    (let ((width0 (max *textbox-minimum-width*
		       (+ (* 2 *textbox-margin*) 4
			  (if (null line-lengths)
			      0 
			      (apply #'max line-lengths)))))
	  (height0 (+ (* 2 *textbox-margin*)
		      (* line-height (max 1 (length buffer))))))
      (when (or (< ^width width0)
		(< ^height height0))
	(message "resizing textbox H:~S W:~S" height0 width0)
	(resize self :height height0 :width width0)))))

(define-method move-end-of-line textbox ()
  (setf ^point-column (length (nth ^point-row ^buffer))))

(define-method move-beginning-of-line textbox ()
  (setf ^point-column 0))

(defun bind-key-to-textbox-insertion (textbox key modifiers &optional (insertion key))
  "For textbox P ensure that the event (KEY MODIFIERS) causes the
text INSERTION to be inserted at point."
 (define-key textbox (string-upcase key) modifiers
	      #'(lambda ()
		  (insert textbox insertion))))

(define-method install-keybindings textbox ()
  ;; install basic keybindings
  (bind-key-to-method self "A" '(:control) :move-beginning-of-line)
  (bind-key-to-method self "E" '(:control) :move-end-of-line)
  (bind-key-to-method self "HOME" nil :move-beginning-of-line)
  (bind-key-to-method self "END" nil :move-end-of-line)
  (bind-key-to-method self "N" '(:control) :next-line)
  (bind-key-to-method self "P" '(:control) :previous-line)
  (bind-key-to-method self "F" '(:control) :forward-char)
  (bind-key-to-method self "B" '(:control) :backward-char)
  (bind-key-to-method self "DOWN" nil :next-line)
  (bind-key-to-method self "UP" nil :previous-line)
  (bind-key-to-method self "RIGHT" nil :forward-char)
  (bind-key-to-method self "LEFT" nil :backward-char)
  (bind-key-to-method self "K" '(:control) :clear)
  (bind-key-to-method self "BACKSPACE" nil :backward-delete-char)
  (bind-key-to-method self "RETURN" nil :newline)
  ;; install keybindings for self-inserting characters
  (map nil #'(lambda (char)
	       (bind-key-to-textbox-insertion self (string char) nil
					     (string-downcase char)))
       *lowercase-alpha-characters*)
  (map nil #'(lambda (char)
	       (bind-key-to-textbox-insertion self (string char) '(:shift)))
       *uppercase-alpha-characters*)
  (map nil #'(lambda (char)
	       (bind-key-to-textbox-insertion self (string char) nil))
       *numeric-characters*)
  ;; other characters
  (bind-key-to-textbox-insertion self "EQUALS" nil "=")
  (bind-key-to-textbox-insertion self "MINUS" nil "-")
  (bind-key-to-textbox-insertion self "EQUALS" '(:control) "+")
  (bind-key-to-textbox-insertion self "SEMICOLON" nil ";")
  (bind-key-to-textbox-insertion self "SEMICOLON" '(:shift) ":")
  (bind-key-to-textbox-insertion self "0" '(:shift) ")")
  (bind-key-to-textbox-insertion self "9" '(:shift) "(")
  (bind-key-to-textbox-insertion self "8" '(:shift) "*")
  (bind-key-to-textbox-insertion self "SPACE" nil " ")
  (bind-key-to-textbox-insertion self "QUOTE" nil "'")
  (bind-key-to-textbox-insertion self "QUOTE" '(:shift) "\""))

(define-method initialize textbox ()
  (send-parent self :initialize self)
  (install-keybindings self))

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
    (if (and (= 0 point-column) (= 0 point-row))
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
	(progn
	  ;;
	  ;; otherwise, delete within current line.
	  (when (= 0 point-column)
	    (let* ((line (nth point-row buffer))
		   (remainder (subseq line point-column)))
	      (setf (nth point-row buffer)
		    (concatenate 'string 
				 (subseq line 0 (- point-column 1))
				 remainder))
	      (decf point-column)))))))
    
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

(define-method render textbox ()
  (when ^visible
    (clear self)
    (when ^auto-fit
      (resize-to-fit self))
    (with-fields (buffer x y width height) self
      (with-field-values (font image point-row) self
	;; measure text
	(let* ((line-height (font-height font))
	       (line-lengths (mapcar #'(lambda (s)
					 (font-text-extents s font))
				     buffer)))
	  ;; draw background
	  (draw-box 0 0 width height
		    ;; :stroke-color (if ^bordered
		    ;; 		      ^foreground-color
		    ;; 		      ^background-color)
		    :color ^background-color)
	  ;; draw text
	  (let ((x0 (+ 0 *textbox-margin*))
		(y0 (+ 0 *textbox-margin*))
		(lines (if ^auto-fit 
			   buffer
			   (nthcdr ^point-row buffer))))
	    (dolist (line lines)
	      (draw-string-solid line x0 y0 
				 :font font :color ^foreground-color)
	      (incf y0 line-height)))
	  ;; draw cursor
	  ;; TODO fix ^point-row to be drawn relative pos in scrolling
	  (when (null ^read-only)
	    (let* ((current-line (nth ^point-row buffer))
	    	   (cursor-width (font-width font))
	    	   (x1 (+ 0 *textbox-margin*
	    		  (font-text-extents (subseq current-line 0 ^point-column)
	    				     font)))
	    	   (y1 (+ 0 *textbox-margin*
	    		  (* line-height ^point-row))))
	      (draw-rectangle x1 y1 cursor-width line-height 
	    		      :color ^cursor-color))))))))

;;; The pager switches between different visible groups of blocks

(define-prototype pager (:parent "IOFORMS:BLOCK")
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
  (send-parent self :initialize self)
  (auto-position self)
  (labels ((s1 () (select self 1))
	   (s2 () (select self 2))
	   (s3 () (select self 3))
	   (s4 () (select self 4))
	   (s5 () (select self 5)))
    (define-key self "F1" nil #'s1)
    (define-key self "F2" nil #'s2)
    (define-key self "F3" nil #'s3)
    (define-key self "F4" nil #'s4)
    (define-key self "F5" nil #'s5)))

(define-method page-property pager (page-name property-keyword)
  (getf (gethash page-name ^properties) property-keyword))

(define-method set-page-property pager (page-name property-keyword value)
  (setf (gethash page-name ^properties)
	(let ((props (gethash page-name ^properties)))
	  (setf (getf props property-keyword) value)
	  props))
  (message "Page property set. ~A" (list page-name (gethash page-name ^properties))))

(define-method hit pager (x y)
  nil)

(define-method select pager (page)
  (let ((newpage (etypecase page
		   (number (car (nth (- page 1) ^pages)))
		   (keyword page))))
    (if (null newpage)
	(message "WARNING: Cannot find page.")
	(progn 
	  (setf ^current-page newpage)
	  ;; respect held keys property setting
	  (if (page-property self newpage :held-keys)
	      (enable-held-keys)
	      (disable-held-keys))
	  ;; insert self always as first block
	  (apply #'ioforms:install-blocks self (cdr (assoc newpage ^pages)))))))

(define-method auto-position pager (&key (width ioforms:*screen-width*))
  (resize self :width width :height ^pager-height)
  (move self :x 0 :y (- ioforms:*screen-height* ^pager-height)))

(define-method add-page pager (keyword blocks &rest properties)
  (assert (listp blocks))
  (push (cons keyword blocks) ^pages))

(define-method get-page-names pager ()
  (remove-duplicates (mapcar #'car ^pages)))

(define-method message pager (formatted-string)
  (setf ^pager-message formatted-string))

(define-method render pager ()
  ;; calculate geometry. always draw
  (when ^visible
    (clear self ^background-color)
    (let ((n 1)
          (line '()))
      (dolist (page ^pages)
        (let ((page-name (car page)))
          ;; build a list of formatted strings
          (push (cons (concatenate 'string 
                                   ^prefix-string
                                   (format nil "~D" n)
                                   ^number-separator-string
                                   (symbol-name page-name)
                                   ^separator-string)
                      ;; highlight current page
                      (if (eq page-name ^current-page)
                          ^highlighted-style ^style))
                line))
        (incf n))
      (push (list " ") line)
      (when ^pager-message 
        (push ^pager-message line))
      ;; draw the string
      (render-formatted-line (nreverse line) 0 0))))

;;; Splitscreen view on 2 blocks with focus border

(define-prototype split (:parent "IOFORMS:BLOCK")
  (active-color :initform "red")
  (inactive-color :initform "blue")
  (focus :initform 0)
  children)

(define-method initialize split (&rest children)
  (setf ^children children))
  
(define-method set-children split (children)
  (setf ^children children))

(define-method render split ()
  (when ^visible
    (let ((y ^y)
          (x ^x)
	  (image ^image)
	  (focused-block (nth ^focus ^children)))
      (dolist (block ^children)
        (move block :x x :y y)
	(render block)
	(draw-image (field-value :image block) x y)
	(when (eq block focused-block)
	  (draw-rectangle x y (field-value :width block)
			  (field-value :height block)
			  :color ^active-color))
        (incf x (1+ (field-value :width block)))))))

(define-method hit split (x y)
  (hit-blocks x y ^children))

(define-method switch-panes split ()
  (let ((newpos (mod (1+ ^focus) (length ^children))))
    (setf ^focus newpos)))

(define-method handle-key split (event)
  (or (let ((func (gethash event ^keymap)))
	(when func
	  (prog1 t
	    (funcall func))))
      (handle-key (nth ^focus ^children) event)))

(define-method forward split (method &rest args)
  (apply #'send self method (nth ^focus ^children) args))

;;; Menus

(define-prototype menu (:parent "IOFORMS:LIST")
  (category :initform :menu)
  (top-level :initform nil)
  action target (expanded :initform nil) (visible :initform t))

(define-method initialize menu 
    (&key action target top-level inputs 
	  schema expanded (label "blank menu item..."))
  (parent/initialize self)
  (setf ^action action
	^expanded expanded
	^target target
	^schema schema
	^top-level top-level
	^inputs inputs
	^label label)
  (when inputs
    (dolist (each inputs)
      (pin each)
      (set-parent each self))))

(define-method accept menu (&rest args) nil)

(define-method run menu ())

(define-method click menu ()
  (with-fields (expanded action target) self
    (if (keywordp action)
        (send action (or target (symbol-value '*system*)))
	(progn 
	  (setf expanded (if expanded nil t))
	  (report-layout-change *script*)))))

(define-method is-expanded menu ()
  ^expanded)

(define-method display-string menu ()	    
  (with-fields (action label top-level) self
    (let ((ellipsis (concatenate 'string label *null-display-string*)))
      (if action
	  (etypecase action
	    (ioforms:object ellipsis)
	    (keyword label))
	  (if top-level label ellipsis)))))

(define-method layout-as-string menu (string)
  (with-fields (height width) self
    (setf height (dash 1 (font-height *block-font*)))
    (setf width 
	  (+ (dash 2) (font-text-extents string *block-font*)))))

(define-method layout menu ()
  (with-fields (expanded dash inputs label width) self
    (if expanded 
	;; we're an expanded submenu. lay it out
	(progn 
	  (setf dash 1)
	  (layout-as-list self)
	  (when label 
	    (setf width 
		  (max width 
		       (dash 4 (font-text-extents label *block-font*)))))
	  ;; make all inputs equally wide
	  (dolist (each inputs)
	    (setf (field-value :width each) (- width (dash 2)))))
	;; we're not expanded. just lay out for label.
	(layout-as-string self (display-string self)))))

(define-method header-height menu ()
  (font-height *block-font*))

(define-method draw-expanded menu (&optional label)
  (with-fields (action x y width height parent inputs) self
;    (if (null parent)
	(draw-background self)
;	(draw-box x y width height :color (find-color self)))
    (draw-label-string self (or label *null-display-string*))
    (let ((header (header-height self)))
      (draw-line (dash 2 x)
		 (dash 2 y header 1)
		 (dash -3 x width)
		 (dash 2 y header 1)
		 :color (find-color self :shadow))
      (dolist (each inputs)
	(draw each)))))

(define-method draw-hover menu ()
  nil)

(define-method draw-border menu ()
  nil)

(define-method draw-highlight menu ()
  (with-fields (y height expanded parent) self
    (when parent
      (with-fields (x width) parent
	(when (not expanded)
	  (draw-box x (+ y (dash 1)) width (+ height 1)
		  :color *highlight-background-color*)
	  (draw-label-string self (display-string self)))))))
  
(define-method draw menu (&optional highlight)
  (with-fields (x y width height label action visible expanded) self
    (when visible
      (if expanded 
	  (draw-expanded self label)
	  ;; otherwise just draw menu name and highlight, if any
	  (draw-label-string self (display-string self))))))

(define-method minimize menu ()
  (setf ^expanded nil))

;; see system.lisp for example menu
(defun make-menu (items &optional target)
  (labels ((expand (item)
	     (if (listp item)
		 (if (listp (first item))
		     (mapcar #'expand item)
		     (apply #'clone "IOFORMS:MENU"
			    :target target
			    (mapcar #'expand item)))
		 item)))
    (expand items)))

;;; A global menu bar

(defblock menubar :category :menu)

(define-method initialize menubar (menus)
  (parent/initialize self)
  (with-fields (inputs) self
    (setf inputs menus)
    (dolist (each menus)
      (setf (field-value :top-level each) t)
      (pin each))))

(define-method draw-border menubar () nil)

(define-method layout menubar ()
  (with-fields (x y width height inputs) self
    (setf x 0 y 0 width *screen-width* height (dash 1))
    (let ((x1 (dash 1)))
      (dolist (item inputs)
	(move-to item x1 y)
	(layout item)
	(incf x1 (dash 2 (field-value :width item)))
	(setf height (max height (field-value :height item)))))))
        
(define-method draw menubar ()
  (with-fields (x y width inputs) self
    (let ((bar-height (dash 2 1 (font-height *block-font*))))
      (draw-box x y 
		width bar-height
		:color (find-color self))
      (draw-line x bar-height width bar-height
		 :color (find-color self :shadow))
      (with-fields (inputs) self
	(dolist (each inputs)
	  (draw each))))))

(define-method close-menus menubar ()
  (with-fields (inputs) self
    (when (some #'is-expanded inputs)
      (mapc #'minimize ^inputs)
      (report-layout-change *script*))))

;;; widgets.lisp ends here
