;;; terminal.lisp --- interactive blocks command-line hypermedia terminal

;; Copyright (C) 2011  David O'Toole

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

(in-package :ioforms)

;;; Command prompt block

(defparameter *prompt-blink-time* 8)
(defparameter *prompt-cursor-color* "magenta")
(defparameter *prompt-cursor-blink-color* "yellow")
(defparameter *cursor-inactive-color* "gray50")

(defparameter *direct-prompt-string* "> ")
(defparameter *forward-prompt-string* "Press CONTROL-X to enter the prompt.")

(defparameter *default-prompt-margin* 4)

(defparameter *default-prompt-history-size* 100)
(defparameter *default-cursor-width* 1)

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

:  yes             -->   (yes %receiver)
:  move :north     -->   (move %receiver :north)
:  attack :west :with :left-hand  --> (attack %receiver :west 
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

:   %up      -->    move :north .
:  shift-%up -->    push :north .
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

The modes can be toggled with CONTROL-X.")
  (mode :documentation "Either :direct or :forward." :initform :direct)
  (clock :initform *prompt-blink-time*)
  (text-color :initform "gray20")
  (visible :documentation "When non-nil, the prompt is drawn." :initform t)
  (receiver :documentation "The object to send command messages to when in :forward mode.")
  (point :initform 0 :documentation "Integer index of cursor within prompt line.")
  (line :initform "" :documentation "Currently edited command line.")
  (background :initform t)
  (prompt-string :initform "> ")
  (category :initform :data)
  (history :initform (make-queue :max *default-prompt-history-size*)
	   :documentation "A queue of strings containing the command history.")
  (history-position :initform 0)
  (debug-on-error :initform nil))

;; (define-method handle-event prompt (event)
;;   "Reject all keypresses when in :forward mode; otherwise handle them
;; normally."
;;   (ecase %mode
;;     ;; returning t stops the frame from trying other blocks
;;     (:direct (prog1 t (let ((func (gethash event %keymap)))
;; 			(when func
;; 			  (funcall func)))))
;;     (:forward (when (equal (normalize-event '("X" :control))
;; 			   event)
;; 		(prog1 t (goto self))))))

(define-method accept prompt (&rest args)
  nil)

(define-method exit prompt ()
  (clear-line self)
  (setf %mode :forward))

(define-method goto prompt ()
  (say self "Enter command below at the >> prompt. Press ENTER when finished, or CONTROL-X to cancel.")
  (setf %mode :direct))

(define-method set-mode prompt (mode)
  (setf %mode mode))

(defun bind-event-to-prompt-insertion (self key mods text)
  (bind-event-to-function self key mods 
			  #'(lambda ()
			      (insert self text))))

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
    ("DELETE" nil :delete-char)
    ("D" (:control) :delete-char)
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

(define-method install-keybindings prompt ()
  ;; install keys that will vary by locale
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

(define-method say prompt (&rest args)
  (apply #'message args))

(define-method initialize prompt ()
  (super%initialize self)
  (install-keybindings self))

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
  (when (< 0 %point) 
    (setf %line (concatenate 'string
			      (subseq %line 0 %point)
			      (subseq %line (1+ %point))))))

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

(define-method read-expression prompt (input-string)
  (handler-case 
      (read-from-string (concatenate 'string "(" input-string ")"))
    (condition (c)
      (print-it c))))

(define-method enter prompt (&optional no-clear)
  (labels ((print-it (c) 
	     (print-data self c :comment)))
    (let* ((*read-eval* nil)
	   (line %line)
	   (sexp (read-expression self line)))
      (unless no-clear (clear-line self))
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
	(queue line %history))))
  (do-after-evaluate self))

(define-method do-after-evaluate prompt ()
  nil)

(define-method history-item prompt (n)
  (assert (integerp n))
  (nth (- (queue-count %history) n)
       (queue-head %history)))

(define-method forward-history prompt ()
  (when (> %history-position 0)
    (setf %line (history-item self (progn (decf %history-position)
					   %history-position)))
    (setf %point (length %line))))
 
(define-method backward-history prompt ()
  (when (< %history-position (queue-count %history))
    (setf %line (history-item self (progn (incf %history-position)
					   %history-position)))
    (setf %point (length %line))))

(define-method set-receiver prompt (receiver)
  (setf %receiver receiver))

(define-method clear-line prompt ()
  (setf %line "")
  (setf %point 0)
  (setf %history-position 0))

(define-method move-end-of-line prompt ()
  (setf %point (length %line)))

(define-method move-beginning-of-line prompt ()
  (setf %point 0))

(define-method draw-cursor prompt 
    (&optional (color "magenta") x-offset y-offset)
  (with-fields (x y width height clock mode point parent background
		  line) self
    (draw-box (+ x (or x-offset 0)
		 (font-text-extents (if (<= point (length line))
					(subseq line 0 point)
					" ")
				    *block-font*)
		 (if x-offset 0 (font-text-extents *direct-prompt-string* *block-font*)))
	      (+ y (or y-offset 0) *default-prompt-margin*)
	      *default-cursor-width*
	      ;; (font-text-extents 
	      ;;  (string (if (< point (length line))
	      ;; 		   (aref line 
	      ;; 			 (max (max 0 
	      ;; 				   (1- (length line)))
	      ;; 			      point))
	      ;; 		   #\Space))
	      (* (font-height *block-font*) 0.8)
	      :color color)))

(define-method label-width prompt () 
  (+ (dash 3) *default-prompt-margin*
     (font-text-extents %prompt-string *block-font*)))

(define-method draw-border prompt () nil)

(define-method update-cursor-clock prompt ()
  ;; keep the cursor blinking
  (with-fields (clock) self
    (decf clock)
    (when (> (- 0 *prompt-blink-time*) clock)
      (setf clock *prompt-blink-time*))))

(define-method draw-focus prompt () 
  (with-fields (clock) self
    (update-cursor-clock self)
    (draw-cursor self (if (minusp clock)
			  *prompt-cursor-color*
			  *prompt-cursor-blink-color*)
		 (dash 4))))

(define-method draw prompt ()
  (with-fields (x y width height clock mode point parent background
		  line prompt-string) self
    ;; possibly draw a background
    ;; (cond ((stringp background)
    ;; 	   (draw-background self background))
    ;; 	  ((eq t background)
    ;; 	   (draw-background self)))
    (let ((strings-y *default-prompt-margin*))
      ;; draw cursor (may be overdrawn with blink when selected
      ;; (when (> point (length line))
      ;;   (setf point (1- (length line))))
      ;; (draw-cursor self *cursor-inactive-color* (dash 3))
      ;; draw prompt string
      (assert (stringp %text-color))
      (draw-string prompt-string
		   (+ x *default-prompt-margin*)
		   (+ y strings-y)
		   :color %text-color
		   :font *block-font*)
      (update-layout-maybe self)
      ;; draw current command line text
      (when (null line) (setf line ""))
      (unless (zerop (length line))
	(draw-string line
		     (+ x *default-prompt-margin* 
			(font-text-extents prompt-string *block-font*))
		     (+ y strings-y)
		     :color %text-color
		     :font *block-font*)))))

(define-method click prompt (mouse-x mouse-y)
  (declare (ignore mouse-y))
  (with-fields (x y width height clock mode point parent background
		  line) self
    ;; find the left edge of the data area
    (let* ((left (+ x (label-width self) (dash 4)))
	   (tx (- mouse-x left)))
      ;; which character was clicked?
      (let ((click-index 
	      (block measuring
		(dotimes (ix (length line))
		  (when (< tx (font-text-extents 
			       (subseq line 0 ix)
			       *block-font*))
		    (return-from measuring ix))))))
	(when (numberp click-index)
	  (setf point click-index))))))

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

;;; General-purpose data entry block.

(define-prototype entry (:parent "IOFORMS:PROMPT")
  (category :initform :value)
  (pinned :initform t)
  (text-color :initform "white")
  (label-color :initform "white")
  options label type-specifier value)

(define-method initialize entry (&key value type-specifier options label label-color parent)
  (super%initialize self)
  ;(assert (and value type-specifier))
  (when parent (setf %parent parent))
  (setf %type-specifier type-specifier
	%options options
	%label label
	%value value)
  ;; fill in the input box with the value
  (setf %line (format nil "~S" value))
  (setf %label (getf options :label))
  (when label-color (setf %label-color label-color))
  (when (null %label)
    (setf %pretty-label (pretty-symbol-string label))))

;; (define-method click entry (mouse-x mouse-y)
;;   (declare (ignore mouse-y))
;;   (with-fields (x y width height clock mode point parent background
;; 		  line) self
;;     ;; find the left edge of the data area
;;     (let* ((left (+ x (label-width self) (dash 3)))
;; 	   (tx (- mouse-x left)))
;;       ;; which character was clicked?
;;       (let ((click-index 
;; 	      (block measuring
;; 		(dotimes (ix (length line))
;; 		  (when (< tx (font-text-extents 
;; 			       (subseq line 0 ix)
;; 			       *block-font*))
;; 		    (return-from measuring ix))))))
;; 	(when (numberp click-index)
;; 	  (setf point click-index))))))

(define-method evaluate entry ()
  %value)

(define-method set-value entry (value)
  (setf %value value))

(define-method get-value entry ()
  %value)

(define-method recompile entry ()
  %value)

(define-method label-string entry ()
  (or (getf %options :label)
      %pretty-label))

(define-method label-width entry ()
  (font-text-extents (label-string self) *block-font*))

(defparameter *minimum-entry-line-width* 16)

(define-method draw-entry-label entry ()
  (let* ((label (label-string self))
	 (label-width (font-text-extents label *block-font*))
	 (line-width (font-text-extents %line *block-font*)))
    (draw-string label
		 (dash 1 %x)
		 (+ %y (dash 1))
		 :color %label-color
		 :font *block-font*)))

(define-method draw entry (&optional nolabel)
  (with-fields (x y options text-color width parent height line) self
    (let ((label-width (label-width self))
	  (line-width (font-text-extents line *block-font*))
	  (fh (font-height *block-font*)))
      ;; draw the label string 
      (assert (stringp text-color))
      (unless nolabel (draw-entry-label self))
      ;; draw input area indicators
      (draw-indicator :top-left-triangle
       (dash 1 x label-width)
       (dash 1 y))
      (draw-indicator :bottom-right-triangle
       (dash 2 x label-width line-width)
       (dash 1 y fh))
      ;; draw current input string
      (when (null line) (setf line ""))
      (unless (zerop (length line))
	(draw-string line
		     (+ (dash 2 x) label-width)
		     (+ y (dash 1))
		     :color %text-color
		     :font *block-font*)))))

(define-method draw-border entry ())

(define-method draw-hover entry ())

(define-method draw-focus entry () 
  (with-fields (clock x y width line parent) self
    (let* ((label (label-string self))
	   (label-width (font-text-extents label *block-font*))
	   (line-width (font-text-extents line *block-font*)))
      ;; draw shaded area for data entry.
      ;; makes the cursor show up a bit better too.
      (draw-box (dash 1.5 x label-width)
		(dash 1 y)
		(dash 2 line-width)
		(+ 1 (font-height *block-font*))
		:color (when parent
			 (find-color parent :shadow)))
      ;; draw cursor.
      (update-cursor-clock self)
      (draw-cursor self (if (minusp clock)
			    *prompt-cursor-color*
			    *prompt-cursor-blink-color*)
		   ;; provide x offset
		   (dash 2 (font-text-extents label *block-font*)))
      ;; redraw content (but not label)
      (draw self :nolabel))))
      ;; draw input area underlining
      ;; (let* ((label-width (font-text-extents label *block-font*))
      ;; 	     (font-height (font-height *block-font*))
      ;; 	     (underline-y (round (dash 1 y (* font-height 0.96)))))
      ;; 	(draw-line  (dash 2 x label-width) underline-y
      ;; 		    (dash 1 x width) underline-y
      ;; 		    :color "white"))))) 

(define-method lose-focus entry ()
  ;; update the entry value if the user mouses away
  (enter self))
		 
(define-method do-sexp entry (sexp)
  (assert (and (listp sexp) (= 1 (length sexp))))
  (let ((datum (first sexp)))
    (if (typep datum %type-specifier)
	(setf %value datum)
	(message "Warning: value entered does not match %TYPE-SPECIFIER."))))

(define-method enter entry ()
  (super%enter self :no-clear))

(define-method draw-contents entry ())

(define-method layout entry ()
  (with-fields (height width value line) self
    (setf height (+ (* 1 *dash*) (font-height *block-font*)))
    (setf width (+ (* 4 *dash*)
		   (font-text-extents (label-string self) *block-font*)
		   (max *minimum-entry-line-width*
			(font-text-extents line *block-font*))))))

(defmacro defentry (name type-specifier value)
  `(define-prototype ,name (:parent "IOFORMS:ENTRY")
     (type-specifier :initform ',type-specifier)
     (value :initform ,value)))

(defentry integer integer 0)
(defentry string string "")
(defentry number number 0)
(defentry non-negative-number (number 0 *) 0)
(defentry float float 0.0)
(defentry symbol symbol nil)
(defentry positive-integer (integer 1 *) 1)
(defentry non-negative-integer (integer 0 *) 0)
(defentry sexp t nil)

;;; Plain text entry

(define-prototype string (:parent entry))

(define-method read-expression string (input-string)
  ;; pass-through; don't read string at all.
  input-string)

(define-method do-sexp string (sexp)
  (assert (stringp sexp))
  (setf %value sexp))
  
;;; Lisp listener prompt that makes active Lisp blocks out of what you type.

(define-prototype block-prompt (:parent prompt)
  (operation :initform :prompt)
  (background :initform nil)
  output)

(define-method initialize block-prompt (output)
  (super%initialize self)
  (setf %output output))

(define-method set-output block-prompt (output)
  (setf %output output))

(define-method do-sexp block-prompt (sexp)
  (with-fields (output) self
    (assert output)
    (let ((*make-block-package* (find-package (project-package-name)))
	  (container (get-parent output)))
      (when container
	(let ((block 
		  (if (symbolp (first sexp))
		      (make-block sexp)
		      (make-block (first sexp)))))
	  (accept container block))))))

(define-prototype listener (:parent list)
  (type :initform :system))

(defparameter *minimum-listener-width* 200)

(define-method initialize listener ()
  (with-fields (image inputs) self
    (let ((prompt (new block-prompt self)))
      (super%initialize self)
      (set-output prompt prompt)
      (setf inputs (list prompt))
      (set-parent prompt self)
      (pin prompt))))

(define-method evaluate listener ()
  (evaluate (first %inputs))) ;; should I evaluate them all?

;; forward keypresses to prompt for convenience
(define-method handle-event listener (event)
    (handle-event (first %inputs) event))

(define-method get-prompt listener ()
  (first %inputs))

;;; Bottom-of-the-screen emacs-style command line

(define-prototype terminal (:parent "IOFORMS:LISTENER")
  (scrollback-length :initform 100)
  (pinned :initform t)
  (category :initform :menu)
  (temporary :initform t)
  (display-lines :initform 4))
  
(define-method layout terminal ()
  (with-fields (x y height width parent inputs) self
    (setf x (field-value :x parent))
    (setf width (field-value :width parent))
    ;; start near bottom of screen
    (let ((y0 (- (field-value :height parent) (dash 1))))
	(setf height (font-height *block-font*))
	(dolist (element inputs)
	  (layout element)
	  (decf y0 (field-value :height element))
	  (move-to element (+ x (dash 1)) y0)
	  (incf height (+ (dash 1) (field-value :height element))))
	(incf height (dash 1)) ;; a little extra room at the top
	;; move to the right spot to keep the bottom on the bottom.
	(setf y y0))))

(define-method accept terminal (input &optional prepend)
  (declare (ignore prepend))
  (with-fields (inputs scrollback-length) self
    (assert (not (null inputs))) ;; we always have a prompt
    (prog1 t
      (assert (is-valid-connection self input))
      (let ((len (length inputs)))
	(when (> len scrollback-length)
	  ;; drop last item in scrollback
	  (setf inputs (subseq inputs 0 (1- len))))
	;; set parent if necessary 
	(when (get-parent input)
	  (unplug-from-parent input))
	  (set-parent input self)
	  (setf inputs 
		(nconc (list (first inputs))
		       (list input)
		       (nthcdr 2 inputs)))))))

(define-method draw terminal ()
  (with-fields (inputs x y height width) self
    (draw-box x y width height :color (find-color self))
    (draw-line 0 y *screen-width* y :color (find-color self :shadow))
    (if (null inputs)
	(draw-label-string self *null-display-string*)
	(dolist (each inputs)
	  (draw each)))))

;;; terminal.lisp ends here
