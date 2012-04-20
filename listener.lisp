;;; listener.lisp --- interactive blocks command-line hypermedia terminal

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
			    (unless (is-a 'world %parent)
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

(define-prototype entry (:super "BLOCKY:PROMPT")
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
    (add-block (world) (new 'text %error-output) *pointer-x* *pointer-y*)))

;;; Easily defining new entry blocks

(defmacro defentry (name type value &rest specs)
  `(define-prototype ,name (:super "BLOCKY:ENTRY")
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

;;; Lisp listener prompt that makes active Lisp blocks out of what you type.

(define-prototype listener-prompt (:super :prompt)
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

(define-prototype listener (:super :list)
  (scrollback-length :initform 100)
  (category :initform :system)
  (temporary :initform t)
  (methods :initform '(:evaluate))
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
    (let* ((x (%window-x (world)))
	   (y0 (+ (%window-y (world))
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

;;; Modeline

(defun-memo modeline-position-string (x y)
    (:key #'identity :test 'equal :validator #'identity)
  (format nil "X:~S Y:~S" x y))

(define-block-macro modeline
    (:super :list
     :fields 
     ((orientation :initform :horizontal)
      (no-background :initform t))
     :inputs (:project-id (new 'string :read-only t)
	      :buffer-id (new 'string :read-only t)
	      :position (new 'string :read-only t)
	      :mode (new 'string :read-only t))))

(define-method update modeline ()
  (set-value %%project-id *project*)
  (set-value %%buffer-id (%buffer-name (world)))
  (set-value %%position
	     (modeline-position-string
	      (%window-x (world))
	      (%window-y (world))))
  (set-value %%mode "(normal)"))

;;; listener.lisp ends here
