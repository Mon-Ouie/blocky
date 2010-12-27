;;; console.lisp --- core operations for IOFORMS

;; Copyright (C) 2006, 2007, 2008, 2009, 2010  David O'Toole

;; Author: David O'Toole <dto@gnu.org> <dto1138@gmail.com>
;; Keywords: multimedia, games

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The "console" is the library which provides all IOFORMS system
;; services. Primitive operations such as opening a window,
;; displaying bitmaps, drawing lines, playing sounds, file access, and
;; keyboard/mouse/joytstick input are handled here. 

;; Currently it uses the cross-platform SDL library (via
;; LISPBUILDER-SDL) as its device driver, and wraps the library for
;; use by the rest of IOFORMS.

;; http://lispbuilder.sourceforge.net/

(in-package :ioforms) 

;;; Keyboard state

;; see also keys.lisp

(defun keyboard-id (key)
  "Look up the SDL symbol corresponding to the IOFORMS symbol KEY. See keys.lisp."
  (let* ((entry (find key *key-identifiers* :key #'first))
	 (entry2 (find (second entry) *sdl-key-identifiers* :key #'second)))
    (first entry2)))

(defun keyboard-mod (mod)
  "Look up the SDL symbol corresponding to the IOFORMS symbol MOD. See keys.lisp."
  (let* ((entry (find mod *key-modifiers* :key #'first))
	 (entry2 (find (second entry) *sdl-key-modifiers* :key #'second)))
    (first entry2)))

(defun keyboard-held-p (key) 
  "Returns the duration in seconds that the key has been depressed over a number of game loops."
  (sdl:key-held-p (keyboard-id key)))

(defun keyboard-pressed-p (key)
  "Returns t if the key has just been depressed in the current game loop."
  (sdl:key-pressed-p (keyboard-id key)))

(defun keyboard-released-p (key)
  "Returns t if the key has just been released in the current game loop."
  (sdl:key-released-p (keyboard-id key)))

(defun keyboard-time-in-current-state (key)
  "Returns the duration in seconds that key is either pressed or depressed."
  (sdl:key-time-in-current-state (keyboard-id key)))

(defun keyboard-time-in-previous-state (key)
  "Returns the duration in seconds that key was in its previous state either pressed or depressed."
  (sdl:key-time-in-previous-state (keyboard-id key)))

(defun keyboard-down-p (key)
  "Returns t if the key is depressed."
  (sdl:key-down-p (keyboard-id key)))

(defun keyboard-modifier-down-p (mod)
  "Returns t if the modifier key is depressed."
  (sdl:mod-down-p (keyboard-mod mod)))

(defun keyboard-keys-down ()
  "Returns a list of the keys that are depressed."
  (labels ((translate (key)
	     (let ((entry (find key *sdl-key-identifiers* :key #'first)))
	       (let ((entry2 (find (second entry) *key-identifiers* :key #'second)))
		 (first entry2)))))
    (mapcar #'translate (sdl:keys-down-p))))

(defun keyboard-modifiers () 
  "Returns a list of the modifier keys that are depressed."
  (labels ((translate (mod)
	     (let ((entry (find mod *sdl-key-modifiers* :key #'first)))
	       (let ((entry2 (find (second entry) *key-modifiers* :key #'second)))
		 (first entry2)))))
    (mapcar #'translate (sdl:mods-down-p))))

;;; Processing once per timestep

(defvar *keys* nil "List of keywords of currently pressed keys.")

(defvar *mods* nil "List of keywords of currently pressed modifiers.")

(defvar *keyboard-timestep-number* 0)

(defun get-keys ()
  (if (/= *keyboard-timestep-number* *timesteps*)
      (setf *keys* (keyboard-keys-down)
	    *mods* (keyboard-modifiers)
	    *keyboard-timestep-number* *timesteps*)
      (setf *keys* nil *mods* nil))
  (values *keys* *mods*))

;;; Logging messages to the standard output

(defparameter *message-logging* nil)

(defun message-to-standard-output (format-string &rest args)
  (apply #'format t format-string args)
  (fresh-line)
  (force-output))

(defvar *message-function* nil)

(defun reset-message-function ()
  (setf *message-function* #'message-to-standard-output))

(defun message (format-string &rest args)
  "Print a log message by passing the arguments to
`*message-function'. When the variable `*message-logging*' is nil,
this output is disabled."
  (when (and *message-logging* 
	     (functionp *message-function*))
    (apply *message-function* format-string args)))

;;; Sequence numbers

(defvar *sequence-number* 0)

(defun genseq (&optional (x 0))
  "Generate an all-purpose sequence number."
  (+ x (incf *sequence-number*)))
   
;;; Hooks

(defun add-hook (hook func)
  "Hooks are special variables whose names are of the form
`*foo-hook*' and whose values are lists of functions taking no
arguments. The functions of a given hook are all invoked (in list
order) whenever the hook is run with `run-hook'.

This function arranges for FUNC to be invoked whenever HOOK is triggered with
`run-hook'. The function should have no arguments."
  (pushnew func (symbol-value hook)))

(defun remove-hook (hook func)
  "Stop calling FUNC whenever HOOK is triggered."
  (setf (symbol-value hook)
	(delete func (symbol-value hook))))

(defun run-hook (hook)
  "Call all the functions in HOOK, in list order."
  (dolist (func (symbol-value hook))
    (funcall func)))

;;; Vector utility macro 

(defmacro do-cells ((var expr) &body body)
  "Execute the forms in BODY with VAR bound successively to the
elements of the vector produced by evaluating EXPR."
  (let ((counter (gensym))
	(vector (gensym)))
    `(progn
       (let* ((,var nil)
	      (,vector (progn ,expr)))
	 (when (vectorp ,vector)
	   (let ((,counter (fill-pointer ,vector)))
	     (decf ,counter)
	     (loop while (>= ,counter 0) 
		   do (setf ,var (aref ,vector ,counter))
		   (progn (decf ,counter)
			  (when ,var ,@body)))))))))

;;; The active blocks list 

;; see also blocks.lisp

(defvar *blocks* nil "List of active block objects. 
These blocks receive input events and are rendered to the screen by
the console. See also `send-event'.

Do not set this variable directly from a project; instead, call
`install-blocks'.")

(defun show-blocks ()
  "Draw the active blocks to the screen."
  (dolist (block *blocks*)
    (send nil :draw block sdl:*default-surface*)))

(defun install-blocks (&rest blocks)
  "User-level function for setting the active block set. Note that
IOFORMS may override the current block set at any time for system menus
and the like."
  (setf *blocks* blocks))

(defun install-block (block)
  (unless (find block *blocks*)
    (setf *blocks* (nconc *blocks* (list block))))
  (show-blocks))

(defun uninstall-block (block)
  (setf *blocks* (delete block *blocks* :test #'eq))
  (show-blocks))

;;; "Classic" key repeat

(defun enable-classic-key-repeat (delay interval)
  ;; (let ((delay-milliseconds (truncate (* delay (/ 1000.0 *frame-rate*))))
  ;; 	(interval-milliseconds (truncate (* interval (/ 1000.0 *frame-rate*)))))
    (sdl:enable-key-repeat delay interval))

(defun disable-classic-key-repeat ()
  (sdl:disable-key-repeat))

;;; "Held Keys" key repeat emulation

(defvar *key-table* (make-hash-table :test 'equal))

(defvar *held-keys* nil)

(defun enable-held-keys (&rest args)
  "Enable key repeat on every frame when held. Arguments are ignored
for backward-compatibility."
  (when args 
    (message "Warning. DELAY and INTERVAL arguments to IOFORMS:ENABLE-HELD-KEYS are deprecated and ignored."))
  (setf *key-table* (make-hash-table :test 'equal))
  (setf *held-keys* t))

(defun disable-held-keys ()
  "Disable key repeat."
  (setf *held-keys* nil))
;;  (sdl:disable-key-repeat))

(defun hold-event (event)
  (when (null (gethash event *key-table*)) 
    (setf (gethash event *key-table*) 0)))

(defun release-held-event (event)
  (setf (gethash event *key-table*) -1))

(defun send-held-events ()
  (unless (null *key-table*)
    (maphash #'(lambda (event counter)
		 (send-event event)
		 ;; A counter value of -1 means that the key release
		 ;; happened before the event had a chance to be sent.
		 ;; These must be removed immediately after sending once.
		 (if (minusp counter)
		     (remhash event *key-table*)
		     ;; Otherwise, keep counting how many frames the
		     ;; key is held for.
		     (when (numberp (gethash event *key-table*))
		       (incf (gethash event *key-table*)))))
	     *key-table*)))

(defun break-events (event)
  (labels ((break-it (event2 ignore)
	     (when (intersection event event2 :test 'equal)
	       (message "Breaking ~S due to match with ~S." event2 event)
	       (remhash event2 *key-table*))))
    (maphash #'break-it *key-table*)))

;;; Event handling and blocks

(defvar *event-handler-function* nil
  "Function to be called with keypress events. Keyboard, mouse,
joystick, and timer events are represented as event lists of the form:

  (STRING . MODIFIERS)

where STRING is a string representing the key or button, and MODIFIERS
is a list of key modifier symbols like :shift, :control, :alt, and so
on.

The modifier list is sorted; thus, events can be compared for
equality with `equal' and used as hashtable keys..")

(defun send-event (event)
  (if (null *event-handler-function*)
      (error "No event handler function installed. 
Please set the variable ioforms:*event-handler-function*")
      (funcall *event-handler-function* event)))

(defun normalize-event (event)
  "Convert EVENT to a normal form suitable for `equal' comparisons."
  (setf (rest event)
	(sort (remove-duplicates (delete nil (rest event)))
	      #'string< :key #'symbol-name))
  event)

;;; Translating SDL key events into IOFORMS event lists

;; see also keys.lisp

(defparameter *other-modifier-symbols* '(:button-down :button-up :axis))

(defun make-key-modifier-symbol (sdl-mod)
  "Translate from the SDL key modifier symbol SDL-MOD to our own
key event symbols."
  (if (or (member sdl-mod *joystick-button-symbols*)
	  (member sdl-mod *other-modifier-symbols*))
      sdl-mod
      (case sdl-mod
	(:SDL-KEY-MOD-NONE nil)
	(:SDL-KEY-MOD-LSHIFT :shift)
	(:SDL-KEY-MOD-RSHIFT :shift)
	(:SDL-KEY-MOD-LCTRL :control)
	(:SDL-KEY-MOD-RCTRL :control)
	(:SDL-KEY-MOD-LALT :alt)
	(:SDL-KEY-MOD-RALT :alt)
	(:SDL-KEY-MOD-LMETA :meta)
	(:SDL-KEY-MOD-RMETA :meta)
	;; for compatibility:
	(:SDL-KEY-NONE nil)
	(:SDL-KEY-LSHIFT :shift)
	(:SDL-KEY-RSHIFT :shift)
	(:SDL-KEY-LCTRL :control)
	(:SDL-KEY-RCTRL :control)
	(:SDL-KEY-LALT :alt)
	(:SDL-KEY-RALT :alt)
	(:SDL-KEY-LMETA :meta)
	(:SDL-KEY-RMETA :meta)
	;; fix for windows
	(:SDL-KEY-MOD-NUM nil)
	(:SDL-KEY-CAPS :caps-lock)
	(:SDL-KEY-MOD-CAPS :caps-lock) ;; macintosh 
	(:SDL-KEY-MODE nil)
	(:SDL-KEY-MOD-MODE :mode)
	(:SDL-KEY-RESERVED nil)
	)))
  
(defun make-key-string (sdl-key)
  "Translate from :SDL-KEY-X to the string \"X\"."
  (let ((prefix "SDL-KEY-"))
    (subseq (symbol-name sdl-key)
            (length prefix))))

(defun make-event (sdl-key sdl-mods)
  "Create a normalized event out of the SDL data SDL-KEY and SDL-MODS.
The purpose of putting events in a normal form is to enable their use
as hash keys."
  (message "SDL KEY AND MODS: ~A" (list sdl-key sdl-mods))
  (normalize-event
   (cons (if (eq sdl-key :joystick) 
	     "JOYSTICK"
	     (if (eq sdl-key :axis) 
		 "AXIS"
		 (make-key-string sdl-key)))
	 (mapcar #'make-key-modifier-symbol
		 (cond ((keywordp sdl-mods)
			(list sdl-mods))
		       ((listp sdl-mods)
			sdl-mods)
		       ;; catch apparent lispbuilder-sdl bug?
		       ((eql 0 sdl-mods)
			nil))))))

;;; Joystick support (gamepad probably required)

(defvar *joystick-button-symbols*
  '(:a :b :x :y :left :right :up :down :select :start))

(defparameter *generic-joystick-mapping*
  '((0 . :button-0)
    (1 . :button-1)
    (2 . :button-2)
    (3 . :button-3)
    (4 . :button-4)
    (5 . :button-5)
    (6 . :button-6)
    (7 . :button-7)
    (8 . :button-8)
    (9 . :button-9)
    (10 . :button-10)
    (11 . :button-11)
    (12 . :button-12)
    (13 . :button-13)
    (14 . :button-14)
    (15 . :button-15)
    (16 . :button-16)
    (17 . :button-17)
    (18 . :button-18)
    (19 . :button-19)
    (20 . :button-20)))

(defvar *joystick-dead-zone* 2000)

(defvar *joystick-axis-mapping* '((0 :left :right)
				  (1 :up :down)))

(defun axis-value-to-direction (axis value)
  (let ((entry (assoc axis *joystick-axis-mapping*)))
    (if entry 
	(if (plusp value)
	    (second (cdr entry))
	    (when (minusp value)
	      (first (cdr entry)))))))

(defvar *joystick-axis-values* (make-array 100 :initial-element 0))

(defun do-joystick-axis-event (axis value state)
  (send-event (make-event :axis 
			      (list (axis-value-to-direction axis value)
				    state))))
	
(defun update-joystick-axis (axis value)
  (let ((state (if (< (abs value) *joystick-dead-zone*)
		   :button-up :button-down)))
    (setf (aref *joystick-axis-values* axis) value)))

(defun poll-joystick-axis (axis)
  (aref *joystick-axis-values* axis))

(defvar *joystick-mapping* *generic-joystick-mapping*)

(defun translate-joystick-button (button)
  (cdr (assoc button *joystick-mapping*)))

(defun symbol-to-button (sym)
  (let ((entry (some #'(lambda (entry)
			 (when (eq sym (cdr entry))
			   entry))
		     *joystick-mapping*)))
    (when entry 
      (car entry))))

(defvar *joystick-device* nil)

(defvar *joystick-buttons* nil
  "The nth element is non-nil when the nth button is pressed.")

(defvar *joystick-position* nil "Current position of the joystick, as a direction keyword.")

(defun reset-joystick ()
  "Re-open the joystick device and re-initialize the state."
  (setf *joystick-device* (sdl-cffi::sdl-joystick-open 0))
  (setf *joystick-buttons* (make-array 100 :initial-element nil))
  (setf *joystick-position* :here))

(defun update-joystick (button state)
  "Update the table in `*joystick-buttons*' to reflect the STATE of
the BUTTON. STATE should be either 1 (on) or 0 (off)."
  (setf (aref *joystick-buttons* button) (ecase state
					   (1 t)
					   (0 nil)))
  (let ((sym (translate-joystick-button button)))
    (labels ((pressed (sym) 
	       (let ((b (symbol-to-button sym)))
		 (when (integerp b)
		   (aref *joystick-buttons* b)))))
      (setf *joystick-position* 
	    (or (cond ((and (pressed :up) (pressed :right))
		       :northeast)
		      ((and (pressed :up) (pressed :left))
		       :northwest)
		      ((and (pressed :down) (pressed :right))
		       :southeast)
		      ((and (pressed :down) (pressed :left))
		       :southwest)
		      ((pressed :up)
		       :north)
		      ((pressed :down)
		       :south)
		      ((pressed :right)
		       :east)
		      ((pressed :left)
		       :west))
		:here)))))

(defun poll-joystick-button (button)
  "Return 1 if the button numbered BUTTON is pressed, otherwise 0."
  (sdl-cffi::sdl-joystick-get-button *joystick-device* button))

(defun poll-all-buttons ()
  (dolist (entry *joystick-mapping*)
    (destructuring-bind (button . symbol) entry
      (update-joystick button (poll-joystick-button button)))))

(defun generate-button-events ()
  (let ((button 0) state sym)
    (loop while (< button (length *joystick-buttons*))
	  do (setf state (aref *joystick-buttons* button))
	     (setf sym (translate-joystick-button button))
	     (when (and state sym)
	       (send-event (make-event :joystick sym)))
	     (incf button))))

;;; The active world

(defvar *world* nil 
"The current world object. Only one may be active at a time. See also
worlds.lisp. Cells are free to send messages to `*world*' at
any time, because it is always bound to the world containing the cell
at the time the cell method is run.")

;;; Timing

(defun get-ticks ()
  (sdl:sdl-get-ticks))

(defvar *dt* 20)

(defvar *timestep-function* nil)

(defun do-timestep (&rest args) 
  (incf *timesteps*)
  (when (functionp *timestep-function*)
    (apply *timestep-function* args)))

(defvar *frame-rate* 30 "The intended frame rate of the game.")

(defun set-frame-rate (&optional (rate *frame-rate*))
  "Set the frame rate for the game."
  (message "Setting frame rate to ~S" rate)
  (setf (sdl:frame-rate) rate))

(defparameter *timesteps* 0)

(defvar *clock* 0 "Number of frames until next timer event.")

(defvar *timer-p* nil "Non-nil if timer events are actually being sent.")

(defun enable-timer ()
  "Enable timer events. The next scheduled event will be the first sent."
  (setf *timer-p* t))

(defun disable-timer ()
  "Disable timer events."
  (setf *timer-p* nil))

(defvar *timer-event* (list nil :timer) 
  "Since all timer events are identical, this is the only one we need.")

(defvar *timer-interval* 15 
"Number of frames to wait before sending each timer event.
Set this to 0 to get a timer event every frame.
Don't set this yourself; use `set-timer-interval'.")

(defun set-timer-interval (interval)
  "Set the number of frames to wait before sending each timer event.
Set it to 0 to get a timer event every frame."
  (setf *timer-interval* interval))

;;; Screen dimensions

(defvar *resizable* nil)

(defparameter *resize-hook* nil)

(defvar *screen-width* 640 "The width (in pixels) of the game
window. Set this in the game startup file.")

(defun set-screen-width (width)
  (setf *screen-width* width))

(defvar *screen-height* 480 "The height (in pixels) of the game
window. Set this in the game startup file.")

(defun set-screen-height (height)
  (setf *screen-height* height))

;;; The main loop of IOFORMS

(defvar *after-initialization-hook* nil)

(defvar *project* "standard")

(defvar *quitting* nil)

(defvar *fullscreen* nil "When non-nil, attempt to use fullscreen mode.")

(defvar *window-title* "ioforms")

(defvar *window-position* :center
  "Controls the position of the game window. Either a list of coordinates or the symbol :center.")

(defun run-main-loop ()
  "Initialize the console, open a window, and play.
We want to process all inputs, update the game state, then update the
display."
  (run-hook '*initialization-hook*)
  (let ((fps (make-instance 'sdl:fps-mixed :dt *dt*)))
    (cond (*fullscreen*
	   (sdl:window *screen-width* *screen-height*
		       :fps fps 
		       :title-caption *window-title*
		       :flags sdl:SDL-FULLSCREEN
		       :position *window-position*))
	  (*resizable*
	   	   (sdl:window *screen-width* *screen-height*
		       :fps fps 
		       :title-caption *window-title*
		       :flags sdl:SDL-RESIZABLE
		       :position *window-position*))
	  (t (sdl:window *screen-width* *screen-height*
			 :fps fps
			 :title-caption *window-title*
			 :position *window-position*)))
    (set-frame-rate *frame-rate*)
    (reset-joystick)
    (sdl:clear-display sdl:*black*)
    (show-blocks)
    (sdl:update-display)
    (run-hook '*after-initialization-hook*)
    (let ((events-this-frame 0))
      (sdl:with-events ()
	(:quit-event () (prog1 t))
	(:video-resize-event (:w w :h h)  
			     (setf *screen-width* w
				   *screen-height* h)
			     (run-hook '*resize-hook*)
			     (sdl:window w h :fps fps :title-caption *window-title*
					 :flags sdl:SDL-RESIZABLE
					 :position *window-position*))
	(:mouse-motion-event (:state state :x x :y y :x-rel x-rel :y-rel y-rel)
			     (let ((block (hit-blocks x y *blocks*)))
			       (when (and block (has-method :mouse-move block))
				 (/mouse-move block x y))))
	(:mouse-button-down-event (:button button :state state :x x :y y)
				  (let ((block (hit-blocks x y *blocks*)))
				    (when (and block (has-method :mouse-down block))
				      (/mouse-down block x y button))))
	(:mouse-button-up-event (:button button :state state :x x :y y)
				  (let ((block (hit-blocks x y *blocks*)))
				    (when (and block (has-method :mouse-up block))
				      (/mouse-up block x y button))))
	(:joy-button-down-event (:which which :button button :state state)
				(when (assoc button *joystick-mapping*)
				  (update-joystick button state)
				  (send-event (make-event :joystick
							      (list (translate-joystick-button button) 
								    :button-down)))))
	(:joy-button-up-event (:which which :button button :state state)  
			      (when (assoc button *joystick-mapping*)
				(update-joystick button state)
				(send-event (make-event :joystick
							    (list (translate-joystick-button button) 
								  :button-up)))))
	(:joy-axis-motion-event (:which which :axis axis :value value)
				(update-joystick-axis axis value))
	(:video-expose-event () (sdl:update-display))
	(:key-down-event (:key key :mod-key mod)
			 (let ((event (make-event key mod)))
			   (if *held-keys*
			       (hold-event event)
			       (send-event event))))
	(:key-up-event (:key key :mod-key mod)
		       (when *held-keys*
			 (let* ((event (make-event key mod))
				(entry (gethash event *key-table*)))
			   (if (numberp entry)
			       (if (plusp entry)
				   (progn (message "Removing entry ~A:~A" event entry)
					  (remhash event *key-table*))
				   (when (zerop entry)
				     ;; This event hasn't yet been sent,
				     ;; but the key release happened
				     ;; now. Mark this entry as pending
				     ;; deletion.
				     (release-held-event event)))
			       (break-events event)))))
	(:idle ()
	       (if *timer-p*
		   (if (zerop *clock*)
		       (progn 
			 (sdl:clear-display sdl:*black*)
			 ;; send held events
			 (when *held-keys*
			   (send-held-events))
			 ;; send timer event
			 (send-event *timer-event*)
			 ;; send any joystick button events
			 ;; (poll-all-buttons)
			 ;;  (generate-button-events)
			 ;; update display
			 (show-blocks)
			 (sdl:update-display)
			 (setf *clock* *timer-interval*))
		       (decf *clock*))
		   ;; clean this up. these two cases aren't that different.
		   (progn 
		     (sdl:with-timestep ()
		       (do-timestep))
		     (sdl:clear-display sdl:*black*)
		     (when *held-keys* (send-held-events)) ;; TODO move this to do-timestep?
		     (show-blocks) 
		     (sdl:update-display))))))))
  
  
;;; The .ioformsrc user init file

(defvar *initialization-hook* nil 
"This hook is run after the IOFORMS console is initialized.
Set system parameters and other settings here.")

(defparameter *user-init-file-name* "ioforms.ini")

(defun load-user-init-file ()
  (let ((file (merge-pathnames (make-pathname :name *user-init-file-name*)
			       (ioforms-directory))))
    (when (probe-file file)
      (load file))))

(defparameter *user-keyboard-layout* :qwerty)

(defparameter *use-sound* t "Non-nil (the default) is to use sound. Nil disables sound.")

;;; IOF resource interchange files

(defparameter *iof-file-extension* ".iof"
"IOF is a simple Lisp data interchange file format readable and
writable by both Emacs Lisp and Common Lisp. An IOF file can contain
one or more data resources. A 'resource' is an image, sound, text,
font, lisp program, or other data whose interpretation is up to the
client.

An IOF resource can be either self-contained, or point to an
external file for its data.

A 'resource record' defines a resource. A resource record is a
structure with the following elements:

 :NAME    A string; the name of the resource.
          The colon character : is reserved and used to specify 
          resource transformations; see below.
 :TYPE    A keyword symbol identifying the data type.
          Corresponding handlers are the responsibility of the client.
          See also `*resource-handlers*' and `load-resource'.

          The special type :iof is used to load the iof file
          specified in :FILE, from (optionally) another project
          whose name is given in :DATA.

          The special type :alias is used to provide multiple names
          for a resource. The :DATA field contains the name of the
          target resource. This name can specify resource
          transformations, see below. 

 :PROPERTIES  Property list with extra data; for example :copyright,
              :license, :author. 
              The special property :AUTOLOAD, when non-nil causes
              the resource to be loaded automatically upon startup 
              (the default is to load resources on demand.)

 :FILE    Name of file to load data from, if any. 
          Relative to directory of IOF file.
 :DATA    Lisp data encoding the resource itself, if any.

In memory, these will be represented by resource structs (see below).
On disk, it's Lisp data printed as text. This text should compress very
well.

The string '()' is a valid .IOF file; it contains no resources.")

(defstruct resource 
  name type properties file data object modified-p)

;; The extra `object' field is not saved in .IOF files; it is used to
;; store driver-dependent loaded resources (i.e. SDL image surface
;; objects and so on). This is used in the resource table.
;; The modified-p field is likewise not stored. 

(defun resource-to-plist (res)
  "Convert the resource record RES into a property list.
This prepares it for printing as part of a IOF file."
  (list :name (resource-name res)
	:type (resource-type res)
	:properties (resource-properties res)
	:file (resource-file res)
	:data (resource-data res)
	:object nil))

;; First we need routines to read and write raw s-expressions to and
;; from text files.

(defconstant *keyword-package* (find-package :keyword))

(defun write-sexp-to-file (filename sexp)
  (message "Writing data to file ~S" filename)
  (with-open-file (file filename :direction :output 
			:if-exists :overwrite
			:if-does-not-exist :create)
    (let ((*package* *keyword-package*))
      (format file "~S" sexp)))
  (message "Writing data to file ~S... Done." filename))

(defun read-sexp-from-file (filename)
  (message "Reading data from ~A..." filename)
  (with-open-file (file filename :direction :input)
    (prog1 (read file)
      (message "Reading data from ~A... Done." filename))))

;; Now tie it all together with routines that read and write
;; collections of records into IOF files.

(defun write-iof (filename resources)
  "Write the RESOURCES to the IOF file FILENAME."
  (write-sexp-to-file filename (mapcar #'resource-to-plist resources)))

(defun read-iof (filename)
  "Return a list of resources from the IOF file FILENAME."
  (mapcar #'(lambda (plist)
	      (apply #'make-resource plist))
	  (read-sexp-from-file filename)))

;;; Resources and projects

(defvar *resources* nil 
  "A hash table mapping resource names to resource records. All loaded
resources go in this one hash table.

The `resource table' maps resource names to their corresponding
records. `Indexing' a resource means that its resource record is added
to the resource table. `Loading' a resource means that any associated
driver-dependent object (SDL image surface, audio buffer object, etc)
is created, which may involve reading an image or sound file from the
disk. This value is stored into the OBJECT field of the resource
record upon loading; see `load-resource'.

The loading operation may be driver-dependent, so each resource
type (i.e. :image, :text, :sound) is handled by its own plugin
function (see `*resource-handlers*').

`Finding' a resource means looking up its record in the resource
table, and loading the resource if it hasn't been loaded already.
A lookup failure results in an error. See `find-resource'.

A `project' is a directory full of resource files. The name of the
project is the name of the directory. Each project must contain a
file called {project-name}.iof, which should contain an index of
all the project's resources. Multiple projects may be loaded at one
time. In addition the special resource `.startup' will be loaded;
if this is of type :lisp, the startup code for your game can go in
that external lisp file.")

(defun initialize-resource-table ()
  "Create a new empty resource table."
   (setf *resources* (make-hash-table :test 'equal)))

(defun index-resource (resource)
  "Add the RESOURCE's record to the resource table.
If a record with that name already exists, it is replaced.  However,
if the resource is an :alias, just the string name of the target
resource is stored; see also `find-resource'."
  (let ((val (if (eq :alias (resource-type resource))
		 (resource-data resource)
		 resource)))
    (setf (gethash (resource-name resource)
		   *resources*) 
	  val)))

;;; Opening and saving projects

(defvar *project* nil "The name of the current project.")

(defvar *project-path* nil "The pathname of the currently opened project. 
This is where all saved objects are stored.")

(defvar *after-open-project-hook* nil)

(defvar *project-package-name* nil)

(defparameter *projects-directory-name* "projects")

(defvar *executable* nil)

(defun project-package-name (&optional (project-name *project*))
  (or *project-package-name* (make-keyword project-name)))

(defun make-directory-maybe (directory)
  (ensure-directories-exist (make-pathname :name "NAME" :type "TYPE"
					   :defaults directory)))

(defun ioforms-directory ()
  (if *executable*
      (make-pathname :directory 
		     (pathname-directory 
		      (car #+sbcl sb-ext:*posix-argv*
			   #+clozure ccl:*command-line-argument-list*)))
      (make-pathname :directory 
		     (pathname-directory 
		      (make-pathname
		       :host (pathname-host #.(or *compile-file-truename*
						  *load-truename*))
		       :device (pathname-device #.(or *compile-file-truename*
						      *load-truename*))
		       :directory (pathname-directory #.(or *compile-file-truename*
							    *load-truename*)))))))

(defun projects-directory ()
  (make-pathname :name *projects-directory-name* 
		 :defaults (ioforms-directory)))

(defun default-project-directories () 
  (let ((projects (projects-directory)))
    (make-directory-maybe projects)
    (list (ioforms-directory) projects)))

(defvar *project-directories* nil
  "List of directories where IOFORMS will search for projects.
Directories are searched in list order.")

(defun search-project-path (project-name)
  "Search the `*project-directories*' path for a directory with the
name PROJECT-NAME. Returns the pathname if found, otherwise nil."
  (let ((dirs *project-directories*))
    (message "Probing directories ~S..." dirs)
    (or 
     (loop 
       for dir in dirs for path
	 = (probe-file (make-pathname :directory 
				      (append (pathname-directory
					       dir) (list project-name))
			    :defaults dir))
       when path return path)
     (error "Cannot find project ~s in paths ~S. 
You must set the variable IOFORMS:*PROJECT-DIRECTORIES* in the configuration file ~~/.ioformsrc
Please see the included file BINARY-README for instructions."
	    project-name dirs))))

(defun find-project-path (project-name)
  "Return the current project path."
  (assert *project*)
  (or *project-path*
      (search-project-path project-name)))

(defun find-project-file (project-name file)
  "Make a pathname for FILE within the project PROJECT-NAME."
  (merge-pathnames file (find-project-path project-name)))

(defun open-project (project &key (autoload t))
  "Load the project named PROJECT. Load any resources marked with a
non-nil :autoload property. This operation also sets the default
object save directory. See also `save-object-resource')."
  (setf *project* project
	*project-path* (search-project-path project)
	*pending-autoload-resources* nil)
  (assert *project-path*)
  (index-project project)
  (when autoload 
    (mapc #'load-resource (nreverse *pending-autoload-resources*)))
  (setf *pending-autoload-resources* nil)
  ;; now load any objects
  (let ((object-index-file (find-project-file project (object-index-filename project))))
    (when (probe-file object-index-file)
      (message "Indexing saved objects from ~S" object-index-file)
      (index-iof project object-index-file)))
  (let ((package (find-package (project-package-name))))
    (when package
      (setf *package* package)))
  (run-hook '*after-open-project-hook*))

(defun directory-is-project-p (dir)
  "Test whether a {PROJECTNAME}.IOF index file exists in a directory."
  (let ((index-filename (concatenate 'string
				     (file-namestring dir)
				     *iof-file-extension*)))
    (probe-file (make-pathname :name index-filename
			       :directory (if (stringp dir)
					      dir
					      (namestring dir))))))

(defun find-projects-in-directory (dir)
  "Search DIR for projects and return a list of their names."
  (let ((dirnames (mapcar #'(lambda (s)
			      (subseq s 0 (1- (length s))))
			  (mapcar #'namestring
				  (directory (concatenate 'string (namestring dir) "/*/"))))))
    (remove-if-not #'directory-is-project-p dirnames)))

(defun find-all-projects ()
  (mapcar #'file-namestring
	  (mapcan #'find-projects-in-directory *project-directories*)))

(defvar *pending-autoload-resources* '())

(defun index-iof (project-name iof-file)
  "Add all the resources from the iof IOF-FILE to the resource
table. File names are relative to the project PROJECT-NAME."
  (let ((resources (read-iof iof-file)))
    (dolist (res resources)
      (if (eq :iof (resource-type res))
	  ;; we're including another iof file. if :data is specified,
	  ;; take this as the name of the project where to look for
	  ;; that iof file and its resources.
	  (let ((include-project (or (resource-data res) 
				    project-name)))
	    (index-iof include-project (find-project-file include-project
							(resource-file res))))
	  ;; we're indexing a single resource.
	  (progn
	    (index-resource res)
	    ;; change the file field into a full pathname, for resources
	    ;; that need to load data from an external file later.
	    (when (resource-file res)
	      (setf (resource-file res)
		    (merge-pathnames (resource-file res)
				     (find-project-path project-name))))
	    ;; save the resource name for later autoloading, if needed
	    (when (getf (resource-properties res) :autoload)
	      (push res *pending-autoload-resources*)))))))

(defun object-index-filename (project-name)
  (concatenate 'string project-name *iof-file-extension*))

(defun index-project (project-name)
  "Add all the resources from the project PROJECT-NAME to the resource
table."
  (let ((index-file (find-project-file project-name
				       (object-index-filename project-name))))
    (index-iof project-name index-file)))

;;; Standard resource names

(defvar *startup* ".startup")

(defvar *default-font* ".default-font")

;;; Creating, saving, and loading object resources in IOF files

;; See also the documentation string for `*iof-file-extension*'.

;; Object resources are IOF resources with type :object. These are
;; used to save serialized objects to disk and read them back
;; again. Each page is stored in one IOF file, containing a single
;; resource with the serialized data stored in the :DATA field of the
;; resource record. Page-names are resource-names, and therefore must
;; be unique within a given IOFORMS project. A page's IOF file is
;; stored in {PROJECTNAME}/{PAGENAME}.iof, and for a given project
;; these IOFs will all be included by
;; {PROJECTNAME}/{PROJECTNAME}-OBJECT-INDEX.IOF, which is an
;; automatically generated IOF index linking to all the serialized
;; page IOF files.

(defun make-object-resource (name object)
  "Make an object resource named NAME (a string) with the CLON object
OBJECT as the data."
  (message "Creating new object resource ~S." name)
  (let ((resource (make-resource :name name 
				 :type :object
				 :object object)))
    (prog1 resource
      (index-resource resource))))

(defun save-object-resource (resource &optional (project *project*))
  "Save an object resource to disk as {PROJECT-NAME}/{RESOURCE-NAME}.IOF."
  (let ((name (resource-name resource)))
    (setf (resource-data resource) (serialize (resource-object resource)))
    (write-iof (find-project-file project 
				 (concatenate 'string (resource-name resource)
					      *iof-file-extension*))
	       (list resource))
    (setf (resource-data resource) nil)))

(defun is-special-resource (resource)
  (string= "*" (string (aref (resource-name resource) 0))))

(defun make-resource-link (resource)
  (make-resource :type :iof 
		 :file (concatenate 'string
				    (resource-name resource)
				    *iof-file-extension*)))
  
(defun save-resource (name resource)
  (let ((pathname (resource-file resource))
	(link (make-resource-link resource)))
    (prog1 link 
      (if (eq :object (resource-type resource))
	  ;; we want to index them all, whether or not we save them all.
	  ;; make a link resource (i.e. of type :iof) to pull this in later
	  (save-object-resource resource)
	  ;; just a normal resource
	  (setf (resource-file link) (namestring pathname)
		(resource-data link) nil))
      ;; finally, mark the original as saved.
      (resource-modified-p resource) nil)))

(defun save-project (&optional force)
  (let (index)
    (labels ((save (name resource) 
	       (when (or force (resource-modified-p resource))
		 (push (save-resource name resource) index))))
      (maphash #'save *resources*)
      (write-iof (find-project-file *project* (object-index-filename *project*)) 
		 (nreverse index)))))

(defun save-everything ()
  (save-project :force))

(defparameter *export-formats* '(:archive :application))

;; (defun export-archive (pathname)

;; (defun export-application

;; (defun export-project (format)

;;;  Resource object loading handlers

(defun load-object-resource (resource)
  "Loads a serialized :OBJECT resource from the Lisp data in the 
:DATA field of the RESOURCE argument. Returns the rebuilt object. See
also the documentation for DESERIALIZE."
  (let ((object (deserialize (resource-data resource))))
    (assert (object-p object))
    (setf (resource-data resource) nil) ;; no longer needed
    object))

(defun load-image-resource (resource)
  "Loads an :IMAGE-type iof resource from a :FILE on disk."
  (sdl-image:load-image (namestring (resource-file resource))
			:alpha 255))
  
(defun load-sprite-sheet-resource (resource)
  "Loads a :SPRITE-SHEET-type iof resource from a :FILE on disk. Looks
for :SPRITE-WIDTH and :SPRITE-HEIGHT properties on the resource to
control the size of the individual frames or subimages."
  (let* ((image (load-image-resource resource))
	 (props (resource-properties resource))
	 (w (or (getf props :width)
                (image-width image)))
	 (h (or (getf props :height)
                (image-height image)))
	 (sw (getf props :sprite-width))
	 (sh (getf props :sprite-height))
	 (sprite-cells (loop for y from 0 to (- h sh) by sh
			     append (loop for x from 0 to (- w sw) by sw
					  collect (list x y sw sh)))))
    (setf (sdl:cells image) sprite-cells)
    (setf (getf props :sprite-cells) sprite-cells)
    image))

(defun load-bitmap-font-resource (resource)
  (let ((props (resource-properties resource)))
    (if (null props)
	(error "Must set properties for bitmap font.")
	(destructuring-bind (&key width height character-map color-key) props
	  (sdl-gfx:initialise-font (make-instance 'SDL:simple-font-definition
						  :width width :height height
						  :character-map character-map
						  :color-key (apply #'sdl:color color-key)
						  :filename (resource-file resource)
						  :pad-x 0 :pad-y 0))))))
    
(defun load-text-resource (resource)
  (with-open-file (file (resource-file resource)
			:direction :input
			:if-does-not-exist nil)
    (loop for line = (read-line file nil)
	  while line collect line)))

(defun load-formatted-text-resource (resource)
  (read-sexp-from-file (resource-file resource)))
    
(defun load-lisp-resource (resource)
  (let* ((source (resource-file resource))
	 (fasl (compile-file-pathname source)))
    ;; do we need recompilation?
    (if (probe-file fasl)
    	(if (> (file-write-date source)
    	       (file-write-date fasl))
	    ;; recompile. 
    	    (load (compile-file source))
    	    ;; no, just load the fasl
    	    (load fasl))
	;; create the fasl for the first time. 
	(load (compile-file source)))))

(defun load-canvas-resource (resource)
  (destructuring-bind (&key width height background)
      (resource-properties resource)
    (let ((canvas (create-image width height)))
      (prog1 canvas
	(when background
	  (draw-box 0 0 width height 
		    ;; TODO support arbitrary rgb and other drawing commands
		    :stroke-color background
		    :color background
		    :destination canvas))))))

(defun load-color-resource (resource)
  (destructuring-bind (red green blue)
      (resource-data resource)
    (sdl:color :r red :g green :b blue)))

(defun load-font-resource (resource)
  (let ((font-name (string-upcase (concatenate 'string 
					       "*font-" 
					       (resource-data resource)
					       "*"))))
    (sdl:initialise-font (symbol-value (intern font-name :lispbuilder-sdl)))))

(defun load-ttf-resource (resource)
  (let* ((size (getf (resource-properties resource) :size))
	 (definition (make-instance 'sdl:ttf-font-definition
	 			    :filename (namestring (resource-file resource))
	 			    :size size)))
    (sdl:initialise-font definition)))

(defun load-music-resource (resource)
  (when *use-sound*
    (sdl-mixer:load-music (namestring (resource-file resource)))))

(defun load-sample-resource (resource)
  (when *use-sound*
    (let ((chunk (sdl-mixer:load-sample (namestring (resource-file resource)))))
      (prog1 chunk
	(when (resource-properties resource)
	  (destructuring-bind (&key volume) (resource-properties resource)
	    (when (numberp volume)
	      (setf (sdl-mixer:sample-volume chunk) volume))))))))

(defparameter *resource-handlers* 
  (list :image #'load-image-resource
	:lisp #'load-lisp-resource
	:object #'load-object-resource
	:sprite-sheet #'load-sprite-sheet-resource
	:color #'load-color-resource
	:music #'load-music-resource
	:bitmap-font #'load-bitmap-font-resource
	:text #'load-text-resource
	:formatted-text #'load-formatted-text-resource
	:sample #'load-sample-resource
	:canvas #'load-canvas-resource
	:ttf #'load-ttf-resource
	:font #'load-font-resource)
  "A property list mapping resource type keywords to handler functions.
Each function should accept one resource record, and return an
object (possibly driver-dependent). When a resource is loaded (with
`load-resource'), the appropriate handler is looked up, and invoked on
the resource record.  The return value is stored in the OBJECT field
of the record.")

;;; Transforming resources

(defvar *resource-transformation-delimiter* #\:)

(defun is-transformable-resource (name)
  (eq (aref name 0)
      *resource-transformation-delimiter*))

(defun next-transformation (name)
  (assert (is-transformable-resource name))
  (let ((delimiter-pos (position *resource-transformation-delimiter* 
				 (subseq name 1))))
    (when delimiter-pos 
      (let* ((*read-eval* nil)
	     (xform-command (subseq name 1 (1+ delimiter-pos))))
	(read-from-string (concatenate 'string 
				       "(" 
				       xform-command
				       ")"))))))

(defun next-source (name)
  (assert (is-transformable-resource name))
  (let ((delimiter-pos (position *resource-transformation-delimiter*
				 (subseq name 1))))
    (if (numberp delimiter-pos)
	(subseq name (1+ delimiter-pos))
	(subseq name 1))))

(defun rotate-image (resource degrees)
  (sdl:rotate-surface degrees :surface (resource-object resource)))

(defun subsect-image (resource x y w h)
(let ((image (sdl:copy-surface :cells (sdl:rectangle :x x :y y :w w :h h)
			       :surface (resource-object resource) :inherit t)))
  (sdl:set-surface-* image :x 0 :y 0)
  image))

(defun scale-image (image &optional (factor 1))
  "Return a scaled version of IMAGE, scaled by FACTOR.
Allocates a new image."
  (assert (integerp factor))
  (lispbuilder-sdl-gfx:zoom-surface factor factor
				    :surface (resource-object image)
				    :smooth nil))

(defvar *resource-transformations* 
  (list :rotate #'rotate-image
	:subimage #'subsect-image
	:scale #'scale-image))

;;; Main user-level functions for finding and loading resources.

(defun load-resource (resource)
  "Load the driver-dependent object of RESOURCE into the OBJECT field
so that it can be fed to the console."
  (message "Attempting to load resource ~S." (resource-name resource))
  (let ((handler (getf *resource-handlers* (resource-type resource))))
    (assert (functionp handler))
    ;; fill in the object field by invoking the handler, if needed
    (when (null (resource-object resource))
      (setf (resource-object resource)
	    (funcall handler resource)))
    (if (null (resource-object resource))
	(error "Failed to load resource ~S." (resource-name resource))
	(message "Loaded resource ~S with result type ~S." 
		 (resource-name resource)
		 (type-of (resource-object resource))))))

(defun find-resource (name &optional noerror)
  "Obtain the resource named NAME, performing any necessary loading
and/or transformations. Unless NOERROR is non-nil, signal an error
when NAME cannot be found."
  ;; can we find the resource straight off? 
  (let ((res (gethash name *resources*)))
    (cond ((resource-p res)
	   ;; yes, load-on-demand
	   (prog1 res
	     (when (null (resource-object res))
	       (load-resource res))))
	  ;; no, is it an alias?
	  ((stringp res)
	   ;; look up the real one and make the alias map to the real resource
	   (setf (gethash name *resources*) 
		 (find-resource res)))
	  ;; not found and not an alias. try to xform
	  ((null res)
	   (if (is-transformable-resource name)
	       ;; ok. let's xform and cache the result
	       (let ((xform (next-transformation name))
		     (source-name (next-source name)))
		 (setf (gethash name *resources*) 
		       (if (null xform)
			   (find-resource source-name)
			   (destructuring-bind (operation . arguments) xform
			     (let* ((xformer (getf *resource-transformations* 
						   (make-keyword operation)))
				    (source-res (find-resource source-name))
				    (source-type (resource-type source-res))
				    (source (resource-object source-res))
				    (xformed-resource (apply xformer source-res
							     arguments)))
			       (make-resource :name name 
					      :type source-type
					      :object xformed-resource))))))
	       ;; can't xform. 
	       (if noerror
		   nil
		   (error "Cannot find resource.")))))))

(defun find-resource-object (name &optional noerror)
  "Obtain the resource object named NAME, or signal an error if not
found."
  (let ((val (find-resource name noerror)))
    (if (resource-p val)
	(resource-object val)
	(if noerror nil (error "Resource ~S not found." name)))))

(defun find-resource-property (resource-name property)
  "Read the value of PROPERTY from the resource RESOURCE-NAME."
  (getf (resource-properties (find-resource resource-name))
	property))

(defun set-resource-modified-p (resource &optional (value t))
  (let ((res (find-resource resource)))
    (setf (resource-modified-p res) value)))

;;; Custom audio generation

(defvar *frequency* 44100)

(defvar *output-chunksize* 2048)

(defvar *output-channels* 2)

(defvar *sample-format* SDL-CFFI::AUDIO-S16LSB)

;;; Voice objects

(defvar *voices* nil)

(define-prototype voice () output)

(define-method initialize voice (&optional (size *output-chunksize*))
  (setf <output> (make-array size :element-type 'float :initial-element 0.0))
  (register-voice self))

(define-method get-output voice ()
  <output>)

(define-method play voice (&rest parameters))
(define-method halt voice ())
(define-method run voice ())

(defun register-voice (voice)
  (pushnew voice *voices* :test 'eq))

(defun unregister-voice (voice)
  (setf *voices*
	(delete voice *voices* :test 'eq)))

(defun cffi-sample-type (sdl-sample-type)
  (ecase sdl-sample-type
    (SDL-CFFI::AUDIO-U8 :uint8) ; Unsigned 8-bit samples
    (SDL-CFFI::AUDIO-S8 :int8) ; Signed 8-bit samples
    (SDL-CFFI::AUDIO-U16LSB :uint16) ; Unsigned 16-bit samples, in little-endian byte order
    (SDL-CFFI::AUDIO-S16LSB :int16) ; Signed 16-bit samples, in little-endian byte order
    ;; (SDL-CFFI::AUDIO-U16MSB nil) ; Unsigned 16-bit samples, in big-endian byte order
    ;; (SDL-CFFI::AUDIO-S16MSB nil) ; Signed 16-bit samples, in big-endian byte order
    (SDL-CFFI::AUDIO-U16 :uint16)  ; same as SDL(SDL-CFFI::AUDIO-U16LSB (for backwards compatability probably)
    (SDL-CFFI::AUDIO-S16 :int16) ; same as SDL(SDL-CFFI::AUDIO-S16LSB (for backwards compatability probably)
    (SDL-CFFI::AUDIO-U16SYS :uint16) ; Unsigned 16-bit samples, in system byte order
    (SDL-CFFI::AUDIO-S16SYS :int16) ; Signed 16-bit samples, in system byte order
    ))

(defun cffi-chunk-buffer (chunk)
  (sdl:fp chunk))

(defun buffer-length (buffer)
  (let ((type (cffi-sample-type *sample-format*)))
    (length (cffi:mem-ref buffer type))))

(defun convert-cffi-sample-to-internal (chunk)
  (let* ((input-buffer (cffi-chunk-buffer chunk))
	 (type (cffi-sample-type *sample-format*))
	 (size (length (cffi:mem-ref input-buffer type))))
    (assert (eq *sample-format* SDL-CFFI::AUDIO-S16LSB)) ;; for now
    (let ((output-buffer (make-array size)))
	(prog1 output-buffer
	  (dotimes (n size)
	    (setf (aref output-buffer n)
		  (/ (float (cffi:mem-aref input-buffer type n))
		     32768.0)))))))

(defun convert-internal-sample-to-cffi (input output &optional limit)
  (let ((type (cffi-sample-type *sample-format*)))
    (dotimes (n 128)
      (setf (cffi:mem-aref output type n)
	    (truncate (* (cffi:mem-aref input type n)
			 32768.0))))))

(defvar *buffer* (make-array 10000 :element-type 'float :initial-element 0.0))

(defvar *sample-generator* nil)

(defvar *foo* nil)

;; (defun music-mixer-callback (user output size)
;;   (setf *foo* t)
;;   (format t "XXXX ~S" *foo*))

  ;; (let ((type (cffi-sample-type *sample-format*)))
  ;;   (dotimes (n size)
  ;;     (setf (cffi:mem-aref output type n) 0))))

  ;; (when *sample-generator*
  ;;   (message "Generating samples")
  ;;   (funcall generator *buffer*)
  ;;   (message "Converting samples to output format...")
  ;;   (convert-internal-sample-to-cffi *buffer* output size)
  ;;   ))

;; (defun register-sample-generator (generator)
;;   (message "Registering sample generator...")
;;   (setf *sample-generator* generator)
;;   (sdl-mixer:register-music-mixer #'music-mixer-callback))

(defun mix-voices (output)
  (message "Mixing voices...")
  ;; create silence
  (dotimes (n *output-chunksize*)
    (setf (aref output n) 0.0))
  ;; mix in voices
  (dolist (voice *voices*)
    (/run voice)
    (let ((input (/get-output voice)))
      (dotimes (n *output-chunksize*)
	(incf (aref output n)
	      (aref input n))))))

;; (defun register-voice-mixer () 
;;   (message "Registering voice mixer...")
;;   (setf *voices* nil)
;;   (register-sample-generator #'mix-voices))

(defvar *buffer-cache* nil)

(defun initialize-buffer-cache ()
  (setf *buffer-cache* (make-hash-table :test 'eq)))

(defun get-sample-buffer (sample)
  (let ((chunk (if (stringp sample)
		   (find-resource-object sample)
		   sample)))
    ;; (when (null *buffer-cache*)
    ;;   (initialize-buffer-cache))
    ;; ;; is it cached?
    ;; (or (gethash chunk *sample-buffers*)
    ;; 	(setf (gethash chunk *sample-buffers*)
	      (convert-cffi-sample-to-internal chunk)))

;;; Regular music/sample functions

;; (defvar *sample-callback* nil)

;; (defun set-sample-callback (func)
;;   (assert (functionp func))
;;   (setf *sample-callback* func))

;; (defvar *music-callback* nil)

;; (defun set-music-callback (func)
;;   (assert (functionp func))
;;   (setf *music-callback* func))

(defvar *channels* 128 "Number of audio mixer channels to use.")

(defun set-music-volume (number)
  "Set the mixer music volume between 0 (silent) and 255 (full volume)."
  (when *use-sound*
    (setf (sdl-mixer:music-volume) number)))

(defun play-music (music-name &rest args)
  "Begin playing the music resource MUSIC-NAME. If the resource
MUSIC-NAME has the property :volume, its value is used as the volume
of the music."
  (when *use-sound*
    (let ((resource (find-resource music-name))
	  (volume (find-resource-property music-name :volume)))
      (assert (eq :music (resource-type resource)))
      (set-music-volume (or volume 255))
      (apply #'sdl-mixer:play-music 
	     (resource-object resource)
	     args))))

(defun seek-music (position)
  (sdl-mixer:music-position position))

(defun halt-music (fade-milliseconds)
  "Stop all music playing."
  (when *use-sound*
    (sdl-mixer:halt-music fade-milliseconds)))

(defun play-sample (sample-name &rest args)
  "When sound is enabled, play the sample resource SAMPLE-NAME."
  (when *use-sound*
    (let ((resource (find-resource sample-name)))
      (assert (eq :sample (resource-type resource)))
      (apply #'sdl-mixer:play-sample 
	     (resource-object resource)
	     args))))

(defun halt-sample (channel &rest args)
  (when *use-sound*
    (apply #'sdl-mixer:halt-sample :channel channel args)))

;;; Font operations

;; An IOF entry for a font looks like this: 

;; (:name ".default-font" 
;;        :type :font 
;;        :properties (:height 14 :width 7) 
;;        :data "7x14")

;; Or use type :ttf for Truetype fonts.

(defun font-height (font)
  (let ((resource (find-resource font)))
    (ecase (resource-type resource)
      (:font (find-resource-property font :height))
      (:ttf (sdl:get-font-height :font (resource-object resource))))))

(defun font-width (font)
  (let ((resource (find-resource font)))
    (ecase (resource-type resource)
      (:font (find-resource-property font :width))
      (:ttf (error "Cannot get width of a TTF font.")))))

(defun font-text-extents (string font)
  (let ((resource (find-resource font)))  
    (ecase (resource-type resource)
      (:font (* (length string)
		(font-width font)))
      (:ttf (values (sdl:get-font-size string :size :w :font (resource-object resource))
		    (sdl:get-font-height :font (resource-object resource)))))))

(defun draw-string-solid (string x y 
			  &key destination (font *default-font*) (color ".white"))
  (sdl:draw-string-solid-* string x y :surface destination :font (find-resource-object font)
			   :color (find-resource-object color)))

(defun draw-string-shaded (string x y &optional (foreground ".white") (background ".black")
			  &key destination (font *default-font*))
  (sdl:draw-string-shaded-* string x y (find-resource-object foreground)
			    (find-resource-object background)
			    :surface destination :font (find-resource-object font)))

(defun draw-string-blended (string x y &key (foreground ".black")
			    destination (font *default-font*))
  (sdl:draw-string-blended-* string x y 
			     :color (find-resource-object foreground)
			     :surface destination :font (find-resource-object font)))

;;; Standard colors

;; The X11 standard colors are loaded by default into the resource
;; table from the raw data in `*x11-color-data*'. See also rgb.lisp.

(defun initialize-colors ()
  "Load the X11 color data into the resource table."
  (dolist (color *x11-color-data*)
    (destructuring-bind (name red green blue) color
      (index-resource (make-resource :name (concatenate 'string "." name)
				     :type :color
				     :data (list red green blue))))))

;;; Creating and displaying images

;; The "driver dependent objects" for IOFORMS images are just SDL:SURFACE
;; objects. (The situation is the same for IOFORMS colors, fonts, and so
;; on). So long as the clients treat the driver-dependent resource
;; objects as opaque, this thin wrapper is sufficient.

;; Below are some image handling functions.

(defun create-image (width height)
  "Create a new IOFORMS image of size (* WIDTH HEIGHT)."
  (assert (and (integerp width) (integerp height)))
  (sdl:create-surface width height))

(defun draw-image (image x y &key (destination sdl:*default-surface*) (render-cell nil))
  "Draw the IMAGE at offset (X Y) on the image DESTINATION.
The default destination is the main window."
  (when render-cell
    (destructuring-bind (x0 y0 w h) render-cell
      (sdl:set-cell-* x0 y0 w h :surface image :index 0)))
  (sdl:draw-surface-at-* image x y :cell 0 :surface destination))

(defun draw-resource-image (name x y &key (destination sdl:*default-surface*) (render-cell nil))
  "Draw the image named by NAME at offset (X Y) on the image DESTINATION.
The default destination is the main window."
  (draw-image (find-resource-object name) x y :render-cell render-cell :destination destination))

(defun image-height (image)
  "Return the height in pixels of IMAGE."
  (let ((img (if (stringp image)
		 (find-resource-object image)
		 image)))
    (sdl:height img)))

(defun image-width (image)
  "Return the width in pixels of IMAGE."
  (let ((img (if (stringp image)
		 (find-resource-object image)
		 image)))
    (sdl:width img)))

;;; Drawing shapes and other primitives

(defun draw-box (x y width height		
		 &key (stroke-color ".white")
		 (color ".black")
		 destination)
  "Draw a filled rectangle at (X Y) of size (* WIDTH HEIGHT)."
  (sdl:draw-box-* x y width height :color (find-resource-object color)
		  :stroke-color (find-resource-object stroke-color)
		  :surface destination))

(defun draw-rectangle (x y width height
		       &key (color ".white")
		       destination)
  (sdl:draw-rectangle-* x y width height :color (find-resource-object color)
			:surface destination))

(defun draw-line (x0 y0 x1 y1 
		     &key 
		     (color ".white")
		     destination)
  (sdl:draw-line-* x0 y0 x1 y1 :surface destination :color (find-resource-object color)))

(defun draw-pixel (x y &key 
		   (color ".white")
		   destination)
  (sdl:draw-pixel-* x y :surface destination :color (find-resource-object color)))

(defun draw-circle (x y radius &key 
		   (color ".white")
		    destination)
  (sdl:draw-circle-* x y radius :surface destination :color (find-resource-object color)))

(defun draw-filled-circle (x y radius &key 
			   (color ".white")
			   destination)
  (sdl-gfx:draw-filled-circle-* x y radius :surface destination :color (find-resource-object color)))

(defun draw-aa-circle (x y radius &key 
		   (color ".white")
		    destination)
  (sdl-gfx:draw-aa-circle-* x y radius :surface destination :color (find-resource-object color)))

;;; Engine status

(defun quit (&optional shutdown)
  (when shutdown 
    (setf *quitting* t))
  (setf *project* nil)
  (sdl:push-quit-event))

(defun reset (&optional (project-name "standard"))
  (setf *quitting* nil)
  (setf *project* project-name)
  (sdl:push-quit-event))

(defvar *copyright-text*
"IOFORMS Visual Common Lisp Multimedia Authoring Tool
Copyright (C) 2006, 2007, 2008, 2009, 2010 by David T O'Toole
<dto@gnu.org> <dto1138@gmail.com>
http://ioforms.org/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

This program includes libSDL 1.2 (Simple Direct Media Layer), which is
provided under the terms of the GNU Lesser General Public License. See
also the file LIBSDL-LICENSE for details.

This program includes the DejaVu fonts family. See the file
./standard/DEJAVU-FONTS-LICENSE for more information.

")

(defvar *library-search-paths-setup-hook* nil)

(defun setup-library-search-paths ()
  (run-hook '*library-search-paths-setup-hook*)
  #+darwin (setf cffi:*foreign-library-directories*
                 (union cffi:*foreign-library-directories*
                        '(#P"/opt/local/lib" #P"/sw/lib/")
                        :test #'equal)))

(defvar *play-args* nil)

(defparameter *do-cffi-loading* t)

(defun do-cffi-loading ()
  (cffi:define-foreign-library sdl
      (:darwin (:or (:framework "SDL")
		    (:default "libSDL")))
      (:unix (:or "libSDL-1.2.so.0.7.2"
		  "libSDL-1.2.so.0"
		  "libSDL-1.2.so"
		  "libSDL.so"
		  "libSDL")))
    (cffi:use-foreign-library sdl)
    ;;
    (cffi:define-foreign-library sdl-mixer
      (:darwin (:or (:framework "SDL_mixer")
		    (:default "libSDL_mixer")))
      (:unix (:or "libSDL_mixer-1.2.so.0.7.2"
		  "libSDL_mixer-1.2.so.0"
		  "libSDL_mixer-1.2.so"
		  "libsdl_mixer-1.2.so.0.2.6" ;; eeebuntu?
		  "libSDL_mixer.so"
		  "libSDL_mixer")))
    (cffi:use-foreign-library sdl-mixer)
    ;;
    (cffi:define-foreign-library sdl-gfx
      (:darwin (:or (:framework "SDL_gfx")
		    (:default "libSDL_gfx")))
      (:unix (:or "libSDL_gfx-1.2.so.0.7.2"
		  "libSDL_gfx-1.2.so.0"
		  "libSDL_gfx-1.2.so"
		  "libSDL_gfx.so.4"
		  "libSDL_gfx.so.13"
		  "libSDL_gfx.so"
		  "libSDL_gfx")))
    (cffi:use-foreign-library sdl-gfx)
    ;;
    (cffi:define-foreign-library sdl-image
      (:darwin (:or (:framework "SDL_image")
		    (:default "libSDL_image")))
      (:unix (:or "libSDL_image-1.2.so.0.7.2"
		  "libSDL_image-1.2.so.0"
		  "libSDL_image-1.2.so.0.1.5" ;; eeebuntu?
		  "libSDL_image-1.2.so"
		  "libSDL_image.so"
		  "libSDL_image")))
    (cffi:use-foreign-library sdl-image))

(defun run (&rest args)
  (destructuring-bind (project &rest arguments) args
    (unwind-protect 
	 ;; see shell.lisp
	 (progn 
	   (setf *system* (clone (symbol-value '=system=)))
	   (when project 
	     (apply #'send nil :open-project *system* project arguments))
	   (send nil :run *system*))
      (sdl:quit-sdl))))

(defun ioforms (&rest args)
  (if (null args)
      (run nil)
      (apply #'run args)))
      
(defmacro defproject (module-name 
		   (&key title description
			 (prompt-prototype =prompt=)
			 timestep timestep-function
			 held-keys 
			 splash-image splash-function splash-music
			 screen-width screen-height
			 keybindings pages
			 &allow-other-keys)
 		   &body startup-forms)
  `(progn
     (ioforms:set-screen-height ,screen-height)
     (ioforms:set-screen-width ,screen-width)
     (setf ioforms:*timestep-function* ,timestep-function)
     (setf ioforms:*dt* ,timestep)
     ,@startup-forms))

;;; console.lisp ends here
