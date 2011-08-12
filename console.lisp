;;; console.lisp --- OS/device driver for BLOCKY

;; Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011  David O'Toole

;; Author: David O'Toole <dto@ioforms.org> <dto1138@gmail.com>
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

;; The "console" is the library which provides all BLOCKY system
;; services. Primitive operations such as opening a window,
;; displaying bitmaps, drawing lines, playing sounds, file access, and
;; keyboard/mouse/joystick input are handled here. 

;; Currently it uses the cross-platform SDL library (via
;; LISPBUILDER-SDL) as its device driver, and wraps the library for
;; use by the rest of BLOCKY.

;; http://lispbuilder.sourceforge.net/

;; The OpenGL support here is derived from code written by Bart Botta
;; for his excellent cl-opengl tutorials:
;; http://3bb.cc/tutorials/cl-opengl/

(in-package :blocky) 

(defvar *edit* nil "This is set to non-nil when the editor is being used.")

(defvar *pending-autoload-resources* '())

(defun random-choose (set)
  (nth (random (length set)) set))

(defmacro restartably (&body body)
  `(restart-case
      (progn ,@body)
    (continue () :report "Continue"  )))

(defvar *world* nil
"The current world object. Only one may be active at a time. See also
worlds.lisp. Sprites and cells are free to send messages to `*world*'
at any time, because `*world*' is always bound to the world containing
the object when the method is run.")

;;; Frame rate 

(defvar *frame-rate* 30 "The intended frame rate of the game.")

(defun set-frame-rate (&optional (rate *frame-rate*))
  "Set the frame rate for the game."
  (message "Setting frame rate to ~S" rate)
  (setf (sdl:frame-rate) rate))

;;; Keyboard state

;; (see also keys.lisp for the symbol listing)

(defun keyboard-id (key)
  "Look up the SDL symbol corresponding to the BLOCKY symbol KEY. See keys.lisp."
  (let* ((entry (find key *key-identifiers* :key #'first))
	 (entry2 (find (second entry) *sdl-key-identifiers* :key #'second)))
    (first entry2)))

(defun keyboard-mod (mod)
  "Look up the SDL symbol corresponding to the BLOCKY symbol MOD. See keys.lisp."
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

(defun holding-control ()
  (or (keyboard-modifier-down-p :lctrl)
      (keyboard-modifier-down-p :rctrl)))

;;; Logging messages to the standard output

(defparameter *message-logging* t)

(defun message-to-standard-output (message-string)
  (format t "~A" message-string)
  (fresh-line)
  (force-output))

(defparameter *message-function* #'message-to-standard-output)

(defun reset-message-function ()
  (setf *message-function* #'message-to-standard-output))

(defvar *message-hook-functions* nil)

(defvar *message-history* nil)

(defun message (format-string &rest args)
  "Print a log message by passing the arguments to
`*message-function'. When the variable `*message-logging*' is nil,
this output is disabled."
    (let ((message-string (apply #'format nil format-string args)))
      (when *message-logging* 
	(funcall *message-function* message-string))
      (dolist (hook *message-hook-functions*)
	(funcall hook message-string))
      (push message-string *message-history*)))

;;; Sequence numbers

(defvar *sequence-number* 0)

(defun genseq (&optional (x 0))
  "Generate an all-purpose sequence number."
  (+ x (incf *sequence-number*)))
   
;;; Hooks

(defun add-to-list (list element)
  (assert (and (symbolp list)
	       (not (null list))))
  (setf (symbol-value list)
	(append (symbol-value list)
		(list element))))
	 
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
  "Evaluate the forms in BODY with VAR bound successively to the
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

(defun hit-blocks (x y &optional (blocks *blocks*))
  (when blocks
    (labels ((try (b)
	       (send :hit b x y)))
      (let ((parent (find-if #'try blocks :from-end t)))
	(when parent
	  (try parent))))))

(defun draw-blocks ()
  "Draw the active blocks to the screen."
  (dolist (block *blocks*)
    (send :draw block)))

(defun install-blocks (&rest blocks)
  "User-level function for setting the active block set. Note that
BLOCKY may override the current block set at any time for system menus
and the like."
  (setf *blocks* blocks))

(defun start (block)
  (unless (find block *blocks* :test 'eq :key #'find-object)
    (setf *blocks* (adjoin block *blocks*))))

(defun stop (block)
  (setf *blocks* (delete block *blocks* :test #'eq :key #'find-object)))

;;; "Classic" key repeat

(defun enable-key-repeat (delay interval)
  (let ((delay-milliseconds (truncate (* delay (/ 1000.0 *frame-rate*))))
  	(interval-milliseconds (truncate (* interval (/ 1000.0 *frame-rate*)))))
    (sdl:enable-key-repeat delay-milliseconds interval-milliseconds)))

(defun disable-key-repeat ()
  (sdl:disable-key-repeat))

;;; "Held Keys" key repeat emulation
;; NOTE: THIS SECTION IS OBSOLETE.

(defvar *key-table* (make-hash-table :test 'equal))

(defvar *held-keys* nil)

(defun enable-held-keys (&rest args)
  "Enable key repeat on every frame when held. Arguments are ignored
for backward-compatibility."
  (when args 
    (message "Warning. DELAY and INTERVAL arguments to BLOCKY:ENABLE-HELD-KEYS are deprecated and ignored."))
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
  (labels ((break-it (event2 rest)
	     (declare (ignore rest))
	     (when (intersection event event2 :test 'equal)
	       (message "Breaking ~S due to match with ~S." event2 event)
	       (remhash event2 *key-table*))))
    (maphash #'break-it *key-table*)))

;;; Event handling and blocks

(defvar *pointer-x* 0)
(defvar *pointer-y* 0)

(defun send-to-blocks (event &optional (blocks *blocks*))
  (labels ((try (block)
	     (send :on-event block event)))
    (some #'try blocks)))

(defvar *event-handler-function* #'send-to-blocks
  "Function to be called with input events. Keyboard, mouse,
and joystick events are represented as 'event lists' of the form:

  (STRING . MODIFIERS)

where STRING is a string representing the key or button, and MODIFIERS
is a list of key modifier symbols like :shift, :control, :alt, and so
on.

The modifier list is sorted; thus, events can be compared for
equality with `equal' and used as hashtable keys.")

(defun send-event (event)
  (if (null *event-handler-function*)
      (error "No event handler function installed. 
Please set the variable blocky:*event-handler-function*")
      (funcall *event-handler-function* event)))

(defun normalize-event (event)
;    (:key #'identity :test 'equal)
  "Convert EVENT to a normal form suitable for `equal' comparisons."
  (let ((name (first event)))
    (cons (make-keyword name)
	  (sort (remove-duplicates (delete nil (rest event)))
		#'string< :key #'symbol-name))))

;;; Translating SDL input events into BLOCKY event lists

(defvar *joystick-button-symbols*
  '(:a :b :x :y ;; face buttons
    :left :right :up :down ;; directional pad
    :select :start ;; menu buttons
    :left-bumper :left-trigger :right-bumper :right-trigger)) ;; shoulder buttons

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
;  (message "SDL KEY AND MODS: ~A" (list sdl-key sdl-mods))
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

;;; Joystick support

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
    (declare (ignore state))
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

(defparameter *energy-dance-pad-mapping*
  '((12 . :up)
    (15 . :left)
    (13 . :right)
    (14 . :down)
    (0 . :downleft)
    (3 . :downright)
    (2 . :upleft)
    (1 . :upright)
    (8 . :select)
    (9 . :start)))

(defparameter *hyperkin-adapter-mapping* 
  '((4 . :up)
    (7 . :left)
    (5 . :right)
    (6 . :down)
    (12 . :downleft)
    (16 . :downright)
    (14 . :upleft)
    (13 . :upright)
    (0 . :select)
    (3 . :start)))

(defparameter *dragon-usb-joystick-mapping*
  '((2 . :a)
    (1 . :b)
    (3 . :x)
    (0 . :y)
    (6 . :left-bumper)
    (7 . :right-bumper)
    (8 . :select)
    (9 . :start)
    (4 . :left-trigger)
    (5 . :right-trigger)))

(defparameter *device-profiles* 
  '(("DragonRise Inc.   Generic   USB  Joystick  " :type :joystick :name "Generic USB Gamepad (DragonRise)" :mapping *dragon-usb-joystick-mapping*)
    ("USB Dance Pa" :name "Generic USB Dance Pad" :type :dance :mapping *energy-dance-pad-mapping*)
    ("GASIA CORP. PS(R) Gamepad Adaptor" :type :joystick :name "Generic PS2->USB Gamepad Adaptor (GASIA Corp.)" :mapping *hyperkin-adapter-mapping*)))

(defvar *joystick-device* nil)

(defvar *joystick-buttons* nil
  "The nth element is non-nil when the nth button is pressed.")

(defvar *joystick-position* nil "Current position of the joystick, as a direction keyword.")

(defun reset-joysticks ()
  "Re-open the joystick device and re-initialize the state."
  (setf *joystick-device* (sdl-cffi::sdl-joystick-open 0))
  (setf *joystick-buttons* (make-array 100 :initial-element nil))
  (setf *joystick-position* :here))

(defun find-device-profile (device-name)
  (let ((entry (assoc device-name *device-profiles* :test 'equal)))
    (when entry (cdr entry))))

(defun scan-for-devices ()
  (message "Scanning for connected devices...")
  (block scanning
    (dotimes (index (sdl:num-joysticks))
      (let ((device (sdl:sdl-joystick-name index)))
	(message "Checking joystick ~S, device name: ~S" index device)
	(let ((profile (find-device-profile device)))
	  (if (null profile)
	      (message "Could not find device profile for ~S. Continuing..." device)
	      (destructuring-bind (&key name mapping type) profile
		(message "Found device profile ~S for ~S." type name)
		(setf *joystick-mapping* mapping))))))))

(defun update-joystick (button state)
  "Update the table in `*joystick-buttons*' to reflect the STATE of
the BUTTON. STATE should be either 1 (on) or 0 (off)."
  (setf (aref *joystick-buttons* button) (ecase state
					   (1 t)
					   (0 nil)))
  (let ((sym (translate-joystick-button button)))
    (declare (ignore sym))
    (labels ((pressed (button-name) 
	       (let ((index (symbol-to-button button-name)))
		 (when (integerp index)
		   (aref *joystick-buttons* index)))))
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
      (declare (ignore symbol))
      (update-joystick button (poll-joystick-button button)))))

(defun generate-button-events ()
  (let ((button 0) state sym)
    (loop while (< button (length *joystick-buttons*))
	  do (setf state (aref *joystick-buttons* button))
	     (setf sym (translate-joystick-button button))
	     (when (and state sym)
	       (send-event (make-event :joystick sym)))
	     (incf button))))

(defparameter *joystick-motion-axes* '(0 1))
(defparameter *joystick-aiming-axes* '(3 2))
(defparameter *joystick-axis-size* 32768.0)
(defparameter *joystick-dead-zone* 6000)
(defparameter *joystick-motion-speed* 4.0)

(defun axis-as-float (axis)
  (/ (poll-joystick-axis axis)
     *joystick-axis-size*))

(defun axis-pressed-p (axis) 
  (< *joystick-dead-zone* (abs (poll-joystick-axis axis))))

(defparameter *trigger-buttons* 
  '(:fire :extension :switch-weapons :boost))

(defparameter *face-buttons*
  '(:x :y))

;;; Timing

(defun get-ticks ()
  (sdl:sdl-get-ticks))

(defvar *dt* 20)

(defun update-blocks ()
  (dolist (block *blocks*)
    (send :update block)))

(defvar *update-function* #'update-blocks)

(defvar *updates*)

(defun do-update (&rest args) 
  (incf *updates*)
  (when (functionp *update-function*)
    (apply *update-function* args)))

(defparameter *updates* 0)

;;; Processing once per update

(defvar *keys* nil "List of keywords of currently pressed keys.")

(defvar *mods* nil "List of keywords of currently pressed modifiers.")

(defvar *keyboard-update-number* 0)

(defun get-keys ()
  (if (= *keyboard-update-number* *updates*)
      (setf *keys* (keyboard-keys-down)
	    *mods* (keyboard-modifiers)
	    *keyboard-update-number* *updates*)
      (setf *keys* nil *mods* nil))
  (values *keys* *mods*))

;;; Screen dimensions

(defparameter *screen-width* 640 "Physical width of the window, in pixels.") 
(defparameter *screen-height* 480 "Physical height of the window, in pixels.")

;; The nominal size of of the window in pixels, in case we just want
;; to scale the scene to match the window instead of showing more of
;; the world. If these are the same as the `*screen-' settings
;; above, then more of the world will be shown when the window size
;; increases.
(defparameter *nominal-screen-width* 640 "Nominal width of the window, in pixels.")
(defparameter *nominal-screen-height* 480 "Nominal height of the window, in pixels.")

(defparameter *gl-screen-width* nil "Width of the window expressed in OpenGL coordinates.")
(defparameter *gl-screen-height* nil "Height of the window expressed in OpenGL coordinates.")

(defparameter *use-nominal-screen-size* nil
  "When non-nil, always show a fixed amount of the world when changing
window size. Otherwise (the default) one onscreen pixel equals one
unit of world space, so that more of the world shows if the window
becomes larger.")
 
(defparameter *z-near* -100)
(defparameter *z-far* 100)

(defun do-orthographic-projection ()
  "Configure OpenGL so that the screen coordinates go from (X0,Y0) at
 top left to ((+ X0 WIDTH) (+ Y0 HEIGHT)) at lower right."
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:viewport 0 0 *screen-width* *screen-height*)
  (if *use-nominal-screen-size*
      (setf *gl-screen-width* *nominal-screen-width*
	    *gl-screen-height* *nominal-screen-height*)
      (setf *gl-screen-width* *screen-width*
	    *gl-screen-height* *screen-height*))
  (gl:ortho 0 *gl-screen-width* *gl-screen-height* 0 *z-near* *z-far*))

(defun do-window (&optional (x0 0) (y0 0) (scale-x 1.0) (scale-y 1.0))
  ;; now move viewing volume
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:translate (- 0 x0)
		(- 0 y0)
		0)
  (gl:scale scale-x scale-y 1))

(defvar *resizable* t)

(defparameter *resize-hook* nil)

;;; The main loop of BLOCKY

(defvar *after-startup-hook* nil)

(defvar *project* nil)

(defvar *quitting* nil)

(defvar *fullscreen* nil "When non-nil, attempt to use fullscreen mode.")

(defvar *window-title* "blocky")

(defvar *window-position* :center
  "Controls the position of the game window. Either a list of coordinates or the symbol :center.")

(defun start-session ()
  "Initialize the console, open a window, and play.
We want to process all inputs, update the game state, then update the
display."
  (let ((fps (make-instance 'sdl:fps-mixed :dt *dt*)))
    (message "Creating OpenGL window...")
    (cond (*fullscreen*
	   (sdl:window *screen-width* *screen-height*
		       :fps fps 
		       :title-caption *window-title*
		       :flags (logior sdl:SDL-FULLSCREEN sdl:SDL-OPENGL)
		       :position *window-position*))
	  (*resizable*
	   	   (sdl:window *screen-width* *screen-height*
		       :fps fps 
		       :title-caption *window-title*
		       :flags (logior sdl:SDL-RESIZABLE sdl:SDL-OPENGL)
		       :position *window-position*))
	  (t (sdl:window *screen-width* *screen-height*
			 :fps fps
			 :flags sdl:SDL-OPENGL
			 :title-caption *window-title*
			 :position *window-position*)))
    ;; cl-opengl needs platform specific support to be able to load GL
    ;; extensions, so we need to tell it how to do so in lispbuilder-sdl
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    ;;
    (message "Creating OpenGL window... Done.")
    (message "SDL driver name: ~A" (sdl:video-driver-name))
    (set-frame-rate *frame-rate*)
    (reset-joysticks)
    (scan-for-devices)
    (do-orthographic-projection)
    (load-project-lisp "STANDARD")
    (run-hook '*after-startup-hook*)
    (message "Finished initializing Blocky for project ~A." *project*)
    (sdl:with-events ()
      (:quit-event () (prog1 t (sdl:quit-sdl :force t)))
      (:video-resize-event (:w w :h h)  
			   (setf *screen-width* w
				 *screen-height* h)
;			   (run-hook '*resize-hook*)
			   (sdl:resize-window w h :title-caption *window-title*
				       :flags (logior sdl:SDL-OPENGL sdl:SDL-RESIZABLE))
			   (do-orthographic-projection))
      (:mouse-motion-event (:x x :y y)
			   (setf *pointer-x* x *pointer-y* y)
			   (let ((block (hit-blocks x y *blocks*)))
			     (when block
			       (send :mouse-move block x y))))
      (:mouse-button-down-event (:button button :x x :y y)
				(let ((block (hit-blocks x y *blocks*)))
				  (when block
				    (send :mouse-down block x y button))))
      (:mouse-button-up-event (:button button :x x :y y)
			      (let ((block (hit-blocks x y *blocks*)))
				(when block
				  (send :mouse-up block x y button))))
      (:joy-button-down-event (:button button :state state)
			      (when (assoc button *joystick-mapping*)
				(update-joystick button state)
				(send-event (make-event :joystick
							(list (translate-joystick-button button) 
							      :button-down)))))
      (:joy-button-up-event (:button button :state state)  
			    (when (assoc button *joystick-mapping*)
			      (update-joystick button state)
			      (send-event (make-event :joystick
						      (list (translate-joystick-button button) 
							    :button-up)))))
      (:joy-axis-motion-event (:axis axis :value value)
			      (update-joystick-axis axis value))
      (:video-expose-event () (sdl:update-display))
      (:key-down-event (:key key :mod-key mod)
		       (let ((event (make-event key mod)))
			 (if *held-keys*
			     (hold-event event)
			     (send-event event))))
      (:key-up-event (:key key :mod-key mod)
		     ;; is this "held keys" code obsolete? it was useful for CONS control
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
	     ;; this lets slime keep working while the main loop is running
	     ;; in sbcl using the :fd-handler swank:*communication-style*
	     #+(and sbcl (not sb-thread)) (restartably
					   (sb-sys:serve-all-events 0))	 
	     (sdl:with-timestep (do-update))
	     (restartably
	       (gl:clear-color 1 1 1 1)
	       (gl:clear)
	       (gl:disable :depth-test)
	       (gl:clear :color-buffer-bit)
	       (gl:enable :texture-2d :blend)	
	       (set-blending-mode :alpha)
	       (draw-blocks)
	       (gl:flush)
	       (sdl:update-display))))))

;;; The BLOCKY.INI user configuration file

(defparameter *user-init-file-name* "blocky.ini")

(defun load-user-init-file ()
  (let ((type :unspecific)) ;; possible sbcl non-compliant behavior
    (let ((file (merge-pathnames (make-pathname :name *user-init-file-name*
						:type type)
				 (blocky-directory))))
      (when (cl-fad:file-exists-p file)
	(load (cl-fad:pathname-as-file file))))))

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

(defvar *keyword-package* (find-package :keyword))

(defun write-sexp-to-file (filename sexp)
  (message "Writing data to file ~S" filename)
  (with-open-file (file filename :direction :output 
			:if-exists :overwrite
			:if-does-not-exist :create)
    (let ((*package* *keyword-package*))
      (with-standard-io-syntax 
	(print sexp file))))
      ;;(format file "~S" sexp)))
  (message "Writing data to file ~S... Done." filename))

(defvar *eof-value* (gensym))

(defun read-sexp-from-file (filename)
  (message "Reading data from ~A..." filename)
  (with-open-file (file filename :direction :input)
    (with-standard-io-syntax 
      (prog1 (loop as sexp = (read file nil *eof-value*)
		   until (eq *eof-value* sexp)
		   collect sexp)
	(message "Reading data from ~A... Done." filename)))))

;; Now tie it all together with routines that read and write
;; collections of records into IOF files.

(defun write-iof (filename resources)
  "Write the RESOURCES to the IOF file FILENAME."
  (write-sexp-to-file filename (mapcar #'resource-to-plist resources)))

(defun read-iof (filename)
  "Return a list of resources from the IOF file FILENAME."
  (labels ((resourcep (s)
	     (keywordp (first s))))
    ;; read the file
    (let ((sexp (read-sexp-from-file filename)))
      ;; find the resource plists; see `read-sexp-from-file'
      (mapcar #'(lambda (s)
		  (apply #'make-resource s))
	      (if (every #'resourcep sexp)
	          sexp
		  (first sexp))))))

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
A lookup failure results in an error. See `find-resource'.")

(defun initialize-resource-table ()
  "Create a new empty resource table."
   (setf *resources* (make-hash-table :test 'equal)))

;;; Opening and saving projects

(defvar *project* nil "The name of the current project.")

(defparameter *project-directory-extension* ".blocky")

(defvar *project-path* nil "The pathname of the currently opened project. 
This is where all saved objects are stored.")

(defvar *after-open-project-hook* nil)

(defvar *executable* nil "Non-nil when running Blocky from a saved
binary image.")

(defparameter *untitled-project-name* "--untitled--")

;;; Project packages

(defvar *project-package-name* nil)

(defun project-package-name (&optional (project-name *project*))
  (make-keyword (or *project-package-name* project-name)))

(defun is-standard-project ()
  (string= "STANDARD" (string-upcase *project*)))

(defun project-package-exists-p (project)
  (assert (not (null project)))
  (find-package (project-package-name (make-keyword project))))

(defun define-project-package (project)
  (assert (stringp project))
  (if (project-package-exists-p project)
      (message "Not defining new package, because user-defined project package ~S already exists. Continuing..." *project-package-name*)
      ;; define the new package
      (setf *project* 
	    project
	    *project-package-name* 
	    (make-keyword project)
	    *package*
	    (make-package (make-keyword project) :use '(:blocky :common-lisp)))))
       
(defun in-project-package (project)
  (assert (not (null project)))
  (if (is-standard-project)
      (setf *package* (find-package :blocky))
      ;; find project-specific package
      (let ((package (project-package-name project)))
        (assert (project-package-exists-p project))
	(message "Found project package ~S." package)
	(setf *package* (find-package package))
	(message "Now in package ~S." package))))

;;; The blocky installation dir
  
(defun blocky-directory ()
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

(defparameter *projects-directory* ".blocky")

(defun projects-directory ()
   (cl-fad:pathname-as-directory 
    (make-pathname :name *projects-directory*
		   :defaults (user-homedir-pathname))))
   
(defun project-directory-name (project)
  (assert (stringp project))
  (concatenate 'string project *project-directory-extension*))  

(defun default-project-pathname (project)
  (assert (stringp project))
  (cl-fad:pathname-as-directory 
   (make-pathname 
    :name (project-directory-name project)
    :defaults (projects-directory)
;   :defaults (user-homedir-pathname)
    :type :unspecific)))

(defun make-directory-maybe (name)
  (ensure-directories-exist 
   (make-pathname :name "NAME" :type :unspecific
		  :defaults 
		  (cl-fad:pathname-as-directory name))))
			     
(defun default-project-directories () 
  (let ((projects (projects-directory)))
;    (make-directory-maybe projects)
    (list (blocky-directory) projects)))

(defvar *project-directories* nil
  "List of directories where BLOCKY will search for projects.
Directories are searched in list order.")

(defun search-project-path (project)
  "Search the `*project-directories*' path for a directory with the
name 'PROJECT-NAME.blocky' Returns the pathname if found, otherwise
nil."
  (let ((dirs (cons (asdf:system-relative-pathname 'blocky "") *project-directories*)))
    (assert (stringp project))
    (or 
     (loop 
       for dir in dirs for path
	 = (cl-fad:directory-exists-p 
	    (cl-fad:pathname-as-directory
	     (make-pathname 
	      :name (project-directory-name project)
	      :defaults dir
	      :type :unspecific)))
       when path return path)
     (prog1 nil
       (message "Cannot find project ~s in paths ~S. Try checking your *PROJECTS-DIRECTORIES* settings in the BLOCKY.INI configuration file. Continuing..."
		project dirs)))))

(defun expand-file-name (resource)
  (when (stringp (resource-file resource))
    (setf (resource-file resource)
	  (merge-pathnames (resource-file resource)
			   (find-project-path *project*)))))

(defun index-resource (resource)
  "Add the RESOURCE's record to the resource table.
If a record with that name already exists, it is replaced.  However,
if the resource is an :alias, just the string name of the target
resource is stored; see also `find-resource'."
  (expand-file-name resource)
  (let ((val (if (eq :alias (resource-type resource))
		 (resource-data resource)
		 resource)))
    (setf (gethash (resource-name resource)
		   *resources*) 
	  val)))

(defmacro defresource (&rest entries)
  (assert (every #'listp entries))
  (let ((each (gensym)))
  `(dolist (,each ',entries)
     (index-resource (apply #'make-resource ,each)))))

(defun find-project-path (project-name)
  "Return the current project path."
  (assert *project*)
  (or *project-path*
      (search-project-path project-name)))

(defun find-project-file (project-name file)
  "Make a pathname for FILE within the project PROJECT-NAME."
  (merge-pathnames file (find-project-path project-name)))

(defun default-project-lisp-file (project-name)
  (find-project-file project-name (concatenate 'string project-name ".lisp")))

(defparameter *object-index-filename* "index.iof")

(defun load-project-objects (project)
  (let ((object-index-file (find-project-file project *object-index-filename*)))
    (when (cl-fad:file-exists-p object-index-file)
      (message "Reading saved objects from ~S" object-index-file)
      (index-iof project object-index-file))))

(defun load-project-lisp (project)
  (let ((lisp (default-project-lisp-file project)))
    (if (cl-fad:file-exists-p lisp)
	(progn (message "Loading lisp for project ~A..." project)
	       (load lisp))
	(message "No default lisp file found in project ~S. Continuing..." project))))

(defun create-project (project &optional destination-directory)
  (assert (stringp project))
  (let* ((directory (or destination-directory (projects-directory)))
	 (dirs (mapcar #'string-upcase (find-projects-in-directory directory))))
    (if (find project dirs :test 'equal)
	(message "Cannot create project ~A, because a project with this name already exists in directory ~A"
		 project directory)
	(let ((dir (default-project-pathname project)))
	  (message "Creating new project ~A in directory ~A..." project dir)
	  (setf *project* project)
	  (prog1 dir
	    (make-directory-maybe dir)
	    (message "Finished creating directory ~A." dir)
	    (message "Finished creating project ~A." project))))))

(defun open-project (project &optional no-error)
  "Load the project named PROJECT. Load any resources marked with a
non-nil :autoload property. This operation also sets the default
object save directory. See also `save-object-resource')."
  (assert (stringp project))
  (message "Opening project: ~A" (string-upcase project))
  (setf *project* project
	*pending-autoload-resources* nil
	*project-package-name* nil)
  ;; possibly create a new project
  (setf *project-path* (search-project-path project))
  (when (null *project-path*)
    (if no-error
	(create-project project)
	(error "Cannot find any project named ~S" project)))
  ;; check path
  (message "Set project path to ~A" (namestring *project-path*)) 
  (assert *project-path*)
  ;; define package if necessary
  (define-project-package project)
  (in-project-package project)
  ;; load everything else
  (index-project project)
  (mapc #'load-resource (nreverse *pending-autoload-resources*))
  (setf *pending-autoload-resources* nil)
  (load-project-objects project)
  (load-database)
  (load-variables)
  (message "Started up successfully. Indexed ~A resources." (hash-table-count *resources*))
   ;; load any user-written lisp
  (load-project-lisp project)
  (run-project-lisp project)
  (run-hook '*after-open-project-hook*))

(defun run-project-lisp (project)
  (message "Running project startup function...")
  (let ((package (find-package (project-package-name project))))
    (if package
	(let ((start-function (intern (string-upcase project) package)))
	  (message "Checking for startup function ~S" start-function)
	  (if (fboundp start-function)
	      (funcall start-function)
	      (message "No default startup function for: ~S. Continuing.." (string-upcase (symbol-name start-function)))))
	(message "Warning: No project package defined. Continuing..."))))

(defun publish-project-as-application (&key (output-file "output.io")
					    project require)
  (assert (stringp project))
  (buildapp::main (list "sbcl"
			"--asdf-path"
			(sb-ext:native-namestring
			 (asdf:system-relative-pathname :blocky "./"))
		       "--load-system" "blocky"
		       "--eval" (format nil 
					"(progn (map nil #'ql:quickload (list :lispbuilder-sdl-mixer :lispbuilder-sdl-ttf :lispbuilder-sdl-gfx :lispbuilder-sdl-image :cl-opengl :cl-fad :buildapp :uuid)) (blocky:play \"~A\"))" project)
		       "--output" (sb-ext:native-namestring (merge-pathnames output-file)))))

(defun directory-is-project-p (dir)
  "Test whether a directory has the .blocky suffix."
  (let ((index-filename (concatenate 'string
				     (file-namestring dir)
				     *iof-file-extension*)))
    (cl-fad:file-exists-p (make-pathname :name index-filename
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

(defun index-iof (project-name iof-file)
  "Add all the resources from the iof IOF-FILE to the resource
table. File names are relative to the project PROJECT-NAME."
  (let ((resources (read-iof iof-file)))
    (message "Loading ~A resources from file ~A:~A..." (length resources)
	     project-name iof-file)
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
	    ;; save the resource name for later autoloading, if needed
	    (when (getf (resource-properties res) :autoload)
	      (push res *pending-autoload-resources*)))))))

(defun index-project (project-name)
  "Add all the resources from the project PROJECT-NAME to the resource
table."
  (let ((index-file (find-project-file project-name *object-index-filename*)))
    (if (cl-fad:file-exists-p index-file)
	(index-iof project-name index-file)
	(message "Did not find index file ~A in project ~A. Continuing..."
		 index-file project-name))))

;;; Standard resource names

(defvar *default-font* "default-font")

;;; Creating, saving, and loading object resources in IOF files

;; See also the documentation string for `*iof-file-extension*'.

(defun make-object-resource (name object)
  "Make an object resource named NAME (a string) with the Lisp object
OBJECT as the resource data."
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
    (if (is-standard-project)
	;; don't save the startup project
	(message "Not saving project STANDARD. Continuing...")
	;; save it
	(labels ((save (name resource) 
		   (when (or force (resource-modified-p resource))
		     (push (save-resource name resource) index))))
	  (maphash #'save *resources*)
	  ;; FIXME: allow to save resources in separate file
	  (write-iof (find-project-file *project* *object-index-filename*)
		     (nreverse index))
	  (save-database)
	  (save-variables)))))

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

;;; Loading images and textures

(defun set-blending-mode (mode)
  (ecase mode 
    (:additive (gl:blend-func :src-alpha :one))
    (:source (gl:blend-func :src-color :zero))
    (:alpha2 (gl:blend-func :one :one-minus-src-alpha))
    (:mask (gl:blend-func :one :zero))
    (:additive2 (gl:blend-func :one :one))
    (:alpha (gl:blend-func :src-alpha :one-minus-src-alpha))))

(defun load-texture 
    (surface &key source-format (internal-format :rgba)
		  (filter :mipmap))
  (let ((texture (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d texture)
    ;; set filtering parameters
    (ecase filter 
      (:linear (gl:tex-parameter :texture-2d :texture-min-filter :linear)
		(gl:tex-parameter :texture-2d :texture-mag-filter :linear))
      (:mipmap (gl:tex-parameter :texture-2d :generate-mipmap t) 
       (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)))
    ;; set wrapping parameters
    (gl:tex-parameter :texture-2d :texture-wrap-r :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    ;; convert image data from SDL surface to GL texture
    (sdl-base::with-pixel (pix (sdl:fp surface))
      (let ((texture-format (ecase (sdl-base::pixel-bpp pix)
                              (1 :luminance)
                              (2 :luminance-alpha)
                              (3 :rgb)
                              (4 :rgba))))
        (assert (and (= (sdl-base::pixel-pitch pix)
                        (* (sdl:width surface) (sdl-base::pixel-bpp pix)))
                     (zerop (rem (sdl-base::pixel-pitch pix) 4))))
        (gl:tex-image-2d :texture-2d 0 internal-format
                         (sdl:width surface) (sdl:height surface)
                         0 (or source-format texture-format)
                         :unsigned-byte (sdl-base::pixel-data pix))))
    texture))

(defvar *textures* nil)

(defun initialize-textures-maybe (&optional force)
  (when (or force (null *textures*))
    (setf *textures* (make-hash-table :test 'equal))))

(defun delete-all-textures ()
  (loop for texture being the hash-values in *textures*
	do (gl:delete-textures (list texture)))
  (initialize-textures-maybe :force))

(defun find-texture (name)
  (assert (stringp name))
  (initialize-textures-maybe)
  (find-resource name)
  (gethash name *textures*))

(defun load-image-resource (resource)
  "Loads an :IMAGE-type iof resource from a :FILE on disk."
  (initialize-textures-maybe)
  (let* ((source-format (getf (resource-properties resource) :format))
	 (surface (sdl-image:load-image (namestring (resource-file resource))
				       :alpha 255))
	 (internal-format :rgba)
	 (texture (load-texture surface
				:source-format source-format
				:internal-format internal-format))
	 (name (resource-name resource)))
    (prog1 surface
      (let ((old-texture (gethash name *textures*)))
	(when old-texture
	  (gl:delete-textures (list old-texture))
	  (remhash name *textures*))
	(progn 
	  (setf (gethash name *textures*) texture))))))

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
    (if (cl-fad:file-exists-p fasl)
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
	  (draw-box 0 0 width height))))))
		    ;; TODO support arbitrary rgb and other drawing commands
		    ;; :stroke-color background
		    ;; :color background
		    ;; :destination canvas))))))

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

;;; Loading and saving the object database

(defun load-database-resource (resource)
  (let ((database (deserialize (resource-data resource))))
    (assert (hash-table-p database))
    (message "Merging ~S objects from database..." (hash-table-count database))
    (prog1 nil
      (merge-hashes *database* database))))

(defun make-database-resource (&optional (database *database*))
  (let ((database2 (make-hash-table :test 'equal)))
    (message "Serializing database...")
    (labels ((store (uuid object)
	       ;; don't save prototypes
	       (when (null (object-name object))
		 ;; (not (is-temporary object)))
		 (setf (gethash uuid database2) object))))
      (maphash #'store database) ;; copy into database2
      (values (make-resource :name "--database--"
			     :type :database
			     :data (serialize database2))
	      (hash-table-count database2)))))

(defun database-file ()
  (assert (not (null *project*)))
  (find-project-file *project* "database.iof"))

(defun save-database (&optional (database *database*))
  (assert (hash-table-p database))
  (let ((file (database-file)))
    (message "Scanning ~S objects in database..." 
	     (hash-table-count database))
    (multiple-value-bind (resource count)
	(make-database-resource database)
      (message "Saving ~S objects from database into ~A..." 
	       count
	       (namestring file))
      (write-iof file (list resource))
      (message "Finished saving database into ~A. Continuing..." file))))
      
(defun load-database (&optional (file (database-file)))
  (message "Looking for object database ~A..." file)
  (if (cl-fad:file-exists-p file)
      (let ((resources (read-iof file)))
	(message "Read ~S resources from ~A" (length resources) file)
	(let ((database (first resources)))
	  (assert (eq :database (resource-type database)))
	  (load-database-resource database)))
      (message "No database file found. Continuing...")))

;;; Loading/saving variables

(defun make-variable-resource (name)
  (assert (and (symbolp name)
	       (boundp name)))
  (make-resource :name name
		 :type :variable
		 :data (serialize (symbol-value name))))

(defun load-variable-resource (resource)
  (assert (eq :variable (resource-type resource)))
  (let ((name (resource-name resource)))
    (message "Setting variable: ~S..." name)
    (setf (symbol-value name)
	  (resource-data resource))))

(defvar *persistent-variables* '(*frame-rate* *updates* *screen-width*
*screen-height* *world* *blocks* *dt* *pointer-x* *pointer-y*
*resizable* *window-title* *script* *system*))
    ;; notice that THIS variable is also persistent!
    ;; this is to avoid unwanted behavior changes in modules
    ;; *persistent-variables*))  ;; FIXME not for now

(defparameter *persistent-variables-file-name* "variables.iof")

(defun persistent-variables-file (&optional (project *project*))
  (find-project-file project *persistent-variables-file-name*))

(defun save-variables (&optional (variables *persistent-variables*))
  (with-standard-io-syntax
    (message "Saving system variables ~A..." variables)
    (write-iof (persistent-variables-file)
	       (mapcar #'make-variable-resource variables))
    (message "Finished saving system variables.")))

(defun load-variables ()
  (with-standard-io-syntax
    (let ((file (persistent-variables-file)))
      (if (cl-fad:file-exists-p file)
	  (progn 
	    (message "Loading system variables from ~A..." file)
	    (mapc #'load-variable-resource 
		  (read-iof file))
	    (message "Finished loading system variables."))
	  (message "No system variables file found in this project. Continuing...")))))
  
;;; Handling different resource types automatically

(defparameter *resource-handlers* 
  (list :image #'load-image-resource
	;; :variable #'load-variable-resource
	:lisp #'load-lisp-resource
	:object #'load-object-resource
	:database #'load-database-resource
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
  ;; (message "Attempting to load resource ~S." (resource-name resource))
  (let ((handler (getf *resource-handlers* (resource-type resource))))
    (assert (functionp handler))
    ;; fill in the object field by invoking the handler, if needed
    (when (null (resource-object resource))
      (setf (resource-object resource)
	    (funcall handler resource)))
    (when (null (resource-object resource))
      (error "Failed to load resource ~S." (resource-name resource)))))
	;; (message "Loaded resource ~S with result type ~S." 
	;; 	 (resource-name resource)
	;; 	 (type-of (resource-object resource))))))

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

(defun delete-all-resources ()
  (loop for resource being the hash-values in *resources*
	do (let ((object (resource-object resource)))
	     (when object
	       (case (resource-type resource)
		 (:image (sdl:free object))
		 (:music (sdl-mixer:free object))
		 (:sample (sdl-mixer:free object)))))
	   (initialize-resource-table)))
	   
;;; Custom audio generation

(defvar *frequency* 44100)

(defvar *output-chunksize* 2048)

(defvar *output-channels* 2)

(defvar *sample-format* SDL-CFFI::AUDIO-S16LSB)

;;; Voice objects

;; (defvar *voices* nil)

;; (define-prototype voice () output)

;; (define-method initialize voice (&optional (size *output-chunksize*))
;;   (setf %output (make-array size :element-type 'float :initial-element 0.0))
;;   (register-voice self))

;; (define-method get-output voice ()
;;   %output)

;; ;;(define-method play voice (&rest parameters))
;; (define-method halt voice ())
;; (define-method run voice ())

;; (defun register-voice (voice)
;;   (pushnew voice *voices* :test 'eq))

;; (defun unregister-voice (voice)
;;   (setf *voices*
;; 	(delete voice *voices* :test 'eq)))

;; (defun cffi-sample-type (sdl-sample-type)
;;   (ecase sdl-sample-type
;;     (SDL-CFFI::AUDIO-U8 :uint8) ; Unsigned 8-bit samples
;;     (SDL-CFFI::AUDIO-S8 :int8) ; Signed 8-bit samples
;;     (SDL-CFFI::AUDIO-U16LSB :uint16) ; Unsigned 16-bit samples, in little-endian byte order
;;     (SDL-CFFI::AUDIO-S16LSB :int16) ; Signed 16-bit samples, in little-endian byte order
;;     ;; (SDL-CFFI::AUDIO-U16MSB nil) ; Unsigned 16-bit samples, in big-endian byte order
;;     ;; (SDL-CFFI::AUDIO-S16MSB nil) ; Signed 16-bit samples, in big-endian byte order
;;     (SDL-CFFI::AUDIO-U16 :uint16)  ; same as SDL(SDL-CFFI::AUDIO-U16LSB (for backwards compatability probably)
;;     (SDL-CFFI::AUDIO-S16 :int16) ; same as SDL(SDL-CFFI::AUDIO-S16LSB (for backwards compatability probably)
;;     (SDL-CFFI::AUDIO-U16SYS :uint16) ; Unsigned 16-bit samples, in system byte order
;;     (SDL-CFFI::AUDIO-S16SYS :int16) ; Signed 16-bit samples, in system byte order
;;     ))

;; (defun cffi-chunk-buffer (chunk)
;;   (sdl:fp chunk))

;; (defun buffer-length (buffer)
;;   (let ((type (cffi-sample-type *sample-format*)))
;;     (length (cffi:mem-ref buffer type))))

;; (defun convert-cffi-sample-to-internal (chunk)
;;   (let* ((input-buffer (cffi-chunk-buffer chunk))
;; 	 (type (cffi-sample-type *sample-format*))
;; 	 (size (length (cffi:mem-ref input-buffer type))))
;;     (assert (eq *sample-format* SDL-CFFI::AUDIO-S16LSB)) ;; for now
;;     (let ((output-buffer (make-array size)))
;; 	(prog1 output-buffer
;; 	  (dotimes (n size)
;; 	    (setf (aref output-buffer n)
;; 		  (/ (float (cffi:mem-aref input-buffer type n))
;; 		     32768.0)))))))

;; (defun convert-internal-sample-to-cffi (input output &optional limit)
;;   (let ((type (cffi-sample-type *sample-format*)))
;;     (dotimes (n 128)
;;       (setf (cffi:mem-aref output type n)
;; 	    (truncate (* (cffi:mem-aref input type n)
;; 			 32768.0))))))

;; (defvar *buffer* (make-array 10000 :element-type 'float :initial-element 0.0))

;; (defvar *sample-generator* nil)

;; (defvar *foo* nil)

;; ;; (defun music-mixer-callback (user output size)
;; ;;   (setf *foo* t)
;; ;;   (format t "XXXX ~S" *foo*))

;;   ;; (let ((type (cffi-sample-type *sample-format*)))
;;   ;;   (dotimes (n size)
;;   ;;     (setf (cffi:mem-aref output type n) 0))))

;;   ;; (when *sample-generator*
;;   ;;   (message "Generating samples")
;;   ;;   (funcall generator *buffer*)
;;   ;;   (message "Converting samples to output format...")
;;   ;;   (convert-internal-sample-to-cffi *buffer* output size)
;;   ;;   ))

;; ;; (defun register-sample-generator (generator)
;; ;;   (message "Registering sample generator...")
;; ;;   (setf *sample-generator* generator)
;; ;;   (sdl-mixer:register-music-mixer #'music-mixer-callback))

;; (defun mix-voices (output)
;;   (message "Mixing voices...")
;;   ;; create silence
;;   (dotimes (n *output-chunksize*)
;;     (setf (aref output n) 0.0))
;;   ;; mix in voices
;;   (dolist (voice *voices*)
;;     (run voice)
;;     (let ((input (get-output voice)))
;;       (dotimes (n *output-chunksize*)
;; 	(incf (aref output n)
;; 	      (aref input n))))))

;; ;; (defun register-voice-mixer () 
;; ;;   (message "Registering voice mixer...")
;; ;;   (setf *voices* nil)
;; ;;   (register-sample-generator #'mix-voices))

;; (defvar *buffer-cache* nil)

;; (defun initialize-buffer-cache ()
;;   (setf *buffer-cache* (make-hash-table :test 'eq)))

;; (defun get-sample-buffer (sample)
;;   (let ((chunk (if (stringp sample)
;; 		   (find-resource-object sample)
;; 		   sample)))
;;     ;; (when (null *buffer-cache*)
;;     ;;   (initialize-buffer-cache))
;;     ;; ;; is it cached?
;;     ;; (or (gethash chunk *sample-buffers*)
;;     ;; 	(setf (gethash chunk *sample-buffers*)
;; 	      (convert-cffi-sample-to-internal chunk)))

;; ;;; Regular music/sample functions

;; ;; (defvar *sample-callback* nil)

;; ;; (defun set-sample-callback (func)
;; ;;   (assert (functionp func))
;; ;;   (setf *sample-callback* func))

;; ;; (defvar *music-callback* nil)

;; ;; (defun set-music-callback (func)
;; ;;   (assert (functionp func))
;; ;;   (setf *music-callback* func))

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

(defun halt-music (&optional (fade-milliseconds 0))
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

(defun initialize-sound ()
  ;; try opening sound
  (when (null (sdl-mixer:open-audio :frequency *frequency*
				    :chunksize *output-chunksize*
				    :enable-callbacks t
				    :format *sample-format*
				    :channels *output-channels*))
    ;; if that didn't work, disable effects/music
    (message "Could not open audio driver. Disabling sound effects and music.")
    (setf *use-sound* nil))
  ;; set to mix lots of sounds
  (sdl-mixer:allocate-channels *channels*))

;;; Standard colors

;; The X11 standard colors are loaded by default into the resource
;; table from the raw data in `*x11-color-data*'. See also rgb.lisp.

(defun initialize-colors ()
  "Load the X11 color data into the resource table."
  (dolist (color *x11-color-data*)
    (destructuring-bind (name red green blue) color
      (index-resource (make-resource :name name
				     :type :color
				     :data (list red green blue))))))

(defun percent-gray (percentage)
  (format nil "gray~S" (truncate percentage)))

(defun percent-grey (percentage)
  (percent-gray percentage))

;;; Creating and displaying images

;; The "driver dependent objects" for BLOCKY images are just SDL:SURFACE
;; objects. (The situation is the same for BLOCKY colors, fonts, and so
;; on). So long as the clients treat the driver-dependent resource
;; objects as opaque, this thin wrapper is sufficient.

;; Below are some image handling functions.

(defun create-image (width height)
  "Create a new BLOCKY image of size (* WIDTH HEIGHT)."
  (assert (and (integerp width) (integerp height)))
  (sdl:create-surface width height))

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

;; &optional (u1 0) (v1 0) (u2 1) (v2 1))
(defun draw-textured-rectangle (x y z width height texture 
				&key (blend :alpha) (opacity 1.0) (vertex-color "white"))
  (if (null blend)
      (gl:disable :blend)
      (progn (gl:enable :texture-2d :blend)	
	     (set-blending-mode blend)))
  (gl:bind-texture :texture-2d texture)
  (set-vertex-color vertex-color)
  (gl:with-primitive :quads
    (let ((x1 x)
	  (x2 (+ x width))
	  (y1 y)
	  (y2 (+ y height)))
      (gl:tex-coord 0 1)
      (gl:vertex x y2 (- 0 z)) ;; z
      (gl:tex-coord 1 1)
      (gl:vertex x2 y2 (- 0 z)) ;; z
      (gl:tex-coord 1 0)
      (gl:vertex x2 y1 (- 0 z)) ;; z
      (gl:tex-coord 0 0)
      (gl:vertex x y (- 0 z))))) ;; z

(defun draw-image (name x y &key (z 0.0) (blend :alpha) (opacity 1.0) (scale-x 1) (scale-y 1))
  (let* ((image (find-resource-object name))
	 (height (* scale-y (sdl:height image)))
	 (width (* scale-x (sdl:width image))))
    (let ((texture (find-texture name)))
      (draw-textured-rectangle x y z width height texture :blend blend :opacity opacity))))

;;; Indicators

(defparameter *active-indicator-color* "yellow")
(defparameter *inactive-indicator-color* "gray70")

(defun indicator-size () (* 0.4 (font-height *default-font*)))

(defparameter *indicators* 
  '(:asterisk "asterisk"
    :top-left-triangle "top-left-triangle-indicator"
    :bottom-right-triangle "bottom-right-triangle-indicator"))

(defun find-indicator-texture (indicator)
  (assert (keywordp indicator))
  (let ((texture-name (getf *indicators* indicator)))
    (assert (stringp texture-name))
    (find-texture texture-name)))

(defun draw-indicator (indicator x y &key color (state :inactive))
  (let ((size (indicator-size)))
    (draw-textured-rectangle x y 0 size size 
			     (find-indicator-texture indicator)
			     :blend :alpha
			     :vertex-color 
			     (or color (ecase state
					 (:active *active-indicator-color*)
					 (:inactive *inactive-indicator-color*))))))

;;; Font operations

;; A bitmap font resource looks like this:

;; (:name "default-font" 
;;        :type :font 
;;        :properties (:height 14 :width 7) ;; monospace only
;;        :data "7x14")

;; Or use type :ttf for Truetype fonts. Don't specify :height and
;; :width in this case; instead use :size N where N is the number of
;; points in the font size, for example :size 12 would be a 12-point
;; version of the font.

(defun-memo font-height (font)
    ;; don't cache null results, because these can happen if
    ;; font-height is called before SDL initialization
    (:key #'first :test 'equal :validator #'identity)
  (let ((resource (find-resource font)))
    (ecase (resource-type resource)
      (:font (find-resource-property font :height))
      (:ttf (sdl:get-font-height :font (resource-object resource))))))

(defun font-width (font)
  (let ((resource (find-resource font)))
    (ecase (resource-type resource)
      (:font (find-resource-property font :width))
      (:ttf (error "Cannot get width of a TTF font.")))))

(defun-memo font-text-width (string font)
    (:key #'identity :test 'equal :validator #'identity)
  (sdl:get-font-size string :size :w :font (find-resource-object font)))

(defun font-text-extents (string font)
  (let ((resource (find-resource font)))  
    (ecase (resource-type resource)
      (:font (* (length string)
		(font-width font)))
      (:ttf (values (font-text-width string font)
		    (font-height font))))))

(defun make-text-image (font string)
  (assert (and (not (null string))
	       (plusp (length string))))
  (multiple-value-bind (width height)
      (font-text-extents string font)
    (let ((surface (sdl:create-surface width height :bpp 8))
	  (texture (first (gl:gen-textures 1))))
      (prog1 texture
	(sdl:draw-string-blended-* string 0 0 
				   :color (find-resource-object "white")
				   :font (find-resource-object font)
				   :surface surface)
	(gl:bind-texture :texture-2d texture)
	(gl:tex-parameter :texture-2d :texture-min-filter :linear)
	(gl:tex-parameter :texture-2d :texture-mag-filter :linear)
	(sdl-base::with-pixel (buffer (sdl:fp surface))
	  (gl:tex-image-2d :texture-2d 0 :alpha width height 0 :alpha :unsigned-byte (sdl-base::pixel-data buffer)))
	(sdl:free surface)))))

(defun-memo find-text-image (font string) 
  (:key #'identity :test 'equal)
  (make-text-image font string))
  
(defun clear-text-image-cache (&key (delete-textures t))
  (let ((table (get-memo-table 'find-text-image)))
    (when table
      (when delete-textures 
	(loop for texture being the hash-values in table
	      do (gl:delete-textures (list texture)))
      (clrhash table)))))

(defun-memo gl-color-values (color-name)
    (:key #'first :test 'equal)
  (let ((color (find-resource color-name)))
    (assert (eq :color (resource-type color)))
    (mapcar #'(lambda (integer)
		(/ integer 255.0))
	    (resource-data color))))

(defun set-vertex-color (color)
  (assert (stringp color))
  (destructuring-bind (red green blue) 
      (gl-color-values color)
    (gl:color red green blue 1)))

(defun draw-string (string x y &key (color "black")
				    (font *default-font*)
				    (z 0))
  (let ((texture (find-text-image font string)))
    (multiple-value-bind (width height) 
	(font-text-extents string font)
      (draw-textured-rectangle x y z width height texture :vertex-color color))))

;;; Drawing shapes and other primitives

(defun draw-line (x0 y0 x1 y1 
		     &key 
		     (color "white"))
  (gl:disable :texture-2d)
  (set-vertex-color color)
  (gl:with-primitive :lines 
    (gl:vertex x0 (+ y0))
    (gl:vertex x1 (+ y1))))

(defun draw-box (x y width height		
 		 &key (color "black"))
  (set-vertex-color color)
  (gl:disable :texture-2d)
  (gl:with-primitive :quads
    (let ((x1 (+ x width))
	  (y1 (+ y height)))
      (gl:vertex x y1)
      (gl:vertex x1 y1)
      (gl:vertex x1 y)
      (gl:vertex x y))))

;; (defun draw-rectangle (x y width height &key color) 
;;   (let ((x1 (+ x width))
;; 	(y1 (+ y height)))
;;     (draw-line x y x1 y1 :color color)))

(defparameter *circle-textures* 
  '(:outline "circle-outline-flat-128"
    :solid "circle-flat-128"))

(defparameter *circle-mask-textures* 
  '(:outline "circle-outline-flat-128-mask"
    :solid "circle-flat-128-mask"))

(defun draw-circle (x y radius 
		    &key (color "white") 
			 (type :outline)
			 (blend :alpha)
			 (z 0))
  (let ((texture (find-texture (getf *circle-textures* type)))
	(left (- x radius))
	(top (- y radius))
	(side (* 2 radius)))
    (draw-textured-rectangle left top z side side texture :blend blend :vertex-color color)))

(defun draw-solid-circle (x y radius &key color (blend :alpha))
  (draw-circle x y radius :color color :type :solid))

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

(defvar *library-search-paths-setup-hook* nil)

(defun setup-library-search-paths ()
  (run-hook '*library-search-paths-setup-hook*)
  #+darwin (setf cffi:*foreign-library-directories*
                 (union cffi:*foreign-library-directories*
                        '(#P"/opt/local/lib" #P"/sw/lib/")
                        :test #'equal)))

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

(defun print-copyright-notice ()
  (dolist (line (split-string-on-lines *copyright-notice*))
    (message line)))

(defun load-standard-resources ()
  (open-project "standard"))

(defun start-up ()
  #+linux (do-cffi-loading)
  ;; add library search paths for Mac if needed
  (setup-library-search-paths)
  ;; get going...
  (message "Starting Blocky...")
  (print-copyright-notice)
  (setf *project-package-name* nil
        *project-directories* (default-project-directories)
	*blocks* nil
	*project* nil
	*message-hook-functions* nil
	*window-title* "blocky"
	*updates* 0
	*resizable* t
	*keyboard-update-number* 0
	*random-state* (make-random-state t))
  (sdl:init-sdl :video t :audio t :joystick t)
  (load-user-init-file) ;; this step may override *project-directories* and so on 
  (initialize-resource-table)
  (initialize-textures-maybe :force)
  (initialize-colors)
  (initialize-sound)
  (initialize-database)
  (load-standard-resources)
  (enable-key-repeat 9 1.2))

(defun shut-down ()
  ;; delete any cached textures and surfaces
  (clear-text-image-cache)
  (delete-all-textures)
  (purge-all-objects)
  (delete-all-resources)
  (sdl-mixer:halt-music)
  (sdl-mixer:close-audio t)
  (sdl:quit-sdl))

(defmacro with-session (&rest body)
  `(progn 
     (start-up)
     ,@body
     (shut-down)))

(defun play (&optional (project *untitled-project-name*))
  (with-session
    (open-project project)
    (when (null *blocks*)
      (new system)
      (start (new shell (new script))))
    (start-session)))

(defun create (project)
  (with-session
    (assert (stringp project))
    (new system)
    (create-project project)
    (open-project project)
    (start (new shell (new script)))
    (start-session)))

(defun edit (&optional (project *untitled-project-name*))
  (with-session
    (let ((*edit* t))
      (open-project project :no-error)
      (start-session))))

;; (defun share (project) ...

;; (defmacro defsession (name &rest body)
;;   `(defun ,name (&optional project)
;;      (start-up)
;;      (

;;; console.lisp ends here
