;;; console.lisp --- OS/device driver for BLOCKY

;; Copyright (C) 2006-2013 David O'Toole

;; Author: David O'Toole <dto@ioforms.org> 
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
;; services. Primitive operations such as opening a window, rendering
;; text, displaying bitmaps, drawing lines, playing sounds, file
;; access, and device input are all handled here.

;; Currently it uses the cross-platform SDL library (via
;; LISPBUILDER-SDL) as its device driver, and wraps the library for
;; use by the rest of BLOCKY.

;; http://lispbuilder.sourceforge.net/

;; The OpenGL support here is derived from code written by Bart Botta
;; for his excellent cl-opengl tutorials:
;; http://3bb.cc/tutorials/cl-opengl/

(in-package :blocky) 

(defvar *gl-window-open-p* nil)

(defvar *pending-resources* '())

(defun add-resource (plist)
  (assert (and (consp plist)
	       (keywordp (first plist))))
  (push plist *pending-resources*))

(defun add-resources (&rest plists)
  (mapcar #'add-resource plists))

(defun random-choose (set)
  (nth (random (length set)) set))

(defmacro restartably (&body body)
  `(restart-case
      (progn ,@body)
    (continue () :report "Continue"  )))

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

(defun holding-alt ()
  (or (keyboard-modifier-down-p :lalt)
      (keyboard-modifier-down-p :ralt)))

(defun holding-shift ()
  (or (keyboard-modifier-down-p :lshift)
      (keyboard-modifier-down-p :rshift)))

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

;;; The active blocks list 

;; see also blocks.lisp

(defvar *blocks* nil "List of active block objects. 
These blocks receive input events and are rendered to the screen by
the console. See also `send-event'.

Do not set this variable directly from a project; instead, call
`install-blocks'.")

(defun hit-blocks (x y &optional (blocks *blocks*))
  (when blocks
    (let ((x0 (truncate x))
	  (y0 (truncate y)))
      (labels ((try (b)
		 (send :hit b x0 y0)))
	(let ((parent (find-if #'try blocks :from-end t)))
	  (when parent
	    (try parent)))))))
  
(defun draw-blocks ()
  "Draw the active blocks to the screen."
  (dolist (block *blocks*)
    (send :draw block)))

(defun install-blocks (&rest blocks)
  "User-level function for setting the active block set. Note that
BLOCKY may override the current block set at any time for system menus
and the like."
  (setf *blocks* blocks))

;;; "Classic" key repeat

(defvar *key-repeat-p* nil)

(defvar *key-repeat-delay* 9)
(defvar *key-repeat-interval* 1.2)

(defun key-repeat-p () *key-repeat-p*)

(defun enable-key-repeat (&optional (delay *key-repeat-delay*) 
				    (interval *key-repeat-interval*))
  (let ((delay-milliseconds (truncate (* delay (/ 1000.0 *frame-rate*))))
  	(interval-milliseconds (truncate (* interval (/ 1000.0 *frame-rate*)))))
    (sdl:enable-key-repeat delay-milliseconds interval-milliseconds)
    (setf *key-repeat-delay* delay)
    (setf *key-repeat-interval* interval)
    (setf *key-repeat-p* t)))

(defun disable-key-repeat ()
  (sdl:disable-key-repeat)
  (setf *key-repeat-p* nil))

;;; Parceling out events to blocks

(defvar *pointer-x* 0)
(defvar *pointer-y* 0)

(defvar *event-hook* nil)

(defun send-to-blocks (event &optional (blocks *blocks*))
  (dolist (hook *event-hook*)
    (funcall hook event))
  (labels ((try (block)
	     (send :handle-event block event)))
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

(defun raw-joystick-event-p (event)
  (eq :raw-joystick (first event)))

(defun joystick-event-p (event)
  (or (raw-joystick-event-p event)
      (eq :joystick (first event))))

(defun normalize-event (event)
  "Convert EVENT to a normal form suitable for `equal' comparisons."
  ;; don't sort joystick event modifiers
  (if (joystick-event-p event)
      event
      (cons (first event)
	    (sort (remove-duplicates (delete nil (rest event)))
		  #'string< :key #'symbol-name))))

;;; Input events for keyboard and joystick etc

(defvar *joystick-button-symbols*
  '(:a :b :x :y ;; face buttons
    :left :right :up :down ;; directional pad
    :select :start ;; menu buttons
    :left-trigger :left-bumper :right-trigger :right-bumper  ;; shoulder buttons
    :left-click :right-click)) ;; clicking the analog sticks

(defparameter *other-modifier-symbols* '(:button-down :button-up))

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
  
(defun make-key-symbol (sdl-key)
  "Translate from :SDL-KEY-X to the symbol :X ."
  (let ((prefix "SDL-KEY-")
	(name (symbol-name sdl-key)))
    (assert (search prefix name))
    (make-keyword (subseq name (length prefix)))))

(defun make-event (code modifiers)
  "Create an input event for the key CODE with MODIFIERS pressed.
The argument CODE may be one of:

   - a keyword symbol naming the keyboard key, such as :RETURN or :SPACE
     (see also `make-key-symbol'.)

   - a one-character string, whose first character is the translated
     Unicode character being bound

   - an integer whose value is the unicode character code from SDL

or, 

   - a cons of the form (key unicode) will be passed through
     unaltered." 
  (assert code)
  ;; pass through joystick events unaltered
  (if (joystick-event-p (cons code modifiers))
      (cons code modifiers)
      (let ((head
	      (etypecase code
		(integer (string (code-char code)))
		(string (prog1 code
			  (assert (= 1 (length code)))))
		(keyword code)
		(cons code))))
	(normalize-event
	 (cons head
	       ;; modifiers
	       (cond ((keywordp modifiers)
		      (list modifiers))
		     ((listp modifiers)
		      modifiers)
		     ;; catch apparent lispbuilder-sdl bug?
		     ((eql 0 modifiers)
		      nil)))))))

(defparameter *default-joystick-profile*
  '(:name "Unknown Joystick"
    :type :joystick
    :left-analog-stick (0 1)
    :right-analog-stick (3 2)
    :buttons ()))

(defvar *joystick-profile* *default-joystick-profile*)

(defvar *user-joystick-profile* nil)

(defvar *joystick-device* nil 
  "The SDL device id of the current joystick.")

(defparameter *joystick-profiles* 
  '(("DragonRise Inc.   Generic   USB  Joystick  " 
     :name "Generic USB Gamepad" :type :joystick
     :left-analog-stick (0 1)
     :right-analog-stick (3 2)
     :buttons ((2 . :a)
	       (1 . :b)
	       (3 . :x)
	       (0 . :y)
	       (6 . :left-bumper)
	       (7 . :right-bumper)
	       (8 . :select)
	       (9 . :start)
	       (4 . :left-trigger)
	       (5 . :right-trigger)))
    ("GreenAsia Inc.    USB Joystick     "
     :name "Generic USB Gamepad" :type :joystick
     :left-analog-stick (0 1)
     :right-analog-stick (3 2)
     :buttons ((2 . :a)
	       (1 . :b)
	       (3 . :x)
	       (0 . :y)
	       (4 . :left-bumper)
	       (5 . :right-bumper)
	       (8 . :select)
	       (9 . :start)
	       (6 . :left-trigger)
	       (7 . :right-trigger)))
    ("USB Dance Pa" 
     :name "Generic USB Dance Pad" :type :dance 
     :buttons  ((12 . :up)
		(15 . :left)
		(13 . :right)
		(14 . :down)
		(0 . :downleft)
		(3 . :downright)
		(2 . :upleft)
		(1 . :upright)
		(8 . :select)
		(9 . :start)))
    ("GASIA CORP. PS(R) Gamepad Adaptor" 
     :name "Generic USB Gamepad" :type :joystick
     :left-analog-stick (0 1)
     :right-analog-stick (2 3)
     :buttons ((4 . :up)
	       (7 . :left)
	       (5 . :right)
	       (6 . :down)
	       (12 . :downleft)
	       (16 . :downright)
	       (14 . :upleft)
	       (13 . :upright)
	       (0 . :select)
	       (3 . :start)))))

(defun find-joystick-profile-by-name (name)
  (let ((entry (assoc name *joystick-profiles* :test 'equal)))
    (when entry (cdr entry))))

(defun find-joystick-profile (indicator)
  (etypecase indicator
    (string (find-joystick-profile-by-name indicator))
    (list indicator)))

(defun joystick-profile ()
  (or *user-joystick-profile* *joystick-profile*))

(defun joystick-name (&optional (profile (joystick-profile)))
  (getf (find-joystick-profile profile) :name))

(defun joystick-type (&optional (profile (joystick-profile))) 
  (getf (find-joystick-profile profile) :type))

(defun joystick-buttons (&optional (profile (joystick-profile)))
  (getf (find-joystick-profile profile) :buttons))

(defun joystick-left-analog-stick (&optional (profile (joystick-profile)))
  (getf (find-joystick-profile profile) :left-analog-stick))

(defun joystick-right-analog-stick (&optional (profile (joystick-profile)))
  (getf (find-joystick-profile profile) :right-analog-stick))

(defun button-to-symbol (button)
  (cdr (assoc button (joystick-buttons))))

(defun symbol-to-button (sym)
  (let ((entry (some #'(lambda (x)
			 (when (eq sym (cdr x))
			   x))
		     (joystick-buttons))))
    (when entry 
      (car entry))))

;; Analog sticks

(defparameter *joystick-axis-size* 32768.0)

(defparameter *joystick-dead-zone* 6000)

(defvar *joystick-axis-values* (make-array 100 :initial-element 0))

(defun update-joystick-axis (axis value)
  (setf (aref *joystick-axis-values* axis) value))

(defun joystick-axis-raw-value (axis)
  (aref *joystick-axis-values* axis))

(defun joystick-axis-pressed-p (axis) 
  (< *joystick-dead-zone* (abs (joystick-axis-raw-value axis))))

(defun joystick-axis-value (axis)
  (/ (joystick-axis-raw-value axis)
     *joystick-axis-size*))

(defun find-heading (x0 y0 x1 y1)
  (atan (- y1 y0) 
	(- x1 x0)))

(defun opposite-heading (heading)
  (- pi heading))

(defun analog-stick-pressed-p (&optional (stick (joystick-left-analog-stick)))
  (destructuring-bind (horizontal vertical) stick
    (or (joystick-axis-pressed-p horizontal)
	(joystick-axis-pressed-p vertical))))

(defun left-analog-stick-pressed-p ()
  (analog-stick-pressed-p (joystick-left-analog-stick)))

(defun right-analog-stick-pressed-p ()
  (analog-stick-pressed-p (joystick-right-analog-stick)))

(defun analog-stick-heading (&optional (stick (joystick-left-analog-stick)))
  (destructuring-bind (horizontal vertical) stick
    (when (analog-stick-pressed-p stick)
      (find-heading 0 0 
		    (joystick-axis-raw-value horizontal)
		    (joystick-axis-raw-value vertical)))))
      
(defun analog-stick-pressure (&optional (stick (joystick-left-analog-stick)))
  (destructuring-bind (horizontal vertical) stick
    (if (or (joystick-axis-pressed-p horizontal)
	    (joystick-axis-pressed-p vertical))
	(/ (distance 0 0
		     (joystick-axis-value horizontal)
		     (joystick-axis-value vertical))
	   ;; scale to [0.0, 1.0]
	   (sqrt 2))
	0.0)))

(defun left-analog-stick-heading ()
  (analog-stick-heading (joystick-left-analog-stick)))

(defun right-analog-stick-heading ()
  (analog-stick-heading (joystick-right-analog-stick)))

(defun left-analog-stick-pressure ()
  (analog-stick-pressure (joystick-left-analog-stick)))

(defun right-analog-stick-pressure ()
  (analog-stick-pressure (joystick-right-analog-stick)))

;; Joystick buttons
	
(defvar *joystick-button-states* nil)

(defun poll-joystick-button (button)
  "Return 1 if the button numbered BUTTON is pressed, otherwise 0."
  (sdl-cffi::sdl-joystick-get-button *joystick-device* button))

(defun update-joystick-button (button state)
  "Update the table in `*joystick-button-states*' to reflect the STATE of
the BUTTON. STATE should be either 1 (on) or 0 (off)."
  (setf (aref *joystick-button-states* button) state))

(defun joystick-button-state (button)
  (poll-joystick-button button))
;;  (aref *joystick-button-states* button))

(defun joystick-button-pressed-p (button)
  (let ((button-number (if (integerp button) 
			   button
			   (symbol-to-button button))))
    (when button-number 
      (= 1 (joystick-button-state button-number)))))

(defun reset-joysticks ()
  "Re-open the joystick device and re-initialize the state."
  (setf *joystick-device* (sdl-cffi::sdl-joystick-open 0))
  (setf *joystick-button-states* (make-array 100 :initial-element nil)))

(defun scan-for-joysticks ()
  (message "Scanning for connected joysticks...")
  (block scanning
    (dotimes (index (sdl:num-joysticks))
      (let ((joystick (sdl:sdl-joystick-name index)))
	(message "Checking joystick ~S, device name: ~S" index joystick)
	(let ((profile (find-joystick-profile joystick)))
	  (if (null profile)
	      (message "Could not find joystick profile for ~S. Continuing with default profile..." joystick)
	      (destructuring-bind (&key name type &allow-other-keys) profile
		(setf *joystick-profile* profile)
		(message "Found joystick profile ~S for ~S." type name))))))))

;;; Frame rate and simulation timing

(defparameter *default-frame-rate* 30)

(defvar *frame-rate* *default-frame-rate*)

(defun set-frame-rate (&optional (rate *frame-rate*))
  "Set the frame rate for the game."
  (message "Setting frame rate to ~S" rate)
  (setf (sdl:frame-rate) rate))

(defun get-ticks ()
  (sdl:sdl-get-ticks))

(defvar *dt* 33)

(defvar *next-update-hook* nil)

(defmacro at-next-update (&body body)
  `(prog1 nil 
     (add-hook '*next-update-hook*
	       #'(lambda () ,@body))))
		 
(defun update-blocks ()
  (run-hook '*next-update-hook*)
  (setf *next-update-hook* nil)
  (dolist (block *blocks*)
    (send :update block)))

(defvar *update-function* #'update-blocks)

(defvar *updates*)

(defun do-update (&rest args) 
  (handler-case 
      (when (functionp *update-function*)
	(apply *update-function* args))
    (floating-point-inexact (fpe)
      (error fpe))))

;; (incf *updates*)

(defparameter *updates* 0)

;;; Screen dimensions

(defparameter *screen-width* 640 "Physical width of the window, in pixels.") 
(defparameter *screen-height* 480 "Physical height of the window, in pixels.")

;; The nominal size of of the window in pixels, in case we just want
;; to scale the scene to match the window instead of showing more of
;; the buffer. If these are the same as the `*screen-' settings
;; above, then more of the buffer will be shown when the window size
;; increases.
(defparameter *nominal-screen-width* 640 "Nominal width of the window, in pixels.")
(defparameter *nominal-screen-height* 480 "Nominal height of the window, in pixels.")

(defparameter *gl-screen-width* 640 "Width of the window expressed in OpenGL coordinates.")
(defparameter *gl-screen-height* 480 "Height of the window expressed in OpenGL coordinates.")

(defparameter *scale-output-to-window* nil
  "When non-nil, always show a fixed amount of the buffer when changing
window size. Otherwise (the default) one onscreen pixel equals one
unit of buffer space, so that more of the buffer shows if the window
becomes larger.")
 
(defparameter *z-near* 0)
(defparameter *z-far* 100)

(defvar *use-texture-blending* t)

(defun enable-texture-blending ()
;  (when *use-texture-blending*
    (gl:enable :texture-2d :blend))

(defun open-viewport ()
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:viewport 0 0 *screen-width* *screen-height*)
  (if *scale-output-to-window*
      (setf *gl-screen-width* *nominal-screen-width*
	    *gl-screen-height* *nominal-screen-height*)
      (setf *gl-screen-width* *screen-width*
	    *gl-screen-height* *screen-height*)))

(defun project-orthographically ()
  (gl:disable :depth-test)
  (gl:clear :color-buffer-bit)
  (enable-texture-blending)
  (set-blending-mode :alpha)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 *gl-screen-width* *gl-screen-height* 0 *z-near* *z-far*))

(defparameter *field-of-view* 45)

(defun project-with-perspective (&key (field-of-view *field-of-view*) (depth *z-far*))
  (gl:enable :depth-test)
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer-bit)
  (enable-texture-blending)
  (set-blending-mode :alpha)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  ;; (glu:perspective field-of-view (/ *gl-screen-width* *gl-screen-height*) *z-near* depth)
  (gl:hint :perspective-correction-hint :nicest))

(defvar *window-x* 0)
(defvar *window-y* 0)
(defvar *window-z* 0)

(defun window-pointer-x (&optional (x *pointer-x*))
  (+ x *window-x*))

(defun window-pointer-y (&optional (y *pointer-y*))
  (+ y *window-y*))
  
(defun transform-window (&key (x 0) (y 0) (z 0) (scale-x 1.0) (scale-y 1.0) (scale-z 1.0))
  (setf *window-x* x)
  (setf *window-y* y)
  (setf *window-z* z)
  ;; now move viewing volume
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:translate (- 0 x)
		(- 0 y)
		(- 0 z))
  (gl:scale scale-x scale-y scale-z))

(defvar *resizable* t)

(defparameter *resize-hook* nil)

;;; The main loop of BLOCKY

(defvar *after-startup-hook* nil)

(defvar *quitting* nil)

(defvar *fullscreen* nil "When non-nil, attempt to use fullscreen mode.")

(defvar *window-title* "blocky")

(defvar *window-position* :center
  "Controls the position of the game window. Either a list of coordinates or the symbol :center.")

(defun start-session ()
  "Initialize the console, open a window, and play.
We want to process all inputs, update the game state, then update the
display."
  (let ((fps (make-instance 'sdl:fps-mixed 
			    :dt (setf *dt* (or *dt* (truncate (/ 1000 *frame-rate*)))))))
    (message "Simulation update time set to ~d milliseconds." *dt*)
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
    ;; get rid of any bogus textures
    (when *textures* (delete-all-textures))
    ;; move along
    (message "Creating OpenGL window... Done.")
    (setf *gl-window-open-p* t)
    (message "SDL driver name: ~A" (sdl:video-driver-name))
    (set-frame-rate *frame-rate*)
    (reset-joysticks)
    (scan-for-joysticks)
    (open-viewport)
    (project-orthographically)
    (load-project-lisp "STANDARD") ;; TODO remove
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
			   (open-viewport)
			   (project-orthographically)
			   ;; handle any blitzed textures. on some platforms/drivers
			   ;; the textures become invalidated after resize
			   (when *clear-cached-images-on-resize*
			     (clear-cached-images)
			     (clear-cached-text-images))
			   )
      (:mouse-motion-event (:x x :y y)
			   (setf *pointer-x* x *pointer-y* y)
			   (let ((block (hit-blocks (window-pointer-x)
						    (window-pointer-y) 
						    *blocks*)))
			     (when block
			       (send :handle-point-motion block
				     (window-pointer-x)
				     (window-pointer-y)))))
      (:mouse-button-down-event (:button button :x x :y y)
				(setf *pointer-x* x *pointer-y* y)
				(let ((block (hit-blocks 
					      (window-pointer-x)
					      (window-pointer-y)
					      *blocks*)))
				  (when block
				    (send :press block
					  (window-pointer-x)
					  (window-pointer-y)
					  button))))
      (:mouse-button-up-event (:button button :x x :y y)
			      (setf *pointer-x* x *pointer-y* y)
			      (let ((block (hit-blocks 					  
					    (window-pointer-x)
					    (window-pointer-y)
					    *blocks*)))
				(when block
				  (send :release block
					(window-pointer-x)
					(window-pointer-y)
					button))))
      (:joy-button-down-event (:button button :state state)
		      (send-event (make-event :raw-joystick (list button :button-down)))
		      (when (assoc button (joystick-buttons))
			(update-joystick-button button state)
			(send-event (make-event :joystick
						(list (button-to-symbol button) 
						      :button-down)))))
      (:joy-button-up-event (:button button :state state)  
			    (send-event (make-event :raw-joystick (list button :button-up)))
			    (when (assoc button (joystick-buttons))
			      (update-joystick-button button state)
			      (send-event (make-event :joystick
						      (list (button-to-symbol button) 
							    :button-up)))))
      (:joy-axis-motion-event (:axis axis :value value)
			      (update-joystick-axis axis value))
      (:video-expose-event () (sdl:update-display))
      (:key-down-event (:key key :mod-key mod :unicode unicode)
		       (send-event
			(make-event 
			 ;; translate data items from SDL format to internal
			 (cons (make-key-symbol key)
			       (when (not (zerop unicode))
				 (string (code-char unicode))))
			 (mapcar #'make-key-modifier-symbol mod))))
;      (:key-up-event (:key key :mod-key mod :unicode unicode)
      (:idle ()
	     ;; this lets slime keep working while the main loop is running
	     ;; in sbcl using the :fd-handler swank:*communication-style*
	     #+(and sbcl (not sb-thread)) (restartably
					   (sb-sys:serve-all-events 0))	 
	     (sdl:with-timestep (do-update))
	     (restartably
	       (gl:clear-color 0 0 0 1)
	       (gl:clear)
	       (draw-blocks)
	       (gl:flush)
	       (sdl:update-display))))))

;;; The user configuration file

(defparameter *user-init-file-name* "blocky-init.lisp")

(defun load-user-init-file ()
  (let ((type :unspecific)) ;; possible sbcl non-compliant behavior
    (let ((file (merge-pathnames (make-pathname :name *user-init-file-name*
						:type type)
				 (blocky-directory))))
      (when (cl-fad:file-exists-p file)
	(load (cl-fad:pathname-as-file file))))))

(defparameter *user-keyboard-layout* :qwerty)

(defparameter *use-sound* t "Non-nil (the default) is to use sound. Nil disables sound.")

;;; BLX resource interchange files

(defparameter *resource-file-extension* ".blx"
"BLX is a simple Lisp data interchange file format. An BLX file can
contain one or more data resources. A 'resource' is an image, sound,
text, font, lisp program, or other data whose interpretation is up to
the client.

An BLX resource can be either self-contained, or point to an
external file for its data.

A 'resource record' defines a resource. A resource record is a
structure with the following elements:

 :NAME    A string; the name of the resource.
          The colon character : is reserved and used to specify 
          resource transformations; see below.
 :TYPE    A keyword symbol identifying the data type.
          Corresponding handlers are the responsibility of the client.
          See also `*resource-handlers*' and `load-resource'.

          The special type :blx is used to load the blx file
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
          Relative to directory of BLX file.
 :DATA    Lisp data encoding the resource itself, if any.

In memory, these will be represented by resource structs (see below).
On disk, it's Lisp data printed as text. This text should compress very
well.

The string '()' is a valid .BLX file; it contains no resources.")

(defstruct resource 
  name type properties file data object system-p)

;; The extra `object' field is not saved in .BLX files; it is used to
;; store driver-dependent loaded resources (i.e. SDL image surface
;; objects and so on). This is used in the resource table.
;; The system-p field is likewise not stored. 

(defun resource-to-plist (res)
  "Convert the resource record RES into a property list.
This prepares it for printing as part of a BLX file."
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
			:if-exists :supersede
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
      (let ((*read-eval* nil))
	(prog1 (loop as sexp = (read file nil *eof-value*)
		     until (eq *eof-value* sexp)
		     collect sexp)
	  (message "Reading data from ~A... Done." filename))))))

;; Now tie it all together with routines that read and write
;; collections of records into BLX files.

(defun save-resource-file (filename resources)
  "Write the RESOURCES to the BLX file FILENAME."
  (write-sexp-to-file filename (mapcar #'resource-to-plist resources)))

(defun load-resource-file (filename &optional system-p)
  "Return a list of resources from the BLX file FILENAME."
  (labels ((resourcep (s)
	     (keywordp (first s))))
    ;; read the file
    (let ((sexp (read-sexp-from-file filename)))
      ;; find the resource plists; see `read-sexp-from-file'
      (mapcar #'(lambda (s)
		  (let ((resource (apply #'make-resource s)))
		    (prog1 resource
		      (setf (resource-system-p resource) system-p))))
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

(defparameter *project-directory-extension* ".blocky")

(defvar *project-path* nil "The pathname of the currently opened project. 
This is where all saved objects are stored.")

(defvar *after-load-project-hook* nil)

(defvar *executable* nil "Non-nil when running Blocky from a saved
binary image.")

(defparameter *untitled* "BLOCKY")

(defvar *project* *untitled*
  "The name of the current project.")

(defvar *recent-projects* nil)

;;; Project packages

(defun standard-project-p (&optional (project *project*))
  (string= "STANDARD" (string-upcase project)))

(defun untitled-project-p (&optional (project *project*))
  (string= project *untitled*))

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
  (user-homedir-pathname))
   ;; (cl-fad:pathname-as-directory 
   ;;  (make-pathname :name *projects-directory*
   ;; 		   :defaults
   
(defun project-directory-name (project)
  (assert (stringp project))
  (remove #\Space project))

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
  (let ((dirs (append (list (user-homedir-pathname)
			    (asdf:system-relative-pathname 'blocky ""))
		      *project-directories*)))
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
       (message "Cannot find project ~s in paths ~S. Try checking your *PROJECTS-DIRECTORIES* settings in the BLOCKY-INIT.LISP configuration file. Continuing..."
		project dirs)))))

(defun set-resource-pathname (resource)
  (when (stringp (resource-file resource))
    (setf (resource-file resource)
	  (merge-pathnames (resource-file resource)
			   (find-project-path *project*)))))

(defun file-name-extension (name)
  (let ((pos (position #\. name :from-end t)))
    (when (numberp pos)
      (subseq name (1+ pos)))))

(defparameter *resource-extensions*
  '(("png" :image)
    ("wav" :sample)
    ("ogg" :music)
    ("lisp" :lisp)))

(defun resource-type-from-name (name)
  (let ((extension (file-name-extension name)))
    (when extension
      (car (cdr (assoc extension *resource-extensions* :test 'equal))))))

(defun index-resource (resource)
  "Add the RESOURCE's record to the resource table.
If a record with that name already exists, it is replaced.  However,
if the resource is an :alias, just the string name of the target
resource is stored; see also `find-resource'."
  (set-resource-pathname resource)
  (setf (gethash (resource-name resource)
		 *resources*) 
	resource))

(defun defresource-expand-plist (plist)
  (destructuring-bind 
      (&key name type file properties &allow-other-keys) plist
    (list :name name 
	  :type (or type (resource-type-from-name name))
	  :properties properties
	  :file (or file name))))

(defmacro defresource (&body entries)
  (let ((plists
	  (cond
	    ;; variable
	    ((and (symbolp (first entries))
		  (boundp (first entries)))
	     (mapcar #'defresource-expand-plist 
		     (symbol-value (first entries))))
	    ;; short form: (defresource "file.ext" &rest PROPERTIES)
	    ((stringp (first entries))
	     (list 
	      (defresource-expand-plist 
		  (list :name (first entries)
			:properties (rest entries)))))
	    ;; inline: (defresource :name ...)
	    ((keywordp (first entries))
	     (list 
	      (defresource-expand-plist entries)))
	    ;; list of property lists
	    ((every #'consp entries)
	     (mapcar #'defresource-expand-plist entries)))))
    `(eval-when (:load-toplevel)
       (blocky:add-resources ',@plists))))
 
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

(defparameter *object-index-filename* "index.blx")

(defun load-project-objects (project)
  (let ((object-index-file (find-project-file project *object-index-filename*)))
    (when (cl-fad:file-exists-p object-index-file)
      (message "Reading saved objects from ~S" object-index-file)
      (index-resource-file project object-index-file))))

(defun load-project-lisp (project)
  (unless (or (untitled-project-p project)
	      (standard-project-p project))
    (let ((lisp (default-project-lisp-file project)))
      (if (cl-fad:file-exists-p lisp)
	  (progn (message "Loading lisp for project ~A..." project)
		 (load lisp))
	  (message "No default lisp file found in project ~S. Continuing..." project)))))

(defun create-project-image (project &key folder-name parent)
  (if (null project)
      (prog1 nil (message "Cannot create project. You must choose a project name."))
      (let* ((directory (or parent (projects-directory)))
	     (dirs (mapcar #'string-upcase (find-directories directory))))
	(if (find project dirs :test 'equal)
	    (prog1 nil 
	      (message "Cannot create project ~A, because a folder with this name already exists in ~A"
		       project directory))
	    (let ((dir (if folder-name 
			   (default-project-pathname folder-name)
			   (default-project-pathname project))))
	      (message "Creating new project ~A in directory ~A..." project dir)
	      (setf *project* project)
	      (prog1 dir
		(make-directory-maybe dir)
		(message "Finished creating directory ~A." dir)
		(message "Finished creating project ~A." project)))))))

(defun project-package ()
  (find-package (make-keyword *project*)))2

(defun load-project-image (project &key without-database with-database)
  (assert (stringp project))
  (message "Opening project: ~A" (string-upcase project))
  (setf *project* project)
  (setf *project-path* (search-project-path project))
  ;; check path
  (message "Set project path to ~A" (namestring *project-path*)) 
  ;; load any .blx files
  (index-project project)
  ;; TODO support :with-database arg as well
  (unless without-database
    (load-database)
    (load-variables))
  (when without-database
    (message "Starting without database or variables loading, due to user command."))
  (message "Started up successfully. Indexed ~A resources." (hash-table-count *resources*)))
 
(defun load-project (&optional (project *project*) parameters)
  ;; don't load database by default
  (destructuring-bind (&key (without-database t) with-database) parameters
    (load-project-image project 
			:without-database without-database
			:with-database with-database)
    ;; load any pending resource defs
    (dolist (plist *pending-resources*)
      ;; (message "processing. ~A remaining" (length *pending-resources*))
      (index-resource (apply #'make-resource plist)))))

(defun directory-is-project-p (dir)
  "Test whether a directory has the .blocky suffix."
  (let ((index-filename (concatenate 'string
				     (file-namestring dir)
				     *resource-file-extension*)))
    (cl-fad:file-exists-p (make-pathname :name index-filename
			       :directory (if (stringp dir)
					      dir
					      (namestring dir))))))

(defun find-directories (dir)
  (mapcar #'(lambda (s)
	      (subseq s 0 (1- (length s))))
	  (mapcar #'namestring
		  (directory (concatenate 'string (namestring dir) "/*/")))))

(defun find-projects-in-directory (dir)
  "Search DIR for projects and return a list of their names."
  (remove-if-not #'directory-is-project-p (find-directories dir)))

(defun find-all-projects ()
  (mapcar #'file-namestring
	  (mapcan #'find-projects-in-directory *project-directories*)))

(defun index-resource-file (project-name resource-file &optional system-p)
  "Add all the resources from the resource-file RESOURCE-FILE to the resource
table. File names are relative to the project PROJECT-NAME."
  (let ((resources (load-resource-file resource-file system-p)))
    (message "Loading ~A resources from file ~A:~A..." (length resources)
	     project-name resource-file)
    (dolist (res resources)
      (if (eq :blx (resource-type res))
	  ;; we're including another blx file. if :data is specified,
	  ;; take this as the name of the project where to look for
	  ;; that blx file and its resources.
	  (let ((include-project (or (resource-data res) 
				     project-name)))
	    (index-resource-file include-project (find-project-file include-project
							  (resource-file res))))
	  ;; we're indexing a single resource.
	  (index-resource res)))))

	    ;; ;; save the resource name for later autoloading, if needed
	    ;; (when (getf (resource-properties res) :autoload)
	    ;;   (push res *pending-resources*)))))))

(defun index-project (project-name)
  "Add all the resources from the project PROJECT-NAME to the resource
table."
  (let ((index-file (find-project-file project-name *object-index-filename*)))
    (if (cl-fad:file-exists-p index-file)
	(index-resource-file project-name index-file
			     (standard-project-p project-name))
	(message "Did not find index file ~A in project ~A. Continuing..."
		 index-file project-name))))

;;; Standard resource names

(defvar *default-font* "default-font")

;;; Creating, saving, and loading object resources in BLX files

;; See also the documentation string for `*resource-file-extension*'.

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
  "Save an object resource to disk as {PROJECT-NAME}/{RESOURCE-NAME}.BLX."
  (setf (resource-data resource) (serialize (resource-object resource)))
  (save-resource-file (find-project-file project 
				(concatenate 'string (resource-name resource)
					     *resource-file-extension*))
	     (list resource))
  (setf (resource-data resource) nil))

(defun special-resource-p (resource)
  (string= "*" (string (aref (resource-name resource) 0))))

(defun make-resource-link (resource)
  (make-resource :type :blx 
		 :file (concatenate 'string
				    (resource-name resource)
				    *resource-file-extension*)))
  
(defun save-resource (name resource)
  (let ((pathname (resource-file resource))
	(link (make-resource-link resource)))
    (prog1 link 
      (if (eq :object (resource-type resource))
	  ;; we want to index them all, whether or not we save them all.
	  ;; make a link resource (i.e. of type :blx) to pull this in later
	  (save-object-resource resource)
	  ;; just a normal resource
	  (setf (resource-file link) (namestring pathname)
		(resource-data link) nil)))))

(defun save-project-image (&optional force)
  (let ((*already-serialized* (make-hash-table :test 'equal)))
    (let (index)
      (if (or (standard-project-p)
	      (untitled-project-p))
	  (message "Cannot save this project.")
	  (labels ((save (name resource)
		     (unless (resource-system-p resource)
		       (push (save-resource name resource) index))))
	    (message "Saving project ~S ..." *project*)
	    ;; (maphash #'save *resources*)
	    ;; FIXME: allow to save resources in separate file
	    (save-resource-file (find-project-file *project* *object-index-filename*)
				(nreverse index))
	    (save-database)
	    (save-variables)
	    (prog1 t (message "Saving project ~S ... Done." *project*)))))))

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

(defvar *default-texture-filter* :mipmap)
(defvar *font-texture-filter* :linear)

(defun use-filter (filter)
  ;; set filtering parameters
  (case filter
    (:linear (gl:tex-parameter :texture-2d :texture-min-filter :linear)
     (gl:tex-parameter :texture-2d :texture-mag-filter :linear))
    (:mipmap (gl:tex-parameter :texture-2d :generate-mipmap t) 
     (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear))
    (:nearest (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
     (gl:tex-parameter :texture-2d :texture-mag-filter :nearest))))

(defun load-texture 
    (surface &key source-format (internal-format :rgba)
		  (filter *default-texture-filter*))
  ;; don't make any bogus textures
  (when *gl-window-open-p*
    (let ((texture (car (gl:gen-textures 1))))
      (gl:bind-texture :texture-2d texture)
      ;; set up filtering
      (use-filter filter)
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
      texture)))

(defvar *textures* nil)

(defun initialize-textures-maybe (&optional force)
  (when (or force (null *textures*))
    (setf *textures* (make-hash-table :test 'equal))))

(defun delete-all-textures ()
  (maphash #'(lambda (name texture)
	       (let ((resource (find-resource name)))
		 (setf (resource-object resource) nil))
	       (gl:delete-textures (list texture)))
	   *textures*)
  (initialize-textures-maybe :force))

(defun cache-image-texture (name)
  (initialize-textures-maybe)
  (let* ((resource (find-resource name))
	 (surface (resource-object resource))
	 (source-format (getf (resource-properties resource) :format))
	 (internal-format :rgba)
	 (texture (load-texture surface
				:source-format source-format
				:internal-format internal-format))
	 (old-texture (gethash name *textures*)))
    (when texture
      (prog1 texture
	;; delete old texture if needed
	(when old-texture
	  (gl:delete-textures (list old-texture))
	  (remhash name *textures*))))))

(defun find-texture (name)
  (assert (stringp name))
  (initialize-textures-maybe)
  ;; make sure underlying image is loaded by SDL
  (find-resource name) 
  ;; see if we need to pump it to the video card
  (or (gethash name *textures*)
      ;; store the new texture and return it
      (setf (gethash name *textures*) 
	    (cache-image-texture name))))
  
(defun load-image-resource (resource)
  "Loads an :IMAGE-type BLX resource from a :FILE on disk."
  (initialize-textures-maybe)
  (let ((surface (sdl-image:load-image (namestring (resource-file resource))
				       :alpha 255)))
    (prog1 surface
      ;; cache height and width as properties
      (setf (resource-properties resource)
	    (append (list :height (sdl:height surface)
			  :width (sdl:width surface))
		    (resource-properties resource))))))

(defun load-sprite-sheet-resource (resource)
  "Loads a :SPRITE-SHEET-type BLX resource from a :FILE on disk. Looks
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
  nil)
  ;; (let ((props (resource-properties resource)))
  ;;   (if (null props)
  ;; 	(error "Must set properties for bitmap font.")
  ;; 	(destructuring-bind (&key width height character-map color-key) props
  ;; 	  (sdl-gfx:initialise-font (make-instance 'SDL:simple-font-definition
  ;; 						  :width width :height height
  ;; 						  :character-map character-map
  ;; 						  :color-key (apply #'sdl:color color-key)
  ;; 						  :filename (resource-file resource)
  ;; 						  :pad-x 0 :pad-y 0))))))
    
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
	       (when (and (null (object-name object))
			  (not (%garbagep object)))
		 (setf (gethash uuid database2) object))))
      (maphash #'store database) ;; copy into database2
      (values (make-resource :name "--database--"
			     :type :database
			     :data (serialize database2))
	      (hash-table-count database2)))))

(defun empty-garbage (&optional (database *database*))
  (loop for object being the hash-keys of database do
    (when (garbagep object)
      (remhash object database))))

(defun database-file ()
  (assert (not (null *project*)))
  (find-project-file *project* "database.blx"))

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
      (save-resource-file file (list resource))
      (message "Finished saving database into ~A. Continuing..." file))))
      
(defun load-database (&optional (file (database-file)))
  (message "Looking for object database ~A..." file)
  (if (cl-fad:file-exists-p file)
      (let ((resources (load-resource-file file)))
	(message "Read ~S resources from ~A" (length resources) file)
	(let ((database (first resources)))
	  (assert (eq :database (resource-type database)))
	  (load-database-resource database)))
      (message "No database file found. Continuing...")))

;;; Loading/saving variables

(defvar *system-variables* '(*recent-projects* *joystick-profile*
  *user-joystick-profile* *joystick-axis-size* *joystick-dead-zone*))

(defvar *safe-variables* '(*frame-rate* *updates* *screen-width*
*screen-height* *buffer* *blocks* *dt* *pointer-x* *author* *project*
*joystick-profile* *user-joystick-profile* *joystick-axis-size* 
*joystick-dead-zone* *pointer-y* *resizable* *window-title* *buffers*
*scale-output-to-window* *persistent-variables*))

(defvar *persistent-variables* '(*frame-rate* *updates* 
				 
				 ;; *screen-width* *screen-height*
				 *buffer* *blocks* *dt* *pointer-x* *author* 
				 *project* *buffers* *scale-output-to-window* 
				 *pointer-y* *resizable*
				 *window-title*
				 ;; notice that THIS variable is also
				 ;; persistent!  this is to avoid
				 ;; unwanted behavior changes in
				 ;; modules when the default value
				 ;; changes.
				 *persistent-variables*))

(defparameter *persistent-variables-file-name* "variables.blx")

(defun persistent-variables-file (&optional (project *project*))
  (find-project-file project *persistent-variables-file-name*))

(defun make-variable-resource (name &optional nodup)
  (assert (and (symbolp name)
	       (boundp name)))
  (assert (member name *safe-variables*))
  (assert (not (eq name '*safe-variables*)))
  (make-resource :name name
		 :type :variable
		 :data (serialize (symbol-value name))))

(defun load-variable-resource (resource)
  (assert (eq :variable (resource-type resource)))
  (let ((name (resource-name resource)))
    (assert (member name *safe-variables*))
    (message "Setting variable: ~S..." name)
    (setf (symbol-value name)
	  (deserialize (resource-data resource)))
    (setf (resource-data resource) nil)))

(defun save-variables (&optional (variables *persistent-variables*))
  (with-standard-io-syntax
    (message "Saving system variables ~A..." variables)
    (save-resource-file (persistent-variables-file)
	       (mapcar #'make-variable-resource variables))
    (message "Finished saving system variables.")))

(defun load-variables ()
  (with-standard-io-syntax
    (let ((file (persistent-variables-file)))
      (if (cl-fad:file-exists-p file)
	  (progn 
	    (message "Loading system variables from ~A..." file)
	    (mapc #'load-variable-resource 
		  (load-resource-file file))
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

(defun transformable-resource-p (name)
  (eq (aref name 0)
      *resource-transformation-delimiter*))

(defun next-transformation (name)
  (assert (transformable-resource-p name))
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
  (assert (transformable-resource-p name))
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

(defun scale-image (image &optional (factor 1)) nil)
;;   "Return a scaled version of IMAGE, scaled by FACTOR.
;; Allocates a new image."
;;   (assert (integerp factor))
;;   (lispbuilder-sdl-gfx:zoom-surface factor factor
;; 				    :surface (resource-object image)
;; 				    :smooth nil))

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
	    (funcall handler resource))
      (assert (resource-object resource)))
    (when (null (resource-object resource))
      (error "Failed to load resource ~S." (resource-name resource)))))
	;; (message "Loaded resource ~S with result type ~S." 
	;; 	 (resource-name resource)
	;; 	 (type-of (resource-object resource))))))
	   
;; (defun make-file-resource-automatically (name &optional properties)
;;   (let ((type (resource-type-from-name name)))
;;     (make-resource :name name :file name :type type :properties properties)

;; (defun index-resource-automatically (parameters)
;;   (let ((res (make-file-resource-automatically name properties)))
;;     (index-resource 
;;      (or res (apply #'make-resource :name name 

;; (defun load-resource-automatically (name)
;;   (load-resource (index-resource-automatically name)))

(defun find-resource (name &optional noerror)
  "Obtain the resource named NAME, performing any necessary
loading. Unless NOERROR is non-nil, signal an error when NAME cannot
be found."
  ;; can we find the resource straight off? 
  (let ((res (gethash name *resources*)))
    (if (resource-p res)
	;; yes, return it and possibly load on demand
	(prog1 res
	  (when (null (resource-object res))
	    (load-resource res)))
	(if noerror
	    nil
	    (error "Cannot find resource ~S" name)))))

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

(defun set-resource-system-p (resource &optional (value t))
  (let ((res (find-resource resource)))
    (setf (resource-system-p res) value)))

(defun delete-all-resources ()
  (loop for resource being the hash-values in *resources*
	do (let ((object (resource-object resource)))
	     (when object
	       (case (resource-type resource)
		 (:image (sdl:free object))
		 (:music (sdl-mixer:free object))
		 (:sample (sdl-mixer:free object)))))
	   (initialize-resource-table)))
	   
(defun clear-cached-images ()
  (loop for resource being the hash-values in *resources*
	do (let ((object (resource-object resource)))
	     (when (and object 
			(eq :image (resource-type resource)))
	       (sdl:free object)
	       (setf (resource-object resource) nil))))
  (delete-all-textures))

(defun clear-cached-text-images ()
  (maphash #'(lambda (key value)
	       (declare (ignore key))
	       (gl:delete-textures (list value)))
	   (get-memo-table 'find-text-image))
  (clear-memoize 'find-text-image))

(defvar *clear-cached-images-on-resize* nil)

;;; Custom audio generation

(defvar *frequency* 44100)

(defvar *output-chunksize* 2048)

(defvar *output-channels* 2)

(defvar *sample-format* SDL-CFFI::AUDIO-S16LSB)

(defvar *channels* 256 "Number of audio mixer channels to use.")

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
;      (load-resource resource)
      (assert (eq :sample (resource-type resource)))
      (assert (not (null (resource-object resource))))
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
				    ;; :enable-callbacks t
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

(defun-memo percent-gray (percentage)
    (:key #'first :test 'equal :validator #'identity)
  (format nil "gray~S" (truncate (abs percentage))))

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
  (find-resource-property image :height))

(defun image-width (image)
  "Return the width in pixels of IMAGE."
  (find-resource-property image :width))

;; &optional (u1 0) (v1 0) (u2 1) (v2 1))
(defun draw-textured-rectangle (x y z width height texture 
				&key (blend :alpha) (opacity 1.0) (vertex-color "white"))
  (if (null blend)
      (gl:disable :blend)
      (progn (enable-texture-blending)	
	     (set-blending-mode blend)))
  (gl:bind-texture :texture-2d texture)
  (set-vertex-color vertex-color opacity)
  (gl:with-primitive :quads
    (let (;;(x1 x)
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

(defvar *image-opacity* nil)

(defun draw-image (name x y &key (z 0.0) (blend :alpha) (opacity 1.0) height width)
  (let ((image (find-resource-object name)))
    (draw-textured-rectangle
     x y z 
     (or width (sdl:width image))
     (or height (sdl:height image))
     (find-texture name)
     :blend blend 
     :opacity (or *image-opacity* opacity))))

;;; Indicators

(defparameter *active-indicator-color* "yellow")
(defparameter *inactive-indicator-color* "gray70")

(defun indicator-size () (* 0.37 (font-height *font*)))

(defparameter *indicators* 
  '(:asterisk :bang :top-left-triangle :bottom-right-triangle
    :down-triangle-open :down-triangle-closed :copy :paste :cut
    :menu :collapse :move :resize :define :close))

(defparameter *indicator-images* 
  '(:asterisk "asterisk"
    :bang "bang"
    :top-left-triangle "top-left-triangle-indicator"
    :down-triangle-open "down-triangle-open"
    :down-triangle-closed "down-triangle-closed"
    :menu "menu"
    :collapse "collapse"
    :move "move"
    :copy "copy"
    :cut "cut"
    :paste "paste"
    :drop "downright"
    :pick-up "upleft"
    :resize "resize"
    :define "define" 
    :close "close"
    :bottom-right-triangle "bottom-right-triangle-indicator"))

(defun find-indicator-texture (indicator)
  (assert (keywordp indicator))
  (let ((texture-name (getf *indicator-images* indicator)))
    (assert (stringp texture-name))
    (find-texture texture-name)))

(defun draw-indicator (indicator x y &key color (scale 1) (state :inactive)
					  background)
  (let ((size (indicator-size)))
    (when background
      (draw-circle (+ x size (dash 1))
		   (+ y size (dash 1)) (* (/ scale 2) size) :color background :type :solid))
    (draw-textured-rectangle x y 0 (* scale size) (* scale size)
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

(defun-memo font-text-width (string &optional (font *font*))
    (:key #'identity :test 'equal :validator #'identity)
  (sdl:get-font-size string :size :w :font (find-resource-object font)))

(defun font-text-extents (string font)
  (let ((resource (find-resource font)))  
    (ecase (resource-type resource)
      (:font (* (length string)
		(font-width font)))
      (:ttf (values (font-text-width string font)
		    (font-height font))))))

(defparameter *use-antialiased-text* t)

(defun make-text-image (font string)
  (assert (and (not (null string))
	       (plusp (length string))))
  (multiple-value-bind (width height)
      (font-text-extents string font)
    (let ((surface (sdl:create-surface width height :bpp 8))
	  (texture (first (gl:gen-textures 1)))
	  (renderer (if *use-antialiased-text*
			#'sdl:draw-string-blended-*
			#'sdl:draw-string-solid-*)))
      (prog1 texture
	(funcall renderer string 0 0 
		 :color (find-resource-object "white")
		 :font (find-resource-object font)
		 :surface surface)
	(gl:bind-texture :texture-2d texture)
	(use-filter *font-texture-filter*)
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

(defun set-vertex-color (color &optional (alpha 1))
  (assert (stringp color))
  (destructuring-bind (red green blue) 
      (gl-color-values color)
    (gl:color red green blue alpha)))

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
 		 &key (color "black") (alpha 1))
  (set-vertex-color color alpha)
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
  (declare (ignore blend))
  (draw-circle x y radius :color color :type :solid))

;;; Engine status

(defun quit (&optional shutdown)
  (when shutdown 
    (setf *quitting* t))
  (setf *project* nil)
  (sdl:push-quit-event))

(defvar *library-search-paths-setup-hook* nil)

(defun setup-library-search-paths ()
  (run-hook '*library-search-paths-setup-hook*)
  #+darwin (setf cffi:*foreign-library-directories*
                 (union cffi:*foreign-library-directories*
                        '(#P"/opt/local/lib" #P"/sw/lib/")
                        :test #'equal))
  )

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
    ;; (cffi:define-foreign-library sdl-gfx
    ;;   (:darwin (:or (:framework "SDL_gfx")
    ;; 		    (:default "libSDL_gfx")))
    ;;   (:unix (:or "libSDL_gfx-1.2.so.0.7.2"
    ;; 		  "libSDL_gfx-1.2.so.0"
    ;; 		  "libSDL_gfx-1.2.so"
    ;; 		  "libSDL_gfx.so.4"
    ;; 		  "libSDL_gfx.so.13"
    ;; 		  "libSDL_gfx.so"
    ;; 		  "libSDL_gfx")))
    ;; (cffi:use-foreign-library sdl-gfx)
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
  (load-project-image "standard"))

(defun start-up ()
  #+linux (do-cffi-loading)
  ;; add library search paths for Mac if needed
  (setup-library-search-paths)
  ;; get going...
  (message "Starting Blocky...")
  (print-copyright-notice)
  (setf *blocks* nil
	*project-folder* nil
	*buffer* nil
	*project* nil
	*clipboard* nil
	*event-hook* nil
	*overlay* nil
	*message-hook-functions* nil
	*window-title* "Blocky"
	*updates* 0
	*resizable* t
	*random-state* (make-random-state t))
  (sdl:init-sdl :video t :audio t :joystick t)
  (load-user-init-file) ;; this step may override *project-directories* and so on 
  (initialize-resource-table)
  (initialize-textures-maybe :force)
  (initialize-colors)
  (initialize-sound)
  (initialize-database)
  (initialize-clipboard-maybe :force)
  (initialize-buffers)
  (load-standard-resources)
  (setf *project* *untitled*)
  (sdl:enable-unicode)
  (enable-key-repeat))

(defun shut-down ()
  ;; delete any cached textures and surfaces
  (clear-text-image-cache)
  (delete-all-textures)
  (purge-all-objects)
  (delete-all-resources)
  (setf *buffers* nil)
  (sdl-mixer:halt-music)
  (sdl-mixer:close-audio t)
  (setf *buffer* nil)
  (setf *project-folder* nil)
  (setf *blocks* nil)
  (setf *overlay* nil)
  (setf *clipboard* nil)
  (setf *dt* nil)
  (setf *frame-rate* *default-frame-rate*)
  (setf *event-hook* nil)
  (sdl:quit-sdl)
  (setf *gl-window-open-p* nil))

(defun reset ()
  (shut-down)
  (start-up))

(defmacro with-session (&rest body)
  `(progn 
     (start-up)
     ,@body
     (shut-down)))

;; (defun play-project (&optional (project *project*) &rest parameters)
;;   ;; deprecated
;;   (destructuring-bind (&key without-database with-database) parameters
;;     (with-session
;; 	(load-project-image project 
;; 			    :without-database without-database
;; 			    :with-database with-database)
;;       (when (null *blocks*)
;; 	(start (new 'buffer)))
;;       (start-session))))

(defun edit (project)
  (with-session
      (load-project-image project)
    (start-session)))

(defvar *buffer-history* nil)

(defun browse (name &optional prototype)
  (let ((page (find-buffer name :prototype prototype)))
    (when page
      (push name *buffer-history*)
      (at-next-update (start-alone page)))))

(defun back ()
  (let ((name (pop *buffer-history*)))
    (when name
      (at-next-update 
       (start-alone (find-buffer name))))))

(defun blocky ()
  (with-session
    (start-alone (find-buffer *desktop*))
    (start-session)))

;;; Editor transport control

(defun pause ()
  (prog1 nil (transport-pause (current-buffer))))

(defun rewind ()
  (prog1 nil (transport-rewind (current-buffer))))

(defun play ()
  (prog1 nil (transport-play (current-buffer))))

(defun toggle-play ()
  (prog1 nil (transport-toggle-play (current-buffer))))

(defun update-parameters () nil)
  ;; (when (current-buffer)
  ;;   (send :update-future (current-buffer))))

(defun current-buffer () *buffer*)

(defun visit (&optional (buffer (current-buffer)))
  (at-next-update (start-alone buffer)))

;;; Emacs integration

(defun eval-in-emacs (expression)
  (if (find-package :swank)
      (let ((sym (intern "EVAL-IN-EMACS" (find-package :swank))))
	(funcall sym expression))
      (message "(eval-in-emacs) failed; swank/emacs not available?")))

(defun glass-toggle ()
  (eval-in-emacs '(glass-toggle)))

(defun glass-show ()
  (eval-in-emacs '(glass-show)))

(defun glass-hide ()
  (eval-in-emacs '(glass-hide)))

(defun glass-show-at (x y)
  (eval-in-emacs 
   `(glass-show :x ,x :y ,y)))

;; (defun stop ()
;;   (error "Not yet implemented."))

;; (defun create (project)
;;   (with-session
;;     (assert (stringp project))
;;     (create-project project)
;;     (load-project-image project)
;;     (start (new 'buffer))
;;     (start-session)))

;; (defun edit (&optional (project *untitled*) force-shell)
;;   (with-session
;;     (let ((*edit* t))
;;       (load-project-image project :no-error)
;;       (when force-shell
;; 	(start (new 'buffer)))
;;       (start-session))))

;;; console.lisp ends here
