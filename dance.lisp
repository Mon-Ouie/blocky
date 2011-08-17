;;; dance.lisp --- rhythm tools for USB dance pads

;; Copyright (C) 2009, 2010  David O'Toole

;; Author: David O'Toole <dto@ioforms.org>
;; Keywords: games

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
 
;;; Overview

(in-package :blocky)

;;; Dance pad support

;;    The diagram below gives the standard dance pad layout for
;;    BLOCKY. Many generic USB and/or game console compatible dance
;;    pads are marked this way. (Some pads are printed with "back"
;;    instead of "select").
;;
;;    select  start
;;    -------------
;;    |B  |^  |A  |
;;    |___|___|___|
;;    |<  |   |>  |
;;    |___|___|___|
;;    |Y  |v  |X  |
;;    |   |   |	  |
;;    -------------
;;
;;    A 10-button dance pad is required (i.e. all four corners must be
;;    buttons, as well as the orthogonal arrows and select/start.)
;;    Konami's older soft home pads lack the lower corner buttons, so
;;    they won't be usable even with a USB adapter. Most generic dance
;;    pads will work just fine. I've also tested this successfully
;;    with the new Konami USB pad for PS3.

;;; Dance gesture input

(defparameter *orthogonal-arrows* '(:left :down :up :right)) ;; in standard order

(defparameter *diagonal-arrows* '(:upleft :upright :downleft :downright))

(defparameter *function-buttons* '(:select :start))

(defparameter *punctuation* '(:period :blank))

(defparameter *dance-arrows* (append *orthogonal-arrows* *diagonal-arrows*))

(defparameter *dance-pad-symbols*
  (append *punctuation* *dance-arrows* *function-buttons*))

;; Configure the BLOCKY engine so that it translates dance pad button presses
;; into standard BLOCKY joystick events.

(setf blocky:*joystick-button-symbols* *dance-pad-symbols*)

;;; Representing dance moves as Lisp lists

;; The global BPM determines the length of a beat. A dance move is a
;; list of events, optionally prefixed with an indicator N. Each
;; element of a move is either 1/N beats long (when N is a nonzero
;; natural number) or are all simultaneous when N is the symbol
;; +. Each element is either a symbol from `*dance-arrows*', the
;; symbol - (meaning no step), or a sublist specifying another dance
;; move. N can be specified at the beginning of a move but the default
;; is 1 if left out. If N is the symbol +, then all the steps in the
;; list must happen simultaneously.

;; Examples:

;; A simple 4-beat dance move: (left right downleft downright)   
;; A dance move with several 8th notes: ((2 left right) left down right)
;; A jump: (+ left right)
;; Two consecutive jumps: ((+ right down) (+ left down))
;; 16th notes: ((4 left - - right) downleft downright down)
;; Urban flow style: 
;; ((2 left right) (+ upleft downright) - (2 (+ downleft upright) left right upright) (+ downleft upright))

(defun hflip (step)
  (case step
    (:up :up)
    (:down :down)
    (:left :right)
    (:right :left)
    (:upleft :upright)
    (:downleft :downright)
    (:upright :upleft)
    (:downright :downleft)))

(defun vflip (step)
  (case step
    (:up :down)
    (:down :up)
    (:left :left)
    (:right :right)
    (:upleft :downleft)
    (:downleft :upleft)
    (:upright :downright)
    (:downright :upright)))

(defun rturn (step)
  (case step
    (:up :right)
    (:right :down)
    (:down :left)
    (:left :up)
    (:upleft :upright)
    (:downleft :upleft)
    (:downright :downleft)
    (:upright :downright)))

(defun lturn (step)
  (case step
    (:up :left)
    (:right :up)
    (:down :right)
    (:left :down)
    (:upleft :downleft)
    (:downleft :downright)
    (:downright :upright)
    (:upright :upleft)))

(defun spin (step)
  (rturn (rturn step)))

;;; Input device configuration

;; The dance pad layout shown above is also available on the numeric
;; keypad.

(defparameter *dance-keybindings* 
  '(("Q" (:control) "quit .")
    ("KP8" nil "up .")
    ("KP4" nil "left .")
    ("KP6" nil "right .")
    ("KP2" nil "down .")
    ("KP1" nil "downleft .")
    ("KP3" nil "downright .")
    ("KP7" nil "upleft .")
    ("KP9" nil "upright .")
    ("KP-ENTER" nil "start .")
    ("KP0" nil "select .")
    ("JOYSTICK" (:up :button-down) :up)   
    ("JOYSTICK" (:left :button-down) :left)
    ("JOYSTICK" (:right :button-down) :right)
    ("JOYSTICK" (:down :button-down) :down)
    ("JOYSTICK" (:downleft :button-down) :downleft)
    ("JOYSTICK" (:downright :button-down) :downright)
    ("JOYSTICK" (:upleft :button-down) :upleft)
    ("JOYSTICK" (:upright :button-down) :upright)
    ("JOYSTICK" (:start :button-down) :start)
    ("JOYSTICK" (:select :button-down) :select)))

;; Including configurations for common dance pads is a good idea.
;; Eventually we need a real configuration menu.


;; (setf *joystick-device-identifiers* 
;;       '(("
  
;;(defparameter *hyperkin-adaptor-string* "GASIA")

;; TODO (defparameter *konami-ps3-pad-mapping* )

(setf blocky:*joystick-mapping* *hyperkin-adapter-mapping*)

(defun get-button-index (arrow)
  (first (find arrow *joystick-mapping* :key #'cdr)))

;;; Displaying arrows as images

(defparameter *large-arrow-height* 64)
(defparameter *large-arrow-width* 64)

(defparameter *large-arrow-images* 
  '(:up "up" :left "left" :right "right" :down "down"
    :upright "upright" :downright "downright" :downright "downright" :upleft "upleft" 
    :period "period" :prompt "prompt" :blank "blank"))

(defparameter *medium-arrow-height* 32)
(defparameter *medium-arrow-width* 32)

(defparameter *medium-arrow-images* 
  '(:up "up-medium" :left "left-medium" :right "right-medium" :down "down-medium"
    :upright "upright-medium" :downright "downright-medium" :downleft "downleft-medium" :upleft "upleft-medium" 
    :period "period-medium" :prompt "prompt-medium" :blank "blank-medium"))

(defparameter *target-arrow-height* 64)
(defparameter *target-arrow-width* 64)

(defparameter *target-arrow-images* 
  '(:up "up-target" :left "left-target" :right "right-target" :down "down-target"
    :upright "upright-target" :downright "downright-target" :downleft "downleft-target" :upleft "upleft-target"))

(defparameter *dark-target-arrow-images* 
  '(:up "up-dark-target" :left "left-dark-target" :right "right-dark-target" :down "down-dark-target"
    :upright "upright-dark-target" :downright "downright-dark-target" :downleft "downleft-dark-target" :upleft "upleft-dark-target"))

(defun arrow-image (arrow &optional (size :medium))
  (let ((images (etypecase size
  		  (:large *large-arrow-images*)
  		  (:medium *medium-arrow-images*))))
    (getf images size)))

(defun arrow-formatted-string (arrow)
  (list nil :image (arrow-image arrow)))

;;; Tracker mode

(defvar *step-tolerance* 20 
  "Maximum time in milliseconds for delay between simultaneous steps.")

(defconstant +ticks-per-minute+ 60000 "Each tick is one millisecond.")

(defun ticks-per-beat (bpm)
  (float (/ +ticks-per-minute+ bpm)))

(define-prototype tracker 
  (:super blocky:=prompt= 
   :documentation "A tracker object is the engine for Track mode.")
  (beats-per-minute :initform 110) 
  (row-remainder :initform 0.0)
  voice playing
   start-position position events chart-name chart-row)

(defun event-time (event) (car event))
(defun event-arrow (event) (cdr event)) 

(define-method handle-key tracker (event)
  (let ((func (gethash event <keymap>)))
    (when (functionp func)
      (prog1 t (funcall func)))))
  
(define-method install-keybindings tracker ()
  (dolist (k *dance-keybindings*)
      (apply #'bind-key-to-prompt-insertion self k)))

(define-method quit tracker ()
  (blocky:quit))

(define-method select tracker ()
  (setf <start-time> nil)
  (setf <playing> nil))

(define-method left-shift-p tracker ()
  (plusp (poll-joystick-button (get-button-index :y))))

(define-method right-shift-p tracker ()
  (plusp (poll-joystick-button (get-button-index :x))))

(define-method either-shift-p tracker ()
  (or (/left-shift-p self)
      (/right-shift-p self)))

(define-method begin-chart tracker (chart-name)
  (setf <chart-start-time> (get-ticks))
  (setf <chart-name> chart-name)
  (setf <chart-row> 0))

(define-method start tracker ()
  (let ((time (get-ticks)))
    (setf <start-time> time)
    (setf <playing> t)
    (/begin-chart self "maniac3")
    (/update-timers self)
    (play-music "voxelay" :loop t)))

(define-method update-timers tracker ()
  (with-fields (beats-per-minute beat-position playing events
				 start-time position chart-name
				 chart-start-time row-remainder
				 chart-row) self
    (let ((time (get-ticks))
	  (minutes-per-tick (/ 1.0 +ticks-per-minute+)))
      (when playing
	(setf position (- time start-time))
	(setf beat-position (/ position (ticks-per-beat beats-per-minute)))
	(let* ((chart (find-resource-object chart-name))
	       (zoom (field-value :zoom chart)))
	  (let ((row (* zoom
			(/ (- time chart-start-time)
			   (ticks-per-beat beats-per-minute)))))
	    (multiple-value-bind (quotient remainder)
		(truncate row)
	      (setf chart-row quotient)
	      (setf row-remainder remainder)))))
      ;; expire button presses
      (labels ((expired (event)
		 (< *step-tolerance* 
		    (abs (- time (event-time event))))))
	(setf events (remove-if #'expired events))))))
      
(define-method do-arrow-event tracker (arrow)
  (/update-timers self)
  (let ((time (get-ticks)))
    (with-fields (events chart-name chart-row row-remainder) self
      (let ((chart (find-resource-object chart-name)))
	(assert chart)
	(push (cons time arrow) events)
	;; now test for hit
	(labels ((pressed (arrow)
		   (find arrow events :key #'cdr)))
	  (let ((steps (/row-steps chart chart-row)))
	  (when (and steps (every #'pressed steps)
		     ;; compare in milliseconds
		     (< (* (/ 1 (ticks-per-beat <beats-per-minute>))
			   row-remainder)
			*step-tolerance*))
	    (play-sample "vox4")
	    (setf events nil)
	    (let ((command (/row-command chart chart-row)))
	      (when command 
		(let ((command-string (/command-string command)))
		  (when (plusp (length command-string))
		    (/insert self command-string)
		    (/execute self))))))))))))

(define-method play tracker (sample)
  (play-sample sample))

(define-method downleft tracker ())

(define-method downright tracker ())

(define-method upleft tracker ())

(define-method upright tracker ()
  (let ((form (if (/either-shift-p self)
		  (/right-form *frame*)
		  (/left-form *frame*))))
    (/activate form)))

(define-method up tracker ()
  (/do-arrow-event self :up)
  (let ((form (if (/either-shift-p self)
		  (/right-form *frame*)
		  (/left-form *frame*))))
    (/move-cursor-up form)))
    
(define-method down tracker ()
  (/do-arrow-event self :down)
  (let ((form (if (/either-shift-p self)
		  (/right-form *frame*)
		  (/left-form *frame*))))
    (/move-cursor-down form)))

(define-method left tracker ()
  (/do-arrow-event self :left)
  (let ((form (if (/either-shift-p self)
		  (/right-form *frame*)
		  (/left-form *frame*))))
    (/move-cursor-left form)))

(define-method right tracker ()
  (/do-arrow-event self :right)
  (let ((form (if (/either-shift-p self)
		  (/right-form *frame*)
		  (/left-form *frame*))))
    (/move-cursor-right form)))
	  
(define-method render tracker ()
  (with-field-values (height width chart-name image chart-row
			     row-remainder) self
    (draw-box 0 0 width height :stroke-color ".black" :color ".black" 
	      :destination image)
    (let* ((chart (when chart-name (find-resource-object chart-name)))
	   (row chart-row)
	   (x 0)
	   (y (if row-remainder 
		  (round (- (* row-remainder *large-arrow-height*)))
		  0)))
      ;; draw targets
      (let ((images (if (< row-remainder 0.2)
			*target-arrow-images*
			*dark-target-arrow-images*)))
	(dolist (arrow *dance-arrows*)
	  (draw-resource-image (getf images arrow)
			       x 0 :destination image)
	  (incf x *large-arrow-width*)))
      ;; draw step chart
      (when chart
	(loop until (>= y height) 
	      do (setf x 0)
		 (dolist (arrow (/row-list chart (truncate row)))
		   (when arrow 
		     (draw-resource-image (getf *large-arrow-images* arrow)
					  x y :destination image))
		   (incf x *large-arrow-width*))
		 (incf row)
		 (incf y *large-arrow-height*))))))
  
(define-prototype help-prompt (:super =prompt=)
  (default-keybindings :initform '(("N" nil "page-down .")
				   ("P" nil "page-up ."))))

(define-prototype help-textbox (:super =textbox=))

(define-method render help-textbox ()
  (/parent>>render self)
  (/message *pager* 
	   (list (format nil " --- Showing lines ~A-~A of ~A. Use PAGE UP and PAGE DOWN to scroll the text." 
			 <point-row> (+ <point-row> <max-displayed-rows>) (length <buffer>)))))

(add-hook '*after-load-module-hook* (lambda ()
				      (/message *pager* (list (format nil "  CURRENT MODULE: ~S." *module*)))
				      (when (string= *module* "dance")
					(/visit *form* "FrontPage"))))

(setf blocky:*output-chunksize* 512)

;;; dance.lisp ends here
