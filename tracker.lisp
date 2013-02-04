;;; tracker.lisp --- procedural audio glitch livecoding

;; Copyright (C) 2009, 2010, 2013  David O'Toole

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


(setf blocky:*output-chunksize* 512)
