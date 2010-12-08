;;; editor.lisp --- IOSKETCH dev module

;; Copyright (C) 2010  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
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

(in-package :iosketch)

;;; Main program. 

(defparameter *window-width* 1024)
(defparameter *window-height* 768)

;;; Palettes to choose objects from

;; A palette is an editor widget whose blocks change according to context.

(define-prototype palette (:parent =editor=)
  type)

(define-method initialize palette ()
  (/parent/initialize self)
  (labels ((random-block ()
	     (clone (first (one-of (list =move= =play-music= =play-sound= =move-to= =if= =do=
					 =when= =start= =stop= =+= ))))))
    (with-fields (script) self
      (setf script (clone =script=))
      (/add script (clone =move= :west 2 :spaces) 20 20)
      (/add script (clone =play-music= "victory") 20 120)
      (/add script (clone =play-sound= "beep") 20 160)
      (/add script (clone =start=) 20 200)
      (/add script (clone =stop=) 20 200)
      (/add script (clone =when= 
      			  (clone =visible?=)
      			  (clone =set-variable= :n nil)) 20 360)
      (/add script (clone =when= 
      			  (clone =my= :stepping) 
      			  (clone =play-sound= "footstep")) 20 400)
      (/add script (clone =if= (clone =my= :color) 
			  (clone =say= "I have a color.")
			  (clone =play-sound= "warning")) 20 420)
      (/add script (clone =if= (clone =my= :boosting) 
			  (clone =move= :north 1 :pixel) 
			  (clone =move= :south 2 :pixels)) 20 420)
      (/add script (clone =when= 
      			  (clone =joystick-button= 2 :down)
			  (clone =if=
				 (clone =my= :fuel)
				 (clone =set-variable= :boosting :yes)
				 (clone =play-sound= "empty")))
	    20 320)
      (/add script (clone =+= 7 5) 20 500))
      ))

;;; A "frame" is a top-level application window.

;; Multiple simultaneous frames are possible.

(defwidget frame
  (active-color :initform ".red")
  (inactive-color :initform ".gray20")
  (pane-widths :initform '(70 15 15))
  (panes :initform nil)
  (focus :initform 1))

(define-method initialize frame (&rest panes)
  (/parent/initialize self)
  (if panes
      (setf <panes> panes)
      (setf <panes>
	    (mapcar #'clone
		    (list =palette= =editor= =editor=)))))

(defparameter *qwerty-keybindings*
  '(;; arrow key cursor movement
    ("UP" nil :move-cursor-up)
    ("DOWN" nil :move-cursor-down)
    ("LEFT" nil :move-cursor-left)
    ("RIGHT" nil :move-cursor-right)
    ;; emacs-style cursor movement
    ("A" (:control) :move-beginning-of-line)
    ("E" (:control) :move-end-of-line)
    ("F" (:control) :move-cursor-right)
    ("B" (:control) :move-cursor-left)
    ;; editing keys
    ("HOME" nil :move-beginning-of-line)
    ("END" nil :move-end-of-line)
    ("PAGEUP" nil :move-beginning-of-column)
    ("PAGEDOWN" nil :move-end-of-column)
    ;; switching windows
    ("TAB" nil :switch-panes)
    ("TAB" (:control) :switch-panes)
    ("LEFT" (:control) :left-pane)
    ("RIGHT" (:control) :right-pane)
    ;; dropping commonly-used cells
    ("1" (:control) :drop-data-cell)
    ("2" (:control) :drop-command-cell)
    ;; performing operations like clone, erase
    ("UP" (:control) :apply-right)
    ("DOWN" (:control) :apply-left)
    ("LEFTBRACKET" nil :apply-left)
    ("RIGHTBRACKET" nil :apply-right)
    ;; marking and stuff
    ("SPACE" (:control) :set-mark)
    ("SPACE" (:alt) :clear-mark)
    ("SPACE" (:meta) :clear-mark)
    ;; numeric keypad
    ("KP8" nil :move-cursor-up)
    ("KP2" nil :move-cursor-down)
    ("KP4" nil :move-cursor-left)
    ("KP6" nil :move-cursor-right)
    ("KP8" (:control) :apply-right)
    ("KP2" (:control) :apply-left)
    ("KP4" (:control) :left-pane)
    ("KP6" (:control) :right-pane)
    ;; entering data and confirm/cancel
    ("RETURN" nil :enter)
    ("RETURN" (:control) :exit) ;; see also handle-key
    ("ESCAPE" nil :cancel)
    ("G" (:control) :cancel)
    ;; view mode
    ("F9" nil :tile-view)
    ("F10" nil :label-view)
    ;; other
    ("X" (:control) :goto-prompt)
    ("X" (:alt) :goto-prompt)
    ("X" (:meta) :goto-prompt)
    ("T" (:control) :next-tool)))

(define-method install-keybindings frame ()
  (/generic-keybind self *qwerty-keybindings*))

(define-method install frame ()
  (apply #'iosketch:install-widgets self <panes>))

(define-method palette-pane frame ()
  (nth 0 <panes>))

(define-method script-pane frame ()
  (nth 1 <panes>))

(define-method world-pane frame ()
  (nth 2 <panes>))

(define-method other-pane frame ()
  (ecase <focus>
    (0 (/script-pane self))
    (1 (/world-pane self))
    (2 (/palette-pane self))))

(define-method selected-pane frame ()
  (nth <focus> <panes>))

(define-method switch-panes frame ()
  (let ((newpos (mod (1+ <focus>) (length <panes>))))
    (setf <focus> newpos)
    (ecase newpos
      (0 (/palette-pane self))
      (1 (/script-pane self))
      (2 (/world-pane self)))))

(define-method render frame ()
  (with-field-values (x y width image height panes visible pane-widths) self
    (when visible
      (labels ((scale (percentage)
		 (truncate (* width (/ (float percentage)
				       100)))))
	(let ((pane-stops (mapcar #'scale pane-widths)))
	  (dolist (widget panes)
	    (/move widget :x x :y y)
	    (/resize widget :height height :width (first pane-stops))
	    (/render widget)
	    (draw-image (field-value :image widget) x y :destination image)
	    (when (eq widget (/selected-pane self))
	      (draw-rectangle x y (field-value :width widget)
			      (field-value :height widget)
			      :color <active-color>
			      :destination image))
	    (incf x (1+ (first pane-stops)))
	    (pop pane-stops)))))))

(define-method hit frame (x y)
  (hit-widgets x y <panes>))

(define-method handle-key frame (event)
  (or (let ((func (gethash event <keymap>)))
	(when func
	  (prog1 t
	    (funcall func))))
      (/handle-key (/selected-pane self) event)))

(define-method forward frame (method &rest args)
  (apply #'send self method (/selected-pane self) args))

(defun editor ()
  (setf iosketch:*window-title* "IOSKETCH")
  (setf iosketch:*resizable* t)
  (iosketch:enable-classic-key-repeat 100 100)
  (iosketch:message "Starting IOSKETCH...")
  (iosketch:initialize)
  (iosketch:set-screen-height *window-height*)
  (iosketch:set-screen-width *window-width*)
  (let* ((frame (clone =frame=)))
    (add-hook 'iosketch:*resize-hook* 
	      #'(lambda ()
		  (/resize frame :width *screen-width* :height *screen-height*)))
    ;;
    (/move frame :x 0 :y 0)
    (/show frame)
    (/resize frame :width *screen-width* :height *screen-height*)
    (/install frame)))

(editor)

;;; editor.lisp ends here
