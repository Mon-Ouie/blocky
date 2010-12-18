;;; editor.lisp --- IOSKETCH main user application window

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
(defparameter *window-height* 720)

(defgsprite circle
  (direction :initform :north)
  (image :initform "circle"))

(define-method emote circle (&rest args)
  (/play-sound self "bloop")
  (apply #'/parent/emote self args))

(define-prototype circle-prompt (:parent =prompt=))

(define-method install-keybindings circle-prompt ()
  (dolist (binding '(("UP" nil "move :north 2 :pixels")
		     ("DOWN" nil "move :south 2 :pixels")
		     ("LEFT" nil "move :west 2 :pixels")
		     ("RIGHT" nil "move :east 2 :pixels")))
    (apply #'bind-key-to-prompt-insertion self binding)))

(defworld myworld 
  (background :initform "bluedot"))

(defvar *circle*)
(defvar *viewport*)
(defvar *myverse*)
(defvar *prompt*)

;;; Palettes to choose objects from

;; A palette is an editor widget whose blocks change according to context.

(define-prototype palette (:parent =editor=)
  type)

(define-method initialize palette ()
  (/parent/initialize self)
  (setf *circle* (clone =circle=))    
  (setf *myverse* (clone =universe=))
  (setf *viewport* (clone =viewport=))
  (setf *prompt* (clone =circle-prompt=))
  (setf *physics-function* 
	#'(lambda (&rest ignore)
	    (when *world* (/run-cpu-phase *world* t))))
  (/install-keybindings *prompt*)
  (/set-receiver *prompt* *circle*)
  (/configure *myverse* :prompt *prompt*
	      :viewport *viewport*
	      :player *circle*)
  (/play *myverse* :address '(=myworld=))
  (/set-world *viewport* *world*)
  (/set-scrolling *viewport* nil)
  (/resize-to-background *world*)
  (/drop-sprite *world* *circle* 50 100)
    (with-fields (script) self
      (/set-recipient script *circle*)
      (/add script (make-block (list (listener))))))
    ;; (/add script (make-block (null)) 10 10)
    ;; (/add script (make-block (if (null) (null)
    ;; 				  (null)
    ;; 				 )) 60 60)
    ;; (/add script (make-block (when (see-player) (set direction (player-direction)))) 90 90)
    ;; (/add script (make-block (animate "blue-costume")) 50 50)
    ;; (/add script (make-block (when (closer-than 10 spaces to player) (fire (my direction))))
    ;; 	   54 44)
    ;; (/add script (make-block (null)) 10 10)
    ;; (/add script (make-block (move west 5 pixels)) 20 20)
    ;; (/add script (make-block (move north 5 pixels)) 30 30)
    ;; (/add script (make-block (move south 5 pixels)) 20 20)
    ;; (/add script (make-block (move east (+ 1 (+ 2 4)) pixels)) 30 30)
    ;; (/add script (make-block (move (my direction) 10 pixels))
    ;; 	  50 50)
    ;; (/add script (make-block (list 
    ;; 			      (emote "Well this is very strange.")))
    ;; 	  55 55)
    ;; (/add script (make-block (list 
    ;; 			      (emote "Maybe I'll go west.")
    ;; 			      (set direction west)))
    ;; 	  60 60)
    ;; (/add script (make-block (play-sound "woom")) 20 120)
    ;; (/add script (make-block (list (play-music "calm")
    ;; 				   (emote "I feel calm.")))
    ;; 	  20 200)))

;;; A "frame" is a top-level application window.

;; Multiple simultaneous frames are possible.

(defwidget frame
  (active-color :initform ".red")
  (inactive-color :initform ".gray20")
  (pane-widths :initform '(70 25 5))
  (panes :initform nil)
  (focus :initform 1))

(define-method initialize frame (&rest panes)
  (/parent/initialize self)
  (if panes
      (setf <panes> panes)
      (setf <panes>
	    (list (clone =palette=)
		  *viewport*
		  (clone =editor=)))))

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
      (/clear self ".gray80")
      (labels ((scale (percentage)
		 (1- (truncate (* width (/ (float percentage)
					  100))))))
	(let ((pane-stops (mapcar #'scale pane-widths)))
	  (dolist (widget panes)
	    (/move widget :x x :y 0)
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
