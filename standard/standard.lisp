;;; standard.lisp --- default startup splash screen and menu

;; Copyright (C) 2008  David O'Toole

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

;;; Commentary:

;;; Code:

(eval-when (:execute :load-toplevel :compile-toplevel) 
  (require :xe2))

(defpackage :xe2-standard
  (:documentation "A default startup splash screen and menu for XE2.")
  (:use :xe2 :common-lisp)
  (:export xe2-standard))

(in-package :xe2-standard)

;;; Choosing modules from a menu

(define-prototype module-launcher (:parent xe2:=menu-item=)
  (tile :initform ".asterisk"))
  
(define-method open module-launcher ()
  (prog1 nil
    (xe2:reset <name>)))

(define-prototype standard-prompt (:parent xe2:=prompt=)
  (default-keybindings :initform '(("UP" nil "cursor-previous .")
				   ("DOWN" nil "cursor-next .")
				   ("SPACE" nil "follow ."))))

;;; Quitting the menu

(define-prototype quit-launcher (:parent =menu-item=)
  (name :initform "Quit XE2")
  (tile :initform ".destroy-self"))

(define-method open quit-launcher ()
  (prog1 nil
    (xe2:quit :shutdown)))

;;; The splash screen widget

(define-prototype splash (:parent =widget=))

(defparameter *splash-width* 160)

(defparameter *splash-height* 100)
  
(define-method render splash ()
  (xe2:draw-resource-image ".splash-screen"
		       (- (/ <width> 2) *splash-width*)
		       (- (/ <height> 2) *splash-height*)
		       :destination <image>))

(defun show-default-splash-screen ()
  (let ((splash (clone =splash=))
	(browser (clone xe2:=browser=))
	(modules (xe2:find-all-modules))
	(prompt (clone =standard-prompt=))
	(quit (clone =quit-launcher=)))
    [resize splash 
	    :height *screen-height* 
	    :width *screen-width*]
    [move splash :x 0 :y 0]
    ;; set up browser with list of modules
    [resize browser :height 300 :width 200]
    [show browser]
    [move browser :x 0 :y 0]
    [set-collection browser 
		    (apply #'vector quit
			   (mapcar #'(lambda (m)
				       (clone =module-launcher= (list m :name m :description "test")))
				   modules))]
    ;; set up prompt
    [resize prompt :height 30 :width 400]
    [move prompt :x 0 :y 0]
    [hide prompt]
    [set-receiver prompt browser]
    ;; go!
    (install-widgets splash prompt browser)))
    
(defun xe2-standard ()
  (setf xe2:*screen-height* 600)
  (setf xe2:*screen-width* 800)
  (show-default-splash-screen))

(xe2-standard)

;;; standard.lisp ends here
