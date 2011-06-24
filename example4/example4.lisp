;;; example4.lisp --- turtle graphics example

;; Copyright (C) 2011  David O'Toole

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

;;; Preamble

(defpackage :example4 
    (:use :ioforms :common-lisp))
  
(in-package :example4)

(setf *screen-width* 800)
(setf *screen-height* 600)
(setf *window-title* "turtle graphics")
(enable-key-repeat 9 3)

(defparameter *font* "sans-bold-12")

;;; Defining a turtle

(defresource (:name "turtle" :type :image :file "turtle.png"))

(defsprite turtle
  :image "turtle"
  :heading 0.0 ;; in radians
  :lines nil
  :states nil
  :color "black"
  :drawing t)

(defun radian-angle (degrees)
  "Convert DEGREES to radians."
  (* degrees (float (/ pi 180))))

(define-method pen-down turtle ()
  (setf %drawing t))

(define-method pen-up turtle ()
  (setf %drawing nil))

(define-method set-color turtle 
  ((color string :default "black" 
		 :label "Pen color"
		 :documentation "test"))
  (setf %color color))

(define-method turn-left turtle ((degrees number :default 90.0 :label "degrees"))
  (incf %heading (radian-angle degrees)))

(define-method turn-right turtle ((degrees number :default 90.0))
  (decf %heading (radian-angle degrees)))

(define-method add-line turtle (x0 y0 x y &key color)
  (push (list x0 y0 x y :color color) 
	%lines))

(define-method clear-lines turtle (x0 y0 x y &key color)
  (setf %lines nil))

(define-method go-forward turtle ((distance number :default 40 
						   :label "Distance to travel"))
  (with-fields (x y heading drawing color) self
    (let ((x0 x)
	  (y0 y))
      (incf x (* distance (cos heading)))
      (incf y (* distance (sin heading)))
      (when drawing
	(add-line self x0 y0 x y :color color)))))

(define-method save-state turtle ()
  (push (list %x %y %heading %color) 
	%states))

(define-method restore-state turtle ()
  (destructuring-bind (x y heading color) 
      (pop %states)
    (setf %x x %y y %color color
	  %heading heading)))

(define-method draw turtle ()
  (next%draw self)
  (dolist (line %lines)
    (apply #'draw-line line)))

(defun example4 ()
  (new system)
  (let ((script (new script))
	(turtle (new turtle)))
    (add-block script turtle
	       (/ *screen-width* 2)
	       (/ *screen-height* 2))
    (dotimes (ring 4)
      (dotimes (petal 40)
	(turn-left turtle 3)
	(save-state turtle)
	(pen-up turtle)
	(go-forward turtle (+ 70 (* ring 60)))
	(dotimes (n 20) 
	  (pen-down turtle)
	  (set-color turtle "light salmon")
	  (go-forward turtle (* 0.6 n))
	  (turn-left turtle 70)
	  (go-forward turtle (* 0.8 n))
	  (set-color turtle "indian red")
	  (turn-right turtle 50)
	  (go-forward turtle (* 1.2 n))
	  (set-color turtle "orange")
	  (turn-left turtle 12)
	  (go-forward turtle (* 1.6 n))
	  (turn-right turtle 10))
	(restore-state turtle)))
      (add-block script (new entry :value 0 :type-specifier 'integer) 40 40) 
      (start (new shell script))))



;;; example4.lisp ends here
