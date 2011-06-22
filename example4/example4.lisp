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

(setf *screen-width* 640)
(setf *screen-height* 480)
(setf *window-title* "turtle graphics")
(enable-key-repeat 9 3)

(defparameter *font* "sans-bold-12")

;;; Defining a turtle

(defresource (:name "turtle" :type :image :file "turtle.png"))

(defsprite turtle
  :image "turtle"
  :heading 0.0 ;; in radians
  :color "black"
  :pen-down nil)

(defun radian-angle (degrees)
  "Convert DEGREES to radians."
  (* degrees (float (/ pi 180))))

(define-method pen-down turtle ()
  (setf %pen-down t))

(define-method pen-up turtle ()
  (setf %pen-down nil))

(define-method set-color turtle ((color string :default "black" 
					       :display-name "Pen color"
					       :documentation "test"))
  (setf %color color))

(define-method turn-left turtle ((degrees number :default 90.0))
  (incf %heading (radian-angle degrees)))

(define-method turn-right turtle ((degrees number :default 90.0))
  (decf %heading (radian-angle degrees)))

(define-method go-forward turtle ((distance number :default 40 
						:display-name "Distance to travel"))
  (with-fields (x y heading pen-down color) self
    (let ((x0 x)
	  (y0 y))
      (incf x (* distance (cos heading)))
      (incf y (* distance (sin heading)))
      (when pen-down
	(draw-line x0 y0 x y :color color)))))

(defun example4 ()
  (new system)
  (let ((script (new script)))
    (add-block script (new turtle) (/ 2 *screen-width*) (/ 2 *screen-height*))
    (start (new shell script))))

;;; example4.lisp ends here
