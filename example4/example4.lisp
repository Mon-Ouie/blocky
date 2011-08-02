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
    (:use :blocky :common-lisp))
  
(in-package :example4)

(setf *screen-width* 800)
(setf *screen-height* 600)
(setf *window-title* "blocky.io")
;; (setf *resizable* t)
(enable-key-repeat 9 2)

(defparameter *font* "sans-bold-12")

;;; Defining a turtle

(defresource 
    (:name "turtle" :type :image :file "turtle.png")
    (:name "dot" :type :image :file "dot.png"))

(defsprite turtle
  :image "turtle"
  :heading 0.0 ;; in radians
  :lines nil
  :states nil
  :color "black"
  :drawing t
  :methods '(:pen-down :pen-up :turn-left :turn-right 
	     :go-forward :pen-ink :save-state :restore-state :clear-lines :sing))

(define-method click turtle (x y)
  (declare (ignore x y))
  (setf *target* self))

(defun radian-angle (degrees)
  "Convert DEGREES to radians."
  (* degrees (float (/ pi 180))))

(define-method (pen-down :category :looks) turtle ()
  (setf %drawing t))

(define-method (pen-up :category :looks) turtle ()
  (setf %drawing nil))

(define-method (pen-ink :category :looks) turtle 
  ((color string :default "black" 
		 :documentation "test"))
  (setf %color color))

(define-method (turn-left :category :motion) turtle ((degrees number :default 90))
  (decf %heading (radian-angle degrees)))

(define-method (turn-right :category :motion) turtle ((degrees number :default 90))
  (incf %heading (radian-angle degrees)))

(define-method add-line turtle (x0 y0 x y &key color)
  (push (list x0 y0 x y :color color) 
	%lines))

(define-method (clear-lines :category :looks) turtle ()
  (setf %lines nil))

(define-method (go-forward :category :motion) turtle ((distance number :default 40))
  (with-fields (x y heading height width drawing color) self
    (let ((x0 (+ x (/ width 2)))
	  (y0 (+ y (/ width 2))))
      (let ((dx (* distance (cos heading)))
	    (dy (* distance (sin heading))))
	(incf x dx)
	(incf y dy)
	(when drawing
	  (add-line self x0 y0 
		    (+ x0 dx)
		    (+ y0 dy)
		    :color color))))))

(define-method (save-state :category :control) turtle ()
  (push (list %x %y %heading %color) 
	%states))

(defresource (:name "turtle-theme" :type :music :file "turtle.xm")) 

(define-method (sing :category :sound) turtle ((song string :default "turtle-theme"))
  (if (zerop (length song))
      (halt-music)
      (play-music song :loop t)))

(define-method (restore-state :category :control) turtle ()
  (destructuring-bind (x y heading color) 
      (pop %states)
    (setf %x x %y y %color color
	  %heading heading)))

(define-method draw turtle ()
  (dolist (line %lines)
    (apply #'draw-line line))
  (super%draw self)
  (let ((distance 6))
  (with-fields (x y heading height width drawing color) self
    (let ((x0 (floor (+ x (/ width 2))))
	  (y0 (floor (+ y (/ width 2)))))
      (let ((dx (* distance (cos heading)))
	    (dy (* distance (sin heading))))
	(draw-image "dot" (+ x0 dx -2) (+ y0 dy -2)))))))

;;; A ladybug

(defresource (:name "ladybug" :type :image :file "ladybug.png"))
(defresource (:name "wandering" :type :music :file "wandering.xm"))

(defsprite ladybug 
  :image "ladybug"
  :methods '(:chirp :wander :sit-quietly :sing)
  :moving nil
  :speed 0.5
  :direction (random-direction))

(define-method update ladybug ()
  (when %moving
    (percent-of-time 0.1 (setf %direction (random-direction)))
    (move self %direction %speed)))
  
(define-method (wander :category :motion) ladybug ((speed number :default 0.15))
  (setf %moving t %speed speed))

(define-method (sit-quietly :category :motion) ladybug ()
  (setf %moving nil))

(defresource (:name "chirp" :type :sample :file "chirp.wav" :properties (:volume 80)))

(define-method (sing :category :sound) ladybug 
    ((song string :default ""))
  (when (> (length song)
	   0)
    (play-music song :loop t)))

(define-method (chirp :category :sound) ladybug ()
  (play-sample "chirp"))

;;; A meadow for the ladybug

(defresource (:name "meadow" :type :image :file "meadow.png"))

(defsprite meadow :image "meadow")
(define-method accept meadow (other) nil)

;;; Put it all together

(defun example4 ()
  (new system)
  (let ((script (new script)))
    (start (new shell script))))

    ;; (dotimes (ring 4)
    ;;   (dotimes (petal 40)
    ;; 	(turn-left turtle 3)
    ;; 	(save-state turtle)

    ;; 	(pen-up turtle)
    ;; 	(go-forward turtle (+ 70 (* ring 60)))
    ;; 	(dotimes (n 20) 
    ;; 	  (pen-down turtle)
    ;; 	  (set-color turtle "light salmon")
    ;; 	  (go-forward turtle (* 0.6 n))
    ;; 	  (turn-left turtle 70)
    ;; 	  (go-forward turtle (* 0.8 n))
    ;; 	  (set-color turtle "indian red")
    ;; 	  (turn-right turtle 50)
    ;; 	  (go-forward turtle (* 1.2 n))
    ;; 	  (set-color turtle "orange")
    ;; 	  (turn-left turtle 12)
    ;; 	  (go-forward turtle (* 1.6 n))
    ;; 	  (turn-right turtle 10))
    ;; 	(restore-state turtle)))
;;      (add-block script (new entry :value 0 :type-specifier 'integer) 40 40) 
    ;; (add-block script (new send :prototype "EXAMPLE4:TURTLE"
    ;; 				:method :pen-down) 100 100)
    ;; (add-block script (new send :prototype "EXAMPLE4:TURTLE"
    ;; 				:method :pen-up) 100 150)
    ;; (add-block script (new send :prototype "EXAMPLE4:TURTLE"
    ;; 				:method :turn-left) 100 200)
    ;; (add-block script (new send :prototype "EXAMPLE4:TURTLE"
    ;; 				:method :turn-right) 100 250)
    ;; (add-block script (new send :prototype "EXAMPLE4:TURTLE"
    ;; 				:method :go-forward) 100 300)
    ;; (add-block script (new send :prototype "EXAMPLE4:TURTLE"
    ;; 				:method :pen-ink) 100 350)
    ;; (add-block script (new send :prototype "EXAMPLE4:TURTLE"
    ;; 				:method :say) 100 400)



;;; example4.lisp ends here
