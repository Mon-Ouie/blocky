;;; example2.lisp --- a basic ioforms example

;; Copyright (C) 2010, 2011  David O'Toole

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

;; First, we set up a package (often called a "namespace" in other
;; languages) with `defpackage' and then enter it with `in-package'.

;; The `:use' declaration shows that we will be importing names from
;; IOFORMS and from the base Common Lisp package.

(defpackage :example2 
    (:use :ioforms :common-lisp))
  
(in-package :example2)

(setf *screen-width* 640)
(setf *screen-height* 480)
(setf *window-title* "ioforms example2 1")

(defresource 
    (:name "blue-dot" :type :image :file "blue-dot.png")
    (:name "red-dot" :type :image :file "red-dot.png")
  (:name "yellow-dot" :type :image :file "yellow-dot.png"))

(defvar *colors* (list "blue-dot" "red-dot" "yellow-dot"))

(defsprite particle 
  :image (random-choose *colors*)
  :direction (random-direction)
  :x (+ 80 (random 100)) 
  :y (+ 80 (random 100)))

(define-method draw particle ()  
  (with-fields (x y image) self
    (draw-image image x y)))

  ;; (gl:with-primitive :triangles
  ;;   (with-fields (x y) self
  ;;     (gl:color 0 0 0)
  ;;     (gl:vertex x y 0)
  ;;     (gl:color (random 0.9) (random 0.2) (random 0.5))
  ;;     (gl:vertex (+ x 80) (+ y 80) 0)
  ;;     (gl:color 1 0 1)
  ;;     (gl:vertex (+ x 120) (+ y 100) 0))))

(defresource (:name "bleep" :type :sample :file "bleep.wav"))

(define-method hit particle (x y))

(define-method bleep particle ()
  (play-sound self "bleep"))

(define-method update particle ()
  (incf ^x (random-choose (list 1 -1)))
  (incf ^y (random-choose (list 1 -1))))

(defun example2 ()
  (message "RUNNING EXAMPLE2!")
  (dotimes (n 50)
    (add-block (new particle))))

;; Now we define the code that runs when your game starts. We define
;; it as a function (using `defun') to be called later by IOFORMS.

;; (What happens is that IOFORMS loads this file before initializing
;; the system, which allows you to set system variables like
;; `*screen-height*' and `*screen-width*' before the window actually
;; opens. Once IOFORMS is fully initialized according to the
;; parameters you set, it will look for a function with the same name
;; as the module---in this case `example2'---and execute it, which
;; hands control back to you.

 
;(defworld whitespace :background "story")

;; (define-method initialize dot ()
;;   (bind-event self (:up) (move :north 5 :pixels))
;;   (bind-event self (:down) (move :south 5 :pixels))
;;   (bind-event self (:right) (move :east 5 :pixels))
;;   (bind-event self (:left) (move :west 5 :pixels)))

;; (play (new universe)
;;       :world (new whitespace)
;;       :dot (new dot))
      
;;; example2.lisp ends here
