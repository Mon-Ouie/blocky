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
(setf *window-title* "Blocky and the Blue Dot")

;;; Here's blocky again, with a new beep feature.

(defresource 
    (:name "blocky" :type :image :file "blocky.png"))

(defsprite blocky
  :image "blocky"
  :default-events
  '(((:up) (move :north 5 :pixels))
    ((:down) (move :south 5 :pixels)) 
    ((:right) (move :east 5 :pixels)) 
    ((:left) (move :west 5 :pixels))
    ((:space) (beep)))
  :x (- (/ *screen-width* 2) 100)
  :y (/ *screen-height* 2))

(defvar *beeped* nil)

(defresource (:name "beep" :type :sample :file "beep.wav"))

(define-method beep blocky ()
  (setf *beeped* t)
  (play-sound self "beep"))

;;; Now the mysterious blue dot.

(defresource 
    (:name "blue-dot" :type :image :file "blue-dot.png")
    (:name "woom" :type :sample :file "woom.wav"))

(defsprite blue-dot
  :image "blue-dot"
  :x 400 :y 245
  :steps 0 
  :speed 1
  :direction :south)

(defparameter *scare-distance* 100)

(define-method run-away blue-dot ()  
  (with-fields (direction steps speed) self
    (play-sound self "woom")
    (setf direction 
	  (opposite-direction (player-direction self)))
    (setf steps 20 speed 2)))

(define-method creep-closer blue-dot ()  
  (with-fields (direction steps speed) self
    (setf *beeped* nil)
    (setf direction (player-direction self))
    (setf steps 25 speed 0.5)))

(define-method update blue-dot ()
  (with-fields (steps direction speed) self
    (if (> *scare-distance* (player-distance self))
	(run-away self)
	(when *beeped*
	  (creep-closer self)))
    (when (plusp steps)
      (move self direction speed :pixels)
      (decf steps))))

;;; And now the dream level. 

(defresource (:name "story1" :type :image :file "story1.png"))

(defworld dream :background "story1")

(defun example2 ()
  (start (new universe)
	:world (new dream)
	:player (new blocky))
  (add-sprite *world* (new blue-dot)))
	
;; Now we define the code that runs when your game starts. We define
;; it as a function (using `defun') to be called later by IOFORMS.

;; (What happens is that IOFORMS loads this file before initializing
;; the system, which allows you to set system variables like
;; `*screen-height*' and `*screen-width*' before the window actually
;; opens. Once IOFORMS is fully initialized according to the
;; parameters you set, it will look for a function with the same name
;; as the module---in this case `example2'---and execute it, which
;; hands control back to you.

 

;; (define-method initialize dot ()
;;   (bind-event self (:up) (move :north 5 :pixels))
;;   (bind-event self (:down) (move :south 5 :pixels))
;;   (bind-event self (:right) (move :east 5 :pixels))
;;   (bind-event self (:left) (move :west 5 :pixels)))

;; (play (new universe)
;;       :world (new whitespace)
;;       :dot (new dot))
      
;;; example2.lisp ends here
