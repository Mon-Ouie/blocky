;;; example2.lisp --- a basic blocky example

;; Copyright (C) 2010, 2011  David O'Toole

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

(setf *screen-width* 640)
(setf *screen-height* 480)
(setf *window-title* "Blocky and the Blue Dot")

;;; Here's blocky again, with a new beep feature.

(defresource 
    (:name "blocky" :type :image :file "blocky.png"))

(define-block blocky
  :image "blocky"
  :default-events
  '(((:up) (move :north 5))
    ((:down) (move :south 5)) 
    ((:right) (move :east 5)) 
    ((:left) (move :west 5))
    ((:space) (beep)))
  :x (- (/ *screen-width* 2) 100)
  :y (/ *screen-height* 2))

(defvar *beeped* nil)

(defresource (:name "beep" :type :sample :file "beep.wav" :properties (:volume 10)))

(define-method beep blocky ()
  (setf *beeped* t)
  (play-sound self "beep"))

;;; Now the mysterious blue dot.

(defresource 
    (:name "blue-dot" :type :image :file "blue-dot.png")
    (:name "woom" :type :sample :file "woom.wav" :properties (:volume 30)))

(define-block blue-dot
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

;;; Music!

(defresource (:name "rappy" :type :music :file "rappy.ogg"))

(defun example2 ()
  (new system)
  (let ((dream (new dream)))
    (start (new universe
		:world dream
		:player (new blocky)))
    (add-sprite dream (new blue-dot))
    (play-music "rappy")))
	      
;;; example2.lisp ends here
