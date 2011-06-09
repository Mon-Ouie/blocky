;;; example3.lisp --- something more interesting

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

(defpackage :example3 
    (:use :ioforms :common-lisp))
  
(in-package :example3)

(setf *screen-width* 640)
(setf *screen-height* 480)
(setf *window-title* "Blocks demo")

(defparameter *font* "sans-12")

(defresource 
    (:name "blocky" :type :image :file "blocky.png"))

(defsprite blocky
  :image "blocky"
  :default-events
  '(((:up) (move :north 5 :pixels))
    ((:down) (move :south 5 :pixels)) 
    ((:right) (move :east 5 :pixels)) 
    ((:left) (move :west 5 :pixels)))
  :x (/ *screen-width* 2)
  :y (/ *screen-height* 2))

(defblock testblock)

(defworld demo)

(defun example3 ()
  (start (new universe)
	 :world (new demo)
	 :player (new blocky))
  (add-sprite *world* (new testblock)))

;;; example3.lisp ends here
