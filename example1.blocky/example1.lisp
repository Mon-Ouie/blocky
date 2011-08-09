;;; example1.lisp --- a basic blocky example with one movable character

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

;; This is an example of using plain Lisp in a Blocky project.

;; First let's set a few global variables. In Common Lisp, global
;; variables are named `*like-this*' with an asterisk on each end, to
;; distinguish them from normal variables.

(setf *screen-width* 640)
(setf *screen-height* 480)
(setf *window-title* "Hello, World")

;; Now make a movable character. First we need to tell the system
;; about the image file.

(defresource 
    (:name "blocky" :type :image :file "blocky.png"))

;; Now we define him as a sprite.

(defsprite blocky
  :image "blocky"
  :default-events
  '(((:up) (move :north 5))
    ((:down) (move :south 5)) 
    ((:right) (move :east 5)) 
    ((:left) (move :west 5)))
  :x (/ *screen-width* 2)
  :y (/ *screen-height* 2))

;;; Then define an empty world with a background image.

(defresource (:name "message" :type :image :file "message.png"))

(defworld hello :background "message")

;; Now we define the code that runs when your game starts. We define
;; it as a function (using `defun') to be called later by BLOCKY.

;; Put everything together by creating a new "hello" world and putting
;; a new player inside it.

(defun example1 ()
  (start (new universe
	      :world (new hello)
	      :player (new blocky))))

;; Check out the other included example projects for more fun.

;;; example1.lisp ends here
