;;; example1.lisp --- a basic ioforms example

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

(defpackage :example1 
    (:use :ioforms :common-lisp))
  
(in-package :example1)

;; Now we should set a few system variables. In Common Lisp, globals
;; are named `*like-this*' with an asterisk on each end, to
;; distinguish them from normal variables.

(setf *screen-width* 640)
(setf *screen-height* 480)
(setf *window-title* "Hello")

;; (setf *resizable* t)
;; (setf *use-nominal-screen-size* t)

;; Now make something!

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

(define-method handle-event blocky (event)
  (message "BLOCKY EVENT: ~S" event)
  (parent/handle-event self event)) 

(defresource (:name "bleep" :type :sample :file "bleep.wav"))

(define-method bleep blocky ()
  (play-sound self "bleep"))

(defresource (:name "story" :type :image :file "story.png"))

(defworld hello :background "story")

;; Now we define the code that runs when your game starts. We define
;; it as a function (using `defun') to be called later by IOFORMS.

;; (What happens is that IOFORMS loads this file before initializing
;; the system, which allows you to set system variables like
;; `*screen-height*' and `*screen-width*' before the window actually
;; opens. Once IOFORMS is fully initialized according to the
;; parameters you set, it will look for a function with the same name
;; as the module---in this case `example1'---and execute it, which
;; hands control back to you.

(defun example1 ()
  (start (new universe)
	 :world (new hello)
	 :player (new blocky)))

;; Check out example #2 for more fun.

;;; example1.lisp ends here
