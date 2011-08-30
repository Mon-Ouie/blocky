;;; example4.lisp --- a matching game

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
(setf *window-title* "example4.blocky")

(defresource 
    (:name "tile1" :type :image :file "tile1.png")
    (:name "tile2" :type :image :file "tile2.png")
    (:name "tile3" :type :image :file "tile3.png")
    (:name "tile4" :type :image :file "tile4.png"))

(defresource 
    (:name "dot1" :type :image :file "dot1.png")
    (:name "dot2" :type :image :file "dot2.png")
    (:name "dot3" :type :image :file "dot3.png")
    (:name "dot4" :type :image :file "dot4.png"))

(defresource
    (:name "crayon" :type :music :file "crayon.ogg"))

(defparameter *wood-tiles* '("tile1" "tile2" "tile3" "tile4"))
(defparameter *dots* '("dot1" "dot2" "dot3" "dot4"))

(defun dot-image (n)
  (nth n *dots*))

(define-block tile
  (image :initform (random-choose *wood-tiles*))
  (methods :initform '(:flip))
  (dot :initform (random 4))
  (face-up :initform nil))
  
(define-method draw tile ()
  (with-fields (x y) self
    (draw-image %image x y)
    (when %face-up
      (draw-image (dot-image %dot) x y))))

(define-method flip tile ()
  (setf %face-up (if %face-up nil t))
  (setf %image (random-choose *wood-tiles*)))

(define-method on-tap tile (x y)
  (flip self))

(defun example4 ()
  (new system)
;  (play-music "crayon" :loop t)
  (start (new shell (new buffer))))
	      
;;; example4.lisp ends here
