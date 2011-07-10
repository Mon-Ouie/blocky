;;; builder.lisp --- the blocky.io application builder 

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

(defpackage :builder 
    (:use :ioforms :common-lisp))
  
(in-package :builder)

(setf *builder-p* t)

(setf *screen-width* 640)
(setf *screen-height* 480)
(setf *window-title* "blocky.io")
(setf *resizable* nil)
(enable-key-repeat 9 2)

;;; A splash screen

(defresource (:name "splash" :type :image :file "splash.png")) 

(defsprite splash 
  :image "splash" :clock 180)

(define-method initialize splash (&optional (clock 380))
  (super%initialize self)
  (update-image-dimensions self) ;; we need the coordinates straight away.
  (setf %x (- (/ *screen-width* 2) (/ %width 2)))
  (setf %y (- (/ *screen-height* 2) (/ %height 2)))
  (setf %clock clock))

(define-method update splash ()
  (with-fields (clock) self
    (if (zerop clock)
	(delete-block *script* self)
	(decf clock))))
    
;;; The main function

(defun builder ()
  (new system)
  (let ((script (new script)))
    (add-block script (new splash))
    (start (new shell script))))

;;; builder.lisp ends here
