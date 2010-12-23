;;; system.lisp --- blocks for basic ioforms operations

;; Copyright (C) 2010  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: 

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

;;; Commentary:

;; (string #\GREEK_SMALL_LETTER_LAMDA))

(defblock system
  (type :initform :system))

(defvar *system* =system=)

(define-method timestep system (&rest args)
  (dolist (block *blocks*)

(define-method initialize system (&rest args)
  (apply #'/parent/initialize self args)
  (setf *timestep-function* #'(lambda (&rest args)
				(apply #'/timestep self args))))
	

(define-method get-blocks system ()
  *blocks*)

(define-method count-blocks system ()
  (apply #'+ (mapcar #'/count *blocks*)))


(define-method start system ())
(define-method stop system ())
(define-method save system ())
(define-method save-all system ())
(define-method new system ())
(define-method open system ())
(define-method get-ticks system



;;; Code:




;;; system.lisp ends here
