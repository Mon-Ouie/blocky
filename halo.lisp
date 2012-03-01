;;; halo.lisp --- morphic-style object handles

;; Copyright (C) 2011  David O'Toole

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

;;; Code:

(in-package :blocky)

(defparameter *handle-scale* 3.2)

(defparameter *indicator-positions* 
  '(:asterisk (0 1)
    :bang (1 0)
    :top-left-triangle (0 0)
    :menu (1/2 0)
;    :collapse (0 2/4)
;    :move (1 2/4)
    :drop (0 1)
    :resize (1 1)
    :reference (0 1/2)
    :close (0 0)
    :bottom-right-triangle (1 1)))

(define-block handle target indicator color)

(define-method initialize handle (&optional target)
  (initialize%super self)
  (setf %target target))

(define-method can-pick handle () t)
(define-method pick handle () self)
(define-method can-escape handle () nil)
(define-method layout handle ())
(define-method toggle-halo handle () nil) ;; don't let halos have halos

(define-method draw handle ()
  (with-fields (x y width height) %target
    (destructuring-bind (px py) (getf *indicator-positions* %indicator)
      (let* ((margin (* *handle-scale* (indicator-size)))
	     (x0 (- x margin))
	     (y0 (- y margin)))
	(setf %x (+ x0 
		    (* px (+ width margin))))
	(setf %y (+ y0 
		    (* py (+ height margin))))
	(setf %width margin)
	(setf %height margin)
	(draw-indicator %indicator %x %y 
			:color "white"
			:scale *handle-scale*
			:background %color)))))

(define-method draw-hover handle ())
		
(defmacro define-handle (name indicator &key (color "gray10"))
  (assert (symbolp name))
  (assert (stringp color))
  `(define-block (,name :super :handle)
     (indicator :initform ,indicator)
     (color :initform ,color)))

(define-handle evaluate :bang)

(define-method tap evaluate (x y)
  (evaluate %target))

(define-handle open-menu :menu)

(define-method tap open-menu (x y)
  (let ((menu (context-menu %target)))
    (add-block (world) menu)
    (move-to menu x y)))

(define-handle drop :drop)

(define-method tap drop (x0 y0)
  (add-object (world) %target))

;; (define-method drag move (x0 y0)
;; (with-fields (x y) %target
;;   (let ((dx (- %x x))
;; 	  (dy (- %y y)))
;;     (move-to %target 
;; 	       (- x0 dx)
;; 	       (- y0 dy)))))

(define-handle resize :resize)

(define-method can-pick resize () t)

(define-method pick resize () self)

(define-method drag resize (x0 y0)
  (with-fields (x y width height) %target
    (resize %target 
	    (- x0 x)
	    (- y0 y))))

(define-handle program :reference)

(define-method pick program ()
  (let ((ref (new 'prog0 %target)))
    (prog1 ref
      (move-to ref *pointer-x* *pointer-y*))))

(define-method tap program (x y)
  (drop self (pick self)))

(define-handle destroy :close)

(define-method tap destroy (x y)
  (assert %target)
  (destroy %target)
  ;; get rid of halo
  (when %parent
    (destroy %parent)))
     
(define-handle collapse :collapse)

;;; The halo itself

(defparameter *halo-handles* 
  '(:evaluate :open-menu :drop :resize :program :destroy))

(define-block halo target)

(define-method initialize halo (target)
  (assert (blockyp target))
  (setf %target target)
  (apply #'initialize%super self
	 (mapcar #'(lambda (handle)
		       (clone (make-prototype-id handle) target))
		 *halo-handles*)))

(define-method layout halo ()
  (with-fields (x y width height) %target
    (let ((size (* *handle-scale* (indicator-size))))
      (setf %x (- x size))
      (setf %y (- y size))
      ;; add twice the halo border to make sure
      ;; we get clicks all the way to the right of the halo
      (setf %width (+ width (* 2 size)))
      (setf %height (+ height (* 2 size))))))

(define-method draw halo ()
  (mapc #'draw %inputs))

(define-method can-pick halo ()
  (can-pick %target))

(define-method pick halo ()
  (pick %target))

(define-method alternate-tap halo (x y)
  (toggle-halo %target))
	  
(define-method draw-hover halo ())
(define-method draw-focus halo ())
(define-method draw-highlight halo ())
(define-method accept halo (other))

;;; halo.lisp ends here
