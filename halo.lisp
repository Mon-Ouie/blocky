;;; halo.lisp --- morphic-style object handles

;; Copyright (C) 2011, 2012  David O'Toole

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

(defparameter *handle-highlight-background-color* "gray50")
(defparameter *handle-highlight-foreground-color* "white")

(defparameter *indicator-positions* 
  '(:asterisk (0 1)
    :bang (0 0)
    :top-left-triangle (0 0)
    :menu (1/2 0)
    :move (3/4 1)
    :drop (0 1)
    :pick-up (2/6 1)
    :resize (1 1)
    :define (0 1/2)
    :close (1 0)
    :copy (1 2/4)
    :cut (1 1/4)
    :bottom-right-triangle (1 1)))

(define-block handle target indicator color foreground-color)

(define-method initialize handle (&optional target)
  (initialize%super self)
  (setf %target target))

(define-method can-pick handle () t)
(define-method pick handle () self)
(define-method can-escape handle () nil)
(define-method layout handle ())
(define-method toggle-halo handle () nil) ;; don't let halos have halos

(define-method highlight handle ()
  (setf %color *handle-highlight-background-color*)
  (setf %foreground-color *handle-highlight-foreground-color*))

(define-method alternate-tap handle (x y) 
  (tap self x y))

(define-method scroll-tap handle (x y) 
  (tap self x y))

(define-method layout handle ()
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
	(setf %height margin)))))

(define-method draw handle ()
  (draw-indicator %indicator %x %y 
		  :color %foreground-color
		  :scale *handle-scale*
		  :background %color))

(define-method draw-hover handle ())
		
(defmacro define-handle (name indicator 
			 &key (color "gray10")
			      (foreground-color "white")
			      fields)
  (assert (symbolp name))
  (assert (stringp color))
  `(define-block (,name :super handle)
     (indicator :initform ,indicator)
     (color :initform ,color)
     (foreground-color :initform ,foreground-color)
     ,@fields))

;;; Evaluation

(define-handle evaluate :bang)

(define-method tap evaluate (x y)
  (evaluate %target))

;;; Getting a context menu

(define-handle open-menu :menu)

(define-method tap open-menu (x y)
  (let ((menu (context-menu %target)))
    (add-block (world) menu)
    (move-to menu x y)))

;;; Dropping things down into the object layer

(define-handle drop :drop)

(define-method tap drop (x0 y0)
  (drop-selection (world)))

(define-method update drop ()
  (when (%quadtree-node %target)
    ;; ghost/highlight when already in object layer
    (highlight self))
  (update%super self))

;;; Picking them up from the object layer

(define-handle pick-up :pick-up)

(define-method tap pick-up (x0 y0)
  (unless (contains (world) %target)
    (add-block (world) %target)))

(define-method update pick-up ()
  (when (null (%quadtree-node %target))
    ;; ghost/highlight when not in object layer
    (highlight self))
  (update%super self))
  
;;; Moving objects or groups of them

(define-handle move :move
  :fields (positions))

(define-method can-pick move () t)

(define-method pick move () self)

(define-method drag move (x0 y0)
  (with-fields (positions) self
    (when (null positions)
      ;; drag all selected objects
      (dolist (thing (cons %target (get-selection (world))))
	(with-fields (x y) thing
	  ;; store initial offset from pointer
	  (push (list thing 
		      (- x x0)
		      (- y y0))
		positions))))
    (dolist (entry positions)
      (destructuring-bind (thing x y) entry
	(move-to thing
		 (+ x x0)
		 (+ y y0))))))

;;; Resizing objects interactively

(define-handle resize :resize)

(define-method can-pick resize () t)

(define-method pick resize () self)

(define-method drag resize (x0 y0)
  (with-fields (x y width height) %target
    (resize %target 
	    (- x0 x)
	    (- y0 y))))

;;; Definitions

(define-handle define :define)

(define-method tap define (x y)
  (show-definition %target))

;;; Destroying objects

(define-handle destroy :close)

(define-method tap destroy (x y)
  (assert %target)
  (destroy %target)
  ;; get rid of halo
  (when %parent
    (destroy %parent)))
     
(define-handle collapse :collapse)

;;; Copy and cut

(define-handle copy :copy)

(define-method tap copy (x y)
  (copy (world) (cons %target (get-selection (world)))))

(define-handle cut :cut)

(define-method tap cut (x y)
  (cut (world) (cons %target (get-selection (world)))))

;;; The halo, which manages all the handles

(defparameter *halo-handles* 
  '(evaluate open-menu drop move pick-up resize define cut copy destroy))

(define-block halo target)

(define-method initialize halo (target)
  (assert (blockyp target))
  (setf %target target)
  (apply #'initialize%super self
	 (mapcar #'(lambda (handle)
		       (clone (make-prototype-id handle) target))
		 *halo-handles*)))

(defun halo-minimum-height () (* 5 *handle-scale* (indicator-size)))
(defun halo-minimum-width () (* 5 *handle-scale* (indicator-size)))

(define-method layout halo ()
  (with-fields (x y width height) %target
    (let ((size (* *handle-scale* (indicator-size))))
      (setf %x (- x size))
      (setf %y (- y size))
      ;; add twice the halo border to make sure we get clicks all the
      ;; way to the right of the halo
      (setf %width (max (+ width (* 2 size)) (halo-minimum-width)))
      (setf %height (max (+ height (* 2 size)) (halo-minimum-height)))
      ;; now lay out the individual items
      (mapc #'layout %inputs))))

(define-method draw halo ()
  (mapc #'draw %inputs))

(define-method can-pick halo ()
  (can-pick %target))

(define-method pick halo ()
  (pick %target))

(define-method scroll-tap halo (x y)
  (toggle-halo %target))
	  
(define-method tap halo (x y)
  (destroy-halo %target))

(define-method draw-hover halo ())
(define-method draw-focus halo ())
(define-method draw-highlight halo ())
(define-method accept halo (other))

;;; halo.lisp ends here
