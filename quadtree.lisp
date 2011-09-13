;;; quadtree.lisp --- for spatial indexing and stuff

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

;;; Code:

(in-package :blocky)

(defvar *quadtree* nil)

(defvar *quadtree-depth* 0)

(defstruct quadtree 
  objects bounding-box
  southwest northeast northwest southeast)

(defun objects-bounding-box (objects)
  ;; some functions for calculating the bounding box
  (labels ((left (thing) (field-value :x thing))
	   (right (thing) (+ (field-value :x thing)
			     (field-value :width thing)))
	   (top (thing) (field-value :y thing))
	   (bottom (thing) (+ (field-value :y thing)
			      (field-value :height thing))))
    ;; let's find the bounding box.
    (list (apply #'min (mapcar #'left objects))
	  (apply #'max (mapcar #'right objects))
	  (apply #'min (mapcar #'top objects))
	  (apply #'max (mapcar #'bottom objects)))))

(defun find-bounding-box (tree)
  (let ((null-box (list 0 0 1000 1000)))
    (if (null tree)
	null-box
	(let ((objects (quadtree-objects tree)))
	  (if (null objects)
	      null-box
	      (objects-bounding-box objects))))))
 
(defun quadtree-map (tree bounding-box function &optional object)
  (unless (null tree)
    (destructuring-bind (top left right bottom)
	  ;; use supplied bounding box?
	(or bounding-box (find-bounding-box tree))
			    ;; no. compute the bounding box instead
	;; compute the center point of this node
	(let ((center-x (* 0.5 (+ left right)))
	      (center-y (* 0.5 (+ top bottom)))
	      (found nil))
	  ;; see if box is contained entirely within any one of the
	  ;; quadrants. 
	  ;;
	  ;; northwest
	  (when (and (<= bottom center-y)
		     (<= right center-x))
	    (setf found t)
	    (when (null (quadtree-northwest tree))
	      (setf (quadtree-northwest tree)
		    (make-quadtree)))
	    (quadtree-map (quadtree-northwest tree)
			  top left right bottom function object))
	  ;; northeast
	  (when (and (<= bottom center-y)
		     (>= left center-x))
	    (setf found t)
	    (when (null (quadtree-northeast tree))
	      (setf (quadtree-northeast tree)
		    (make-quadtree)))
	    (quadtree-map (quadtree-northeast tree)
			  top left right bottom function object))
	  ;; southwest
	  (when (and (>= top center-y)
		     (<= right center-x))
	    (setf found t)
	    (when (null (quadtree-southwest tree))
	      (setf (quadtree-southwest tree)
		    (make-quadtree)))
	    (quadtree-map (quadtree-southwest tree)
			  top left right bottom function object))
	  ;; southeast
	  (when (and (>= bottom center-y)
		     (>= right center-x))
	    (setf found t)
	    (when (null (quadtree-southeast tree))
	      (setf (quadtree-southeast tree)
		    (make-quadtree)))
	    (quadtree-map (quadtree-southeast tree)
			  top left right bottom function object))
	  ;; process the present node.
	  ;; see also `quadtree-map-objects'
	  (let ((*quadtree-bottom-p* (not found)))
	    (funcall function tree)))))

(defun quadtree-insert (tree object)
  (quadtree-map tree (multiple-value-list (bounding-box object))
		#'(lambda (node)
		    (when *quadtree-bottom-p*
		      (push (find-object object)
			    (quadtree-objects node))))))

(defun quadtree-delete (tree object0)
  (let ((object (find-object object0)))
    (quadtree-map tree (multiple-value-list (bounding-box object))
		  #'(lambda (node)
		      (when *quadtree-bottom-p*
			(setf (quadtree-objects node)
			      (delete object (quadtree-objects node) :test 'eq)))))))

(defun quadtree-map-objects (tree top left right bottom function)
  (quadtree-map tree (multiple-value-list (bounding-box object))
		#'(lambda (node)
		    (mapc function (quadtree-objects node)))))

(defun quadtree-find-objects (tree top left right bottom)
  (let (result)
    (quadtree-map-objects 
     tree (multiple-value-list (bounding-box object))
     #'(lambda (x)
	 (push x result)))
    (nreverse result)))

;(defun draw-quadtree (tree)

;; (defun quadtree-count (tree)
;;   (if (null tree)
;;       0
;;       (+ (length (quadtree-objects tree))
;; 	 (quadtree-count (quadtree-northwest tree))
;; 	 (quadtree-count (quadtree-northeast tree))
;; 	 (quadtree-count (quadtree-southwest tree))
;; 	 (quadtree-count (quadtree-southeast tree)))))
	  
(defun quadtree-map-collisions (tree top left right bottom function)
  (labels ((colliding (object)
	     (multiple-value-bind (t0 l0 r0 b0) 
		 (bounding-box object)
	       (and (<= l0 right) (<= left r0)
		    (<= t0 bottom) (<= top b0)))))
    (quadtree-map-objects 
     tree
     (multiple-value-list (bounding-box object))
     #'(lambda (object)
	 (when (colliding object)
	   (funcall function object))))))

(defun quadtree-collide (tree object)
  (assert (blockyp object))
    (quadtree-map-collisions 
     tree
     (multiple-value-list (bounding-box object))
     #'(lambda (thing)
	 (when (and (colliding-with object thing)
		    (not (object-eq object thing)))
	   (on-collide object thing))))))

    
		;; ;; assign all objects to new subtrees by filtering
		;; (let ((box (list top left bottom right))) 
		;;   (make-quadtree 
		;;    :center-x center-x
		;;    :center-y center-y
		;;    :objects (remove-if-not #'in-here objects)
		;;    :southwest (build-quadtree 
		;; 	       (remove-if-not #'in-southwest objects) 
		;; 	       :depth depth :bounding-box box)
		;;    :southeast (build-quadtree 
		;; 	       (remove-if-not #'in-southeast objects)
		;; 	       :depth depth :bounding-box box)
		;;    :northwest (build-quadtree 
		;; 	       (remove-if-not #'in-northwest objects) 
		;; 	       :depth depth :bounding-box box)
		;;    :northeast (build-quadtree 
		;; 	       (remove-if-not #'in-northeast objects) 
		;; 	       :depth depth :bounding-box box))))))))))

;;; quadtree.lisp ends here
