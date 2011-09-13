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

(defvar *quadtree-bottom-p* nil)

(defstruct quadtree 
  objects center-x center-y
  southwest northeast northwest southeast)

(defun quadtree-map (tree top left right bottom function)
  (let ((center-x (quadtree-center-x tree))
	(center-y (quadtree-center-y tree))
	(found nil))
    ;; process northwest
    (when (and (<= top center-y)
	       (<= left center-x))
      (setf found t)
      (quadtree-map (quadtree-northwest tree)
		    top left right bottom function))
    ;; northeast
    (when (and (<= top center-y)
	       (>= right center-x))
      (setf found t)
      (quadtree-map (quadtree-northeast tree)
		    top left right bottom function))
    ;; southwest
    (when (and (>= bottom center-y)
	       (<= left center-x))
      (setf found t)
      (quadtree-map (quadtree-southwest tree)
		    top left right bottom function))
    ;; southeast
    (when (and (>= bottom center-y)
	       (>= right center-x))
      (setf found t)
      (quadtree-map (quadtree-southeast tree)
		    top left right bottom function))
    ;; process the present node.
    ;; see also `quadtree-map-objects'
    (let ((*quadtree-bottom-p* (not found)))
      (funcall function tree))))

(defun quadtree-insert (tree object)
  (multiple-value-bind (top left right bottom) (bounding-box object)
    (quadtree-map tree top left right bottom 
		  #'(lambda (node)
		      (when *quadtree-bottom-p*
			(push (find-object object)
			      (quadtree-objects node)))))))

(defun quadtree-delete (tree object0)
  (let ((object (find-object object0)))
    (multiple-value-bind (top left right bottom) (bounding-box object)
      (quadtree-map tree top left right bottom 
		    #'(lambda (node)
			(when *quadtree-bottom-p*
			  (setf (quadtree-objects node)
				(delete object (quadtree-objects node) :test 'eq))))))))

(defun quadtree-move-to (tree object x y)
  (quadtree-delete tree object)
  (move-to object x y)
  (quadtree-insert tree object))

(defun quadtree-map-objects (tree top left right bottom function)
  (quadtree-map tree top left right bottom
		#'(lambda (node)
		    (mapc function (quadtree-objects node)))))

(defun quadtree-find-objects (tree top left right bottom)
  (let (result)
    (quadtree-map-objects tree top left right bottom
			  #'(lambda (x)
			      (push x result)))
    (nreverse result)))
	  
(defun quadtree-map-collisions (tree top left right bottom function)
  (labels ((colliding (object)
	     (multiple-value-bind (t0 l0 r0 b0) 
		 (bounding-box object)
	       (and (<= l0 right) (<= left r0)
		    (<= t0 bottom) (<= top b0)))))
    (quadtree-map-objects
     tree top left right bottom
     #'(lambda (object)
	 (when (colliding object)
	   (funcall function object))))))

(defun quadtree-collide (tree object)
  (assert (blockyp object))
  (multiple-value-bind (top left right bottom)
      (bounding-box object)
    (quadtree-map-collisions 
     tree top left right bottom
     #'(lambda (thing)
	 (when (and (colliding-with object thing)
		    (not (object-eq object thing)))
	   (on-collide object thing))))))
    
(defun build-quadtree (objects &key (depth 8) bounding-box)
  (labels ((left (thing) (field-value :x thing))
	   (right (thing) (+ (field-value :x thing)
			     (field-value :width thing)))
	   (top (thing) (field-value :y thing))
	   (bottom (thing) (+ (field-value :y thing)
			      (field-value :height thing))))
    (if (= 1 depth)
	;; if we've reached the maximum depth, 
	;; just collect everything at this node.
	(make-quadtree :objects objects)
	;; we may need to subdivide. 
	;; let's find the bounding box.
	(destructuring-bind (top left right bottom)
	    ;; use supplied bounding box?
	    (or bounding-box 
		;; no. find bounding box instead
		(list (apply #'min (mapcar #'left objects))
		      (apply #'max (mapcar #'right objects))
		      (apply #'min (mapcar #'top objects))
		      (apply #'max (mapcar #'bottom objects))))
	  ;; ensure next level of recursion
	  (decf depth)
	  ;; find center of this node's space
	  (let ((center-x (* 0.5 (+ left right)))
		(center-y (* 0.5 (+ top bottom))))
	    ;; build functions for sorting objects into quadrants
	    (labels ((in-northwest (object)
		       (and (< (right object) center-x)
			    (< (bottom object) center-y)))
		     (in-northeast (object)
		       (and (> (left object) center-x)
			    (< (bottom object) center-y)))
		     (in-southwest (object)
		       (and (< (right object) center-x)
			    (> (top object) center-y)))
		     (in-southeast (object)
		       (and (> (left object) center-x)
			    (> (top object) center-y)))
		     ;; if object bounding box is not entirely
		     ;; contained in one of the quadrants, leave it at
		     ;; this tree level
		     (in-here (object)
		       (and (not (in-northwest object))
			    (not (in-northeast object))
			    (not (in-southwest object))
			    (not (in-southeast object)))))
	      ;; assign all objects to new subtrees by filtering
	      (make-quadtree 
	       :center-x center-x
	       :center-y center-y
	       :objects (remove-if-not #'in-here objects)
	       :southwest (build-quadtree (remove-if-not #'in-southwest objects) :depth depth)
	       :southeast (build-quadtree (remove-if-not #'in-southeast objects) :depth depth)
	       :northwest (build-quadtree (remove-if-not #'in-northwest objects) :depth depth)
	       :northeast (build-quadtree (remove-if-not #'in-northeast objects) :depth depth))))))))

;;; quadtree.lisp ends here
