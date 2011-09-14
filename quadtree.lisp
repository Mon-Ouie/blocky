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

(defvar *quadtree-here-p* nil)

(defvar *quadtree-depth* 0)
 
(defvar *max-quadtree-depth* 8)

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

(defun valid-bounding-box (box)
  (and (listp box)
       (= 4 (length box))
       (destructuring-bind (top left right bottom) box
	 (and (< left right) (< top bottom)))))

(defun northeast-quadrant (bounding-box)
  (assert (valid-bounding-box bounding-box))
  (destructuring-bind (top left right bottom) bounding-box
    (list top (float (/ (+ left right) 2))
	  right (float (/ (+ top bottom) 2)))))

(defun southeast-quadrant (bounding-box)
  (assert (valid-bounding-box bounding-box))
  (destructuring-bind (top left right bottom) bounding-box
    (list (float (/ (+ top bottom) 2)) (float (/ (+ left right) 2))
	  right bottom)))

(defun northwest-quadrant (bounding-box)
  (assert (valid-bounding-box bounding-box))
  (destructuring-bind (top left right bottom) bounding-box
    (list top left
	  (float (/ (+ left right) 2)) (float (/ (+ top bottom) 2)))))

(defun southwest-quadrant (bounding-box)
  (assert (valid-bounding-box bounding-box))
  (destructuring-bind (top left right bottom) bounding-box
    (list (float (/ (+ top bottom) 2)) left
	  (float (/ (+ left right) 2)) bottom)))

(defun build-quadtree (bounding-box &optional (depth 4))
  (assert (plusp depth))
  (assert (valid-bounding-box bounding-box))
  (decf depth)
  (if (zerop depth)
      (make-quadtree :bounding-box bounding-box)
      (make-quadtree :bounding-box bounding-box
		     :northwest (build-quadtree (northwest-quadrant bounding-box) depth)
		     :northeast (build-quadtree (northeast-quadrant bounding-box) depth)
		     :southwest (build-quadtree (southwest-quadrant bounding-box) depth)
		     :southeast (build-quadtree (southeast-quadrant bounding-box) depth))))

(defun quadtree-map (tree bounding-box function)
  (block mapping
    ;; if function returns non-nil, early return
    (assert (quadtree-p tree))
    (assert (functionp function))
    (unless (null bounding-box)
      (assert (valid-bounding-box bounding-box)))
    (let ((*quadtree-depth* (1+ *quadtree-depth*)))
      (destructuring-bind (top left right bottom)
	  ;; use supplied bounding box?
	  (or bounding-box
	      ;; no. use the whole region bounding box instead, so
	      ;; we'll end up doing the subtrees
	      (quadtree-bounding-box tree))
	;; compute the center point of this node
	(let ((center-x (* 0.5 (+ left right)))
	      (center-y (* 0.5 (+ top bottom)))
	      (found nil))
	  ;; are there any child nodes?
	  ;; any quadrant will suffice to test.
	  (when (quadtree-northwest tree)
	    ;; see if box is contained entirely within any one of
	    ;; the quadrants.
	    ;; northwest
	    (when (and (<= bottom center-y)
		       (<= right center-x))
	      (setf found t)
	      (let ((result
		      (quadtree-map (quadtree-northwest tree) 
				    bounding-box function)))
		(when result (return-from mapping result))))
	    
	    ;; northeast
	    (when (and (<= bottom center-y)
		       (>= left center-x))
	      (setf found t)
	      (let ((result
		      (quadtree-map (quadtree-northeast tree) 
				    bounding-box function)))
		(when result (return-from mapping result))))

	    ;; southwest
	    (when (and (>= top center-y)
		       (<= right center-x))
	      (setf found t)
	      (let ((result
		      (quadtree-map (quadtree-southwest tree) 
				    bounding-box function)))
		(when result (return-from mapping result))))
	    ;; southeast
	    (when (and (>= bottom center-y)
		       (>= right center-x))
	      (setf found t)
	      (let ((result
		      (quadtree-map (quadtree-southeast tree) 
				    bounding-box function)))
		(when result (return-from mapping result)))))
	  ;; process the present node.
	  ;; see also `quadtree-map-objects'
	  (let ((*quadtree-here-p* (not found)))
	    (funcall function tree)))))))

(defun quadtree-show (tree &optional bounding-box0)
  (let ((bounding-box (or bounding-box0 
			  (quadtree-bounding-box tree))))
    (quadtree-map 
     tree bounding-box
     #'(lambda (node)
	 (destructuring-bind (top left right bottom) 
	     (quadtree-bounding-box node)
	   (prog1 nil
	     (draw-box left top (- right left) (- bottom top)
		       :color "magenta" :alpha 0.3)))))))
	
(defun quadtree-insert (tree object)
  (quadtree-map tree (multiple-value-list (bounding-box object))
		#'(lambda (node)
		    (when *quadtree-here-p*
		      (push (find-object object)
			    (quadtree-objects node))))))

(defun quadtree-delete (tree object0)
  (let ((object (find-object object0)))
    (quadtree-map tree (multiple-value-list (bounding-box object))
		  #'(lambda (node)
		      (when *quadtree-here-p*
			(prog1 t
			  (setf (quadtree-objects node)
				(delete object (quadtree-objects node) :test 'eq))))))))

(defun quadtree-map-objects (tree bounding-box function)
  (quadtree-map tree bounding-box
		#'(lambda (node)
		    (mapc function (quadtree-objects node)))))

(defun quadtree-find-objects (tree)
  (let (result)
    (quadtree-map-objects 
     tree nil
     #'(lambda (x)
	 (push x result)))
    (nreverse result)))

(defun quadtree-count (tree)
  (length (quadtree-find-objects tree)))
 
(defun quadtree-map-collisions (tree bounding-box function)
  (destructuring-bind (top left right bottom) bounding-box
    (quadtree-map-objects 
     tree
     bounding-box
     #'(lambda (object)
	 (when (multiple-value-bind (t0 l0 r0 b0) 
		   (bounding-box object)
		 (and (<= l0 right) (<= left r0)
		      (<= t0 bottom) (<= top b0)))
	   (funcall function object))))))

(defun quadtree-collide (tree object)
  (assert (blockyp object))
    (quadtree-map-collisions 
     tree
     (multiple-value-list (bounding-box object))
     #'(lambda (thing)
	 (when (and (colliding-with object thing)
		    (not (object-eq object thing)))
	   (on-collide object thing)))))

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
