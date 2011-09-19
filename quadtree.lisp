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

(defparameter *default-quadtree-depth* 6)
 
(defstruct quadtree 
  objects bounding-box level
  southwest northeast northwest southeast)

(defun is-leaf (node)
  ;; testing any quadrant will suffice
  (null (quadtree-southwest node)))

(defun objects-bounding-box (objects)
  ;; some functions for calculating the bounding box
  (labels ((left (thing) (field-value :x thing))
	   (right (thing) (+ (field-value :x thing)
			     (field-value :width thing)))
	   (top (thing) (field-value :y thing))
	   (bottom (thing) (+ (field-value :y thing)
			      (field-value :height thing))))
    ;; let's find the bounding box.
    (list (reduce #'min (mapcar #'left objects))
	  (reduce #'max (mapcar #'right objects))
	  (reduce #'min (mapcar #'top objects))
	  (reduce #'max (mapcar #'bottom objects)))))

(defun bounding-box-contains (box0 box1)
  (destructuring-bind (top0 left0 right0 bottom0) box0
    (destructuring-bind (top1 left1 right1 bottom1) box1
      (and (< top0 top1)
	   (< left0 left1)
	   (>= right0 right1)
	   (>= bottom0 bottom1)))))

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

(defun quadtree-process (node bounding-box processor)
  (assert (quadtree-p node))
  (assert (valid-bounding-box bounding-box))
  (assert (functionp processor))
  (when (bounding-box-contains (quadtree-bounding-box node) bounding-box)
    (when (not (is-leaf node))
      (let ((*quadtree-depth* (1+ *quadtree-depth*)))
	(quadtree-process (quadtree-northwest node) bounding-box processor)
	(quadtree-process (quadtree-northeast node) bounding-box processor)
	(quadtree-process (quadtree-southwest node) bounding-box processor)
	(quadtree-process (quadtree-southeast node) bounding-box processor)))
    (funcall processor node)))

(defun build-quadtree (bounding-box0 &optional (depth *default-quadtree-depth*))
  (assert (plusp depth))
  (assert (valid-bounding-box bounding-box0))
  (let ((bounding-box (mapcar #'float bounding-box0)))
    (decf depth)
    (if (zerop depth)
	(make-quadtree :bounding-box bounding-box)
	(make-quadtree :bounding-box bounding-box
		       :northwest (build-quadtree (northwest-quadrant bounding-box) depth)
		       :northeast (build-quadtree (northeast-quadrant bounding-box) depth)
		       :southwest (build-quadtree (southwest-quadrant bounding-box) depth)
		       :southeast (build-quadtree (southeast-quadrant bounding-box) depth)))))

(defun quadtree-search (node bounding-box)
  "Return the smallest quadrant enclosing BOUNDING-BOX at or below
NODE, if any."
  (assert (quadtree-p node))
  (assert (valid-bounding-box bounding-box))
  ;; (message "~A ~A Searching quadrant ~S for bounding box ~S" 
  ;; 	   *quadtree-depth* (make-string (1+ *quadtree-depth*) :initial-element (character "."))
  ;; 	   (quadtree-bounding-box node) bounding-box)
  (when (bounding-box-contains (quadtree-bounding-box node) bounding-box)
    ;; ok, it's in the overall bounding-box.
    (if (is-leaf node)
	;; there aren't any quadrants to search.
	node
	(or
	 ;; search the quadrants.
	 (let ((*quadtree-depth* (1+ *quadtree-depth*)))
	   (or (quadtree-search (quadtree-northwest node) bounding-box)
	       (quadtree-search (quadtree-northeast node) bounding-box)
	       (quadtree-search (quadtree-southwest node) bounding-box)
	       (quadtree-search (quadtree-southeast node) bounding-box)))
	 ;; none of them are suitable. stay here
	 node))))

(defun quadtree-insert (tree object)
  (let ((node0
	  (quadtree-search 
	   tree
	   (multiple-value-list 
	    (bounding-box object)))))
    ;; (when (null node0)
    ;;   ;; object has left the quadtree's assigned space.
    ;;   (exit object))
    (let ((node (or node0 tree)))
      ;; (message "Inserting ~S ~S"
      ;; 	       (get-some-object-name object) 
      ;; 	       (object-address-string object))
      (assert (not (find (find-object object)
			 (quadtree-objects node)
			 :test 'eq)))
      (push (find-object object)
	    (quadtree-objects node))
      (assert (find (find-object object)
		    (quadtree-objects node)
		    :test 'eq)))))

(defun quadtree-delete (tree object0)
  (let ((object (find-object object0)))
    (let ((node0
	    (quadtree-search 
	     tree
	     (multiple-value-list 
	      (bounding-box object)))))
      ;; (when (null node0)
      ;; 	;; object has left the quadtree's assigned space.
      ;; 	(exit object))
      (let ((node (or node0 tree)))
      ;; (message "Deleting ~S ~S"
      ;; 	       (get-some-object-name object) 
      ;; 	       (object-address-string object))
 	(assert (find object
		      (quadtree-objects node)
		      :test 'eq))
	(setf (quadtree-objects node)
	      (delete object (quadtree-objects node) :test 'eq))
	(assert (not (find object
			   (quadtree-objects node)
			   :test 'eq)))))))

(defun quadtree-map-collisions (tree bounding-box processor)
  (assert (functionp processor))
  (assert (valid-bounding-box bounding-box))
  (quadtree-process
   tree
   bounding-box
   #'(lambda (node)
       (dolist (object (quadtree-objects node))
	 (when (colliding-with-bounding-box object bounding-box)
	   (funcall processor object))))))

(defun quadtree-collide (tree object)
  (assert (blockyp object))
  (quadtree-map-collisions 
   tree
   (multiple-value-list (bounding-box object))
   #'(lambda (thing)
       (when (and (colliding-with object thing)
		  (not (object-eq object thing))
		  (field-value :collision-type thing))
	 (on-collide object thing)))))

(defun quadtree-show (tree &optional object)
  (when tree
      ;; (dolist (ob (quadtree-objects tree))
      ;; 	(multiple-value-bind (top left right bottom) 
      ;; 	    (bounding-box ob)
      ;; 	  (draw-string (prin1-to-string *quadtree-depth*)
      ;; 		       left top
      ;; 		       :color "yellow")))
      (let ((bounding-box (quadtree-bounding-box tree)))
	(destructuring-bind (top left right bottom) bounding-box
	  (if (null object)
	      (draw-box (+ left 10) (+ top 10) (- right left 10) (- bottom top 10)
			:color "magenta"
			:alpha 0.1)
	      (when (colliding-with-rectangle 
		     object top left (- right left) (- bottom top))
		(draw-box left top (- right left) (- bottom top)
			  :color "cyan"
			  :alpha 0.1)))))
      (let ((*quadtree-depth* (1+ *quadtree-depth*)))
	(quadtree-show (quadtree-northeast tree) object)
	(quadtree-show (quadtree-northwest tree) object)
	(quadtree-show (quadtree-southeast tree) object)
	(quadtree-show (quadtree-southwest tree) object))))

(defun quadtree-test ()
  (let ((*quadtree* (build-quadtree '(0 0 1024 1024)))
	things)
    (dotimes (i 200)
      (let ((thing (new block)))
	(quadtree-insert *quadtree* thing)
;	(resize thing 18 18)
	(move-to thing i i)
	(push thing things)))
    (dolist (thing things)
      (quadtree-delete *quadtree* thing))))
    
;;; quadtree.lisp ends here
