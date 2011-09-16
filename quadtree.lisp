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
    (list (apply #'min (mapcar #'left objects))
	  (apply #'max (mapcar #'right objects))
	  (apply #'min (mapcar #'top objects))
	  (apply #'max (mapcar #'bottom objects)))))

(defun bounding-box-contains (box1 box2)
  (destructuring-bind (top left right bottom) box1
    (destructuring-bind (top0 left0 right0 bottom0) box2
      (and (< top top0)
	   (< left left0)
	   (> right right0)
	   (> bottom bottom0)))))

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

(defun build-quadtree (bounding-box &optional (depth 6))
  (assert (plusp depth))
  (assert (valid-bounding-box bounding-box))
  (decf depth)
  (if (zerop depth)
      (make-quadtree :bounding-box bounding-box :level 0)
      (make-quadtree :bounding-box bounding-box :level depth
		     :northwest (build-quadtree (northwest-quadrant bounding-box) depth)
		     :northeast (build-quadtree (northeast-quadrant bounding-box) depth)
		     :southwest (build-quadtree (southwest-quadrant bounding-box) depth)
		     :southeast (build-quadtree (southeast-quadrant bounding-box) depth))))

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

(defun quadtree-search (node bounding-box)
  "Return the smallest quadrant enclosing BOUNDING-BOX at or below
NODE, if any."
  (assert (quadtree-p node))
  (assert (valid-bounding-box bounding-box))
  ;; (message "Searching quadrant ~S ~S, object ~S" 
  ;; 	   *quadtree-depth* (quadtree-bounding-box node) bounding-box)
  (when (bounding-box-contains (quadtree-bounding-box node) bounding-box)
    (if (is-leaf node)
	;; there aren't any quadrants to search.
	node
	;; search the quadrants.
	(let ((*quadtree-depth* (1+ *quadtree-depth*)))
	  (or (quadtree-search (quadtree-northwest node) bounding-box)
	      (quadtree-search (quadtree-northeast node) bounding-box)
	      (quadtree-search (quadtree-southwest node) bounding-box)
	      (quadtree-search (quadtree-southeast node) bounding-box)
	      ;; none of them are suitable. stay here.
	      node)))))

(defun quadtree-insert (tree object)
  (let ((node0 
	  (quadtree-search 
	   tree
	   (multiple-value-list 
	    (bounding-box object)))))
    (let ((node (or node0 tree)))
      ;; (message "Inserting ~S at level ~S"
      ;; 	       (get-some-object-name object)
      ;; 	       (quadtree-level node))
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
      (let ((node (or node0 tree)))
	;; (message "Deleting ~S from level ~S"
	;; 	 (get-some-object-name object)
	;;        (quadtree-level node))
	(assert (find object
		      (quadtree-objects node)
		      :test 'eq))
	(setf (quadtree-objects node)
	      (delete object (quadtree-objects node) :test 'eq))
	(assert (not (find object
			   (quadtree-objects node)
			   :test 'eq)))))))

(defun quadtree-purge (tree object)
  (quadtree-process tree (quadtree-bounding-box tree)
		    #'(lambda (node)
			(setf (quadtree-objects node)
			      (delete (find-object object)
				      (quadtree-objects node)
				      :test 'eq)))))

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
		  (not (object-eq object thing)))
	 (on-collide object thing)))))

(defun quadtree-show (tree &optional object)
  (when tree
    (let ((*quadtree-depth* (1+ *quadtree-depth*)))
      ;; (dolist (ob (quadtree-objects tree))
      ;; 	(multiple-value-bind (top left right bottom) 
      ;; 	    (bounding-box ob)
      ;; 	  (draw-string (prin1-to-string *quadtree-depth*)
      ;; 		       left top
      ;; 		       :color "red")))
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
      (quadtree-show (quadtree-northeast tree) object)
      (quadtree-show (quadtree-northwest tree) object)
      (quadtree-show (quadtree-southeast tree) object)
      (quadtree-show (quadtree-southwest tree) object))))

;; (defun quadtree-map-objects (tree bounding-box function)
;;   (quadtree-map tree bounding-box
;; 		#'(lambda (node)
;; 		    (mapc function (quadtree-objects node)))))

;; (defun quadtree-find-objects (tree)
;;   (let (result)
;;     (quadtree-map-objects 
;;      tree nil
;;      #'(lambda (x)
;; 	 (push x result)))
;;     (nreverse result)))

;; (defun quadtree-count (tree)
;;   (length (quadtree-find-objects tree)))
 

;;; quadtree.lisp ends here
