;;; sidebar.lisp --- palette of dictionary words

;; Copyright (C) 2013  David O'Toole

;; Author: David O'Toole <dto@ioforms.org>
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

(in-package :blocky)

(defparameter *sidebar-enter-sensitivity* 12)
(defparameter *sidebar-minimum-width* 200)
(defparameter *sidebar-margin* 16)
(defparameter *sidebar-spacing* 3)
(defparameter *sidebar-scroll-speed* 3)

(define-block sidebar
  (row :initform 0)
  (displayed-rows :initform 0))
(defparameter *sidebar-menu* 
  '("(clear-selection)"
    "(copy)"
    "(cut)"
    "(paste)"
    "(paste-at-pointer)"
    "(paste-as-new-buffer)"
    "(paste-from \"\" 0 0)"
    "(trim (current-buffer))"
    "(rename (current-buffer) \"\")"
    "(switch-to-buffer \"\")"
    "(save-project) "
    "(load-project \"\")"
    "(make-project \"\")"
    "(exit)"))

(defun all-sidebar-phrases ()
  (append *sidebar-menu* (all-words)))

(defun make-menu-expression (string)
  (assert (stringp string))
  (new 'expression :line string))

(define-method initialize sidebar ()
  (with-fields (inputs) self
    (setf inputs (mapcar #'make-menu-expression *sidebar-menu*))
    (dolist (input inputs)
      (setf (%parent input) self))))

(define-method add-item sidebar (item)
  ;; (push item %inputs))
  (setf %inputs
  	(append %inputs (list item))))

;; (add-hook '*after-define-functions*
;; 	  #'(lambda (word)
;; 	      (when *sidebar* 
;; 		(add-phrase *sidebar* (make-phrase word)))))

(define-method scroll-up sidebar ()
  (with-fields (inputs row) self
    (decf row *sidebar-scroll-speed*)
    (setf row (max 0 row))))

(define-method scroll-down sidebar ()
  (with-fields (inputs row) self
    (incf row *sidebar-scroll-speed*)
    (setf row (min row (1- (length inputs))))))

(define-method layout sidebar ()
  (with-fields (height width displayed-rows parent inputs row) self
    ;; use the right side of the screen.
    (let* ((x0 (+ (%window-x (current-buffer))
		 (- *gl-screen-width* *sidebar-minimum-width*)))
	   (x (+ x0 *sidebar-margin*))
	   (y0 (%window-y (current-buffer)))
	   (ymax (+ y0 *gl-screen-height*))
	   (y y0)
	   (elements (nthcdr row inputs)))
      ;; move self
      (setf %y y0)
      (setf %x x0)
      (setf height *gl-screen-height*)
      (setf width *sidebar-minimum-width*)
      ;; only layout/show what will fit onscreen.
      (setf displayed-rows 0)
      (block nil
	(dolist (element elements)
	  (incf y *sidebar-spacing*)
	  (layout element)
	  (move-to element x y)
	  (incf y (%height element))
	  (setf width (max width (%width element)))
	  (if (> y ymax)
	      (return)
	      (incf displayed-rows)))))))

(define-method hit sidebar (x y)
  (when (within-extents x y %x %y (+ %x %width) (+ %y %height))
    self))

(define-method can-pick sidebar () t)
(define-method draw-hover sidebar ())

(defun duplicate-safely (thing)
  (let ((dupe (duplicate thing)))
    (prog1 dupe
      (setf (%quadtree-node dupe) nil)
      (setf (%parent dupe) nil))))

(define-method pick sidebar (&optional nodup)
  (let ((x (window-pointer-x))
	(y (window-pointer-y))
	(candidates (subseq %inputs %row (+ %row %displayed-rows))))
    (labels ((try (it)
	       (hit it x y)))
      (let* ((pos (position-if #'try candidates))
	     (phrase (when pos (nth pos candidates))))
	(when phrase
	  (if nodup phrase
	      (let ((phrase2 (duplicate-safely phrase)))
		(prog1 phrase2
		  (move-to phrase2 (%x phrase) (%y phrase))))))))))

(define-method tap sidebar (x y)
  (let ((thing (pick self :nodup)))
    (when thing (evaluate-here thing))))
  
(defparameter *always-show-sidebar* nil)

(define-method draw sidebar ()
  (with-fields (inputs row displayed-rows x y height width) self
    (when (or *always-show-sidebar* 
	      (hit self (window-pointer-x) (window-pointer-y)))
      (draw-box x y width height :color "gray30")
      (dotimes (n displayed-rows)
	(draw (nth (+ n row) inputs))))))

;;; sidebar.lisp ends here
