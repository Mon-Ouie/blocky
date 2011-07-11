;;; trees.lisp --- generic folding hierarchical list widget with
;;; indentation and headlines, a la orgmode

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

(in-package :ioforms)

;;; Trees

(define-prototype tree (:parent "IOFORMS:LIST")
  (category :initform :structure)
  (top-level :initform nil)
  (locked :initform nil)
  (temporary :initform t)
  action target (expanded :initform nil) (visible :initform t))

(define-method children tree () %inputs)

(define-method initialize tree 
    (&key action target top-level subtree pinned locked
	  expanded (name "no label..."))
  (super%initialize self)
  (setf %action action
	%pinned pinned
	%expanded expanded
	%locked locked
	%target target
	%top-level top-level
	%inputs subtree
	%label name)
  ;; become the parent
  (when subtree
    (dolist (each subtree)
;      (pin each)
      (set-parent each self))))

(define-method evaluate tree ()
  (mapcar #'evaluate %inputs))

(define-method toggle-expanded tree (&optional force)
  (with-fields (expanded locked) self
    (when (or force (not locked))
      (setf expanded (if expanded nil t))
      (invalidate-layout self))))

(define-method is-expanded tree ()
  %expanded)

(define-method expand tree (&optional force)
  (when (or force (not %locked))
    (setf %expanded t)
    (invalidate-layout self)))

(define-method unexpand tree (&optional force)
  (when (or force (not %locked))
    (setf %expanded nil)
    (invalidate-layout self)))

(define-method click tree (x y)
  (declare (ignore x y))
  (toggle-expanded self))

(define-method display-string tree ()	    
  (with-fields (action label top-level) self
    (let ((ellipsis (concatenate 'string label *null-display-string*)))
      (if action
	  (etypecase action
	    (ioforms:object ellipsis)
	    ((or keyword function) label))
	  (if top-level label ellipsis)))))

(define-method layout-as-string tree (string)
  (with-fields (height width) self
    (setf height (dash 1 (font-height *block-font*)))
    (setf width 
	  (+ (dash 2) (font-text-extents string *block-font*)))))

(define-method layout tree ()
  (with-fields (expanded dash inputs label width) self
    (if expanded 
	;; we're an expanded subtree. lay it out
	(progn 
	  (setf dash 1)
	  (layout-as-list self)
	  (when label 
	    (setf width 
		  (max width 
		       (dash 4 (font-text-extents label *block-font*)))))
	  ;; make all inputs equally wide
	  (dolist (each inputs)
	    (setf (field-value :width each) (- width (dash 2)))))
	;; we're not expanded. just lay out for label.
	(layout-as-string self (display-string self)))))

(define-method header-height tree ()
  (font-height *block-font*))

(define-method header-width tree ()
  (if %expanded
      (dash 2 (font-text-extents (display-string self) *block-font*))
      %width))

(define-method hit tree (mouse-x mouse-y)
  (with-field-values (x y expanded inputs width height) self
    (when (within-extents mouse-x mouse-y x y (+ x width) (+ y height))
      (flet ((try (item)
	       (hit item mouse-x mouse-y)))
	(if (not expanded)
	    self
	    ;; we're expanded. is the mouse to the left of this
	    ;; tree's header tab thingy?
	    (if %top-level
		(when (and (< mouse-x (+ x (header-width self)))
			   (< (header-height self) mouse-y))
		  (some #'try inputs))
		(or (some #'try inputs) self)))))))
		
;;       (let ((hh (header-height self))
;; 	    (hw (header-width self)))
;; ;;	(message "HIT TREE")
;; 	(if (< y mouse-y (+ y hh))
;; 	    ;; we're even with the header text for this tree.
;; 	    ;; are we touching it?
;; 	    (if (< x mouse-x (+ x hw))
;; 		;; mouse is over tree title. return self to get event
;; 		;; we're in the corner (possibly over top of the text
;; 		;; of the next tree item's title in the tree bar). 
;; 		;; so, we close this tree.
;; 		(prog1 nil (unexpand self)))
;; 	    (labels ((try (it)
;; 		       (hit it mouse-x mouse-y)))
;; 	      (some #'try inputs)))))))

(define-method draw-hover tree ()
  nil)

(define-method draw-border tree ()
  nil)

(define-method draw-highlight tree () 
  nil)

(defparameter *tree-tab-color* "gray60")
(defparameter *tree-title-color* "white")

(define-method draw-expanded tree (&optional label)
  (with-field-values (action x y width height parent inputs top-level) self
    (let ((display-string (or label *null-display-string*))
	  (header (header-height self)))
      (if top-level
	  ;; draw the header a bit differently to avoid over-drawing
	  ;; other headers in a menu bar situation.
	  (progn (draw-patch self
			     x
			     (dash 3 y)
			     (dash 2 x (header-width self))
			     (dash 1 y header)
			     :color *tree-tab-color*)
		 (draw-label-string self display-string *tree-title-color*)
		 ;; draw the rest of the tree background
		 (draw-patch self
			     x (dash 2 y header)
			     (dash 0 x width)
			     (- (dash 1 y height) (dash 1))))
	  (progn (draw-patch self x y (+ x width) (+ y height))
		 (draw-label-string self display-string)
		 (draw-line (+ x 1) (dash 2 y header) 
			    (+ x width -1) (dash 2 y header)
			    :color (find-color self :highlight))))
      ;; draw subtree items
      (dolist (each inputs)
	(draw each)))))
  
(define-method draw-unexpanded tree (&optional label)
  (draw-background self)
  (draw-label-string self (or label (display-string self))))

(define-method draw tree (&optional highlight)
  (with-fields (visible expanded) self
    (when visible
      (if expanded 
	  (draw-expanded self label)
	  (draw-unexpanded self label)))))

;; see system.lisp for example tree menu
(defun make-tree (items &key target (tree-prototype "IOFORMS:TREE"))
  (labels ((xform (item)
	     (if (listp item)
		 (if (listp (first item))
		     (mapcar #'xform item)
		     (apply #'clone tree-prototype
			    :target target
			    (mapcar #'xform item)))
		 item)))
    (xform items)))

(defun make-menu (items &key target)
  (make-tree items :target target :tree-prototype "IOFORMS:MENU"))

;;; Menus

(define-prototype menu (:parent :tree)
  (action :initform nil)
  (category :initform :menu))

;; menu items should not accept any dragged widgets.
(define-method accept menu (&rest args) nil)

(define-method click menu (x y)
  (declare (ignore x y))
  (with-fields (action target) self
    (typecase action 
      (function (funcall action))
      (keyword (send action (or target (symbol-value *system*))))
      (otherwise
       ;; we're a submenu, not an individual menu command.
       (toggle-expanded self)))))
  
(define-method draw-unexpanded menu (&optional label)
  (draw-label-string self (or label (display-string self))))

(define-method draw-highlight menu ()
  (with-fields (y height expanded parent top-level) self
    (when parent
      (with-fields (x width) parent
	;; don't highlight top-level trees.
	(when (and (not expanded) (not top-level))
	  (draw-box x (+ y (dash 1)) width (+ height 1)
		  :color *highlight-background-color*)
	  (draw-label-string self (display-string self)))))))

;;; menus.lisp ends here
