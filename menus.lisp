;;; menus.lisp --- ioforms menu widgets

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

;;; Menus

(define-prototype menu (:parent "IOFORMS:LIST")
  (category :initform :menu)
  (top-level :initform nil)
  (temporary :initform t)
  action target (expanded :initform nil) (visible :initform t))

(define-method initialize menu 
    (&key action target top-level inputs 
	  schema expanded (label "blank menu item..."))
  (next%initialize self)
  (setf %action action
	%expanded expanded
	%target target
	%schema schema
	%top-level top-level
	%inputs inputs
	%label label)
  (when inputs
    (dolist (each inputs)
      (pin each)
      (set-parent each self))))

(define-method accept menu (&rest args) nil)

(define-method run menu ())

(define-method toggle-expanded menu ()
  (with-fields (expanded) self
    (setf expanded (if expanded nil t))
    (report-layout-change *script*)))

(define-method is-expanded menu ()
  %expanded)

(define-method expand menu ()
  (setf %expanded t)
  (report-layout-change *script*))

(define-method unexpand menu ()
  (setf %expanded nil)
  (report-layout-change *script*))

(define-method click menu ()
  (with-fields (expanded action target) self
    (if (keywordp action)
        (send action (or target (symbol-value '*system*)))
	;; we're a submenu, not an individual menu command.
	(toggle-expanded self))))

(define-method display-string menu ()	    
  (with-fields (action label top-level) self
    (let ((ellipsis (concatenate 'string label *null-display-string*)))
      (if action
	  (etypecase action
	    (ioforms:object ellipsis)
	    (keyword label))
	  (if top-level label ellipsis)))))

(define-method layout-as-string menu (string)
  (with-fields (height width) self
    (setf height (dash 1 (font-height *block-font*)))
    (setf width 
	  (+ (dash 2) (font-text-extents string *block-font*)))))

(define-method layout menu ()
  (with-fields (expanded dash inputs label width) self
    (if expanded 
	;; we're an expanded submenu. lay it out
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

(define-method header-height menu ()
  (font-height *block-font*))

(define-method header-width menu ()
  (if %expanded
      (dash 2 (font-text-extents (display-string self) *block-font*))
      %width))

(define-method draw-expanded menu (&optional label)
  (with-field-values (action x y width height parent inputs) self
    (let ((display-string (or label *null-display-string*))
	  (header (header-height self)))
    ;; draw the top of the menubar a bit differently to prevent 
    ;; over-drawing other menu bar items.
    (draw-patch self
     x
     (dash 3 y)
     (dash 2 x (header-width self))
     (dash 1 y header)
     :color "gray87")
    (draw-label-string self display-string)
    ;; draw the rest of the menu background
    (draw-patch self
     x (dash 2 y header)
     (dash 2 x width)
     (- (+ y height) (dash 1)))
    ;; draw submenu items
    (dolist (each inputs)
      (draw each)))))

(define-method hit menu (mouse-x mouse-y)
  (with-field-values (x y expanded inputs width height) self
    (when (within-extents mouse-x mouse-y x y (+ x width) (+ y height))
      (flet ((try (item)
	       (hit item mouse-x mouse-y)))
	(if (not expanded)
	    self
	    ;; we're expanded. is the mouse to the left of this
	    ;; menu's header tab thingy?
	    (when (and (< mouse-x (+ x (header-width self)))
		       (< (header-height self) mouse-y))
	      (some #'try inputs)))))))
		
;;       (let ((hh (header-height self))
;; 	    (hw (header-width self)))
;; ;;	(message "HIT MENU")
;; 	(if (< y mouse-y (+ y hh))
;; 	    ;; we're even with the header text for this menu.
;; 	    ;; are we touching it?
;; 	    (if (< x mouse-x (+ x hw))
;; 		;; mouse is over menu title. return self to get event
;; 		;; we're in the corner (possibly over top of the text
;; 		;; of the next menu item's title in the menu bar). 
;; 		;; so, we close this menu.
;; 		(prog1 nil (unexpand self)))
;; 	    (labels ((try (it)
;; 		       (hit it mouse-x mouse-y)))
;; 	      (some #'try inputs)))))))

(define-method draw-hover menu ()
  nil)

(define-method draw-border menu ()
  nil)

(define-method draw-highlight menu ()
  (with-fields (y height expanded parent top-level) self
    (when parent
      (with-fields (x width) parent
	;; don't highlight top-level menus.
	(when (and (not expanded) (not top-level))
	  (draw-box x (+ y (dash 1)) width (+ height 1)
		  :color *highlight-background-color*)
	  (draw-label-string self (display-string self)))))))
  
(define-method draw menu (&optional highlight)
  (with-fields (x y width height label action visible expanded) self
    (when visible
      (if expanded 
	  (draw-expanded self label)
	  ;; otherwise just draw menu name and highlight, if any
	  (draw-label-string self (display-string self))))))

;; see system.lisp for example menu
(defun make-menu (items &optional target)
  (labels ((xform (item)
	     (if (listp item)
		 (if (listp (first item))
		     (mapcar #'xform item)
		     (apply #'clone "IOFORMS:MENU"
			    :target target
			    (mapcar #'xform item)))
		 item)))
    (xform items)))

(define-method point-through menu (mouse-x mouse-y)
  (with-field-values (x y width) self
    (within-extents mouse-x mouse-y
		    (+ x (header-width self)) 
		    y
		    (+ x width)
		    (+ y (header-height self)))))

;;; A global menu bar

(defblock menubar :category :menu :temporary t)

(define-method initialize menubar (&optional menus)
  (apply #'next%initialize self 
	 (mapcar #'find-object menus))
  (with-fields (inputs) self
    (dolist (each inputs)
      (setf (field-value :top-level each) t)
      (pin each))))

(define-method hit menubar (mouse-x mouse-y)
  (with-fields (x y width height inputs) self
    (when (within-extents mouse-x mouse-y x y (+ x width) (+ y height))
      ;; are any of the menus open?
      (let ((opened-menu (find-if #'is-expanded inputs)))
	(labels ((try (m)
		   (when m (hit m mouse-x mouse-y))))
	  (let ((moused-menu (find-if #'try inputs)))
	    (if (and ;; moused-menu opened-menu
		     (object-eq moused-menu opened-menu))
		;; we're over the opened menu, let's check if 
		;; the user has moused onto the other parts of the menubar
	        (flet ((try-other (menu)
			 (when (not (object-eq menu opened-menu))
			   (try menu))))
		  (let ((other (some #'try-other inputs)))
		    ;; are we touching one of the other menubar items?
		    (if (null other)
			;; nope, just hit the opened submenu items.
			(try opened-menu)
			;; yes, switch menus.
			(prog1 other
			  (unexpand opened-menu)
			  (expand other)))))
		;; we're somewhere else. just try the main menus in
		;; the menubar.
		(let ((candidate (find-if #'try inputs)))
		  (if (null candidate)
		      ;; the user moused away. close the menus.
		      self
		      ;; we hit one of the other menus.
		      (if opened-menu
			  ;; there already was a menu open.
			  ;; close this one and open the new one.
			  (prog1 candidate
			    (unexpand opened-menu)
			    (expand candidate))
			  ;; no menu was open---just hit the menu headers
			  (some #'try inputs)))))))))))
			  		    	  
(define-method draw-border menubar () nil)

(define-method layout menubar ()
  (with-fields (x y width height inputs) self
    (setf x 0 y 0 width *screen-width* height (dash 1))
    (let ((x1 (dash 1)))
      (dolist (item inputs)
	(move-to item x1 y)
	(layout item)
	(incf x1 (dash 2 (header-width item)))
	(setf height (max height (field-value :height item)))))))
        
(define-method draw menubar ()
  (with-fields (x y width inputs) self
    (let ((bar-height (dash 2 1 (font-height *block-font*))))
      (draw-box x y 
		width bar-height
		:color (find-color self))
      (draw-line x bar-height width bar-height
		 :color (find-color self :shadow))
      (with-fields (inputs) self
	(dolist (each inputs)
	  (draw each))))))

(define-method close-menus menubar ()
  (with-fields (inputs) self
    (when (some #'is-expanded inputs)
      (mapc #'unexpand %inputs))))

;;; menus.lisp ends here
