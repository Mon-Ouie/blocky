;;; player.lisp --- defining game objects 

;; Copyright (C) 2008, 2009, 2010, 2011  David O'Toole

;; Author: David O'Toole %dto@ioforms.org
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
;; along with this program.  If not, see %http://www.gnu.org/licenses/.

;;; Code:

;;(in-package :blocky)

;;; Locating the player  

;; TODO move this to worlds.lisp

;; (define-method player-direction block ()
;;   "Return the general compass direction of the player from X0,Y0."
;;   (with-fields (player) *world*
;;     (multiple-value-bind (x0 y0) (xy-coordinates self)
;;       (multiple-value-bind (x1 y1) (xy-coordinates player)
;; 	(direction-to y0 x0 y1 x1)))))

;; (define-method player-distance block ()
;;   "Return the straight line distance of the player from X0,Y0."
;;   (with-fields (player) *world*
;;     (multiple-value-bind (x0 y0) (xy-coordinates self)
;;       (multiple-value-bind (x1 y1) (xy-coordinates player)
;; 	(distance x0 y0 x1 y1)))))

;; ;;; Block movement

;; (define-method move-to-grid block (r c)
;;   (delete-block *world* self %row %column)
;;   (drop-block *world* self r c))

;; (define-method move-to block (x y &optional z)
;;   (assert (and (numberp x) (numberp y)))
;;   (with-field-values (grid-size) *world*
;;     (let ((nearest-row (round y grid-size))
;; 	  (nearest-column (round x grid-size)))
;;       (move-to-grid self nearest-row nearest-column))))


;; ;;; Locating the block in grid space

;; (define-method is-grid-located block ()
;;   "Returns non-nil if this block is located somewhere on the grid."
;;   (and (integerp %row) (integerp %column)))

;; (define-method grid-coordinates block ()
;;   (values %row %column))


;; (define-method move block (direction &optional (distance 1) ignore-obstacles)
;;   (error "This move method needs rewriting."))
;; ;;   "Move this block one step in DIRECTION on the grid. If
;; ;; IGNORE-OBSTACLES is non-nil, the move will occur even if an obstacle
;; ;; is in the way. Returns non-nil if a move occurred."
;; ;;   (let ((world *world*))
;; ;;     (multiple-value-bind (r c) 
;; ;; 	(step-in-direction %row %column direction distance)
;; ;;       (cond ((null (grid-location world r c)) ;; are we at the edge?
;; ;; 	     ;; return nil because we didn't move
;; ;; 	     (prog1 nil
;; ;; 	     ;; edge conditions only affect player for now
;; ;; 	       (when (is-player self)
;; ;; 		 (ecase (field-value :edge-condition world)
;; ;; 		   (:block nil)
;; ;; 		   (:wrap nil) ;; TODO implement this for planet maps
;; ;; 		   (:exit (exit *universe*))))))
;; ;; 	    (t
;; ;; 	     (when (or ignore-obstacles 
;; ;; 		       (not (obstacle-at-p *world* r c)))
;; ;; 	       ;; return t because we moved
;; ;; 	       (prog1 t
;; ;; 		 (move-block world self r c))))))))


;; ;;; Sprites

;; (defblock sprite 
;;   (collision-type :initform :aabb)
;;   (type :initform :sprite)
;;   (height :initform nil :documentation "The cached width of the bounding box.")
;;   (width :initform nil :documentation "The cached height of the bounding box."))

;; Convenience macro for defining blocks

;; (defmacro defblock (name &body args)
;;   `(define-prototype ,name (:super "BLOCKY:BLOCK")
;;      ,@args))

;; (defun is-block (ob)
;;   (when (eq :block (field-value :type ob))))

;; (defun is-block (ob)
;;   (when (eq :block (field-value :type ob))))


;; ;;; Block locations

;; (define-method grid-coordinates block ()
;;   (values (truncate (/ %y (field-value :tile-size *world*)))
;; 	  (truncate (/ %x (field-value :tile-size *world*)))))

;; (define-method xy-coordinates block ()
;;   (values %x %y))

;; (define-method coordinates block ()
;;   (values %x %y %z))

;; ;;; Layout


;(define-method layout block ())

;; (define-method draw-highlight block ())
;; (define-method draw-hover block ())
;; (define-method draw-border block ())

;;; Block movement


;;; Collision detection

;;; Object dropping

;; (define-method drop block (thing &optional (delta-x 0) (delta-y 0))
;; ;;  (assert (is-block thing))
;;   (with-field-values (x y) self
;;     (drop-block *world* thing (+ x delta-x) (+ y delta-y))))

;;; Playing a sound


;;; Temporary text balloons

;; (defun seconds->frames (seconds)
;;   (truncate (* seconds blocky:*frame-rate*)))

;; (defblock balloon 
;;   :text "..."
;;   :font *font*
;;   :clock (seconds->frames 5))

;; (define-method initialize balloon (string &key (seconds 5.0) (font *font*))
;;   (with-fields (text clock) self
;;     (setf clock (seconds->frames seconds))
;;     (setf text string)
;;     (setf %font font)
;;     (multiple-value-bind (width height) 
;; 	(font-text-extents string font)
;;       (setf %width width)
;;       (setf %height height))))

;; (define-method draw balloon ()
;;   (with-fields (x y clock text) self
;;     (decf clock)
;;     (if (plusp clock)
;; 	(blocky:draw-string text x y :font %font :color "black")
;; 	(remove-block *world* self))))


;;; player.lisp ends here
