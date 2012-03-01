;;; syntax.lisp --- visual lisp macros for a blocky-in-blocky funfest

;; Copyright (C) 2011  David O'Toole

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

;;; Code:

(in-package :blocky)

(deflist empty-socket)

(define-method accept empty-socket (other-block)
  "Replace this empty socket with OTHER-BLOCK."
  (accept %parent other-block))

;;; Sending to a particular target

(defvar *target* nil)

(defmacro with-target (target &rest body)
  `(let ((*target* ,target))
     ,@body))

;;; Generic message block. 

(define-block message prototype method schema target label button-p)

(define-method recompile message ()
  ;; grab argument values from all the input widgets
  (mapcar #'recompile %inputs))

(define-method evaluate message ()
  (apply #'send %method 
	 ;; with-target will override,
	 ;; also will fall back on parent
	 (or *target* %target %parent)
	 (mapcar #'evaluate %inputs)))

(define-method tap message (x y)
  (declare (ignore x y))
  (when %button-p
    (evaluate self)))

(define-method can-pick message () t)

(define-method pick message ()
  ;; allow to move parent block by the labels of this message block
  (if %button-p self %parent))

(define-method accept message (block)
  ;; make these click-align instead
  (assert (blockyp block))
  nil)

(defun-memo pretty-symbol-string (thing)
    (:key #'first :test 'equal :validator #'identity)
  (let ((name (etypecase thing
		(symbol (symbol-name thing))
		(string thing))))
    (string-downcase 
     (substitute #\Space #\- name))))

(define-method initialize message (&key prototype schema method label target (button-p t))
  (initialize%super self)
  (setf %target target)
  (setf %button-p button-p)
  (let* ((schema0
	   (or schema
	       (method-schema (find-prototype (or prototype target)) method)))
	 (inputs nil)
	 (proto (or prototype (when target
				(object-name (find-super target))))))
    (dolist (entry schema0)
      (push (clone (if (eq 'string (schema-type entry))
		       "BLOCKY:STRING" "BLOCKY:ENTRY")
		   :value (schema-option entry :default)
		   :parent (find-uuid self)
		   :type-specifier (schema-type entry)
		   :options (schema-options entry)
		   :label (concatenate 'string
				    ":" ;; mimic the keyword arguments visually
				    (string-downcase (symbol-name (schema-name entry)))))
	    inputs))
    (when inputs 
      (setf %inputs (nreverse inputs)))
    (let ((category (when proto
		      (method-option (find-prototype proto)
				     method :category))))
      (when category (setf %category category))
      (setf %schema schema0
	    %prototype proto
	    %method method
	    %label (or label (pretty-symbol-string method))))))

(define-method draw message ()
  (with-fields (x y width height label inputs) self
    (when %button-p
      (with-style :flat
	(draw-patch self x y (+ x width) (+ y height))))
    (let ((*text-baseline* (+ y (dash 1))))
      (draw-label-string self label "white")
      (dolist (each inputs)
	(draw each)))))

(define-method draw-hover message ()
  nil)

;;; A reference to another block

(define-block reference
  (target :initform nil)
  (iwidth :initform 0)
  (category :initform :data))

(define-method evaluate reference () 
  %target)

(define-method recompile reference ()
  (recompile %target))

(define-method set-target reference (target)
  (setf %target 
	(cond 
	  ((stringp target)
	   (prog1 target
	     (assert (find-object target))))
	  ((blockyp target)
	   (find-uuid target)))))

(define-method initialize reference (&optional target)
  (when target
    (set-target self target)))

(define-method accept reference (new-block)
  (prog1 nil ;; signal not accepting
    (set-target self new-block)))

(defun-memo make-reference-name (target)
    (:key #'first :test 'equal :validator #'identity)
  (concatenate 'string
	       (get-some-object-name target)
	       " "
	       (object-address-string target)))

(defparameter *null-reference-string* "(null reference)")

(define-method layout reference () 
  (with-fields (target x y iwidth width height) self
    (if target
	(let ((image (field-value :image target))
	      (name (make-reference-name target)))
	  (setf iwidth (if image (image-width image) 0))
	  (setf width (dash 8 iwidth (font-text-width name *font*)
			    (* *handle-scale* (indicator-size))))
	  (setf height (dash 2 (font-height *font*)
			     (if image (image-height image) 0))))
	(setf width (dash 8 iwidth (font-text-width *null-reference-string* *font*))
	      height (dash 4 (font-height *font*))))))

(define-method draw reference ()
  (with-fields (target x y width height iwidth) self
    ;; (draw-background self)
    (let ((offset (* *handle-scale* (indicator-size))))
      (if (null target)
	  (draw-string *null-reference-string* 
		       (+ offset x) y)
	(let ((image (field-value :image target))
	      (name (make-reference-name target)))
	  (if image
	      (draw-image image 
			  (dash 1 x)
			  (dash 1 y))
	      (draw-indicator :asterisk 
			      x y
			      :scale *handle-scale*
			      :background "purple"
			      :color "cyan"))
	  (draw-string name (dash 1 x offset iwidth) (dash 1 y)))))))

;;; Palettes to tear cloned objects off of 

(define-block (palette :super :list) 
  source
  (style :initform :rounded)
  (height :initform 100)
  (width :initform 100))

(define-method hit palette (x y)
  (when (within-extents x y %x %y (+ %x %width) (+ %y %height))
    self))

(define-method can-pick palette () t)

(define-method pick-drag palette (x y)
  (labels ((hit-it (ob)
	     (hit ob x y)))
    (setf %source (some #'hit-it %inputs))
    (if %source
	(make-clone %source)
	self)))

;;; syntax.lisp ends here
