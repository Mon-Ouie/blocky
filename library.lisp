;;; library.lisp --- standard blocks library for blocky

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

;;; Vertically stacked list of blocks

(define-block list
  (dash :initform 2)
  (frozen :initform nil)
  (orientation :initform :vertical)
  (operation :initform :empty-list)
  (category :initform :structure))

(defun make-visual-list ()
  (clone "BLOCKY:LIST"))

(define-method frozenp list () %frozen)

(define-method freeze list ()
  (setf %frozen t)
  (mapc #'pin %inputs))

(define-method unfreeze list ()
  (setf %frozen nil)
  (mapc #'unpin %inputs))

(define-method evaluate list () self)

(define-method recompile list () 
  (mapcar #'recompile %inputs))
  "Return the computed result of this block.  By default, all the
inputs are evaluated."
  ;; (prog1 self
  ;;   (evaluate-inputs self)))

(define-method can-pick list ()
  (not %frozen))

;; (define-method pick list ()
;;   (when %frozen self))

(defparameter *null-display-string* "   ")

(define-method set-orientation list (orientation)
  (assert (member orientation '(:horizontal :vertical)))
  (setf %orientation orientation))

;; (define-method tap list (x y)
;;   (dolist (block %inputs)
;;     (evaluate block)))

(define-method can-accept block () 
  (not %frozen))

(define-method accept list (input &optional prepend)
  (assert (blockyp input))
  (when (not %frozen)
    (prog1 t
      (invalidate-layout self)
      (with-fields (inputs) self
	(if inputs
	    ;; we've got inputs. add it to the list (prepending or not)
	    (progn 
	      (assert (valid-connection-p self input))
	      ;; set parent if necessary 
	      (when (get-parent input)
		(unplug-from-parent input))
	      (set-parent input self)
	      (setf inputs 
		    (if prepend
			(append (list input) inputs)
			(append inputs (list input)))))
	    ;; no inputs yet. make a single-element inputs list
	    (progn
	      (setf inputs (list input))
	      (set-parent input self)))))))

(define-method take-first list ()
  (with-fields (inputs) self
    (let ((block (first inputs)))
      (prog1 block
	(unplug self block)))))

(define-method get-length list ()
  (length %inputs))

(define-method header-height list () 0)

(define-method label-width list ()
  (+ (* 2 *dash*)
     (expression-width *null-display-string*)))

(define-method layout-as-null list ()
  (with-fields (height width) self
    (setf width (+ (* 4 *dash*)
		   (font-text-width *null-display-string*
				      *font*))
	  height (+ (font-height *font*) (* 4 *dash*)))))

(define-method layout-vertically list ()
  (with-fields (x y height width inputs dash) self
    (flet ((ldash (&rest args)
	     (apply #'dash 1 args)))
    (let* ((header-height (ldash (header-height self)))
	   (y0 (ldash y header-height))
	   (line-height (font-height *font*)))
      (setf height (ldash line-height))
      (setf width (dash 8))
      (dolist (element inputs)
	(move-to element (ldash x) y0)
	(layout element)
	(incf height (field-value :height element))
	(incf y0 (field-value :height element))
	(setf width (max width (field-value :width element))))
      (incf height (dash 1))
      (incf width (dash 3))))))

(define-method layout-horizontally list ()
  (with-fields (x y height width inputs dash) self
    (flet ((ldash (&rest args)
	     (apply #'dash 1 args)))
    (let* ((header-height (ldash (header-height self)))
	   (x0 (+ x (dash 1)))
	   (y0 (ldash y))
	   (line-height (font-height *font*)))
      (setf height (ldash line-height))
      (setf width (dash 8))
      (dolist (element inputs)
	(move-to element (ldash x0) y0)
	(layout element)
	(setf height (max height (+ (ldash) (field-value :height element))))
	(incf x0 (field-value :width element))
	(incf width (field-value :width element))
	(incf width (dash 1)))
      (incf height (dash 1))
      (incf width (dash 3))))))

(define-method layout list ()
  (with-fields (inputs) self
    (if (null inputs)
	(layout-as-null self)
	(ecase %orientation
	  (:horizontal (layout-horizontally self))
	  (:vertical (layout-vertically self))))))

(define-method draw-header list () 0)

(define-method draw list ()
  (with-fields (inputs) self
    (draw-background self)
    (if (null inputs)
	(draw-label-string self *null-display-string*)
	(dolist (each inputs)
	  (draw each)))))
  ;; (when (not %frozen)
  ;;   (draw-indicator :bottom-right-triangle 
  ;; 		    (+ %x %width (dash -4))
  ;; 		    (+ %y %height (dash -4))
  ;; 		    :color "white"
  ;; 		    :scale 1.5)))

(define-method initialize list (&rest blocks)
  (apply #'block%initialize self blocks))
;  (freeze self))

(defmacro deflist (name &rest body)
  `(define-block (,name :super :list) ,@body))

(defun null-block () (new 'list))

(deflist empty-socket)

(define-method accept empty-socket (other-block)
  "Replace this empty socket with OTHER-BLOCK."
  (accept %parent other-block))

;;; Horizontal list

(define-block (hlist :super list)
  (:category :initform :system)
  (:orientation :initform :horizontal))

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

;;; A generic color swatch

(define-block color 
  :pinned nil
  :methods '(:set-color)
  :name "gray50"
  :width (dash 20) :height (dash 20))

(define-method set-color color
    ((name string :default "gray50"))
  (setf %name name))

(define-method draw color ()
  (with-fields (x y width height red green blue) self
    (with-style :rounded
      (draw-patch self x y (+ x width) (+ y height)
		  :color %name))))

(define-method layout color ())

(define-method initialize color (&optional (name "gray50"))
  (initialize%super self)
  (setf %name name))

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

;;; If and when

;; (define-block if 



;;; library.lisp ends here
