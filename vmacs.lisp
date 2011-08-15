;;; vmacs.lisp --- object-oriented, hardware-accelerated Lisp macros

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

(in-package :blocky)

(defmacro defmacro% ((name &key (inputs nil)
				(super :block)
				(category :structure))
		     &rest body)
    `(progn 
       (defblock (,name :super ,super)
	 (category :initform ,category)
	 (inputs :initform ,inputs)
       (define-method evaluate ,name ()
	 (eval (recompile self)))
       (define-method recompile ,name ()
	 ,@body))))

;;; prevent evaluation

(defblock (quote :super list)
  :category :operators)

(define-method evaluate quote () self)

;;; Sending a group of messages to a particular target

(defmacro% (with-target 
	       :inputs ((new socket)
			(new list)))
	   (destructuring-bind (target body) 
	       (mapcar #'recompile %inputs)
	     `(with-target ,target
		,body)))

;;; Vertically stacked list of blocks

(defblock list
  (dash :initform 2)
  (operation :initform :empty-list)
  (category :initform :structure))

(defparameter *null-display-string* "...")

(define-method on-click list (x y)
  (dolist (block %inputs)
    (evaluate block)))

(define-method accept list (input &optional prepend)
  (verify input)
  (with-fields (inputs) self
    (if inputs
	;; we've got inputs. add it to the list (prepending or not)
	(prog1 t
	  (assert (is-valid-connection self input))
	  ;; set parent if necessary 
	  (when (get-parent input)
	    (unplug-from-parent input))
	  (set-parent input self)
	  (setf inputs 
		(if prepend
		    (append (list input) inputs)
		    (append inputs (list input)))))
    	;; no inputs yet. make a single-element inputs list
	(prog1 input 
	  (setf inputs (list input))
	  (set-parent input self)))))

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
				      *block-font*))
	  height (+ (font-height *block-font*) (* 4 *dash*)))))

(define-method layout-as-list list ()
  (with-fields (x y height width inputs dash) self
    (flet ((ldash (&rest args)
	     (apply #'dash 1 args)))
    (let* ((header-height (ldash (header-height self)))
	   (y0 (ldash y header-height))
	   (line-height (font-height *block-font*)))
      (setf height (ldash line-height))
      (setf width (dash 8))
      (dolist (element inputs)
;	(message "layout: ~S" (list x y0 width))
	(move-to element (ldash x) y0)
	(layout element)
	(incf height (+ (ldash) (field-value :height element)))
	(incf y0 (field-value :height element))
	(setf width (max width (field-value :width element))))
      (incf height (dash 1))
      (incf width (dash 10))))))

(define-method layout list ()
  (with-fields (inputs) self
    (if (null inputs)
	(layout-as-null self)
	(layout-as-list self))))

(define-method draw-header list () 0)

(define-method draw list ()
  (with-fields (inputs) self
    (draw-background self)
    (if (null inputs)
	(draw-label-string self *null-display-string*)
	(dolist (each inputs)
	  (draw each)))))

(define-method initialize list (&rest blocks)
  (apply #'super%initialize self blocks)
  ;; allow them to be freely removed
  (dolist (each %inputs)
    (unpin each)))

(defmacro deflist (name &rest body)
  `(defblock (,name :super :list) ,@body))

(defun null-block () (new list))

(deflist empty-socket)

(define-method accept empty-socket (other-block)
  "Replace this empty socket with OTHER-BLOCK."
  (accept %parent other-block))

;;; Generic method invocation block. The bread and butter of doing stuff.

(defblock send prototype method schema target label)

(define-method evaluate send ()
  (apply #'send %method 
	 (or *target* %target) ;; with-target will override
	 (mapcar #'evaluate %inputs)))

(define-method on-click send (x y)
  (declare (ignore x y))
  (evaluate self))

(define-method accept send (block)
  ;; make these click-align instead
  (verify block) 
  nil)

(defun pretty-symbol-string (thing)
  (let ((name (etypecase thing
		(symbol (symbol-name thing))
		(string thing))))
    (string-downcase 
     (substitute #\Space #\- name))))

(define-method initialize send (&key prototype method label target)
  (super%initialize self)
  (setf %target target)
  (let ((schema (method-schema (find-prototype prototype) method))
	(inputs nil))
    (dolist (entry schema)
      (push (new entry
		 :value (schema-option entry :default)
		 :parent (find-uuid self)
		 :type-specifier (schema-type entry)
		 :options (schema-options entry)
		 :label (concatenate 'string
				    ":" ;; mimic the keyword arguments visually
				    (symbol-name (schema-name entry))))
	    inputs))
    (when inputs 
      (setf %inputs (nreverse inputs)))
    (let ((category (method-option (find-prototype prototype)
				   method :category)))
      (when category (setf %category category))
      (setf %schema schema
	    %prototype prototype
	    %method method
	    %label (or label (pretty-symbol-string method))))))

(define-method draw send ()
  (with-fields (x y width height label inputs) self
    (draw-patch self x y (+ x width) (+ y height))
    (let ((*text-base-y* (+ y (dash 1))))
      (draw-label-string self label "white")
      (dolist (each inputs)
	(draw each)))))

(define-method draw-hover send ()
  nil)

;;; fields

(defblock field
  :category :variables
  :inputs (list (new string :label "name")
		(new socket :label "value"))) ;; TODO: allow any block as a value

(define-method evaluate field ()
  (destructuring-bind (name value) 
      (mapcar #'recompile %inputs)
    (list name :initform value)))

(define-method draw field ()
  (with-fields (x y width height inputs) self
    (draw-patch self x y (+ x width) (+ y height))
    (mapc #'draw inputs)))

(define-method accept field (thing)
  (declare (ignore thing))
  nil)

;;; defblock

(defblock (defblock :super tree))

(define-method initialize defblock ()
  (super%initialize 
   self 
   :label "defblock"
   :locked t :expanded t
   :subtree (list 
	     (new string :label "name")
	     (new tree :label "options"
		       :subtree (list (new string :value "block" :label "super")))
	     (new tree :label "fields" :subtree (list (new list))))))

(define-method recompile defblock ()
  (destructuring-bind (name super fields) 
      (mapcar #'recompile %inputs)
    (let ((block-name (make-symbol (first name)))
	  (super (make-prototype-id (first super))))
	(append (list 'defblock (list block-name :super super))
		fields))))

(define-method evaluate defblock ()
  (eval (recompile self)))

;;; arguments

(defblock argument
  :category :variables
  :inputs (list (new string :label "name")
		(new entry :label "type")
		(new string :label "default")))

(define-method evaluate argument ()
  (destructuring-bind (name type default) 
      (mapcar #'recompile %inputs)
    (list (make-symbol name) type :default default)))

(define-method draw argument ()
  (with-fields (x y width height inputs) self
    (draw-patch self x y (+ x width) (+ y height))
    (mapc #'draw inputs)))

;;; methods 

(defblock (define-method :super tree))

(define-method initialize define-method ()
  (apply #'super%initialize self 
	 :label "define method"
	 :expanded t :locked t
	 :subtree
	 (list 
	  (new string :label "name")
	  (new tree :label "for block"
		    :subtree (list (new string :value "name" :label "")))
	  (new tree :label "definition" :subtree (list (new script))))))

(define-method recompile define-method ()
  (destructuring-bind (name prototype definition) 
      (mapcar #'recompile %inputs)
    (let ((method-name (make-symbol (first name)))
	  (prototype-id (make-prototype-id prototype)))
      (append (list 'define-method method-name prototype-id)
	      (first definition)))))

(define-method evaluate define-method ()
  (eval (recompile self)))

;;; vmacs.lisp ends here
