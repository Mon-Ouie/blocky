;;; phrase.lisp --- interactive blocks for basic lisp data types

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

(define-block phrase
  (spacing :initform 1)
  (dash :initform 2)
  (frozen :initform nil)
  (orientation :initform :vertical)
  (operation :initform :empty-phrase)
  (category :initform :structure))

(define-method frozenp phrase () %frozen)

(define-method freeze phrase ()
  (setf %frozen t)
  (mapc #'pin %inputs))

(define-method unfreeze phrase ()
  (setf %frozen nil)
  (mapc #'unpin %inputs))

(define-method evaluate phrase () 
  (mapcar #'evaluate %inputs))

(define-method recompile phrase () 
  (mapcar #'recompile %inputs))

(defparameter *null-display-string* "   ")

(define-method set-orientation phrase (orientation)
  (assert (member orientation '(:horizontal :vertical)))
  (setf %orientation orientation))

(define-method toggle-orientation phrase ()
  (setf %orientation 
	(ecase %orientation
	  (:horizontal :vertical)
	  (:vertical :horizontal))))

(define-method can-accept phrase () 
  (not %frozen))

(define-method can-pick phrase () t)

(define-method pick phrase ()
  (if %pinned %parent self))
      
(define-method as-drag phrase (x y)
  (labels ((try (it)
	     (hit it x y)))
    (if %frozen
	(phrase-root self)
	(or (some #'try %inputs) self))))

(define-method accept phrase (input &optional prepend)
  (assert (blockyp input))
  (when (not %frozen)
    (prog1 t
      (invalidate-layout self)
      (with-fields (inputs) self
	(if inputs
	    ;; we've got inputs. add it to the phrase (prepending or not)
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

(define-method take-first phrase ()
  (with-fields (inputs) self
    (let ((block (first inputs)))
      (prog1 block
	(unplug self block)))))

(define-method get-length phrase ()
  (length %inputs))

(define-method header-height phrase () 0)

(define-method label-width phrase () 0)

(define-method layout-as-null phrase ()
  (with-fields (height width) self
    (setf width (+ (* 4 *dash*)
		   (font-text-width *null-display-string*
				      *font*))
	  height (+ (font-height *font*) (* 4 *dash*)))))

(define-method layout-vertically phrase ()
  (with-fields (x y height width spacing inputs dash) self
    (flet ((ldash (&rest args)
	     (apply #'dash 1 args)))
    (let* ((header-height (header-height self))
	   (y0 (+ y (if (zerop header-height) spacing (dash 2 header-height))))
	   (line-height (font-height *font*)))
      (setf height (ldash))
      (setf width (dash 6))
      (dolist (element inputs)
	(move-to element (ldash x) y0)
	(layout element)
	(incf height (field-value :height element))
;	(incf height spacing)
	(incf y0 (field-value :height element))
	(setf width (max width (field-value :width element))))
      (incf width (dash 2))))))

(define-method layout-horizontally phrase ()
  (with-fields (x y height spacing width inputs dash) self
    (flet ((ldash (&rest args) (apply #'+ %spacing args)))
      (let ((x0 (+ x spacing))
	    (y0 (ldash y))
	    (line-height (font-height *font*)))
	(setf height (ldash line-height))
	(setf width (dash 2))
	(dolist (element inputs)
	  (move-to element x0 y0)
	  (layout element)
	  (setf height (max height (+ (ldash) (field-value :height element))))
	  (incf x0 (field-value :width element))
	  (incf width (field-value :width element)))
;	  (incf width spacing))
	(incf height spacing)))))

(define-method layout phrase ()
  (with-fields (inputs) self
    (if (null inputs)
	(layout-as-null self)
	(ecase %orientation
	  (:horizontal (layout-horizontally self))
	  (:vertical (layout-vertically self))))))

(define-method insert-before phrase (index object)
  (with-fields (inputs) self
    (setf inputs
	  (append (subseq inputs 0 index)
		  (list object)
		  (subseq inputs index)))))

(define-method draw-header phrase () 0)

(define-method draw phrase ()
  (with-fields (inputs) self
    (unless %no-background 
      (draw-background self))
    (if (null inputs)
	(draw-label-string self *null-display-string*)
	(dolist (each inputs)
	  (draw each)))))

(define-method alternate-tap phrase (x y)
  (execute (recompile self)))

(defmacro defphrase (name &rest body)
  `(define-block (,name :super phrase) ,@body))

;;; From phrases to S-expressions, and back

(defun make-sentence (contents) 
  (let ((phrase (apply #'new 'phrase contents)))
    (prog1 phrase
      (update-parent-links phrase)
      (with-fields (orientation no-background dash spacing) phrase
	(setf orientation :horizontal)
	(setf no-background t)
	(setf dash 1)
	(setf spacing 0)))))
  
(defun make-paragraph (contents) 
  (let ((phrase (apply #'new 'phrase contents)))
    (prog1 phrase
      (update-parent-links phrase)
      (freeze (first (%inputs phrase))) ;; wait, is this wrong? 
      (with-fields (orientation no-background dash spacing) phrase
	(setf orientation :vertical)
	(setf dash 1)
	(setf spacing 0)))))

(defun phrasep (x) (is-a 'phrase x))

(defun phrase-root (phrase)
  (let ((p phrase))
    (assert (phrasep p))
    (loop while (phrasep (%parent p))
	  do (setf p (%parent p)))
    p))

(defun make-phrase (sexp)
  (cond
    ;; pass-through already created objects
    ((blockyp sexp)
     sexp) 
    ;; lists become phrases
    ((consp sexp)
     (funcall 
      (if (consp (first sexp))
	  #'make-paragraph
	  #'make-sentence)
      (mapcar #'make-phrase sexp)))
    ;; 
    ((eq '&body sexp)
     (make-sentence nil))
    ;; base case
    (t (new 'expression :value sexp :read-only t))))

(defun compile-phrase (phrase)
  ;; also compiles entries!
  (recompile phrase))

(defun duplicate-phrase (phrase)
  (make-phrase (compile-phrase phrase)))

;;; phrase.lisp ends here
