;;; minibuffer.lisp --- emacs-like minibuffer for blocky

;; Copyright (C) 2012, 2013  David O'Toole

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

;;; Modeline

(defun-memo modeline-position-string (x y)
    (:key #'identity :test 'equal :validator #'identity)
  (format nil "X:~S Y:~S" x y))

(define-block-macro modeline
    (:super list
     :fields 
     ((orientation :initform :horizontal)
      (spacing :initform 4)
      (no-background :initform t))
     :inputs (:project-id (new 'label :read-only t)
	      :buffer-id (new 'label :read-only t)
	      :position (new 'label :read-only t)
	      :mode (new 'label :read-only t))))

(define-method update modeline ()
  (set-value %%project-id *project*)
  (set-value %%buffer-id (%buffer-name (current-buffer)))
  (set-value %%position
	     (modeline-position-string
	      (%window-x (current-buffer))
	      (%window-y (current-buffer))))
  (set-value %%mode
	     (if (current-buffer)
		 (if (%paused (current-buffer))
		     "(paused)"
		     "(playing)")
		 "(empty)")))

;;; Custom data entry for Minibuffer. See also basic.lisp 

(define-block (minibuffer-prompt :super prompt)
  (operation :initform :prompt)
  (background :initform nil)
  output)

(define-method debug-on-error minibuffer-prompt ()
  (setf *debug-on-error* t))

(define-method print-on-error minibuffer-prompt ()
  (setf *debug-on-error* nil))

(define-method initialize minibuffer-prompt (&optional output)
  (next-method self)
  (print-on-error self)
  (setf %output output))

(define-method set-output minibuffer-prompt (output)
  (setf %output output))

(define-method can-pick minibuffer-prompt () t)

(define-method pick minibuffer-prompt ()
  %parent)

(define-method do-sexp minibuffer-prompt (sexp)
  (with-fields (output) self
    (assert output)
    (let ((container (get-parent output)))
      (assert container)
      (execute sexp)
      ;; eval and possibly stack output
      (let ((new-block 
      	      (when *stack* (wordify *stack*))))
      	  ;; spit out result block, if any
      	  (when new-block 
      	    (accept container new-block))))))

      ;; ;; add command words
      ;; (accept container (wordify sexp)))))

(define-method label-width minibuffer-prompt ()
  (dash 2 (font-text-width *default-prompt-string* *font*)))

(define-method do-after-evaluate minibuffer-prompt ()
  ;; print any error output
  (when (and %parent (stringp %error-output)
	     (plusp (length %error-output)))
    (accept %parent (new 'text %error-output))))

;;; The Minibuffer is a pop-up command program and Forth prompt. Only
;;; shows one line, like in Emacs.

(define-block (minibuffer :super list)
  (temporary :initform t)
  (display-lines :initform 12))

(defparameter *minimum-minibuffer-width* 200)

(define-method initialize minibuffer ()
  (with-fields (image inputs) self
    (let ((prompt (new 'minibuffer-prompt self))
	  (modeline (new 'modeline)))
      (list%initialize self)
      (set-output prompt prompt)
      (setf inputs (list modeline prompt))
      (set-parent prompt self)
      (set-parent modeline self)
      (pin prompt)
      (pin modeline))))

(define-method layout minibuffer ()
  (with-fields (height width parent inputs) self
    ;; start by calculating current height
    (setf height (font-height *font*))
    (setf width 0)
    ;; update all child dimensions
    (dolist (element inputs)
      (layout element)
      (incf height (field-value :height element))
      (callf max width (dash 2 (field-value :width element))))
    ;; now compute proper positions and re-layout
    (let* ((x (%window-x (current-buffer)))
	   (y0 (+ (%window-y (current-buffer))
		 *gl-screen-height*))
	   (y (- y0 height (dash 3))))
      (dolist (element inputs)
	(decf y0 (field-value :height element))
	(move-to element x y0)
	(layout element))
      (setf %y y)
      (setf %x x)
      ;;  a little extra room at the top and sides
      (incf height (dash 3)))))
      ;; ;; move to the right spot to keep the bottom on the bottom.
      ;; (setf y (- y0 (dash 1))))))

(define-method get-prompt minibuffer ()
  (second %inputs))

(define-method enter minibuffer ()
  (enter (get-prompt self)))
 
(define-method evaluate minibuffer ()
  (evaluate (get-prompt self)))

(define-method focus minibuffer ()
  (grab-focus (get-prompt self)))

(define-method debug-on-error minibuffer ()
  (debug-on-error (get-prompt self)))

(define-method print-on-error minibuffer ()
  (print-on-error (get-prompt self)))

(define-method accept minibuffer (input &optional prepend)
  (declare (ignore prepend))
  (with-fields (inputs scrollback-length) self
    (assert (not (null inputs))) ;; we always have a prompt
    (prog1 t
      (assert (valid-connection-p self input))
      (let ((len (length inputs)))
	;; (when (> len scrollback-length)
	;;   ;; drop last item in scrollback
	;;   (setf inputs (subseq inputs 0 (1- len))))
	;; set parent if necessary 
	(adopt self input)
	(setf inputs 
	      (nconc (list (first inputs) 
			   (second inputs)
			   input)
		     (nthcdr 2 inputs)))))))

(define-method draw minibuffer ()
  (with-fields (inputs x y height width) self
    (draw-box x y *gl-screen-width* height :color "gray20" :alpha 0.5)
    (mapc #'draw inputs)))

;;; minibuffer.lisp ends here
