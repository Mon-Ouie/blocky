;;; buffers.lisp --- collecting related blocks into groups

;; Copyright (C) 2008, 2009, 2010, 2011  David O'Toole

;; Author: David O'Toole <dto@blocky.org>
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

(in-package :blocky)

;;; Grouping blocks into buffers with buffer-local variables

(defvar *buffers* nil)

(defun initialize-buffers ()
  (setf *buffers* (make-hash-table :test 'equal)))

(defun get-buffer (name)
  (gethash name *buffers*))

(defun make-buffer (&rest args)
  (let* ((buffer (apply #'clone :buffer args))
	 (name (field-value :name buffer))) ;; name may be uniqified
    (assert (not (gethash name *buffers*)))
    (prog1 buffer
      (setf (gethash name *buffers*)
	    buffer))))
  
(defun uniquify-buffer-name (name)
  (let ((n 1)
	(name0 name))
    (block naming
      (loop while name0 do
	(if (get-buffer name0)
	    (setf name0 (format nil "~A.~S" name n)
		  n (1+ n))
	    (return-from naming name0))))))

(defmacro with-buffer (buffer &rest body)
  `(let ((*buffer* (find-uuid ,buffer)))
     (assert (blockyp *buffer*))
     ,@body))

(define-block (buffer :super list)
  (mode :initform nil)
  (name :initform nil)
  (variables :initform (make-hash-table :test 'eq))
  (needs-layout :initform t))

(define-method initialize buffer (&key blocks variables name
				       (width (dash 120))
				       (height (dash 70)))
  (apply #'super%initialize self blocks)
  (setf %name (uniquify-buffer-name name))
  (setf %width width
	%height height)
  (when variables (setf %variables variables)))

(define-method setvar buffer (var value)
  (setf (gethash var %variables) value))

(define-method getvar buffer (var)
  (gethash var %variables))

(defun buffer-local-variable (var-name)
  (getvar *block* var-name))

(defun (setf buffer-local-variable) (var-name value)
  (setvar *block* var-name value))

(defmacro with-buffer-local-variables (vars &rest body)
  (labels ((make-clause (sym)
	     `(,sym (buffer-local-variable ,(make-keyword sym)))))
    (let* ((symbols (mapcar #'make-non-keyword vars))
	   (clauses (mapcar #'make-clause symbols)))
      `(symbol-macrolet ,clauses ,@body))))

;;; Addressing the elements in the buffer

;; (define-method element buffer (row column)
;;   (with-field-values (inputs) self
;;     (when (consp inputs)
      
(define-method invalidate-layout buffer ()
  (setf %needs-layout t))

(define-method delete-block buffer (block)
  (assert (blockyp block))
  (assert (contains self block))
  (delete-input self block))

(define-method bring-to-front buffer (block)
  (with-fields (inputs buffer) self
    (assert (contains buffer block))
    (delete-input self block)
    (append-input self block)))

(define-method on-update buffer ()
  (with-buffer self 
    (dolist (each %inputs)
      (on-update each))
    (update-layout self)))

(define-method update-layout buffer (&optional force)
  (with-fields (inputs needs-layout) self
    (when (or force needs-layout)
      (dolist (each inputs)
	(layout each))
      (setf needs-layout nil))))

(define-method append-input buffer (block)
  (assert (blockyp block))
  (with-fields (inputs) self
    (assert (not (contains self block)))
    (set-parent block self)
    (setf inputs (nconc inputs (list block)))))

(define-method add-block buffer (block &optional x y)
  (assert (blockyp block))
  ;(assert (not (contains self block)))
  (append-input self block)
  (when (and (integerp x)
	     (integerp y))
    (move-to block x y))
  (invalidate-layout self))
	  
;;; buffers.lisp ends here
