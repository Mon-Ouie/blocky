;;; forth.lisp --- forth-style concatenative word language for Blocky

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

(defvar *words* nil)

(defvar *stack* nil)

(defvar *program* nil)

(defun initialize-words ()
  ;; words are symbols so we use 'eq
  (setf *words* (make-hash-table :test 'eq)))

(initialize-words)

(defstruct word name body properties arguments)

(defun word-definition (word)
  (gethash word *words*))

(defun set-word-definition (name definition)
  (format t "WORD DEF ~S ~S" name definition)
  (assert (not (null name)))
  (assert (symbolp name)) 
  (assert (not (keywordp name)))
  ;; (assert (or (word-p definition)
  ;; 	      (listp definition)))
  (setf (gethash name *words*) definition))

(defmacro define-word (name arguments &body body)
  `(progn 
     (defun ,name ,arguments ,@body)
     (set-word-definition 
      ',name
      (make-word :name ',name
		 :arguments ',arguments
		 :body ',name))
     (export ',name)))

(define-word define ()
  ;; grab remainder of input as definition
  (destructuring-bind (name &rest definition) *program*
    ;; forth definitions are stored as vectors
    (set-word-definition 
     name
     (make-word :name name
		:body (apply #'vector definition)))
    ;; stop parsing.
    (setf *program* nil)))

(define-word forget-word (word)
  (let ((definition (word-definition word)))
    (when (vectorp (word-body definition))
      (remhash word *words*))))

(defun forget-all-words ()
  (loop for word being the hash-keys of *words*
	do (forget-word word)))

(defun execute-word (word)
  (let ((definition (word-definition word)))
    (when (null definition) (error "Unknown word: ~A" word))
    (let ((body (word-body definition)))
      (etypecase body
	;; it's an embedded list. push it.
	(cons 
	 (push body *stack*))
	;; it's a function word (i.e. a primitive)
	(symbol 
	 (assert (fboundp word))
	 ;; grab arguments and invoke function
	 (let ((arguments (word-arguments definition)))
	   (dotimes (n (length arguments))
	     (push (pop *stack*) arguments))
	   (apply body (nreverse arguments))))
	;; it's a forth definition
	(vector
	 (map nil #'execute-word body))))))

(defun execute-program (program)
  (let ((*program* program))
    (loop while *program* 
	  do (execute-word (pop *program*)))))

(defun program-from-string (string)
  (with-input-from-string (stream string)
    (loop for sexp = (read stream nil)
	  while sexp collect sexp)))

(defun execute-program-string (string)
  (execute-program (program-from-string string)))


;; (defmacro define-function-word (name arguments &optional symbol)
;;   `(setf (word-definition ',name)
;; 	 (make-word :name ',name
;; 		    :arguments ',arguments
;; 		    ;; point to the function
;; 		    :body ',(or symbol name))))
;; (define-function-word + (a b))
;; (define-function-word concatenate (a b))

  
;;; forth.lisp ends here
