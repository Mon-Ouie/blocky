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

(define-word forget-all-words ()
  (loop for word being the hash-keys of *words*
	do (forget-word word)))

(defun execute-word (word)
  (let ((definition (word-definition word)))
    (when (null definition) (error "Unknown word: ~A" word))
    (let ((body (word-body definition)))
      (etypecase body
	;; it's a literal. push it
	((or cons string number character)
	 (push body *stack*))
	;; it's a forth definition. execute it.
	(vector
	 (map nil #'execute-word body))
	;; it's a function word (i.e. a primitive)
	(symbol 
	 (assert (fboundp word))
	 ;; grab arguments and invoke function
	 (let ((arguments (word-arguments definition))
	       (values nil))
	   (dotimes (n (length arguments))
	     (push (pop *stack*) values))
	   (apply body (nreverse values))))))))

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

;; (define-word foo () (format t " foo ") (push 3 *stack*))
;; (define-word bar () (format t " bar ") (push 5 *stack*))
;; (define-word baz (a b) (format t " baz ") (push (+ a b) *stack*))
;; *stack*
;; (execute-word 'foo)
;; *stack*
;; (execute-word 'bar)
;; *stack*
;; (execute-word 'baz)
;; *stack*
;; (setf *stack* nil)
;; *stack*
;; (execute-program-string "foo bar baz")
;; (execute-program-string "define quux foo bar baz")
;; (execute-program-string "quux")
  
;;; forth.lisp ends here
