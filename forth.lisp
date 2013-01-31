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

(defun fpush (x) (push x *stack*))
(defun fpop () (pop *stack*))
(defun next-word () (when *program* (first *program*)))
(defun grab-next-word () (pop *program*))

(defun end-marker-p (word) 
  (and (symbolp word)
       (string= "END" (symbol-name word))))

(defun else-marker-p (word) 
  (and (symbolp word)
       (string= "ELSE" (symbol-name word))))

(defun grab-until-end ()
  (let (words word)
    (block grabbing
      (loop while *program* do 
	(setf word (grab-next-word))
	(if (end-marker-p word)
	    (return-from grabbing)
	    (push word words))))
    (nreverse words)))

(defun initialize-words ()
  ;; words are symbols so we use 'eq
  (setf *words* (make-hash-table :test 'eq)))

(initialize-words)

(defstruct word name body properties arguments)

(defun word-definition (word)
  (gethash word *words*))

(defun set-word-definition (name definition)
  (assert (not (null name)))
  (assert (symbolp name)) 
  (assert (not (keywordp name)))
  (setf (gethash name *words*) definition))

(defmacro define-word (name arguments &body body)
  "Define a primitive word called NAME.
The BODY-forms execute later when the word NAME is executed.
The ARGUMENTS (if any) are auto-pulled from the stack."
  `(set-word-definition 
    ',name
    (make-word :name ',name
	       :arguments ',arguments
	       :body #'(lambda ,arguments ,@body))))

(defun define-program-word (name program)
  "Define a word as a sequence of words."
  (set-word-definition 
   name
   (make-word :name name
	      ;; forth definitions are stored as lists
	      :body program)))

;; defining words in source

(define-word end () nil)
(define-word else () nil)

(define-word define ()
  (destructuring-bind (name &rest definition) 
      (grab-until-end)
    (define-program-word name definition)))

(defun forget-word (word)
  (let ((definition (word-definition word)))
    (when (consp (word-body definition))
      (remhash word *words*))))

(define-word forget (word)
  (forget-word word))

(defun forget-all-words ()
  (loop for word being the hash-keys of *words*
	do (forget-word word)))

;;; The interpreter

(defun execute-word (word)
  (if (typep word '(or cons string number character))
      ;; it's a literal. push it
      (push word *stack*)
      ;; otherwise try looking it up.
      (let ((definition (word-definition word)))
	(if (null definition)
	    ;; unknown symbol. 
	    ;; check to see if it's a field read/write
	    (cond 
	      ;; write field
	      ((keywordp word)
	       (setf (field-value word *self*) 
		     (fpop)))
	      ;; read field
	      ((field-reference-p word)
	       (fpush (field-value
		       (make-keyword
			;; strip percent sign 
			(subseq (symbol-name word) 1))
		       *self*)))
	      ;; not a field reference.
	      (t (error "Cannot execute unknown word: ~A" word)))
	    ;;
	    ;; found a definition. execute the body.
	    (let ((body (word-body definition)))
	      (etypecase body
		;; it's a forth definition. execute it.
		(cons
		 (let ((*program* body))
		   (loop while *program*
			 do (execute-word (pop *program*)))))
		;; it's a function word (i.e. a primitive)
		(function
		 ;; grab arguments (if any) and invoke primitive function
		 (let (arguments)
		   (dotimes (n (length (word-arguments definition)))
		     (push (fpop) arguments))
		   (apply body (nreverse arguments))))))))))
  
(defun execute-program (program)
    (let ((*program* program))
      (loop while *program* 
	    do (execute-word (grab-next-word)))))

(defun program-from-string (string)
  (with-input-from-string (stream string)
    (loop for sexp = (read stream nil)
	  while sexp collect sexp)))

(defun execute-program-string (string)
  (execute-program (program-from-string string)))

(defmacro forth (&rest words)
  `(execute-program ',words))

;;; Control flow

(define-word not (boolean)
  (fpush (if boolean nil t)))

(define-word if (boolean then else)
  (execute-program (if boolean then else)))

(define-word each (elements body)
  (dolist (element elements)
    (fpush element)
    (execute-program body)))

;; TODO map, filter

;; (define-word reduce (elements initial-value body)

;;; Object-orientation

(define-word new () (fpush (new (fpop))))
(define-word self () (fpush *self*))
(define-word this () (setf *self* (fpop)))

;; articles quote the next word.
;; examples:
;;    "a block"
;;    "a robot"
;;    "with (1 2 3)"

(define-word a () (fpush (grab-next-word)))
(define-word an () (fpush (grab-next-word)))
(define-word the () (fpush (grab-next-word)))
(define-word with () (fpush (grab-next-word)))
(define-word to () (fpush (grab-next-word)))

(defun drop-article ()
  (grab-next-word))

;; the copula "is" defines new objects from old.
;; examples: 
;;     "a robot is a block"
;;     "a robot with (health bullets inventory) is a block"
;;     "an enemy is a robot"

(define-word is (name)
  (drop-article)
  (let* ((super (grab-next-word))
	 (fields (when (consp (first *stack*))
		   (fpop))))
    (eval `(define-block (,name :super ,super) ,@fields))))

;; invoking a method without any arguments.

(define-word send (method)
  (send (make-keyword method) *self*))

;; the "to...does...end" idiom defines behavior for verbs.
;; examples: 
;;    "to fire a robot with (direction) does ... end"
;;    "to destroy an enemy does ... end"

(define-word does ()
  ;; ignore argument list for now
  (when (consp (first *stack*))
    (fpop))
  (let* ((super (fpop))
	 (method (fpop))
	 (body (grab-until-end)))
    ;; define a self-verb shortcut
    (execute-program `(define ,method the ,method send end))
    ;; install the method
    (format t "BODY: ~A" body)
    (eval `(define-method ,method ,super ()
	     (execute-program ',body)))))

;; '
;; (progn 
;;   (forget-all-words)
;;   (setf *stack* nil)
;;   (define-word foo () (format t " foo ") (push 3 *stack*))
;;   (define-word bar () (format t " bar ") (push 5 *stack*))
;;   (define-word baz (a b) (format t " baz ") (push (+ a b) *stack*))
;;   (define-word yell () (format t "WOOHOO!!"))
;;   (execute-program-string "foo bar baz")
;;   (execute-program-string "define quux foo bar baz")
;;   (execute-program-string "quux")
;;   (execute-program '(quux 100 baz))
;;   (forth quux 100 baz)
;;   (forth a robot is a block)
;;   (forth to fire a robot does quux 200 baz yell end)
;;   (forth a robot new)
;;   (forth a robot new this fire))
  
;;; forth.lisp ends here
