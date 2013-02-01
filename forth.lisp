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
  (setf (gethash name *words*) definition))

(defmacro define-word (name arguments &body body)
  "Define a primitive word called NAME with Lisp code.
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
      (fpush word)
      ;; otherwise try looking it up.
      (let ((definition (word-definition word)))
	(if (null definition)
	    (error "Cannot execute unknown word: ~A" word)
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
  
(defun execute (program)
    (let ((*program* program))
      (loop while *program* 
	    do (execute-word (grab-next-word)))))

(defun program-from-string (string)
  (with-input-from-string (stream string)
    (loop for sexp = (read stream nil)
	  while sexp collect sexp)))

(defun execute-string (string)
  (execute (program-from-string string)))

(defmacro forth (&rest words)
  `(execute ',words))

;;; Control flow

(define-word not (boolean)
  (fpush (if boolean nil t)))

(define-word if (boolean then else)
  (execute (if boolean then else)))

(define-word each (elements body)
  (dolist (element elements)
    (fpush element)
    (execute body)))

;; (define-word map (elements body)
;; (define-word filter (elements body)
;; (define-word reduce (elements initial-value body)

;;; Accessing fields. See also `define-block'.

(defun define-field-accessor-words (input)
  (dolist (name (mapcar #'make-keyword input))
    ;; :foo pushes value of field FOO on the stack
    (execute `(define ,name 
		the ,name get))
    ;; !bar sets field BAR to value on top of stack
    (execute `(define ,(intern (concatenate 'string "!" (symbol-name name)))
		the ,name set))))

(define-word get (field)
  (fpush (field-value field *self*)))

(define-word set (field)
  (setf (field-value field *self*) (fpop)))

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

;; invoking a Blocky method without any arguments.

(define-word send (method)
  (send (make-keyword method) *self*))

;; invoking a Forth method

(define-word call (method object)
  (execute (field-value (make-keyword method) object)))

;; telling  object to invoke one of its methods

(define-word tell (program object)
  (let ((*self* object))
    (execute program)))

;; the "to...do...end" idiom defines behavior for verbs.
;; examples: 
;;    "to fire a robot with (direction) do ... end"
;;    "to destroy an enemy do ... end"

(define-word do ()
  ;; ignore argument list for now
  (when (consp (first *stack*))
    (fpop))
  (let* ((super (fpop))
	 (method (fpop))
	 (body (grab-until-end)))
    ;; define a self-verb shortcut 
    (execute `(define ,method the ,method self call end))
    ;; install the forth definition in the prototype
    (setf (field-value (make-keyword method)
		       (find-object super))
	  body)))

;;; start stop initialize destroy remove copy tag tag? here there it
;;; me untag contains? drop drop-at event update move forward left
;;; right backward show hide menu draw animate scale animate image
;;; draw resize center play collide colliding? head distance frames
;;; seconds later damage enter exit pop !heading !tags !parent !x !y
;;; !z !blend !opacity !width !height !depth !image

;; example:  "explode1.png" draw ("explode2.png" draw) 0.1 seconds later
;;   (destroy) enemy tell 


;; '
;; (progn 
;;   (forget-all-words)
;;   (setf *stack* nil)
;;   (define-word foo () (format t " foo ") (push 3 *stack*))
;;   (define-word bar () (format t " bar ") (push 5 *stack*))
;;   (define-word baz (a b) (format t " baz ") (push (+ a b) *stack*))
;;   (define-word yell () (format t "WOOHOO!!"))
;;   (execute-string "foo bar baz")
;;   (execute-string "define quux foo bar baz")
;;   (execute-string "quux")
;;   (execute '(quux 100 baz))
;;   (forth quux 100 baz)
;;   (forth a robot is a block)
;;   (forth to fire a robot do quux 200 baz yell end)
;;   (forth a robot new)
;;   (forth a robot new this fire))
  
;;; forth.lisp ends here
