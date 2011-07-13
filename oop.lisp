;;; oop.lisp --- object-oriented visual programming

;; Copyright (C) 2011  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
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

(in-package :ioforms)

;;; This file is a sort of visual wrapper around prototypes.lisp.

;;; prevent evaluation

(defblock (quote :super list)
  :category :operators)

(define-method evaluate quote () self)

;;; fields

(defblock field
  :category :variables
  :inputs (list (new string :label "name")
		(new entry :label "value"))) ;; TODO: allow any block as a value

(define-method evaluate field ()
  (destructuring-bind (name value) 
      (mapcar #'recompile %inputs)
    (list name :initform value)))

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
		       :subtree (list (new string :value "block" :label "block name")))
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



;;; oop.lisp ends here
