;;; meta.lisp --- visual lisp macros for a blocky-in-blocky funfest

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

;; This file implements a visual layer on top of prototypes.lisp, so
;; that OOP can occur visually. Understanding the terms used in
;; prototypes.lisp will help in reading the present file.


;;; Send the messages in a list to the referent of the first element

;; (define-block-macro prog0 
;;   (:super list
;;    :fields ((category :initform :structure))
;;    :initforms ((pin (first %inputs))))
;;   (error "Recompilation not yet defined for prog0."))

;; (define-method initialize prog0 (target)
;;   (initialize%block self (new 'reference target))
;;   (pin (first %inputs)))

;; (define-method evaluate prog0 ()
;;   (destructuring-bind (target &rest body) %inputs
;;     (mapcar #'evaluate (rest %inputs))
;;       (with-target (evaluate target)
;; 	(mapcar #'evaluate body))))

;; ;;; Defining blocks visually

;; (define-block-macro define-block 
;;   (:super list
;;    :fields ((header :initform nil))
;;    :inputs ((new 'message 
;; 		  :label "define block"
;; 		  :schema '((name string :default "my-block") 
;; 			    (super string :default "block"))
;; 		  :button-p nil)))
;;   ;; grab field specifiers
;;   (let ((specs (mapcar #'recompile (rest %inputs))))
;;     ;; grab parameters from message entry block
;;     (destructuring-bind (name super)
;; 	(recompile (first %inputs))
;;       ;; build up the compiled form
;;       (append (list 'define-block
;; 		    (list (make-non-keyword name)
;; 			  :super (make-prototype-id super)))
;; 	      specs))))

;; ;;; Defining fields 

;; (define-block-macro field 
;;   (:fields ((category :initform :variables))
;;    :inputs ((new 'string :label "field")
;; 	    (new 'socket :label ":default")))
;;   ;; grab args
;;   (destructuring-bind (name value) 
;;       (mapcar #'recompile %inputs)
;;     ;; build field spec
;;     (list name :initform value)))

;; (define-method draw field ()
;;   (let ((*text-baseline* (+ %y (dash 1))))
;;     (super%draw self)))

;; (define-method accept field (thing)
;;   (declare (ignore thing))
;;   nil)

;; ;;; Arguments

;; (define-block-macro argument
;;   (:fields ((category :initform :variables))
;;    :inputs ((new 'string :label "name")
;; 	    (new 'entry :label "type")
;; 	    (new 'string :label "default")))
;;   (destructuring-bind (name type default) 
;;       (mapcar #'recompile %inputs)
;;     (list (make-symbol name) type :default default)))

;; ;;; Defining methods

;; (define-block-macro define-method 
;;   (:super list
;;    :inputs ((new 'message 
;; 		 :button-p nil
;; 		 :schema '((name string :default "") 
;; 			   (prototype string :default "" :label "for block:"))
;; 		 :label "define method")))
;;   ;; create a define-method form 
;;   (let ((inputs (mapcar #'recompile %inputs)))
;;     ;; grab contents of message block
;;     (destructuring-bind ((name prototype) arguments body) inputs
;;       (let ((method-name (make-symbol name))
;; 	    (prototype-id (make-prototype-id prototype))
;; 	    (lambda-list (mapcar #'first arguments)))
;; 	;; now produce a plain lisp method definition
;; 	`(define-method ,method-name ,prototype-id ,lambda-list ,@body)))))

;; ;;; Use quote to prevent evaluation

;; (define-block-macro quote
;;   (:super list
;;    :fields ((category :initform :operators)))
;;    `(quote (,@(field-value :inputs self))))
	     
;;; meta.lisp ends here
