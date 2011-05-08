;;; prototypes.lisp --- an alternative object system for Common Lisp

;; Copyright (C) 2007, 2008, 2009, 2010, 2011  David O'Toole
;; Author: David O'Toole ^dto@gnu.org
;; Keywords: oop
;; Version: 1.8
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file implements a simple prototype-based object system for
;; Common Lisp. In this alternative view of object-orientation, there
;; are no classes; instead, objects are cloned from other objects
;; called "prototypes", from which the new objects may inherit data
;; and behavior. The details of inheritance, message passing, and
;; field lookup are inspired by the Io language and the Garnet UI
;; toolkit.

;; http://en.wikipedia.org/wiki/Prototype-based_programming
;; http://en.wikipedia.org/wiki/Message_passing
;; http://www.cliki.net/Garnet
;; http://iolanguage.com/about/

;; TODO add docs/lambdas/argslists to slash funcs
;; TODO fix parent lookup
  
;;; Code: 

(in-package :ioforms)

;;; Emacs Lisp compatibilty macro 

(defmacro while (test &body body)
  `(loop while ,test do ,@body))

;;; Utility functions

(defun make-keyword (S)
  "Make the symbol or string S into a keyword symbol."
  (etypecase S
    (string (intern (string-upcase S) :keyword))
    (symbol (intern (symbol-name S) :keyword))))

(defun make-non-keyword (S)
  "Make the symbol or string S into a non-keyword symbol."
  (etypecase S
    (symbol (intern (symbol-name S)))
    (string (intern S))))

(defun merge-hashes (a b)
  (prog1 a
    (maphash #'(lambda (k v)
		 (setf (gethash k a) v))
	     b)))
	       
(defun make-special-variable-name (S &optional package)
  "Make the symbol S into a special variable name. This is used to
make the names of the objects made with `define-prototype'."
  (let ((name (concatenate 'string "=" (symbol-name S) "=")))
    (if package
	(intern name package)
	(intern name))))

;;; Object data structure

;; Each object's "bookkeeping data" is stored in a structure. The
;; structure itself stands for the object.

(defstruct object
  ;; The most important features of an object are its "fields" or data
  ;; members. We use a hash table (or plist) to represent the field
  ;; collection. Methods are just function-valued fields. See
  ;; `field-value' and `set-field-value'.
  fields
  ;; Objects can inherit field values from a prototype object which
  ;; then influences the new object's behavior. We must store a link
  ;; to this "parent" object so that `field-value' can obtain the
  ;; inherited field values.
  parent
  ;; Objects may have names. A name is a symbol that identifies the
  ;; object. Named objects are "prototypes" from which other objects
  ;; may be created or "cloned".
  name
  ;; The last few methods called are cached in this alist:
  cache)

;;; Fields

;; An object's field collection is either a hash table or plist. The
;; function `field-value' implements the chaining field lookups that
;; make inheritance work in IOFORMS.

;; If a field value is not present in a given object's field
;; collection, the object's parent is also checked for a value, and
;; then its parent, and so on. This is how objects can inherit data
;; and behavior from prototypes. See `field-value'.

;; When you set the value of any field, the parent's value is
;; hidden from that point on. There is no way to remove a local field
;; value. See `set-field-value'.

(defvar *lookup-failure* (gensym)
  "A value returned in order to signify the failure of a field lookup.
When looking up fields, we need a default value to indicate that a
lookup failed (see `gethash'). But this value could be a perfectly
legitimate field value, and the system would then falsely report a
field lookup error because it could not tell the difference. By using
an uninterned symbol as that default value, we can be sure that it
won't be `eq' to anything. See `field-value'.

This is only used internally. In most situations, a field access or
method call that references a non-existent field will signal a
`no-such-field' error.")

(define-condition no-such-field (error)
  ((field-name :initarg :field-name :accessor field-name)
   (object :initarg :object :accessor object))
  (:report (lambda (condition stream)
	     ;; TODO improve object printing
	     (format stream "No such field ~S in object ~S." 
		     (field-name condition)
		     (object condition)))))
		     
;; Next come the main user-level functions for setting/getting field
;; values. 

(defun fref (fields key)
  (etypecase fields
    (list (getf fields key *lookup-failure*))
    (hash-table (gethash key fields *lookup-failure*))))

(defun set-fref (fields key value)
  (etypecase fields
    (list (setf (getf fields key) value))
    (hash-table (setf (gethash key fields) value)))
  (values value fields))
  
(defsetf fref set-fref)

(defun field-value (field object &optional noerror)
  "Return the value of FIELD in OBJECT.
If the FIELD has no value in OBJECT, then the object's parent is also
checked, and so on. If a value is found during these checks, it is
returned. If a value cannot be found, an error of type `no-such-field'
is signaled, unless NOERROR is non-nil; in that case,
`*lookup-failure*' is returned. See also `has-field'."
  (declare (optimize (speed 3)))
  (let (pointer result found)
    (setf pointer object)
    ;; search the chain of objects for a field value.
    (loop while (and pointer (not found)) do
	 (setf result (fref (object-fields pointer) field))
	 (if (eq *lookup-failure* result)
	     ;; it's not here. search the parent, if any.
	     (setf pointer (object-parent pointer))
	     ;; we found a value in this object.
	     (setf found t)))
    (if found 
	result
	(if noerror 
	    *lookup-failure*   
	    (error 'no-such-field :field-name field :object object)))))

(defun set-field-value (field object value)
  "Set OBJECT's FIELD to VALUE.
The new value overrides any inherited value."
  (prog1 value
    (multiple-value-bind (value fields)
	(set-fref (object-fields object) field value)
      (when (listp fields)
	(setf (object-fields object) fields)))))
  
(defsetf field-value set-field-value)

(defmacro $ (field expr)
  `(field-value ,(make-keyword field) ,expr))

(defun has-field (field object)
  "Return non-nil if FIELD has any value in OBJECT."
  (not (eq *lookup-failure* (field-value field object :noerror))))

(defun has-method (method object)
  (and (has-field method object)
       (or (symbolp (field-value method object))
	   (functionp (field-value method object)))))

(defun with-fields-ex (fields expression binding-type body)
  (assert (member binding-type '(let symbol-macrolet)))
  (assert (listp fields))
  (let ((object-sym (gensym)))
    (labels ((make-clause (sym)
	       (list (make-non-keyword sym)
		     `(field-value ,(make-keyword sym) ,object-sym))))
      `(let ((,object-sym ,expression))
	 (,binding-type ,(mapcar #'make-clause fields)
			,@body)))))

(defmacro with-fields (fields expression &body body)
  "Bind FIELDS from the object EXPRESSION to names in the local
environment in evaluating BODY. Local symbol macros are used, meaning
that each reference (get or set) is a slot access on the object."
  (with-fields-ex fields expression 'symbol-macrolet body))

(defmacro with-field-values (fields expression &body body)
  "Bind the names in FIELDS to the values of the corresponding fields
in EXPRESSION, in evaluating BODY. Each slot is accessed only once,
upon binding."
  (with-fields-ex fields expression 'let body))

;;; Field options

;; Every prototype has a set of field options. Field options record
;; metadata regarding fields; for example, `define-method' stores the
;; argument list and documentation string for the method in the
;; prototype's field options property list.

(defun field-options (field object)
  "Obtain the options property list for FIELD in OBJECT."
  (second (assoc field (field-value :field-descriptors object))))

(defun set-field-options (field object options)
  "Set the options property list to OPTIONS for FIELD in OBJECT." 
  (setf (field-value :field-descriptors object)
	(remove-duplicates (acons field options
				  (field-value :field-descriptors object))
			   :key #'car :from-end t)))

(defsetf field-options set-field-options)

(defun field-option-value (field object option)
  "Return the value of the OPTION for FIELD in OBJECT."
  (getf (field-options field object) option))

(defun set-field-option-value (field object option value)
  "Set the value of the OPTION for FIELD in OBJECT to VALUE."
  (symbol-macrolet ((options (second (assoc field (field-value :field-descriptors object)))))
    (when options
      (setf (getf options option) value))))
  
(defsetf field-option-value set-field-option-value)

(defun field-documentation (field object)
  (field-option-value field object :documentation))

;;; Basic SLIME auto documentation support

(defvar *method-documentation* nil)

(defvar *method-arglists* nil)

(defun initialize-documentation-tables ()
  (setf *method-documentation* (make-hash-table :test 'equal))
  (setf *method-arglists* (make-hash-table :test 'equal)))

(defun method-documentation (method-symbol)
  (if (null *method-documentation*)
      (prog1 nil (initialize-documentation-tables))
      (gethash (make-keyword method-symbol)
	       *method-documentation*)))

(defun set-method-documentation (method-symbol docstring)
  (unless *method-documentation*
    (initialize-documentation-tables))
  (setf (gethash (make-keyword method-symbol)
		 *method-documentation*)
	docstring))

(defsetf method-documentation set-method-documentation)
	  
(defun method-arglist (method-symbol)
  (if (null *method-arglists*)
      (prog1 :not-available (initialize-documentation-tables))
      (gethash (make-keyword method-symbol)
	       *method-arglists* :not-available)))

(defun method-arglist-for-swank (method-symbol)
  (let ((result (method-arglist method-symbol)))
    (if (listp result)
	(cons 'self result)
	result)))

(defun set-method-arglist (method-symbol arglist)
  (unless *method-arglists*
    (initialize-documentation-tables))
  (setf (gethash (make-keyword method-symbol)
		 *method-arglists*)
	arglist))

(defsetf method-arglist set-method-arglist)

;;; Methods and messages

;; Methods are function-valued fields whose first argument is the
;; object to operate on. The remaining arguments are the arguments to
;; the method. The `define-method' macro defined later will insert
;; this implicit `self' argument for you, and implement some syntactic
;; sugar.

;; First we implement method caching, which avoids hash table lookups 
;; for repeatedly used methods. 

(defconstant +cache-size+ 6)

(defun initialize-method-cache (object)
  (setf (object-cache object) 
	(list (cons nil nil) (cons nil nil) (cons nil nil)
	      (cons nil nil) (cons nil nil) (cons nil nil))))

(defun cache-method (object method func)
  (let ((cache (object-cache object))
	(entry nil))
    (assert (listp cache))
    (rotatef (sixth cache)
	     (fifth cache)
             (fourth cache)
             (third cache)
    	     (second cache)
	     (first cache))
    (setf entry (first cache))
    (setf (car entry) method)
    (setf (cdr entry) func)))

(defun cache-lookup (object method)
  (cdr (assoc method (object-cache object))))

;; Next comes the basic function for sending a message synchronously
;; and obtaining a return value.  

(defun send (method object &rest args)
  "Invoke the method identified by the keyword METHOD on the OBJECT with ARGS.
If the method is not found, attempt to forward the message."
  ;; See also `send-queue' and `send-parent'
  (assert (object-p object))
  ;; check the cache
  (let ((func (cache-lookup object method)))
    (if func
	;; cache hit. finish up.
	(apply func object args)
	;; cache miss. look for a value.
	(progn (setf func (field-value method object :noerror))
	       (if (not (eq *lookup-failure* func))
		   ;; store in cache and then call.
		   (progn 
		     (cache-method object method func)
		     (apply func object args))
		   ;; no such method. try forwarding
		   (if (has-field :forward object)
		       (apply (field-value :forward object)
			      object method args)
		       (error (format nil "Could not invoke method ~S" method))))))))

;;; Message forwarding

;; When a message cannot be delivered because no corresponding
;; function was found, IOFORMS attempts to re-send the message via the
;; object's `forward' method (if any).

;; An object's `forward' method should accept the method-key as the
;; first argument, and the arguments of the original message as the
;; remaining arguments.

;; We also want to be able to invoke the prototype (or "parent's")
;; version of a method; for example during initialization, one might
;; wish to run the parent's initializer as the first statement in the
;; child's.

(define-condition null-parent (error)
  ((method-key :initarg :message :accessor method-key)
   (object :initarg :object :accessor object))
  (:report (lambda (condition stream)
	     (format stream "Cannot find parent method ~S for object ~S." 
		     (method-key condition)
		     (object condition)))))

;; TODO Rewrite this function

(defun find-nth-named-parent (object method-key n)
    (assert (plusp n))
    (let ((pointer object)
	  (x n))
      (labels ((find-parent (p)
		 (let ((parent (object-parent p)))
		   (etypecase parent
		     (object parent)
		     (symbol (symbol-value parent)))))
	       (next-parent ()
		 (setf pointer (find-parent pointer))))
	(loop while (and pointer
			 (not (zerop x)))
	   do (progn 
		(if (object-name pointer)
		    (progn (decf x)
			   (unless (zerop x)
			     ;; don't obliterate the pointer.
			     (next-parent)))
		    (next-parent))))
	;; what happened?
	(if (zerop x)
	    ;; we found it.
	    pointer
	    ;; no nth method found.
	    (error "Cannot find parent method for n=~A" n)))))
    
(defun find-parent (ob)
  (if (object-name ob)
      (object-parent ob)
      (object-parent (object-parent ob))))

(defvar *send-parent-depth* 2)

(defun initialize-ioforms ()
  (format t "~A" *copyright-text*)
;;  (initialize-documentation-tables)
  (setf *send-parent-depth* 2)) ;;FIXME

(defvar *send-parent-key* nil)

(defun send-parent (method-key object &rest args)
  "Invoke the parent's version of a method on OBJECT with ARGS."
  (let* ((depth (if (not (eq method-key *send-parent-key*))
		    2 
		    *send-parent-depth*))
	 (method-source (find-nth-named-parent object method-key 
					      depth)))
    (if method-source
	(let ((*send-parent-depth* (1+ *send-parent-depth*))
	      (*send-parent-key* method-key))
	  (apply (field-value method-key method-source) object args))
	(error 'null-parent :method-key method-key :object object))))

;;; Message queueing

;; In some situations, an object will wish to queue up messages to be
;; sent elsewhere at a later time. `send-queue' will do this.

;; First we need a general queue mechanism.

(defstruct queue head tail count max)

(define-condition empty-queue (error) ())

(defun unqueue (Q)
  (when (null (queue-head Q))
    (error 'empty-queue))
  (when (eq (queue-head Q)
	    (queue-tail Q))
    ;; only one item is in the queue; kill the tail pointer
    (setf (queue-tail Q) nil))
  ;; now unqueue
  (decf (queue-count Q))
  (pop (queue-head Q)))

(defun queue (item Q)
  (let ((element (cons item nil)))
    (if (null (queue-tail Q))
	;; handle empty queue
	(progn 
	  (setf (queue-tail Q) element
		(queue-head Q) (queue-tail Q)
		(queue-count Q) 1))
	;; handle nonempty queue
	(progn 
	  (setf (cdr (queue-tail Q))
		element)
	  (pop (queue-tail Q))
	  (incf (queue-count Q)))))
  ;; now prevent exceeding any max that's been set. this is useful to
  ;; prevent allocating all memory when you don't care about throwing
  ;; away old objects.
  (when (and (numberp (queue-max Q))
	     (< (queue-max Q) (queue-count Q)))
    (unqueue Q)))

;; And now the message queueing functionality.

(defvar *message-queue* nil "This variable is bound to the current
message queue, if any.")

(defun queue-message (method-key receiver args)
  "Enter a message into the current `*message-queue*'."
  (queue (list method-key receiver args) *message-queue*))

(defun queued-messages-p ()
  "Return non-nil if there are queued messages."
  (not (null (queue-head *message-queue*))))

(defun unqueue-message ()
  "Remove the next message from the queue. The returned message is a
list of the form (METHOD-KEY RECEIVER ARGS)."
  (unqueue *message-queue*))

(defun unqueue-and-send-message ()
  (let ((msg (unqueue-message)))
    (destructuring-bind (method-key receiver args) msg
      (apply #'send method-key receiver args))))

(defmacro with-message-queue (expr &body body)
  "Run the BODY forms, capturing any queued output messages to the
message queue resulting from the evaluation of EXPR."
  `(let ((*message-queue* ,expr))
     ,@body))

(defun send-queue (method-key object &rest args)
  "Queue a message. Returns nil."
  (queue-message method-key object args))

;;; Field reference syntax

;; Within method bodies, you can access the fields of `self' with the
;; shorthand
;;
;;   ^foo
;;
;; instead of writing
;;
;;   (field-value :foo self)
;;
;; For example:
;;
;;   (princ ^name)
;;   (setf ^width 10)
;; 
;; Because `self' is not bound outside of method bodies, we use a
;; code-walker to implement the syntax described above. 

(defvar *field-reference-prefix* "^")

(defun transform-tree (tester transformer tree)
  (cond ((consp tree)
	 ;; it's a cons. process the two subtrees.
	 (destructuring-bind (left . right) tree
	   (cons
	    ;; process left subtree.
	    (if (funcall tester left)
		(funcall transformer left)
		;; nothing to transform here. move on down the left side.
		(if (consp left)
		    (transform-tree tester transformer left)
		    left))
	    ;; process right subtree.
	    (transform-tree tester transformer right))))
	;; it's not a cons. test it.
	((funcall tester tree)
	 (funcall transformer tree))
	;; it failed the test. leave it alone.
	(t tree)))

;; Now we turn to the syntax itself and the required tree
;; transformations.

;;; field references of the form ^foo

(defun field-reference-p (form)
  "Return non-nil if FORM is a symbol like ^foo."
  (if (symbolp form)
      (let* ((name (symbol-name form))
	     (len (length name)))
	(string= (subseq name 0 1)
		 *field-reference-prefix*))))
	     ;; (string= ">" (subseq name (- len 1) len))))))

(defun transform-field-reference (ref)
  "Change the symbol REF from ^foo to (field-value :foo self)."
  (let ((name (symbol-name ref)))
    (list 'field-value 
	  (make-keyword (subseq name 1))
	  'self)))
	
(defun transform-method-body (body)
  "Process the forms in BODY to transform field references."
  (transform-tree #'field-reference-p
		  #'transform-field-reference
		  body))

;;; Definining methods

;; The `define-method' macro defined below is the main top-level facility
;; for adding methods to prototypes.

(defmacro define-method
    (method-name prototype-name arglist &body method-body)
  "Define a new method.

METHOD-NAME is a symbol naming the method.  PROTOTYPE-NAME is the name
of the prototype you are defining a method for. This should be a
symbol (without equals signs---see Prototype Names. ARGLIST is
the argument list for the method. If METHOD-BODY begins with a string,
this string becomes the documentation string for the method.

Any DECLARE forms must appear as the first non-string sexp.

The forms in METHOD-BODY are executed when the method is invoked.
The hidden argument `self' may be referred to as needed within
the method body; it is bound to the object upon which the method
was invoked."
  ;; build the components of the defun
  (let* ((documentation (if (stringp (first method-body))
			    (first method-body)))
	 (body2 (remove-if #'stringp (transform-method-body method-body)))
	 ;; handle DECLARE forms when these appear first
	 (declaration (when (and (listp (first body2))
				 (eq 'declare (first (first body2))))
			(first body2)))
	 (prototype-special-name (make-special-variable-name prototype-name))
	 (field-name (make-keyword method-name))
	 (method-symbol-name (symbol-name method-name))
	 (method-symbol method-name) ;; fixme, unclear naming
	 (defun-symbol (intern (concatenate 'string
					    (symbol-name prototype-name) 
					    "/"
					    method-symbol-name)))
	 (slash-defun-symbol (intern (concatenate 'string
						  "/"
						  method-symbol-name)))
	 (queue-defun-symbol (intern (concatenate 'string
						  "/QUEUE/"
						  method-symbol-name)))
	 (parent-defun-symbol (intern (concatenate 'string
						  "/PARENT/"
						  method-symbol-name)))
	 (queue-defun-symbol-noslash (intern (concatenate 'string
						  "QUEUE/"
						  method-symbol-name)))
	 (parent-defun-symbol-noslash (intern (concatenate 'string
						  "PARENT/"
						  method-symbol-name))))
    (let ((name (gensym)))
      `(progn 
	 (if (not (boundp ',prototype-special-name))
	     (error (format nil "Cannot define method ~A for nonexistent prototype ~A"
			    ',method-name ',prototype-name)))
	 (let ((prototype (symbol-value ',prototype-special-name)))
	   ;; define the method's Lisp function
	   (defun ,defun-symbol (self ,@arglist)
	     ,@(if documentation (list documentation))
	     ,declaration
	     ,@(if declaration 
		   (rest body2)
		     body2))
	   ;; store the method's function in the prototype's field
	   (setf (field-value ,field-name prototype) ',defun-symbol)
	   ;; add new method-descriptor for this method to the prototype
	   (setf (field-option-value ,field-name prototype :documentation) ,documentation)
	   (setf (field-option-value ,field-name prototype :arguments) ',arglist)
	   ;; update documentation cache
	   (when ,documentation (setf (method-documentation ',method-name) ,documentation))
	   (setf (method-arglist ',method-name) ',arglist)
	   (export ',defun-symbol)
	   (let ((,name ,(make-keyword method-name)))
	     (unless (fboundp ',method-symbol)
	       (defun ,method-symbol (self &rest args)
		 ,@(when documentation (list documentation))
		 (apply #'send ,name self args))
	       (export ',method-symbol)
	       ;; for (foo bar baz...)
	       (defun ,slash-defun-symbol (self &rest args)
		 ,@(when documentation (list documentation))
		 (apply #'send ,name self args))
	       (export ',slash-defun-symbol)
	       ;; and for message queueing
	       (defun ,queue-defun-symbol (self &rest args)
		 ,@(if documentation (list documentation))
		 (apply #'send-queue ,name self args))
	       (export ',queue-defun-symbol)
	       (defun ,queue-defun-symbol-noslash (self &rest args)
		 ,@(if documentation (list documentation))
		 (apply #'send-queue ,name self args))
	       (export ',queue-defun-symbol-noslash)
	       ;; and for parent calls.
	       (defun ,parent-defun-symbol (self &rest args)
		 ,@(if documentation (list documentation))
		 (apply #'send-parent ,name self args))
	       (export ',parent-defun-symbol)
	       (defun ,parent-defun-symbol-noslash (self &rest args)
		 ,@(if documentation (list documentation))
		 (apply #'send-parent ,name self args))
	       (export ',parent-defun-symbol-noslash))))))))

;;; Defining prototypes

;; Objects are created by cloning them from "prototypes". Prototypes
;; are named objects that represent prototypical instances of a
;; certain kind of object. This section implements `define-prototype',
;; the top-level user macro for defining new object prototypes.

;; First we need to define the written syntax for field options, so
;; that we can write these options into prototype declarations later.

(defun transform-declaration (D)
  "Convert the declaration D into a canonical field
descriptor.

The descriptor D must be either a symbol, in which case a field is
defined with no options, or a list of the form:

 (:FIELD-NAME . OPTIONS)

Where OPTIONS is a property list of field options.

The returned entry will be of the form:

 (:FIELD-NAME OPTIONS) 

and will be suitable for use with the functions that operate on field
descriptors, and for inclusion in the association list
^field-descriptors.

See also `define-prototype'.
"
  (etypecase D
    (symbol (list (make-keyword D) nil))
    (list (list (make-keyword (car D)) (cdr D)))))

(defun compose-blank-fields (&optional descriptors (type :hash))
  (ecase type
    (:hash (let ((fields (make-hash-table :test 'eq)))
	     (dolist (d descriptors)
	       (setf (gethash (make-keyword (car d)) fields) nil))
	     fields))
    (:list (let (fields)
	     (dolist (d descriptors)
	       (push nil fields)
	       (push (make-keyword (car d)) fields))
	     fields))))

;; Initforms (i.e. the values of the field option `:initform')
;; initialize fields. A field's initform (if any) is evaluated when an
;; object is cloned from a prototype, and becomes the value of the
;; field for the new instance (instead of inheriting the prototype's
;; value.) See also `define-prototype'.

;; Now we make setf statements (i.e. "field initializers") out of these
;; field initforms.

(defun make-field-initializer (descriptor)
  "Create a setf statement that initializes a field.
The initform is taken from DESCRIPTOR. If there is no initform
specified, no setf statement is generated, because in this case the
slot value is inherited."
  (destructuring-bind (field (&key initform &allow-other-keys)) descriptor
    (if initform `(set-field-value ,field self ,initform))))

(defun plist-to-descriptors (plist)
  (let (descriptors)
    (loop while plist do
      (let* ((field (pop plist))
	     (value (pop plist)))
	(push (list field :initform value)
	      descriptors)))
    (nreverse descriptors)))
	
(defmacro define-prototype (name
			    (&key parent 
				  documentation
				  &allow-other-keys)
			    &body declarations)
  "Create a new object prototype (possibly based on another prototype).

NAME should be a symbol naming the prototype. A special variable is
created, with equals signs bracketing the name; this variable's value
is the resulting prototype. For example, if your prototype is named
`foo', the special variable will be named `=foo=', and you create
objects with:

 (clone =foo=)

See also `clone'. 

The second argument is a property list of options for the
prototype. Valid keys are:

 :DOCUMENTATION     The documentation string for this prototype.
 :PARENT            The parent prototype from which the new prototype will 
                    inherit fields. This form is evaluated.
                     
DECLARATIONS should be a list, each entry of which is
either a list of the form

  (FIELD-NAME . OPTIONS)

or, simply a symbol naming the field---a shorthand for declaring a
field with that name and no options. See also
`transform-declaration'.

OPTIONS is a property list of field options. Valid keys are:

 :INITFORM          A form evaluated to initialize the field
                    upon cloning. If :initform is not provided,
                    the value is inherited from the PARENT.
                    With \":initform nil\", the field is initialized 
                    with the value nil.
 :DOCUMENTATION     Documentation string for the field.
"
  (let* ((pre-descriptors (if (keywordp (first declarations))
			      (plist-to-descriptors declarations)
			      declarations))
	 (descriptors (mapcar #'transform-declaration 
			      pre-descriptors))
	 (proto-symbol (make-special-variable-name name))
	 (field-initializer-body (delete nil (mapcar #'make-field-initializer 
						     descriptors))))
    `(progn
       ;; Need this at top-level for compiler to know about the special var
       (defparameter ,proto-symbol nil)
       (let* ((fields (compose-blank-fields))
	      ;;(fields (make-hash-table :test 'eq))
	      (blanks (compose-blank-fields ',descriptors))
	      (prototype (make-object :fields fields
				      :name ',proto-symbol
				      :parent ,parent))
	      (parent-descriptors (field-value :field-descriptors 
					       ,parent
					       :noerror))
	      (descriptors2 (append ',descriptors (when (listp parent-descriptors)
						    parent-descriptors))))
	 (initialize-method-cache prototype)
	 (setf (fref fields :field-descriptors) descriptors2)
	 (setf (fref fields :documentation) ,documentation)
	 (setf (fref fields :initialize-fields) (function (lambda (self) 
						     ,@field-initializer-body)))
	 (merge-hashes fields blanks)
	 (defparameter ,proto-symbol prototype)
	 (send :initialize-fields prototype)
	 ;; the prototype's parent may have an initialize method.
	 ;; if so, we need to initialize the present prototype.
	 (when (has-field :initialize prototype)   
	   (send :initialize prototype))
	 prototype))))

;;; Printing objects

(defun object-address-string (ob)
  (let ((string (with-output-to-string (s)
		  (print-unreadable-object (ob s :identity t)))))
    (subseq string
	    (1+ (search "{" string))
	    (search "}" string))))

;;; Cloning objects

(defmacro new (prototype-name &rest initargs)
  `(clone ,(make-special-variable-name prototype-name)
	  ,@initargs))

(defun clone (prototype &rest initargs)
  "Create a new object from PROTOTYPE and pass INITARGS to the
initializer. The new object is created with fields for which INITFORMS
were specified (if any; see `define-prototype'); the INITFORMS are
evaluated, then any applicable initializer is triggered."
  (let ((new-object (make-object :parent prototype 
				 :fields (compose-blank-fields nil :list))))
    (initialize-method-cache new-object)
    (send :initialize-fields new-object)
    (if (has-field :initialize new-object)
	(apply #'send :initialize new-object initargs))
    new-object))

;;; Serialization support

;; The functions SERIALIZE and DESERIALIZE convert
;; (almost abitrary) trees of Lisp objects (including IOFORMS objects)
;; into printable S-expressions for storing as plain text, and from
;; this printed representation back into living IOFORMS objects.

;; The method names :SERIALIZE and :DESERIALIZE are reserved for
;; serialization use. :SERIALIZE, if such a method is present, is
;; invoked before serialization. The object being serialized may use
;; this hook to pre-process its fields. :DESERIALIZE is likewise
;; invoked (if present) after reading the object from disk, and is
;; used to recover from deserialization. The reserved field
;; ^EXCLUDED-FIELDS is a list of field names (keyword symbols) which
;; are not serialized; typically these will be properly re-initialized
;; by the :DESERIALIZE method. 

(defconstant +object-type-key+ :%IOF%OBJECT%)
(defconstant +hash-type-key+ :%IOF%HASH-TABLE%)

(defun serialize (object)
  "Convert a Lisp object a print-ready S-expression.
Invokes :BEFORE-SERIALIZE on IOFORMS objects whenever present. Any fields
named in the list ^EXCLUDED-FIELDS of said object will be ignored."
  ;; use labels here so we can call #'serialize
  (labels ((hash-to-list (hash)
	     (let (plist)
	       (labels ((collect (k v)
			  (push (serialize v) plist)
			  (push k plist)))
		 (maphash #'collect hash)
		 (cons +hash-type-key+ (cons (hash-table-test hash) plist))))))
    (typecase object 
      (hash-table (hash-to-list object))
      (list (mapcar #'serialize object))
      (string object)
      (vector (map 'vector #'serialize object))
      (object (let ((excluded-fields (when (has-field :excluded-fields object)
					    (field-value :excluded-fields object))))
		     ;; possibly prepare object for serialization.
		     (when (has-method :before-serialize object)
		       (send :before-serialize object))
		     (let ((parent-name (object-name (object-parent object)))
			   (name (object-name object))
			   (fields (object-fields object))
			   (plist nil))
		       (assert (and parent-name 
				    (null name)
				    (listp fields))) ;; TODO handle hashes?
		       (labels ((collect (field value)
				  (unless (member field excluded-fields)
				    (push (serialize value) plist)
				    (push field plist))))
			 ;; make serialized/cleaned plist 
			 (loop while fields
			       do (let ((field (pop fields))
					(value (pop fields)))
				    (collect field value)))
			 ;; cons up the final serialized thing
			 (list +object-type-key+
			       :parent parent-name
			       :fields plist)))))
      (otherwise object))))

(defun deserialize (data)
  "Reconstruct Lisp objects (including IOFORMS-derived objects) from an
S-expression made by SERIALIZE. Invokes :AFTER-DESERIALIZE on IOFORMS
objects after reconstruction, wherever present."
  (cond 
    ;; handle IOFORMS objects
    ((and (listp data) (eq +object-type-key+ (first data)))
     (destructuring-bind (&key parent fields &allow-other-keys)
	 (rest data)
       (let ((object (make-object :fields (mapcar #'deserialize fields)
				  :parent (symbol-value parent))))
	 (prog1 object
	   (initialize-method-cache object)
	   ;; possibly recover from deserialization
	   (when (has-method :after-deserialize object)
	     (send :after-deserialize object))))))
    ;; handle hashes
    ((and (listp data) (eq +hash-type-key+ (first data)))
     (destructuring-bind (type-key test &rest plist)
	 data
       (let ((hash (make-hash-table :test test)))
	 (loop while plist do
	   (let* ((key (pop plist))
		  (value (pop plist)))
	     (setf (gethash key hash) (deserialize value))))
	 hash)))
    ((listp data)
     (mapcar #'deserialize data))
    ;; passthru
    (t data)))

(defun /parent/initialize (object &rest args)
  (apply #'send-parent :initialize object args))

(defun parent/initialize (object &rest args)
  (apply #'send-parent :initialize object args))

(defun /queue/initialize (object &rest args)
  (apply #'send-queue :initialize object args))

(defun queue/initialize (object &rest args)
  (apply #'send-queue :initialize object args))

;;; prototypes.lisp ends here
