;;; prototypes.lisp --- an alternative object system for Common Lisp

;; Copyright (C) 2007, 2008, 2009, 2010, 2011  David O'Toole
;; Author: David O'Toole dto@ioforms.org
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

;;; Code: 

(in-package :blocky)

(defvar *debug-on-error* nil)

(defvar *author* nil)

(defvar *copyright-notice*
"This is the Blocky programming language.
Copyright (C) 2006-2011 by David T O'Toole <dto@ioforms.org>
http://blocky.io/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program, in the included file named \"COPYING\".
If not, see <http://www.gnu.org/licenses/>.

On some platforms, Blocky is distributed along with libSDL 1.2 (Simple
Direct Media Layer), which is provided under the terms of the GNU
Lesser General Public License. See also the file LIBSDL-LICENSE for
details.

Some functions in the file logic.lisp are based on code written by
Peter Norvig in his book 'Paradigms of Artificial Intelligence
Programming'. See logic.lisp for details.

Some of the OpenGL functions in console.lisp are derived from code in
Bart Botta's CL-OPENGL tutorials; see http://3bb.cc/tutorials/cl-opengl/

This program includes the free DejaVu fonts family in the subdirectory
./standard.blocky/. For more information, see the file named
DEJAVU-FONTS-LICENSE in that subdirectory.
")

;;; Extended argument lists

(defun is-extended-arglist (lambda-list)
  "An extended argument list is like an ordinary CL argument list,
but with each argument's entry replaced by a triple:

  (ARGUMENT-NAME DATA-TYPE &rest OPTIONS)

These triples may be arranged in the extended argument list just as
in a `destructuring-bind', i.e. `&optional', `&key', and all the
other destructuring features:

 ((POSITIONAL-ARG1 TYPE OPTIONS) (POSITIONAL-ARG2 TYPE OPTIONS)
  &KEY (KEYWORD-ARG1 TYPE OPTIONS) (KEYWORD-ARG2 TYPE OPTIONS))

ARG is the argument name (a symbol). DATA-TYPE is a Common Lisp
identifier such as `integer' or `(or integer symbol)' or the like.
See the documentation for the function `schema-option' for more
information on the OPTIONS field.

NOTE: &key and &optional are not yet implemented for extended
arglists.
"
  (and (not (null lambda-list))
       (listp (first lambda-list))
       (not (null (first lambda-list)))))

(defun schemap (datum)
  (and (consp datum)
       (every #'consp datum)
       (every #'symbolp
	      (mapcar #'first datum))))

(defun schema-name (schema)
  (first schema))

(defun schema-type (schema)
  (second schema))

(defun schema-options (schema)
  (nthcdr 2 schema))

(defun schema-option (schema option)
  "Find the value (if any) of the option named OPTION within the
  extended argument list schema SCHEMA. The following keywords are
  valid for OPTION:

  :DEFAULT   The default value for the argument. With no default,
             the presentation history is consulted for a value.

  :DOCUMENTATION     The documentation string.

  :LABEL   User-visible name of the argument. If left unset, the
                  default is `foo bar baz' for the command
                  `foo-bar-baz'.

  :WHEN           Only read the value if this predicate-form returns 
                  non-nil when invoked on the value.

  Not yet supported:

  :PROMPT    A string (or a form evaluating to a string) used as the
             prompt for this argument.

  :PROMPT-MODE   :raw means that prompt is just printed.
                 :normal (the default) specifies standard reformatting:
               
                       Command Name (type1) :  <---- bright red input star
                               (type2 [default: foo) ...
                               (keywords) :Keyword Name (type3)


  :DEFAULT-TYPE   The presentation type of the argument value. Use
                  this with :default when the default value could
                  be interpreted more than one way.

  :PROVIDE-DEFAULT  When non-nil, the above options relating to
                    defaults are activated.

  :DISPLAY-DEFAULT   When non-nil, the default is printed in the
                     prompt. Default is t.
  :CONFIRM ..."
  (assert (keywordp option))
  (getf (schema-options schema) option))

(defun make-lambda-list-entry (entry)
  "Make an ordinary lambda list item corresponding to ENTRY, an
element of an extended argument list."
  (assert (and (not (null entry))
	       (listp entry)))
  (let ((name (schema-name entry)) 
	(default (schema-option entry :default)))
    (if (null default)
	name
	(list name default))))

(defun make-lambda-list (arglist)
  "Return an ordinary function lambda list corresponding to the
extended argument list ARGLIST."
  (cons '&optional (mapcar #'make-lambda-list-entry arglist)))

;;; Method dictionary 

(defvar *methods* nil)

(defun initialize-methods ()
  (setf *methods* (make-hash-table :test 'equal)))

(initialize-methods)

(defun make-method-id (prototype method)
  (let ((name (object-name (find-object prototype))))
    (assert (stringp name))
    (concatenate 'string (symbol-name method)
		 "%" (subseq name (1+ (position (character ":") name))))))

(defun find-method-id (prototype method &optional create)
  (assert prototype)
  (assert method)
  (let ((pointer prototype))
    (block searching
      (loop while pointer do
	(let ((id (make-method-id pointer method)))
	  (let ((result (gethash id *methods*)))
	    (if (or result create)
		(return-from searching id)
		(prog1 nil 
		  (setf pointer (find-super pointer))))))))))

(defun find-method-data (name method &optional no-error)
  (assert (hash-table-p *methods*))
  (let ((id (find-method-id name method no-error)))
    (let ((result (gethash id *methods*)))
      (if result 
	  (values-list result)
	  (unless no-error (error "Cannot find method: ~S" 
				  (list name method)))))))
  
(defun add-method-to-dictionary (prototype method arglist &optional options)
  (when (null *methods*)
    (initialize-methods))
  (let ((id (find-method-id prototype method :create)))
    (assert (stringp id))
    (setf (gethash id *methods*) (list arglist options prototype method))
    (values id arglist)))

(defun method-defun-symbol (method-symbol-name prototype-name)
  (intern (concatenate 'string
		       prototype-name "%" method-symbol-name )))

(defun method-options (name method &optional noerror)
  (multiple-value-bind (schema options)
      (find-method-data name method noerror)
    (declare (ignore schema))
    options))

(defun method-option (name method option)
  (getf (method-options name method)
	option))
	           
(defun method-schema (prototype method)
  (assert (hash-table-p *methods*))
  (let ((id (find-method-id prototype method)))
    (assert (stringp id))
    (let ((result (gethash id *methods*)))
      (when result
	(first result)))))

(defun method-argument-entry (prototype method index)
  (assert (integerp index))
  (let ((schema (method-schema prototype method)))
    (assert (< index (length schema)))
    (nth index schema)))
    
(defun method-argument-count (prototype method)
  (length (method-schema prototype method)))

(defun method-argument-type (prototype method index)
  (schema-type (method-argument-entry prototype method index)))

(defun method-argument-name (prototype method index)
  (schema-name (method-argument-entry prototype method index)))

(defun method-argument-options (prototype method index)
  (schema-options (method-argument-entry prototype method index)))

;;; Prototype dictionary

(defvar *prototypes* nil)

(defun initialize-prototypes ()
  (setf *prototypes* (make-hash-table :test 'equal)))

(initialize-prototypes)

(defun add-prototype (object)
  (when (null *prototypes*)
    (initialize-prototypes))
  (setf (gethash (object-name object)
		 *prototypes*)
	(find-object object)))

(defun find-prototype (name &optional noerror)
  (assert (hash-table-p *prototypes*))
  (or (gethash name *prototypes*)
      (unless noerror
	(error "Cannot find prototype named ~S" name))))

;;; UUID object dictionary

(defun make-uuid ()
  (uuid:print-bytes 
   nil 
   (uuid:make-v4-uuid)))
;; why doesn't v3 work? produces always same id
;; (uuid:make-v3-uuid uuid:+namespace-oid+ "blocky.io")))

(defvar *database* nil)

(defun initialize-database ()
  (setf *database* 
	(make-hash-table :test 'equal :size 8192)))

(initialize-database)

(defun add-object-to-database (object)
  (when (null *database*)
    (initialize-database))
  (setf (gethash (object-uuid object)
		 *database*)
	object))

(defun find-object-by-uuid (uuid &optional noerror)
  (or (gethash uuid *database*)
      (unless noerror
	(error "Cannot find object for uuid ~S" uuid))))

(defun purge-all-objects ()
  (flet ((count-entries () (+ (hash-table-count *database*)
		      (hash-table-count *prototypes*))))
    (let ((before-count (count-entries)))
      (message "Searching in ~A objects for externals..." before-count)
      (flet ((purge (id object)
	       (let ((name (object-name object)))
		 (unless (and (stringp name)
			      (search "BLOCKY:" name))
		   (remhash id *database*)))))
	(maphash #'purge *database*)
	(maphash #'purge *prototypes*)
	(let ((delta (- before-count (count-entries))))
	  (message "Removed ~A external objects." delta))))))
		  
;;; Finding any object by proto-name or UUID

(defun find-object (thing &optional no-error) 
  (when (not (null thing))
    (let ((result 
	    (etypecase thing
	      (symbol (symbol-value thing))
	      (string (or (find-object-by-uuid thing :noerror)
			  (find-prototype thing :noerror)))
	      (object thing))))
      (prog1 result
	(unless no-error
	  (assert result))))))
      
(defun find-super (object)
  (object-super (find-object object)))

(defun find-super-prototype-name (object)
  (let ((super (object-super (find-object object))))
    (when super (object-name (find-object super)))))

(defun object-eq (a b)
  (when (and a b)
    (eq (find-object a)
	(find-object b))))

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
    (string (intern (string-upcase S)))))

(defun merge-hashes (a b &optional predicate)
  (prog1 a
    (maphash #'(lambda (key value)
		 (when (or (null predicate)
			   (funcall predicate key))
		   (setf (gethash key a) value)))
	     b)))
	       
(defvar *make-prototype-id-package* nil)

(defun make-prototype-id (thing &optional (package (find-package :blocky)) create) 
  (let ((delimiter ":"))
    (if (null thing)
	(error "Cannot make a prototype ID for nil.")
	(string-upcase
	 (etypecase thing
	   (blocky:object (object-name thing))
	   (string 
	    (apply #'concatenate 'string 
		   (if (search delimiter thing)
		       (list thing)
		       (list (package-name package)
			     delimiter thing))))
	   (symbol 
	    ;; check for things that are already in COMMON-LISP package
	    (let ((thing-package (symbol-package thing)))
	      (let ((prefix (if (eq thing-package (find-package :common-lisp))
				"BLOCKY" ;; override if so
				(package-name thing-package))))
		(let ((name (concatenate 'string prefix delimiter (symbol-name thing))))
		  (let ((proto (find-prototype name :noerror)))
		    (if proto name
			(if create name
			    (concatenate 'string "BLOCKY" delimiter (symbol-name thing))))))))))))))
  

		      ;; 	   ;; is there a built-in prototype with this
		      ;; 	   ;; name, or is it something from CL-USER?
		      ;; (list 
		      ;;  (if (find-prototype project-candidate :noerror)
		      ;; 	   project-candidate
		      ;; 	   (concatenate 'string "BLOCKY:" 
		      ;; 			      (symbol-name thing))))))))))))


;;; Object data structure

;; Each object's "bookkeeping data" is stored in a structure. The
;; structure represents the object, and typically the programmer will
;; not need to access these structure fields.

(defstruct object
  ;; The most important features of an object are its "fields" or data
  ;; members. We use a hash table (or plist) to represent the field
  ;; collection. Methods are just function-valued fields. See
  ;; `field-value' and `set-field-value'.
  fields
  ;; Objects can inherit field values from a prototype object which
  ;; then influences the new object's behavior. We must store a link
  ;; to this "super" object so that `field-value' can obtain the
  ;; inherited field values.
  super
  ;; Objects may have names. A name is a string that identifies the
  ;; object. Named objects are "prototypes" from which other objects
  ;; may be created or "cloned".
  name
  ;; Here's the uuid string.
  uuid
  ;; The last few methods called are cached in this alist.
  cache)

(defun find-uuid (object)
  (when object
    (object-uuid (find-object object))))

(defun verify (thing)
  (assert (object-p (find-object thing))))

(defun blockyp (thing)
  (or (blocky:object-p thing)
      (and (stringp thing)
	   (find-object thing :no-error))))

;;; Fields

;; An object's field collection is either a hash table or plist. The
;; function `field-value' implements the chaining field lookups that
;; make behavior inheritance work in BLOCKY.

;; If a field value is not present in a given object's field
;; collection, the object's super is also checked for a value, and
;; then its super, and so on. This is how objects can inherit data
;; and behavior from prototypes. See `field-value'.

;; When you set the value of any field, the super's value is
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

(defun field-value (field thing &optional noerror)
  "Return the value of FIELD in the object THING.
If the FIELD has no value in THING, then the object's super is also
checked, and so on. If a value is found during these checks, it is
returned. If a value cannot be found, an error of type `no-such-field'
is signaled, unless NOERROR is non-nil; in that case,
`*lookup-failure*' is returned. See also `has-field'."
  (declare (optimize (speed 3))
	   (inline fref set-fref object-fields object-super))
  (let ((pointer (find-object thing))
	result found)
    ;; search the chain of objects for a field value.
    (loop while (and pointer (not found)) do
	 (setf result (fref (object-fields pointer) field))
	 (if (eq *lookup-failure* result)
	     ;; it's not here. search the super, if any.
	     (setf pointer (find-object (object-super pointer)))
	     ;; we found a value in this object.
	     (setf found t)))
    (if found result
	(if noerror 
	    *lookup-failure*   
	    (error 'no-such-field :field-name field :object thing)))))

(defun map-fields (function object)
  "For each field in OBJECT's field collection, the supplied FUNCTION
is invoked with the field-name and corresponding value as its two
arguments."
  (let ((fields (object-fields object)))
    (etypecase fields
      (hash-table (prog1 nil (maphash function fields)))
      (list (loop while fields 
		  do (funcall function 
			      (pop fields) 
			      (pop fields)))))))

(defun has-local-value (field thing)
  (let ((object (find-object thing)))
    (not (eq *lookup-failure* (fref (object-fields object) field)))))

(defun set-field-value (field thing value)
  "Set OBJECT's FIELD to VALUE.
The new value overrides any inherited value."
  (declare (inline set-fref object-fields find-object))
  (prog1 value
    (let ((object (find-object thing)))
      (multiple-value-bind (value fields)
	  (set-fref (object-fields object) field value)
	    ;; don't lose new list heads
	(prog1 value 
	  (when (listp fields)
	    (setf (object-fields object) fields)))))))
  
(defsetf field-value set-field-value)

(defun has-field (field object)
  "Return non-nil if FIELD has any value in OBJECT or its supers."
  (not (eq *lookup-failure* (field-value field object :noerror))))

(defun has-method (method object)
  (let ((val (field-value method object :no-error)))
    (and val (symbolp val) (fboundp val))))

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
;; for repeatedly used methods. (This is similar to the GNU Objective C
;; implementation.)

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

(defun method-cache-lookup (object method)
  (declare (optimize (speed 3))
	   (inline object-cache)
	   (type keyword method))
  (cdr (assoc method (object-cache object))))

;; Next comes the basic function for sending a message synchronously
;; and obtaining a return value.  

;; When a message cannot be delivered because no corresponding
;; function was found, BLOCKY attempts to re-send the message via the
;; object's `forward' method (if any).

;; An object's `forward' method should accept the method-key as the
;; first argument, and the arguments of the original message as the
;; remaining arguments.

;; We also want to be able to invoke the prototype (or "super's")
;; version of a method; for example during initialization, one might
;; wish to run the super's initializer as the first statement in the
;; child's.

(defun send (method thing &rest args)
  "Invoke the method identified by the keyword METHOD on the OBJECT with ARGS.
If the method is not found, attempt to forward the message."
  ;; See also `send-queue' and `send-super'
  (let ((object (find-object thing)))
    (when (not (object-p object))
      (error "Cannot send message to non-object: ~A. Did you forget the `self' argument?" object))
    ;; check the cache
    (let ((func (method-cache-lookup object method)))
      (if func
	  ;; cache hit. invoke the method and finish up.
	  (apply func object args)
	  ;; cache miss. look for a value.
	  (progn (setf func (field-value method object :noerror))
		 (if (not (eq *lookup-failure* func))
		     ;; store in local cache and then invoke the method
		     (progn 
		       (cache-method object method func)
		       (apply func object args))
		     ;; no such method. try forwarding
		     (if (has-field :forward object)
			 (apply (field-value :forward object)
				object method args)
			 (error (format nil "Could not invoke method ~S" method)))))))))

(define-condition null-next (error)
  ((method-key :initarg :message :accessor method-key)
   (object :initarg :object :accessor object))
  (:report (lambda (condition stream)
	     (format stream "Cannot find next method ~S for object ~S." 
		     (method-key condition)
		     (object condition)))))

(defun definition (method object)
  (block finding
    (loop while object do 
      (when (has-local-value method object)
	(return-from finding 
	  (values (field-value method object) object)))
      (setf object (find-super object)))))

(defun definer (method object)
  (multiple-value-bind (definition definer)
      (definition method object)
    (declare (ignore definition))
    definer))

(defun next-definer (method object)
  (if (has-local-value method object)
      (definer method (find-super object))
      (next-definer method (find-super object))))

(defun next-definition (method object)
  (field-value method (next-definer method object)))

(defvar *next-search-start* nil)

(defun send-super (method object &rest arguments)
  "Invoke next version of METHOD on OBJECT, passing ARGUMENTS.  We do
this by finding the current implementation (via slot lookup), then
finding the next implementation after that."
      (apply (next-definition method (find-object object))
	     object arguments))

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
;;   %foo
;;
;; instead of writing
;;
;;   (field-value :foo self)
;;
;; For example:
;;
;;   (princ %name)
;;   (setf %width 10)
;; 

(defvar *field-reference-prefix* "%")

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

;;; field references of the form %foo

(defun field-reference-p (form)
  "Return non-nil if FORM is a symbol like %foo."
  (if (symbolp form)
      (let* ((name (symbol-name form)))
	(string= (subseq name 0 1)
		 *field-reference-prefix*))))

(defun make-accessor-macrolet-clause (symbol)
  (list symbol
	`(field-value 
	  ,(make-keyword
	     ;; strip percent sign 
	    (subseq (symbol-name symbol) 1))
	  self)))

(defun make-accessor-flet-clause (symbol)
  `(,symbol (thing)
	    (field-value ,symbol thing)))

(defun transform-method-body (body)
  (let (symbols)
    ;; collect %foo symbols used in body
    (transform-tree #'field-reference-p
		    #'(lambda (symbol)
			(prog1 symbol
			  (push symbol symbols)))
		    body)
    ;; arrange for the substitution
    `(symbol-macrolet 
	 ,(mapcar #'make-accessor-macrolet-clause 
	   (remove-duplicates symbols))
       ,@body)))

;; (defun transform-field-reference (ref)
;;   "Change the symbol REF from %foo to (field-value :foo self)."
;;   (let ((name (symbol-name ref)))
;;     (list 'field-value 
;; 	  (make-keyword (subseq name 1))
;; 	  'self)))
       
;;; Definining methods

;; The `define-method' macro defined below is the main top-level facility
;; for adding methods to prototypes.

(defmacro define-method
    (method-specifier prototype-name arglist &body method-body)
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
  (assert method-specifier)
  (when (listp prototype-name)
    (error "Must specify a prototype name, found argument list instead. Did you forget the prototype name?"))
  ;; build the components of the defun
  (let ((method-name (etypecase method-specifier
		       (symbol method-specifier)
		       (list (first method-specifier))))
	(options (when (listp method-specifier)
		   (rest method-specifier))))
    (let* ((documentation (if (stringp (first method-body))
			      (first method-body)))
	   (body2 (remove-if #'stringp (transform-method-body method-body)))
	   ;; handle DECLARE forms when these appear first
	   (declaration (when (and (listp (first body2))
				   (eq 'declare (first (first body2))))
			  (first body2)))
	   (declaration2 (append '(declare (ignorable self))
				 (when declaration
				   ;; paste, skipping the declaration keyword
				   (rest declaration))))
	   (prototype-id (make-prototype-id prototype-name nil :create))
	   (field-name (make-keyword method-name))
	   (method-symbol-name (symbol-name method-name))
	   (method-symbol method-name) ;; fixme, unclear naming
	   (defun-symbol (method-defun-symbol 
			  method-symbol-name 
			  (symbol-name prototype-name)))
	   (queue-defun-symbol (intern (concatenate 'string
						    "QUEUE%"
						    method-symbol-name)))
	   (next-defun-symbol (intern (concatenate 'string
						   "SUPER%"
						   method-symbol-name)))
	   (method-lambda-list (if (is-extended-arglist arglist)
				   (make-lambda-list arglist)
				   arglist)))
      (let ((name (gensym)))
	`(let ((prototype (find-prototype ,prototype-id :noerror)))
	   ;; make sure it exists
	   (when (null prototype)
	     (error (format nil "Cannot define method ~A for nonexistent prototype ~A"
			    ',method-name ,prototype-id)))
	   ;; define the method's Lisp function
	   (defun ,defun-symbol (self ,@method-lambda-list)
	     ,@(if documentation (list documentation))
	     ,declaration2
	     ,(if declaration 
		  (rest body2)
		  body2))
	   ;; store the method's function in the prototype's field
	   (setf (field-value ,field-name prototype) ',defun-symbol)
	   ;; add this method to the method dictionary
	   (add-method-to-dictionary 
	    ,prototype-id
	    ,(make-keyword method-name)
	    ',arglist
	    ',options)
	   ;; define the other functions
	   (export ',defun-symbol)
	   (let ((,name ,(make-keyword method-name)))
	     (unless (fboundp ',method-symbol)
	       ;; tag the symbol as a method
	       (setf (get ',method-symbol 'is-method) t)
	       (defun ,method-symbol (self &rest args)
		 ,@(when documentation (list documentation))
		 (apply #'send ,name self args))
	       (export ',method-symbol)
	       ;; and for message queueing
	       (defun ,queue-defun-symbol (self &rest args)
		 ,@(when documentation (list documentation))
		 (apply #'send-queue ,name self args))
	       (export ',queue-defun-symbol)
	       ;; and for next-method calls.
	       (defun ,next-defun-symbol (self &rest args)
		 ,@(when documentation (list documentation))
		 (apply #'send-super ,name self args))
	       (export ',next-defun-symbol))))))))
  
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
%field-descriptors.

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
	
(defun make-field-accessor-forms (descriptor)
  (let* ((field-name (first descriptor))
	 (accessor-name (make-non-keyword 
			 ;; we use a double percent sign for the
			 ;; accessor functions, because otherwise the
			 ;; symbol macro definition for the "self"
			 ;; shortcut symbols would conflict.
			 (concatenate 'string "%" (symbol-name field-name)))))
    `((unless (fboundp ',accessor-name)
	(defun ,accessor-name (thing)
	  (field-value ,field-name thing))
	(export ',accessor-name)))))

(defmacro define-prototype (name
			    (&key super 
				  documentation
				  &allow-other-keys)
			    &body declarations)
  "Create a new object prototype (possibly based on another prototype).

NAME should be a symbol or string naming the prototype. You can create
objects with

 (clone :foo)

See also `clone'. 

The second argument is a property list of options for the
prototype. Valid keys are:

 :DOCUMENTATION     The documentation string for this prototype.
 :SUPER            The super prototype from which the new prototype will 
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
                    the value is inherited from the SUPER.
                    With \":initform nil\", the field is initialized 
                    with the value nil.
 :DOCUMENTATION     Documentation string for the field.
"
  (let* ((pre-descriptors (if (keywordp (first declarations))
			      (plist-to-descriptors declarations)
			      declarations))
	 (descriptors (mapcar #'transform-declaration 
			      pre-descriptors))
	 (prototype-id (make-prototype-id name (project-package-name) t ))
	 (field-initializer-body (delete nil (mapcar #'make-field-initializer 
						     descriptors))))
    `(let* ((super-sym ,(when super `(make-prototype-id ,super)))
	    (uuid (make-uuid))
	    (fields (compose-blank-fields ',descriptors))
	    (prototype (make-object :fields fields
				    :name ,prototype-id
				    :uuid uuid
				    :super (find-object super-sym))))
       ,@(mapcan #'make-field-accessor-forms descriptors)
       (setf (fref fields :field-descriptors) ',descriptors)
       (setf (fref fields :documentation) ,documentation)
       (setf (fref fields :initialize-fields) (function (lambda (self) 
						,@field-initializer-body)))
       (initialize-method-cache prototype)
       ;; set the default initforms
       (send :initialize-fields prototype)
       ;; the prototype's super may have an initialize method.
       ;; if so, we need to initialize the present prototype.
       (when (has-field :initialize prototype)
	 (send :initialize prototype))
       ;; now add it to the dictionaries
       (add-prototype prototype)
       (add-object-to-database prototype)
       ;; return the uuid and object
       (values uuid prototype))))

;;; Cloning and duplicating objects
  
(defmacro new (prototype-name &rest initargs)
  `(clone ,(make-prototype-id prototype-name)
	  ,@initargs))

(defun clone (prototype &rest initargs)
  "Create a new object from PROTOTYPE and pass INITARGS to the
initializer. The new object is created with fields for which INITFORMS
were specified (if any; see `define-prototype'); the INITFORMS are
evaluated, then any applicable initializer is triggered."
  ;; navigate to parent if a non-prototype is passed
  (when (not (object-name (find-object prototype)))
    (setf prototype (find-super prototype)))
  ;; now clone it
  (let ((uuid (make-uuid)))
    (let ((new-object (make-object :super (find-object prototype)
				   :uuid uuid
				   :fields (compose-blank-fields nil :list))))
    (prog1 uuid
      (initialize-method-cache new-object)
      (send :initialize-fields new-object)
      (if (has-field :initialize new-object)
	  (apply #'send :initialize new-object initargs))
      (add-object-to-database new-object)))))

;;; Serialization support

;; The functions SERIALIZE and DESERIALIZE convert
;; (almost abitrary) trees of Lisp objects (including BLOCKY objects)
;; into printable S-expressions for storing as plain text, and from
;; this printed representation back into living BLOCKY objects.

;; The method names :BEFORE-SERIALIZE and :AFTER-DESERIALIZE are
;; reserved for serialization use. :BEFORE-SERIALIZE, if such a method
;; is present, is invoked before serialization. The object being
;; serialized may use this hook to pre-process its
;; fields. :AFTER-DESERIALIZE is likewise invoked (if present) after
;; reading the object from disk, and is used to recover from
;; deserialization. The reserved field %EXCLUDED-FIELDS is a list of
;; field names (keyword symbols) which are not serialized; typically
;; these will be properly re-initialized by the :AFTER-DESERIALIZE
;; method.

(defconstant +object-type-key+ :%IOF1%OBJECT%)
(defconstant +hash-type-key+ :%IOF1%HASH%)

(defun serialize (object)
  "Convert a Lisp object a print-ready S-expression.
Invokes :BEFORE-SERIALIZE on BLOCKY objects whenever present. Any fields
named in the list %EXCLUDED-FIELDS of said object will be ignored."
  ;; use labels here so we can call #'serialize
  (with-standard-io-syntax
    (labels ((hash-to-list (hash)
	       (let (plist)
		 (labels ((collect (k v)
			    (push (serialize v) plist)
			    (push k plist)))
		   (maphash #'collect hash)
		   (cons +hash-type-key+ (cons (hash-table-test hash) plist))))))
      (typecase object 
	(hash-table (hash-to-list object))
	(cons 
	 (if (consp (cdr object)) ;; it's a list
	     (mapcar #'serialize object)
	     (cons (serialize (car object)) ;; it's a dotted pair
		   (serialize (cdr object)))))
	(string object)
	(vector (map 'vector #'serialize object))
	(object (let ((excluded-fields (when (has-field :excluded-fields object)
					 (field-value :excluded-fields object))))
		  ;; possibly prepare object for serialization.
		  (when (has-method :before-serialize object)
		    (send :before-serialize object))
		  (let ((super-name (object-name (object-super object)))
			(name (object-name object))
			(uuid (object-uuid object))
			(fields (object-fields object))
			(plist nil))
		    (assert (and super-name 
				 (null name)
				 (stringp uuid)
				 (listp fields)))
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
			    :super super-name
			    :uuid uuid
			    :fields plist)))))
	(otherwise object)))))
  
(defun deserialize (data)
  "Reconstruct Lisp objects (including BLOCKY-derived objects) from an
S-expression made by SERIALIZE. Invokes :AFTER-DESERIALIZE on BLOCKY
objects after reconstruction, wherever present."
  (with-standard-io-syntax 
    (cond 
      ;; handle BLOCKY objects
      ((and (listp data) (eq +object-type-key+ (first data)))
       (destructuring-bind (&key super uuid fields &allow-other-keys)
	   (rest data)
	 (let ((object (make-object :fields (mapcar #'deserialize fields)
				    :uuid uuid
				    :super (find-prototype super))))
	   (prog1 object
	     (initialize-method-cache object)
	     ;; possibly recover from deserialization
	     (when (has-method :after-deserialize object)
	       (send :after-deserialize object))))))
      ;; handle hashes
      ((and (listp data) (eq +hash-type-key+ (first data)))
       (destructuring-bind (test &rest plist)
	   (rest data)
	 (let ((hash (make-hash-table :test test)))
	   (loop while plist do
	     (let* ((key (pop plist))
		    (value (pop plist)))
	       (setf (gethash key hash) (deserialize value))))
	   hash)))
      ((consp data)
       (if (consp (cdr data))
	   ;; it's a list
	   (mapcar #'deserialize data)
	   ;; it's a dotted pair
	   (cons (deserialize (car data))
		 (deserialize (cdr data)))))
      ;; passthru
      (t data))))

(defun super%initialize (object &rest args)
  (apply #'send-super :initialize object args))

(defun queue%initialize (object &rest args)
  (apply #'send-queue :initialize object args))

(defun duplicate (original)
  (let ((duplicate (deserialize (serialize original))))
    (setf (object-uuid (find-object duplicate))
	  (make-uuid))
    (add-object-to-database (find-object duplicate))))

;;; Printing objects

(defun get-some-object-name (ob)
  (if (null ob)
      "NULL!!"
      (let ((object (find-object ob)))
	(if (object-p object)
	    (find-super-prototype-name object)
	    "Unknown object"))))

(defun object-address-string (ob)
  (subseq (find-uuid ob) 0 5))
  ;; (let ((string (with-output-to-string (s)
  ;; 		  (print-unreadable-object (ob s :identity t)))))
  ;;   (subseq string
  ;; 	    (1+ (search "{" string))
  ;; 	    (search "}" string))))

(defun print-iob (foo stream)
  (let ((object (find-object foo)))
    (format stream "#<IOB ~A ~A ~A>" 
	    (get-some-object-name object)
	    (object-address-string object)
	    (object-uuid object))))

(defun install-iob-printer ()
  (defmethod print-object ((foo blocky:object) stream)
    (print-iob foo stream)))

;;; Brute force debugging
    
(defun blocky-trace-all ()
  (do-external-symbols (sym (find-package :blocky))
    (when (fboundp sym)
      (ignore-errors (eval `(trace ,sym))))))

(defun blocky-untrace-all ()
  (do-external-symbols (sym (find-package :blocky))
    (when (fboundp sym)
      (ignore-errors (eval `(untrace ,sym))))))

;;; Printing a block

(defun print-block (B)
  (let (fields)
    (flet ((add-field (field value)
	     (push (list field value) fields)))
      (typecase B
	(blocky:object 
	 (let ((f2 (object-fields B)))
	   (etypecase f2
	     (hash-table (maphash #'add-field f2))
	     (list (setf fields f2)))
	   (cons (get-some-object-name B) fields)))
	(list (mapcar #'print-block B))
	(otherwise B)))))

(defun split-string-on-lines (string)
  (with-input-from-string (stream string)
    (loop for line = (read-line stream nil)
	  while line collect line)))

;;; prototypes.lisp ends here
