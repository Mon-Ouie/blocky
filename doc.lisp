;;; doc.lisp --- extract blocky docs into orgmode format

;; Copyright (C) 2009-2011  David O'Toole

;; Author: David O'Toole dto@ioforms.org
;; Keywords: lisp, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see ^http://www.gnu.org/licenses/.

(in-package :blocky)

(defun is-method (symbol)
  (and (fboundp symbol)
       (get 'symbol 'is-method)))

(defun document-extended-argument (entry stream)
  (format stream "~A" entry))

(defun heading (level text stream)
  (fresh-line stream) 
  (format stream "~A ~A" 
   (make-string level :initial-element (character "*"))
   text)
  (fresh-line stream))

(defun document-method (prototype method stream)
  (let ((symbol (intern (make-method-id prototype method))))
    (destructuring-bind (arglist options prototype method)
	(find-method-data prototype method)
      (heading 2 (format nil "~A /(method)/" (make-non-keyword method))
	       stream)
      (heading 3 "Arguments" stream)
      (dolist (arg arglist)
	(document-extended-argument arg stream)
	(fresh-line stream))
      (heading 3 "Documentation" stream)
      (format stream "~A" (documentation symbol 'function))
      (fresh-line stream))))

(defun document-function (symbol stream)
  (heading 2 (format nil "** ~A /(function)/" symbol)
	   stream)
  (heading 3 "Arguments" stream)
  (format stream "~S" (sb-introspect:function-lambda-list (fdefinition symbol)))
  (heading 3 "Documentation" stream)
  (format stream "~A" (documentation symbol 'function))
  (fresh-line stream))

(defun document-prototype (name stream)
  (heading 1 (format nil "~A /(prototype)/" name)
	   stream))






    (do-external-symbols (sym package-name)
      (when (< 3 (length (symbol-name sym)))
	(push sym syms)))
    (setf syms (sort syms #'string<))

    ;; print preamble
    (dolist (line preamble-lines)
      (format stream "~A" line)
      (fresh-line stream))

(when preamble-file 
      (setf preamble-lines (with-open-file (file preamble-file
						 :direction :input
						 :if-does-not-exist nil)
			     (loop for line = (read-line file nil)
				   while line collect line))))


    ;; sort symbols
    (setf syms (remove-if #'(lambda (s)
    			       (when (clon-prototype-p s)
    				 (push s protos)))
    			   syms))
    (setf syms (remove-if #'(lambda (s)
    			       (when (clon-method-p s)
    				 (push s methods)))
    			   syms))
    ;; document prototypes
    (setf protos (nreverse protos))
    (dolist (p protos)
      (let (pile
	    (field-descriptors (when (and (clon-prototype-p p) (boundp p))
				 (field-value :field-descriptors (symbol-value p))))
	    (parent-name (clon-parent-name p)))
	(dolist (m methods)
	  (multiple-value-bind (method-name proto-name) 
	      (clon-method-p m)
	    (fresh-line t)
	    (when (string= proto-name (remove-delimiters p))
	      (push m pile))))
	(setf pile (sort pile #'(lambda (s z)
				  (multiple-value-bind (method-name1 proto-name1)
				      (clon-method-p s)
				    (multiple-value-bind (method-name2 proto-name2)
					(clon-method-p z)
				      (string> method-name1 method-name2))))))
	(when pile
	  (do-heading (format nil "~A (prototype)" (symbol-name p)) stream)
	  (when parent-name
	    (fresh-line stream)
	    (format stream "** Parent prototype")
	    (fresh-line stream)
	    (format stream ": ~A" parent-name)
	    (fresh-line stream))
	  (let ((doc (field-value :documentation (symbol-value p))))
	    (when (stringp doc)
	      (format stream "** Documentation")
	      (fresh-line stream)
	      (format stream "~A" doc)
	      (fresh-line stream)))
	  (when field-descriptors
	    (format stream "*** Fields")
	    (fresh-line stream)
	    (dolist (d field-descriptors)
	      (fresh-line stream)
	      (destructuring-bind (name (&key documentation initform &allow-other-keys)) d
		(when name (format stream "**** ~A (field)" name))
		(when documentation 
		  (fresh-line stream)
		  (format stream "***** Documentation")
		  (fresh-line stream)
		  (format stream "~A" documentation)
		  (fresh-line stream)
		  (when initform 
		    (format stream "***** Initialization form")
		    (fresh-line stream)
		    (format stream ": ~S" initform))))))
	  (fresh-line stream)
	  (dolist (proto (reverse pile))
	    (document-symbol proto stream)))))
    ;; document syms
    (do-heading "Symbols" stream)
    (dolist (sym syms)
      (document-symbol sym stream))))

(defun document-package-to-file (package-name output-file &optional preamble-file)
  (with-open-file (stream output-file :direction :output :if-exists :supersede)
    (document-package package-name stream preamble-file)))
			    
;; (document-package :clon t)
;; (document-package-to-file :ioforms #P"/home/dto/notebook/ioforms-reference.org" #P"/home/dto/ioforms/doc-preamble.org")
;; (document-package :ioforms t #P"/home/dto/ioforms/doc-preamble.org")

;;; doc.lisp ends here
