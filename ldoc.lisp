;;; ldoc.lisp --- extract and format documentation from lisp files

;; Copyright (C) 2009  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :iomacs)

;; todo show parent name if any

(defun clon-prototype-p (form)
  (when (symbolp form)
    (let* ((name (symbol-name form))
	   (len (length name)))
      (and (string= "=" (subseq name 0 1))
	   (string= "=" (subseq name (- len 1) len))))))

(defun clon-method-p (form)
  (when (symbolp form)
    (let* ((delimiter ">>")
	   (name (symbol-name form))
	   (len (length name))
	   (delimiter-pos (search delimiter name)))
      (when (numberp delimiter-pos)
	(values (subseq name 0 delimiter-pos)
		(subseq name (+ 2 delimiter-pos)))))))

(defun clon-parent-name (form)
  (when (and (symbolp form) (boundp form))
    (assert (symbol-value form))
    (let ((parent (object-parent (symbol-value form))))
      (when parent 
	(object-name parent)))))
    
(defun remove-delimiters (form)
  (let* ((name (symbol-name form))
	 (len (length name)))
    (subseq name 1 (- len 1))))

(defun document-symbol (symbol stream)
  "Documentation string."
  (let* ((type (if (clon-prototype-p symbol)
		   'variable
		   (if (fboundp symbol) 
		       (if (macro-function symbol) 
			   'function 
			   'function)
		       'variable)))
	 (type-name (if (clon-method-p symbol)
			"method" (if (clon-prototype-p symbol)
				     "prototype"
				     (if (fboundp symbol)
					 (if (macro-function symbol)
					     "macro" "function")
					 "variable"))))
	 (doc (if (clon-prototype-p symbol)
		  (field-value :documentation (symbol-value symbol))
		  (documentation symbol type)))
	 (name (if (clon-prototype-p symbol)
		   (remove-delimiters symbol)
		   (if (clon-method-p symbol)
		       (multiple-value-bind (method-name prototype-name) (clon-method-p symbol)
			 (format nil "~A [~A]" method-name prototype-name))
		       (symbol-name symbol))))
	 (args (when (fboundp symbol) (sb-introspect:function-lambda-list (fdefinition symbol)))))
    (format stream "** ~A (~A)" name type-name)
    (fresh-line stream)
    (when args
      (format stream "*** Arguments")
      (fresh-line stream)
      (format stream "~A" (if (clon-method-p symbol)
			      (cdr args) args))
      (fresh-line stream))
    (when doc
      (format stream "*** Documentation")
      (fresh-line stream)
      (format stream "~A" doc))
    (fresh-line stream)))

(defun do-heading (name stream)
  (fresh-line stream)
  (format stream "* ~A" name)
  (fresh-line stream))

(defun document-package (package-name stream &optional preamble-file)
  (let (syms protos methods proto-hashes preamble-lines)
    (when preamble-file 
      (setf preamble-lines (with-open-file (file preamble-file
						 :direction :input
						 :if-does-not-exist nil)
			     (loop for line = (read-line file nil)
				   while line collect line))))
    (do-external-symbols (sym package-name)
      (when (< 3 (length (symbol-name sym)))
	(push sym syms)))
    (setf syms (sort syms #'string<))
    ;; print preamble
    (dolist (line preamble-lines)
      (format stream "~A" line)
      (fresh-line stream))
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
;; (document-package-to-file :iomacs #P"/home/dto/notebook/iomacs-reference.org" #P"/home/dto/iomacs/doc-preamble.org")
;; (document-package :iomacs t #P"/home/dto/iomacs/doc-preamble.org")

;;; ldoc.lisp ends here
