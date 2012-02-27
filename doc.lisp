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

(defun methodp (symbol)
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
  (let* ((symbol (intern (make-method-id prototype method)))
	 (doc (documentation symbol 'function)))
    (when doc
      (multiple-value-bind (arglist options prototype method)
	  (find-method-data prototype method)
	(heading 2 (format nil "~A (method)" (make-non-keyword method))
		 stream)
	(heading 3 "Arguments" stream)
	(dolist (arg arglist)
	  (document-extended-argument arg stream)
	  (fresh-line stream))
	(heading 3 "Documentation" stream)
	(format stream "~A" doc)
	(fresh-line stream)))))
  
(defun document-function (symbol stream)
  (let ((doc (documentation symbol 'function)))
    (when doc
      (let ((type (cond 
		    ((macro-function symbol) 'macro)
		    ((fboundp symbol) 'function))))
	(heading 2 (format nil "~A (~A)" symbol (string-downcase (symbol-name type)))
		 stream)
	(heading 3 "Arguments" stream)
	(format stream "~S" (sb-introspect:function-lambda-list (fdefinition symbol)))
	(heading 3 "Documentation" stream)
	(format stream "~A" doc)
	(fresh-line stream)))))

(defun document-variable (symbol stream)
  (heading 2 (format nil "~A (variable)" symbol)
	   stream)
  (heading 3 "Documentation" stream)
  (format stream "~A" (documentation symbol 'variable))
  (fresh-line stream))

(defun find-prototype-methods (prototype)
  (let ((proto (find-object prototype))
	(methods '()))
    (labels ((extract (k v)
	       (when (and (symbolp v)
			  (fboundp v))
		 (push k methods))))
      (maphash #'extract 
	       (object-fields proto)))
    (sort methods #'string<)))

(defun document-prototype (name stream)
  (heading 1 (format nil "~A (prototype)" (subseq name (1+ (position (character ":") name))))
	   stream)
  (let ((proto (gethash name *prototypes*)))
    (when proto
      (let* ((field-descriptors 
	       (field-value :field-descriptors proto))
	     (parent-name (find-super-prototype-name proto))
	     (methods (find-prototype-methods proto)))
	(when parent-name
	  (heading 3 (format nil "Parent name: ~A" parent-name) stream))
	(let ((doc (field-value :documentation proto)))
	  (when (stringp doc)
	    (heading 2 "Documentation" stream)
	    (format stream "~A" doc)
	    (fresh-line stream)))
	(when field-descriptors
	  (heading 2 "Fields" stream)
	  (dolist (d field-descriptors)
	    (fresh-line stream)
	    (destructuring-bind (name (&key documentation initform &allow-other-keys)) d
	      (when (and name documentation)
		(format stream "*** ~A (field)" name)
		(heading 4 "Documentation" stream)
		(format stream "~A" documentation)
		(fresh-line stream)
		(when initform 
		  (heading 4 "Initialization form" stream)
		  (format stream "~S" initform))))))
	(fresh-line stream)
	;; methods
	(message "Documenting ~S methods..." (length methods))
	(dolist (m methods)
	  (document-method name m stream))
	methods))))

(defun preamble-file-lines (preamble-file)
  (with-open-file (file preamble-file
			:direction :input
			:if-does-not-exist nil)
    (let* ((len (file-length file))
           (string (make-string len)))
      (read-sequence string file)
      (split-string-on-lines string))))
    ;; (loop for line = (read-line file nil)
    ;; 	  while line collect line)))

(defun document-package (package-name &key (stream t) preamble-file title)
  (let ((package (find-package package-name))
	symbols functions variables) 
    ;; header
    (when title 
      (format stream "#+TITLE: ~A" title)
      (fresh-line stream))
    (format stream "#+OPTIONS: toc:2 *:nil")
    (fresh-line stream)
    (do-external-symbols (symbol package)
      (push symbol symbols))
    ;; remove method symbols and method defun symbols
    (setf symbols 
	  (sort 
	   (remove-if #'(lambda (s)
			  (or (search "%" (symbol-name s))
			      (methodp s)))
		      symbols)
	   #'string<))
    ;; print preamble
    (let ((preamble-lines 
	    (when preamble-file
	      (preamble-file-lines preamble-file))))
      (when preamble-file 
	(dolist (line preamble-lines)
	  (format stream "~A " line)
	  (fresh-line stream))))
    ;; document prototypes and their respective methods
    (let (prototypes)
      (loop for prototype being the hash-keys in *prototypes* do
	    (push prototype prototypes))
      (setf prototypes 
	    (mapcar #'(lambda (name)
			(subseq name (1+ (position (character ":") name))))
		    prototypes))
      (dolist (prototype (sort prototypes #'string<))
	(let ((methods (document-prototype (concatenate 'string "BLOCKY:" prototype) stream)))
	  (dolist (method methods)
	    (setf symbols (remove (make-non-keyword method) symbols :test 'eq)))
	  )))
    ;; document syms
    (heading 1 "Functions, Macros, and Variables" stream)
    (dolist (sym symbols)
      (if (fboundp sym)
	  (document-function sym stream)
	  (document-variable sym stream)))))

(defun document-package-to-file (package-name output-file &key preamble-file title)
  (with-open-file (stream output-file :direction :output :if-exists :supersede)
    (document-package package-name :title title :stream stream :preamble-file preamble-file)))

;; (document-package-to-file :blocky #P"/home/dto/ioweb/reference.org" :title "Blocky reference manual" :preamble-file #P"~/blocky/guide.org")

;;; doc.lisp ends here
