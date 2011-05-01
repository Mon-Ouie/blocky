;;; browser.lisp --- object discovery and interaction widget menu thing

;; Copyright (C) 2008, 2011  David O'Toole

;; Author: David O'Toole ^dto@gnu.org
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
;; along with this program.  If not, see ^http://www.gnu.org/licenses/.

;;; Commentary:

;; Browsable items should provide the following fields:

;;  :tile  --- String image icon name
;;  :name  --- String object name

;; You must also define some methods:

;;  (is-disabled item) should return non-nil when the menu item is to
;;  be grayed out.

;;  (open item) should return:
;; 
;;  (values COMMAND-STRING MENU-SPEC)

;;; Code:

(in-package :ioforms)

(define-prototype menu-item (:parent =widget=)
  (tile :initform ".asterisk")
  (name :initform "<blank menu item>")
  (key :initform nil)
  (description :initform "<blank menu item description>")
  (sub-menu :initform nil)
  (command-string :initform ""))

(define-method initialize menu-item (&optional item-spec)
  (when item-spec
    (destructuring-bind (command-string &key key description sub-menu name
					&allow-other-keys) item-spec
      (setf ^key key 
	    ^description description
	    ^name name
	    ^sub-menu sub-menu
	    ^command-string command-string))))

;; (define-method is-disabled item

(define-method open menu-item ()
  (values ^command-string 
	  ^sub-menu))

;;; browser menu

(define-prototype browser (:parent =formatter=)
  (collection :documentation "The vector of browsable CLON objects being browsed.")
  (cursor :initform 0
	  :documentation "The array index of the currently selected object.")
  (visible :initform t)
  (history :documentation "Recently browsed collections.")
  (prompt :documentation "Prompt to receive command messages."))

(define-method set-prompt browser (prompt)
  (setf ^prompt prompt))

(define-method cursor-next browser ()
  (when (array-in-bounds-p ^collection (+ 1 ^cursor))
    (incf ^cursor)))

(define-method cursor-previous browser ()
  (when (array-in-bounds-p ^collection (- ^cursor 1))
    (decf ^cursor)))

(define-method cursor-item browser ()
  (aref ^collection ^cursor))

(define-method follow browser ()
  (let* ((item (cursor-item self)))
    (push ^collection ^history)
    (multiple-value-bind (result sub-menu) (open item)
      (cond ((and (vectorp result) (every #'object-p result))
	     (set-collection self result))
	    ((stringp result)
	     (assert ^prompt)
	     (insert ^prompt result)))
      ;; ((null result)
      ;;  (hide self)))
      (when (and (not (null sub-menu)) (boundp sub-menu))
	(message "HGEY!")
	(set-collection-from-menu-spec self (symbol-value sub-menu))))))
	    	     
(define-method back browser ()
  (setf ^collection (pop ^history)))

(define-method print-object browser (object &optional selected-p)
  "Print the OBJECT in the browser as a new formatted line.
When SELECTED-P is non-nil, draw the highlighted (or otherwise
visually distinguished) version of the line."
  (if (null object)
      (println self " (EMPTY) " :foreground ".gray20")
      (progn 
	(let ((tile (field-value :tile object))
	      (label (or (field-value :name object)
			 (field-value :description object))))
	  (if selected-p
	      (print self ">" :foreground ".yellow" :background ".purple")
	      (print self " "))
	  (print self " ")
	  (print self nil :image tile)
	  (print self " ")
	  (println self label)))))

(define-method update browser ()
  (let ((collection ^collection)
	(cursor ^cursor))
    (delete-all-lines self)
    (dotimes (n (length collection))
      (print-object self (aref collection n) (= cursor n)))))

(define-method set-collection browser (collection)
  (setf ^collection collection)
  (setf ^cursor 0))

(define-method set-collection-from-menu-spec browser (menu-spec)
  (let ((c nil))
    (dolist (m menu-spec)
      (push (clone =menu-item= m) c))
    (set-collection self (coerce (nreverse c) 'vector))))

(define-method initialize browser ()
  (parent>>initialize self)
  (bind-key-to-method self "UP" nil :cursor-previous)
  (bind-key-to-method self "DOWN" nil :cursor-next)
  (bind-key-to-method self "LEFT" nil :back)
  (bind-key-to-method self "TAB" nil :toggle-visible)
  (bind-key-to-method self "SPACE" nil :follow))

;;; Directional browser

(defparameter *choose-direction-menu* 
  '((":east ." :name "East")
    (":northeast ." :name "Northeast")
    (":southeast ." :name "Southeast")
    (":west ." :name "West")
    (":northwest ." :name "Northwest")
    (":southwest ." :name "Southwest")
    (":north ." :name "North")
    (":south ." :name "South")))

;;; A container browser with numbered slots 

;;; An equipment browser

;; (define-prototype equipment (:parent =browser=))

;; (define-method print-object equipment (object &optional selected-p)
;;   (declare (ignore selected-p))
;;   (let* ((tile (field-value :tile object))
;; 	 (name (field-value :name object))
;; 	 (equipment (field-value :equipment object))
;; 	 (equipment-slots (field-value :equipment-slots object))
;; 	 (fill-width (apply #'max 5 (mapcar #'(lambda (s) 
;; 					      (length (symbol-name s)))
;; 					  equipment-slots)))
;; 	 item)
;;     (print self "Equipment for: ")
;;     (print self name)
;;     (print self nil :image tile)
;;     (space self)
;;     (println self name)
;;     (dolist (slot equipment-slots)
;;       (setf item (equipment-slot object slot))
;;       (print self (format nil (format nil "~~~dS " fill-width) slot))
;;       (when item
;; 	(print self nil :image (field-value :tile item))
;; 	(space self)
;; 	(print self (field-value :name item)))
;;       (newline self))
;;     (newline self)))
    
;;; browser.lisp ends here
