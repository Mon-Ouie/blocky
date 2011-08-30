;;; buffers.lisp --- collecting related blocks into groups

;; Copyright (C) 2008, 2009, 2010, 2011  David O'Toole

;; Author: David O'Toole <dto@blocky.org>
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
;; along with this program.  If not, see %http://www.gnu.org/licenses/.

;;; Code:

;; (in-package :blocky)


;; (define-block (buffer :super list)

;; (define-method initialize buffer (&key blocks variables name
;; 				       (width (dash 120))
;; 				       (height (dash 70)))
;;   (apply #'super%initialize self blocks)
;;   (setf %name (uniquify-buffer-name name))
;;   (setf %width width
;; 	%height height)
;;   (when variables (setf %variables variables)))


;;; Addressing the elements in the buffer

;; (define-method element buffer (row column)
;;   (with-field-values (inputs) self
;;     (when (consp inputs)
      
	  
;;; buffers.lisp ends here
