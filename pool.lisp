;;; pool.lisp --- simple data pool for reusing lisp structs and other data

;; Copyright (C) 2009  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: lisp, data, tools, etc

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

;;; Commentary:

;; Several parts of RLX frequently re-use structures and other
;; data---such as pathfinding nodes and geometric points. This file
;; defines a "pool" for reusable structures with the following
;; features:

;;  - Array-based with integer indices and lazy allocation of elements
;;  - Removal of an element does not delete the structure stored at 
;;    that array location. Instead we just mark the array location as
;;    "inactive" (see `pool-active-at-p')
;;  - Subsequent newly activated nodes re-use the data structure as-is

;;; Code:

(in-package :rlx)

;; A "pool element" wraps a reusable structure with a flag called
;; `active-p' specifying whether that element's structure is in
;; use. See `pool-active-at-p', `pool-allocate-position',
;; `pool-activate-position', etc.

(defstruct pool-element 
  active-p ;; Non-nil when data structure is created and in use. 
  data)

(defstruct pool 
  ;; The following are required slots. You must specify a value for
  ;; them when using `create-pool' (see below).
  constructor ;; Function that creates a new element.
  growth-rate ;; Real size multiplier when growing array.
  ;; Read-only slots. 
  vector ;; The storage array for the data.
  size ;; Number of positions in <vector>
  count ;; Number of data-occupied positions in <vector>
  )

(defun create-pool (&key constructor growth-rate initial-size)
  "Create a new pool object whose element data are of the type
returned by CONSTRUCTOR. GROWTH-RATE is the resize factor used when
the pool runs out of room. INITIAL-SIZE is the initial size of the
vector."
  (assert (functionp constructor))
  (assert (and (numberp growth-rate) (integerp initial-size)))
  (let ((vector (make-array initial-size 
				 :element-type 'pool-element 
				 :adjustable t)))
    (make-pool :constructor constructor
	       :growth-rate growth-rate 
	       :size initial-size
	       :vector vector
	       :count 0)))

(defun pool-active-at-p (pool n)
  "Returns non-nil when position N in pool POOL is in use."
  (let* ((vector (pool-vector pool))
	 (element (aref vector n)))
    (when element
      (pool-element-active-p element))))

(defun pool-allocate-position (pool n)
  "Allocate a data structure for position N in the POOL."
  (setf (pref pool n) 
	(make-pool-element :data (funcall (pool-constructor pool)))))

(defun pool-activate-position (pool n)
  "Mark position N as active in POOL, and allocate the position if
necessary."
  (when (not (pool-element-p (pref pool n)))
    (pool-allocate-position pool n))
  (setf (pool-element-active-p (pref pool n)) t))

(defun pool-deactivate-position (pool n)
  "Mark position N as inactive."
  (assert (pool-element-p (pref pool n)))
  (setf (pool-element-active-p (pref pool n)) nil))
  
(defun pref (pool n)
  "Return the POOL element data structure at position N.
This is an internal function; use `pref-data' to obtain the user
data items stored in the pool."
  (aref (pool-vector pool) n))

(defun set-pref (pool n value)
  "Set the POOL element data structure at position N. This is an
internal function; use `set-pref-data' to store user data items stored
in the pool."
  (setf (aref (pool-vector pool) n) value))

(defsetf pref set-pref)

(defun pref-data (pool n)
  "Get the data at position N in POOL."
  (when (pool-active-at-p pool n)
    (pool-element-data (pref pool n))))

(defun set-pref-data (pool n data)
  "Set the data at position N in POOL."
  (let ((element (pref pool n)))
    (when (null (pref pool n))
      (pool-activate-position pool n))
    (setf (pool-element-data (pref pool n)) data)
    (setf (pool-element-active-p (pref pool n)) t)))

(defsetf pref-data set-pref-data)

;; (defun pool-resize 
;;; pool.lisp ends here
