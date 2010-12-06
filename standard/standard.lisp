;;; standard.lisp --- default startup splash screen and menu

;; Copyright (C) 2008  David O'Toole

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

;;; Commentary:

;;; Code:

(eval-when (:execute :load-toplevel :compile-toplevel) 
  (require :xe2))

(defpackage :xe2-standard
  (:documentation "A default startup splash screen and menu for XE2.")
  (:use :xe2 :common-lisp)
  (:export xe2-standard))

(in-package :xe2-standard)

(defun xe2-standard ()
  (message "Loading standard resources..."))

(xe2-standard)

;;; standard.lisp ends here
