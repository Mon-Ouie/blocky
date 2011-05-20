;;; system.lisp --- a block to manage the console.lisp API for you

;; Copyright (C) 2010, 2011  David O'Toole

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

;;; Code:

(in-package :ioforms)

(defvar *system* nil)

;; (defblock system
;;   (type :initform :system)
;;   (shell :initform nil)
;;   (running :initform nil))

;; (define-method get-blocks system ()
;;   ^children)

;; (define-method count-blocks system ()
;;   (with-fields (children) self
;;     (apply #'+ (mapcar #'/count children))))

;; (define-method start system ()
;;   (setf ^running t))

;; (define-method stop system ()
;;   (setf ^running nil))

;; (define-method tick system (&rest args)
;;   (with-fields (running children shell) self
;;     (when running
;;       (dolist (block children)
;; 	(apply #'tick block args)))))

;; (define-method resize system (&key height width))

;; (define-method initialize system (&rest args)
  ;; (enable-classic-key-repeat 100 100)
  ;; (labels ((do-resize ()
  ;; 	     (resize *system* 
  ;; 		     :width *screen-width* 
  ;; 		     :height *screen-height*)))
  ;;   (add-hook '*resize-hook* #'do-resize))
  ;; (ioforms:install-blocks self))

;; (define-method new system (project)
;;   (assert (stringp project))
;;   (let ((dir (find-project-path project)))
;;     (when (directory-is-project-p dir)
;;       (error "Project ~S aready exists." project))
;;     (make-directory-maybe dir)
;;     (open-project self project)))
	
;; (define-method run system ()
;;   (run-project *project*)
;;   (run-main-loop))

;; (define-method open system (project)
;;   (open-project project))

;; (define-method save system ()
;;   (save-project))

;; (define-method save-everything system ()
;;   (save-project :force))

;; (define-method ticks system ()
;;   (get-ticks))

;; (define-method shutdown system ()
;;   ;; TODO destroy textures
;;   (ioforms:quit t))

;; (define-method draw system (destination)
;;   FIXME)

;;; system.lisp ends here

