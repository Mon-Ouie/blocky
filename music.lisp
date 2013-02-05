;;; music --- csound SFX and process-music composition system for Blocky

;; Copyright (C) 2007, 2013 David O'Toole

;; Author: David O'Toole <dto@ioforms.org>
;; Keywords: multimedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(in-package :blocky)

(defvar *samples* nil "Hash-table mapping *samples* to
integers.  Because function tables are numbered in csound, we
need to keep track of which *samples* go with which table numbers.")

(defvar *next-sample-number* 1)

(defun import-sample (sample)
  "Load the sample SAMPLE from the current project. Return the
assigned csound sample number."
  ;;
  ;; create hash table if necessary
  (when (null *samples*)
    (setf *samples* (make-hash-table :test 'equal)))
  (setf (gethash sample *samples*) *next-sample-number*)
  (prog1 *next-sample-number* 
    (incf *next-sample-number*)))

(defun sample-number (sample)
  "Get the function-table number from the sample named SAMPLE. If
SAMPLE has not yet been assigned a number, assign it."
  (let ((number (gethash sample *samples*)))
    (if (numberp number)
	number
      (import-sample sample))))

(defun directory-samples (dir)
  (remove-if-not #'sample-filename-p 
		 (directory-files dir)))

(defun project-samples ()
  (directory-samples (project-directory)))

(defun load-all-samples (project)
  (setf *next-sample-number* 1)
  (setf *samples* nil)
  (mapcar #'import-sample (project-samples)))

;;; Generators

(defvar *activity-generators* nil "Hash table mapping
activity generator names to activity generators.")

(defmacro define-activity (name activity-params event-params docstring &rest body)
  "Define a new activity generator. Calls are of the form

   (defactivity NAME ACTIVITY-PARAMS EVENT-PARAMS DOCSTRING BODY...)

where ACTIVITY-PARAMS and EVENT-PARAMS are of the form

   ((:param1 :tooltip TOOLTIP-STRING :docstring DOCSTRING
                       :column-header HEADER-STRING)
    (:param2 PLIST...)
    ...)

The ACTIVITY-PARAMS are arguments to the activity generator
function. They may alter the generated activity. The EVENT-PARAMS
are used during performance."  
  (labels ((build-param-list (specs) 
			     `(&key 
			       ,@(mapcar (lambda (s)
					   (intern (substring (symbol-name s) 1)))
					 (mapcar 'car specs))))
	   (format-param-docs (specs)
			      (concat "\n\nParameters: \n"
				      (mapconcat (lambda (spec)
						   (destructuring-bind 
						       (&key tooltip docstring &allow-other-keys) 
						       (cdr spec)
						     (format " %S ---  %s\n%s" 
							     (car spec)
							     tooltip docstring)))
						 specs
						 "\n"))))
    (let ((function-name (intern (concat "ligeti::" (symbol-name name)))))
      `(progn
	 (defun* ,function-name
	     ,(build-param-list activity-params)
	   ,(format-param-docs activity-params)
	   ,@body)
	 (when (null *activity-generators*)
	   (setf *activity-generators* (make-hash-table :test 'equal)))
	 (puthash ,(symbol-name name) 
		  (list ',function-name ',event-params)
		  *activity-generators*)))))
  
;; (@* "Activities")

(defstruct activity instrument-number instrument-spec event-params)
  
(defvar *activities* nil "Hash table mapping activity names to activities.")

(defvar *next-activity-number* 1)

(defun make-activity (name generator-name &rest activity-params)
  (let ((entry (gethash generator-name *activity-generators*)))
    (if entry 
	(destructuring-bind (generator event-params) entry 
	  (puthash name
		   (make-activity :instrument-spec (apply generator activity-params)
					 :instrument-number *next-activity-number*
					 :event-params event-params)
		   *activities*)
	  (prog1 *next-activity-number*
	    (incf *next-activity-number*)))
      (error "no such activity generator: %s" generator-name))))
	
;; (@* "Events")
;; 
;; For now, events are lists of the form (start-time duration p4 p5 p6
;; ...)  The start-time comes from the cell-mode row number.  The
;; duration is determined by the number of rows between the event and
;; the next event (or the next noteoff.)

(defun make-event (start-time duration &rest p-fields)
  "Create an event. This is trivial now but the format may change in the future."
  (append (list instrument-number start-time duration) p-fields))

;; (@* "*Sequences*")
;; 
;; *Sequences* gather a bundle of events together to be sent to a
;; particular activity.

(defstruct sequence name activity event-params events)

(defvar *sequences* nil "Hash table mapping sequence names to *sequences*.")

(defun sequence-file (sequence-name)
  (project-file (concat sequence-name ".ligeti.cell")))

(defun make-sequence (&rest args)
  (let* ((sequence (apply 'make-sequence args))
	 (sequence-name (sequence-name sequence))
	 (sequence-file (sequence-file sequence-name)))
    (puthash sequence-name sequence *sequences*)
    (with-temp-buffer 
      (linkd-insert-datablock-template (sequence-activity sequence))
      (write-region (point-min) (point-max) sequence-file)
    (find-file sequence-file))))



(defun to-csound ()
  (interactive)
  (let (sexps)
    (labels ((--> (&rest S) (mapcar (lambda (s)
				      (push s sexps))
				    S)))
      ;;
      ;; *activities* --> instruments
      (maphash (lambda (name spec)
		 (destructuring-bind 
		     (&key instrument-spec instrument-number &allow-other-keys) spec
		   (--> :instr instrument-number `(apply 'list ',instrument-spec))))
	       *activities*)
      ;;
      ;; *samples* --> ftables
      (--> :score)
      (maphash (lambda (sample table-number)
		 (--> `(insert ,(format "f %d 0 0 1 \"%s\" 0 0 0\n" 
					table-number (sample-file sample)))))
	       *samples*))

    ;;
    ;; put it all together
    (cons 'csound-composition (nreverse sexps))))


(defun initialize-activities ()
  (setf *activity-generators* (make-hash-table :test 'equal))
  ;;
  (setf *activities* (make-hash-table :test 'equal))
  (setf *next-activity-number* 1)
  ;;
  (setf *samples* (make-hash-table :test 'equal))
  (setf *next-sample-number* 1)
  ;;
  (setf *sequences* (make-hash-table :test 'equal))
  (setf *next-sequence-number* 1))

		     


;;; music.lisp ends here
