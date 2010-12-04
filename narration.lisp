;;; narration.lisp --- log and narrate gameplay

;; Copyright (C) 2009  David O'Toole

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

;;; Code:

(in-package :iomacs)

;;; Verbosity determines when a message is important enough to output.

(defvar *default-message-verbosities*
  '(:move 3
    :move-cell 3
    :fire 2
    :drop 2
    :drop-cell 3
    :impel 3
    :step 3
    :die 1
    :delete-from-world 3
    :damage 1
    :expend-action-points 3
    :expend-default-action-points 3
    :stat-effect 1
    :damage 1
    :narrate nil 
    :narrateln nil
    :explode 1
    :print-object-tag nil
    :newline nil
    :take 1
    :print-separator nil))

(defvar *message-verbosities* *default-message-verbosities*
  "Property list mapping message keywords to the integers from 1-3.
A message outputs if its verbosity level is less than or equal to the
narration window's verbosity level. Level 1 is normal gameplay output,
while levels 2 and 3 offer increading debug information. Values of nil
and t mean to never (and always, respectively) output, regardless of
verbosity level.")

(defun set-message-verbosities (plist &optional (include-default t))
  (setf *message-verbosities* (append plist 
				      (when include-default 
					*default-message-verbosities*))))

(defun add-message-verbosities (plist)
  (setf *message-verbosities*
	(append plist *message-verbosities*)))

;;; Translating actions into natural language.

(defvar *default-action-translations*
  '(:move "Moves"
    :move-cell "Moves (via World)"
    :fire "Fires"
    :drop "Drops"
    :drop-cell "Drops Cell"
    :impel "Impels"
    :step "Steps"
    :die "Dies"
    :delete-from-world "Deletes"
    :damage "Damages"
    :expend-action-points "Expends Action Points"
    :expend-default-action-points  "Expends Default Action Points"
    :stat-effect "Effects Statistic"
    :attack "Attacks"
    :explode "Explodes"
    :narrate nil 
    :narrateln nil
    :print-object-tag nil
    :newline nil
    :take "Picks up"
    :print-separator nil))

(defvar *action-translations* *default-action-translations*)

(defun action-translation (action)
  (or (getf *action-translations* action)
      (symbol-name action)))
  
;;; The narration widget

(define-prototype narrator (:parent =formatter=)
  (verbosity :initform 0
	     :documentation "Integer between 0 and 3 (inclusive).")
  (passive-voice-actions :documentation
"List of action words to use passive voice in narrating.
http://en.wikipedia.org/wiki/Passive_voice"
                         :initform nil)
  (repeat-count :initform 0)
  (last-line :initform nil)
  (line-number :initform 0))

(define-method set-verbosity narrator (&optional (value 1))
  (setf <verbosity> value))

(define-method narrate narrator (control-string &rest args)
  (/print self 
	 (apply #'format nil control-string args)))

(define-method narrateln narrator (control-string &rest args)
  (/println self 
	   (apply #'format nil control-string args)))

(define-method say narrator (control-string &rest args)
  (let ((last-line <last-line>)
	(this-line (list (list (apply #'format nil control-string args)))))
    (if (equal last-line this-line)
	;; it's a repeat. make new line with Nx repeat 
	(progn (incf <repeat-count>)
	       (vector-pop <lines>)
	       (message "Repeating message ~Sx" <repeat-count>)
	       (/println self (apply #'format nil (concatenate 'string 
						      control-string 
						      (format nil " (Repeated ~Sx)" <repeat-count>))
				     args)))
	;; new 
	(progn 
	  (message "New message ~S" (cons control-string args))
	  (setf <repeat-count> 0)
	  (/println self (apply #'format nil control-string args))))
    (setf <last-line> (list (list (apply #'format nil control-string args))))))

(define-method narrate-message narrator (sender action receiver args &optional force)
  (unless (zerop <verbosity>)
    (let ((A (or sender iomacs:=asterisk=))
	  (B (if (has-field :tile receiver) 
		 receiver 
		 iomacs:=gray-asterisk=))
	  (action-verbosity (getf *message-verbosities* action t)))
      (when (member action <passive-voice-actions>)
	(rotatef A B))
      (when (or force
		(and (not (null action-verbosity))
		     (or (eq t action-verbosity)
			 (and (numberp action-verbosity)
			      (>= <verbosity> action-verbosity)))))
	(/print self (prin1-to-string <line-number>))
	(incf <line-number>)
	(/print-separator self)
	(/print-object-tag self A)
	(/print-separator self)
	(/print-image self (icon-image action))
	(/space self)
	(/print self (action-translation action)
	       :foreground ".white" :background ".gray30")
	(/print-separator self)
	(if (eq A B)
	    (/print self "SELF" :foreground ".white" :background ".blue")
	    (/print-object-tag self B))
	(/print-separator self)
	;; print args
	(dolist (arg args)
	  (/space self)
	  (if (object-p arg)
	      (/print-object-tag self arg)
	      (/print self (format nil "~A" arg))))
	(/newline self)))))



;;; narration.lisp ends here
