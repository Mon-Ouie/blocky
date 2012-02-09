;;; universe.lisp --- quadtree-based procedural universe generation

;; Copyright (C) 2011  David O'Toole

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

;;; Universes are composed of connected worlds.

(in-package :blocky)

(defvar *universe* nil)

(define-block universe 
  (worlds :initform (make-hash-table :test 'equal)
	  :documentation "Address-to-world mapping.")
  prompt
  (current-address :initform nil)
  (player :initform nil)
  (stack :initform '()))
 
(define-method update universe ()
  (when %world (update %world)))

(define-method draw universe ()
  (when %world (draw %world)))

(define-method handle-event universe (event)
  (with-fields (world) self
    (when world
      (handle-event world event))))

(define-method initialize universe (&key address player world)
  "Prepare a universe for play at the world identified by ADDRESS with
PLAYER as the player."
  (setf *universe* self)
  (when world 
    (setf %world world)
    (setf *world* world)
    (setf *buffer* world))
  (when player (setf %player player))
  (when %player 
    (add-player world %player)))

(defun make-universe ()
  (new universe))

(define-method add-world universe (address world)
  (setf (gethash (normalize-address address) %worlds) world))
 
(define-method remove-world universe (address)
  (remhash (normalize-address address) %worlds))

(define-method get-world universe (address)
  (gethash (normalize-address address) %worlds))

(define-method get-player universe ()
  %player)

(define-method add-player universe (player)
  (setf %player player))

(define-method get-current-world universe ()
  (first %stack))

(define-method get-current-address universe ()
  %current-address)

(define-method destroy universe ()
  (setf %worlds (make-hash-table :test 'equal))
  (setf %stack nil)
  (setf %current-address nil))

(define-method generate universe (address)
  (destructuring-bind (prototype &rest parameters) address
    (let ((world (clone (symbol-value prototype))))
      (prog1 world
	;; make sure any loadouts or intializers get run with the proper world
	(let ((*world* world)) 
	  (build world parameters))))))

(define-method visit-address universe (address)
  (assert address)
  (let ((candidate (get-world self address)))
    (if (null candidate)
	(add-world self address
		   (if (stringp address)
		       ;; possibly load object from disk
		       (find-resource-object address)
		       ;; synthesize
		       (generate-world self address)))
	candidate)))

(defun normalize-address (address)
  "Sort the plist ADDRESS so that its keys come in alphabetical order
by symbol name. This enables them to be used as hash keys."
  (etypecase address
    (string address)
    (list (assert (and (stringp (first address))
		       (or (null (rest address))
			   (keywordp (second address)))))
       (labels ((all-keys (plist)
		  (let (keys)
		    (loop while (not (null plist))
			  do (progn (push (pop plist) keys)
				    (pop plist)))
		    keys)))
	 (let (address2)
	   (dolist (key (sort (all-keys (cdr address)) #'string> :key #'symbol-name))
	     ;; build sorted plist
	     (push (getf (cdr address) key) address2)
	     (push key address2))
	   (cons (car address) address2))))))

(defparameter *default-space-size* 10)

;;; Missions and Goals

(defstruct goal 
  name 
  description
  condition ;; either a symbol or a function (or nil)
  state ; one of nil, :achieved, :failed
  prerequisites)

(defun check-condition (goal)
  (etypecase goal
    (keyword (check-condition (mission-variable-value goal)))
    (goal (or (eq :achieved (goal-state goal))
	      (let ((condition (goal-condition goal))
		    (prerequisites (goal-prerequisites goal)))
		(when (and (etypecase condition
			     (symbol (symbol-value condition))
			     (function (funcall condition)))
			   (or (null prerequisites)
			       (every #'check-condition prerequisites)))
		  (setf (goal-state goal) :achieved)))))))

(defun achieve (goal &optional force)
  (let ((prerequisites (goal-prerequisites goal)))
    (when (or force (every #'check-condition prerequisites))
      (setf (goal-state goal) t))))

(defvar *mission* nil)

(define-block mission
  name 
  title
  description
  address
  universe
  variables)

(define-method set-variable mission (var value)
  (setf (gethash var %variables) value))

(define-method get-variable mission (var)
  (gethash var %variables))

(defun mission-variable-value (var-name)
  (get-variable *mission* var-name))

(defun set-mission-variable-value (var-name value)
  (set-variable *mission* var-name value))

(defsetf mission-variable-value set-mission-variable-value)

(defmacro with-mission-locals (vars &rest body)
  (labels ((make-clause (sym)
	     `(,sym (mission-variable-value ,(make-keyword sym)))))
    (let* ((symbols (mapcar #'make-non-keyword vars))
	   (clauses (mapcar #'make-clause symbols)))
      `(symbol-macrolet ,clauses ,@body))))

(define-method is-completed mission ()
  "Return T if all goal-valued mission variables are achieved."
  (with-fields (variables) self
    (block checking 
      (labels ((check (name goal)
		 (when (and (goal-p goal) 
			    (null (check-condition goal)))
		   (return-from checking nil))))
	(maphash #'check variables)
	(return-from checking t)))))
	       
;; (define-method begin mission (player)
;;   (assert (object-p player))
;;   (with-fields (name description address universe variables) self
;;     (assert (listp address))
;;     (when (null universe)
;;       (setf universe (if (null *universe*)
;; 			 (new universe)
;; 			 *universe*)))
;;     ;; this probably works better if you have already set up a universe.
;;     (setf *mission* self)
;;     (play universe :player player :address address)
;;     (do-prologue self)))
      
(define-method do-prologue mission ())

(define-method win mission ())

(define-method lose mission ())

(define-method end mission ())

(define-method run mission ())

(defmacro defmission (name (&key title description address)
		      &rest goals)
  (let ((hash (gensym)))
    (labels ((set-goal (entry)
	       (destructuring-bind (var-name &rest goal-props) entry
		 `(setf (gethash ,(make-keyword var-name) ,hash) (make-goal ,@goal-props)))))
      `(let ((,hash (make-hash-table)))
	 (progn ,@(mapcar #'set-goal goals))
	 (define-prototype ,name (:super "BLOCKY:MISSION")
	   (name :initform ,(make-keyword name))
	   (description :initform ,description)
	   (address :initform ,address)
	   (variables :initform ,hash)
	 (title :initform ,title))))))

;; The flow goes defmission, initialize, begin, win/lose, end

(defparameter *test-grammar* 
  '((mission >> (at location please goal+ in exchange for reward))
    (location >> mars zeta-base nebula-m corva-3)
    (goal+ >> goal (goal and goal+))
    (goal >> (defeat foe) (defend friend) (activate button) (retrieve documents)
     (collect mineral+))
    (mineral+ >> mineral (mineral and mineral+))
    (mineral >> endurium technetium molybdenum francium a-biosilicates)
    (foe >> scanner biclops unique)
    (friend >> transport skiff soldier scientist)
    (unique >> zx-90 xioblade)
    (reward >> money part)
    (money >> 10000 20000 30000 40000 50000)
    (part >> muon-pistol lepton-cannon ion-shield-belt)))

;;; universe.lisp ends here
