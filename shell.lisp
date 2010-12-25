;;; shell.lisp --- blocks library for basic ioforms operations

;; Copyright (C) 2010  David O'Toole

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

(in-package :ioforms)

(defblock system
  (type :initform :system)
  (running :initform nil))

(define-method get-blocks system ()
  *blocks*)

(define-method count-blocks system ()
  (apply #'+ (mapcar #'/count *blocks*)))

(define-method start system ()
  (setf <running> t))

(define-method stop system ()
  (setf <running> nil))

(define-method step system (&rest args)
  (when <running> 
    (dolist (block *blocks*)
      (/step block))))

(define-method initialize system ()
  #+linux (do-cffi-loading)
  (ioforms:initialize)
  (apply #'/parent/initialize self args)
  (setf *timestep-function* #'(lambda (&rest args)
				(apply #'/step self args)))
  (reset-message-function)
  (setf *project-package-name* nil
        *project-directories* (ioforms:default-project-directories)
	*world* nil
	*timesteps* 0
	*keyboard-timestep-number* 0
	*initialization-hook* nil
	*play-args* args
	*random-state* (make-random-state t)
	*project* "*notes*")
	;; add library search paths for Mac if needed
	(setup-library-search-paths)
	(sdl:with-init (sdl:SDL-INIT-VIDEO sdl:SDL-INIT-AUDIO sdl:SDL-INIT-JOYSTICK)
	   (load-user-init-file) ;; this step may override *project-directories*
	   (initialize-resource-table)
	   (initialize-colors)
	   (when *use-sound*
	     ;; try opening sound
	     (when (null (sdl-mixer:open-audio :frequency *frequency*
					       :chunksize *output-chunksize*
					       :enable-callbacks t
					       :format *sample-format*
					       :channels *output-channels*))
	       ;; if that didn't work, disable effects/music
	       (message "Could not open audio driver. Disabling sound effects and music.")
	       (setf *use-sound* nil))
	     ;; set to mix lots of sounds
	     (sdl-mixer:allocate-channels *channels*))
	   (index-project "standard")
	   (ioforms:run-main-loop)))

(define-method new-project system (project)
  (assert (stringp project))
  (let ((dir (find-project-path project)))
    (when (directory-is-project-p dir)
      (error "Project ~S aready exists." project))
    (make-directory dir)
    (open-project project)))

(define-method open-project system (project)
  (open-project project))

(define-method save-project system ()
  (save-project))

(define-method save-everything system ()
  (save-everything))

(define-method get-ticks system ()
  (get-ticks))

;;; IF block

(defblock if 
  (type :initform :control)
  (result :initform nil)
  (schema :initform '(:block :block :block))
  (arguments :initform '(nil nil nil)))

(define-method execute if (target)
  <results>)

(define-method execute-arguments if (target)
  (with-fields (arguments results) self
    (destructuring-bind (predicate then else) arguments
      (if (/run predicate target)
	  (/run then target)
	  (/run else target)))))

;;; Get field value

(defblock my 
  (type :initform :variables)
  (schema :initform '(:block)))

(define-method execute my (target)
  (with-fields (results) self
    (field-value (make-keyword (car results))
		 target)))

;;; Set field value

(defblock set
  (type :initform :variables)
  (schema :initform '(:symbol :anything))
  (arguments :initform '(:counter 1)))

;;; Talking 

(defblock emote 
  (type :initform :looks)
  (schema :initform '(:string)))

(define-method execute emote (target)
  (/emote target 
	  (list (list (list (first <results>) :font *block-font*
			    :foreground ".black")))
	  :timeout 200 :style :clear))

;;; Other blocks

(defblock say 
  (type :initform :message)
  (schema :initform '(:string))
  (arguments :initform '("Hello!")))

(defblock move
  (type :initform :motion)
  (schema :initform '(:symbol :integer :symbol))
  (arguments :initform '(:north 10 :pixels)))

(defblock move-to
  (type :initform :motion)
  (schema :initform '(:unit :integer :integer))
  (arguments :initform '(:space 0 0)))

(defblock joystick-button
  (type :initform :sensing)
  (schema :initform '(:integer :symbol))
  (arguments :initform '(1 :down)))

(defblock visible?
  (type :initform :variables)
  (schema :initform nil)
  (arguments :initform nil))

(defblock set-variable 
  (type :initform :variables)
  (schema :initform '(:symbol :block))
  (arguments :initform '(:n nil)))

(defblock animate 
  (type :initform :looks)
  (schema :initform '(:string))
  (arguments :initform '(nil)))

(defblock play-music 
  (type :initform :sound)
  (schema :initform '(:string))
  (arguments :initform '("fanfare")))

(define-method execute play-music (target)
  (/play-music target (first <results>) :loop t))

(defblock play-sound 
  (type :initform :sound)
  (schema :initform '(:string))
  (arguments :initform '("boing")))

(defblock when 
  (type :initform :control)
  (schema :initform '(:block :block))
  (arguments :initform '(nil nil)))

(defblock unless
  (type :initform :control)
  (schema :initform '(:block :block))
  (arguments :initform '(nil nil)))

(defblock fire
  (type :initform :control)
  (schema :initform '(:block))
  (arguments :initform '(:south)))

(defblock see-player
  (type :initform :sensing)
  (schema :initform nil)
  (arguments :initform nil))

(defblock player-direction
  (type :initform :sensing)
  (schema :initform nil)
  (arguments :initform nil))

(defblock closer-than
  (type :initform :sensing)
  (schema :initform '(:block :block :block :block))
  (arguments :initform '(10 spaces to player)))
  
(defblock +
  (type :initform :operators)
  (schema :initform '(:number :number))
  (arguments :initform '(nil nil)))

(define-method execute + (target)
  (with-fields (results) self
    (when (every #'integerp results)
      (apply #'+ results))))

;;; shell.lisp ends here
