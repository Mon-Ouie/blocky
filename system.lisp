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

(defblock system
  (type :initform :system)
  (shell :initform nil)
  (running :initform nil))

(define-method get-blocks system ()
  ^children)

(define-method count-blocks system ()
  (with-fields (children) self
    (apply #'+ (mapcar #'/count children))))

(define-method start system ()
  (setf ^running t))

(define-method stop system ()
  (setf ^running nil))

(define-method tick system (&rest args)
  (with-fields (running children shell) self
    (when (null shell)
      (setf shell (clone =shell=))
      (setf *default-font* *block-font*)
      (resize shell :width *screen-width* :height *screen-height*)
      (create-image shell)
      (move shell 0 0)
      (switch-to-script shell (clone =script=))
      (add shell (clone =listener=) 0 0)
      (add shell (clone =listener=) 20 20)
      (add shell (clone =integer=) 80 80)
      (add shell (clone =listener=) 90 90))
    (when running
      (dolist (block children)
	(apply #'tick block args)))))

(define-method forward system (method &rest args)
  (apply #'send nil method ^script args))

(define-method resize system (&key height width)
  ;; (parent/resize self :height height :width width)
  (with-fields (shell) self
    (resize shell :height height :width width)
    (layout shell)))

(define-method hit system (x y)
  (with-fields (shell) self
    (hit shell x y)))

(define-method initialize system (&rest args)
  #+linux (do-cffi-loading)
  (message "Starting IOFORMS Shell...")
  (initialize-ioforms)
  (sdl:init-sdl :video t :audio t :joystick t)
  (apply #'/parent/initialize self args)
  (reset-message-function)
  (setf *project-package-name* nil
        *project-directories* (ioforms:default-project-directories)
	*world* nil
	*system* self
	*updates* 0
	*keyboard-update-number* 0
	*initialization-hook* nil
	*play-args* args
	*random-state* (make-random-state t))
  ;; add library search paths for Mac if needed
  (setup-library-search-paths)
  ;;(sdl:with-init (sdl:SDL-INIT-VIDEO sdl:SDL-INIT-AUDIO sdl:SDL-INIT-JOYSTICK)
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
  (open-project "standard")
  (setf *window-title* "ioforms")
  (setf *resizable* t)
  (enable-classic-key-repeat 100 100)
  (labels ((do-resize ()
	     (resize *system* 
		     :width *screen-width* 
		     :height *screen-height*)))
    (add-hook '*resize-hook* #'do-resize))
  (ioforms:install-blocks self))
	
(defparameter *default-shell-width* 1024)
(defparameter *default-shell-height* 720)

(define-method run system ())

(define-method open system (project)
  (open-project project))

(define-method save system ()
  (save-project))

;; (define-method new system (project)
;;   (assert (stringp project))
;;   (let ((dir (find-project-path project)))
;;     (when (directory-is-project-p dir)
;;       (error "Project ~S aready exists." project))
;;     (make-directory-maybe dir)
;;     (open-project self project)))

(define-method save-everything system ()
  (save-project :force))

(define-method ticks system ()
  (get-ticks))

(define-method draw system (destination)
  (with-fields (shell) self
    (when shell
      (with-fields (image) shell
	(when image
	  (render shell)
	  (draw-image image 0 0 :destination destination))))))

;;; Interactive editor shell

(define-prototype block-prompt (:parent =prompt=)
;;  (operation :initform :prompt)
  output 
  (rows :initform 10))

(define-method initialize block-prompt (output)
  (parent/initialize self)
  (setf ^output output))

(define-method do-sexp block-prompt (sexp)
  (with-fields (output rows) self
    (assert output)
    (let ((container (get-parent output)))
      (when container
	(accept container 
		 (let ((*make-block-package* (find-package :ioforms)))
		   (if (symbolp (first sexp))
		       (make-block-ext sexp)

		       (make-block-ext (first sexp)))))
	(when (> (length container) rows)
	  (pop container))))))

(define-prototype listener (:parent =list=)
  (type :initform :system)
  (schema :initform '(:block)))

(defparameter *minimum-listener-width* 200)

(define-method initialize listener ()
  (with-fields (image arguments) self
    (let ((prompt (clone =block-prompt= self)))
      (parent/initialize self)
      (resize prompt 
	       :width *minimum-listener-width*
	       :height (+ (* 2 *dash*) 
			  (font-height *default-font*)))
      (assert (field-value :image prompt))
      (setf (first arguments) prompt))))

(define-method run listener ()
  (with-fields (arguments) self
    (destructuring-bind (prompt) arguments
      (run prompt))))

(define-prototype shell (:parent =block=)
  (selection :initform ()
  	     :documentation "Subset of selected blocks.")
  (script :initform nil)
  (drag :initform nil 
  	:documentation "Block being dragged, if any.")
  (hover :initform nil
	 :documentation "Block being hovered over, if any.")
  (ghost :initform (clone =block=))
  (buffer :initform nil)
  (drag-start :initform nil
	      :documentation "A cons (X . Y) of widget location at start of dragging.")
  (drag-offset :initform nil
	       :documentation "A cons (X . Y) of mouse click location on dragged block.")
  (needs-redraw :initform t)
  (modified :initform nil 
	  :documentation "Non-nil when modified since last save."))

(define-method layout shell ()
  (layout ^script))

(define-method initialize shell ()
  (parent/initialize self))

(define-method script-blocks shell ()
  (field-value :arguments ^script))

(define-method switch-to-script shell (script) 
  (assert (ioforms:object-p script))
  (setf ^script script))
  
(define-method add shell (block &optional x y)
  (with-fields (needs-redraw script) self
    (add script block x y)
    (setf needs-redraw t)))

(define-method delete-child shell (block)
  (delete ^script block))

(define-method select shell (block)
  (with-fields (selection arguments) self
    (pushnew block selection)))

(define-method select-if shell (predicate)
  (with-fields (selection arguments) self
    (setf selection (remove-if predicate arguments))))

(define-method unselect shell (block)
  (with-fields (selection) self
    (setf selection (delete block selection))))

(define-method handle-key shell (keys)
  (with-fields (selection needs-redraw) self
    (when (= 1 (length selection))
      (when (first selection)
	(handle-key (first selection) keys)
	(setf needs-redraw t)))))

(define-method resize shell (&key width height)
  (with-fields (prompt buffer needs-redraw image) self
    (parent/resize self :width width :height height)
    (setf buffer (create-image width height))
    (setf needs-redraw t)))

(define-method redraw shell ()
  (with-fields (buffer selection needs-redraw width height) self
    (let ((blocks (script-blocks self)))
      (draw-box 0 0 width height 
		:color *background-color*
		:stroke-color *background-color*
		:destination buffer)
      (dolist (block blocks)
	(layout block))
      (dolist (block blocks)
	(when (find block selection)
	  (draw-border block buffer))
	(draw block buffer))
      (setf needs-redraw nil))))

(define-method begin-drag shell (mouse-x mouse-y block)
  (with-fields (drag arguments script drag-start ghost drag-offset) self
    (setf drag block)
    (when (is-member script block)
      (delete-child script block))
    (let ((dx (field-value :x block))
	  (dy (field-value :y block))
	  (dw (field-value :width block))
	  (dh (field-value :height block)))
      (with-fields (x y width height) ghost
	(let ((x-offset (- mouse-x dx))
	      (y-offset (- mouse-y dy)))
	  (when (null drag-start)
	    (setf x dx y dy width dw height dh)
	    (setf drag-start (cons dx dy))
	    (setf drag-offset (cons x-offset y-offset))))))))

(define-method hit-blocks shell (x y)
  (labels ((hit-it (b)
	     (hit b x y)))
    (let ((parent 
	   (find-if 
	    #'hit-it 
	    (script-blocks self)
	    :from-end t)))
      (when parent
	(hit parent)))))

(define-method render shell ()
  (with-fields 
      (script needs-redraw buffer image drag-start selection
      drag modified hover ghost prompt) self
    ;; render any selected blocks to their offscreen images
    (dolist (block selection)
      (let ((block-image (field-value :image block)))
    	(when block-image 
    	  (render block))))
    ;; now render
    (when needs-redraw 
      (redraw self))
    (draw-image buffer 0 0 :destination image)
    (when drag 
      (layout drag)
      (draw-ghost ghost image)
      (draw drag image)
      (when hover 
	(draw-hover hover image)))))

(define-method mouse-down shell (x y &optional button)
  (let ((block (hit-blocks self x y)))
    (when block
      (case button
	(1 (begin-drag self x y block))
	(3 (run block))))))

(define-method mouse-move shell (mouse-x mouse-y)
  (with-fields (arguments hover drag-offset drag-start drag) self
    (setf hover nil)
    (when drag
      (destructuring-bind (ox . oy) drag-offset
	(let ((target-x (- mouse-x ox))
	      (target-y (- mouse-y oy)))
	  (setf hover (hit-blocks self target-x target-y))
	  (move drag target-x target-y))))))

(define-method mouse-up shell (x y &optional button)
  (with-fields 
      (arguments needs-redraw drag-offset drag-start hover
	      selection drag modified) 
      self
    (when drag
      (let ((drag-parent (get-parent drag)))
	(when drag-parent
	  (unplug-from-parent drag))
	(let ((sink hover))
	  (if sink
	      ;; dropping on another block
	      (unless (accept sink drag)
		(add self drag))
	      ;; dropping on background
	      (add self drag)))))
    (setf selection nil)
    (when drag (select self drag))
    (setf drag-start nil
	  drag-offset nil
	  drag nil
	  needs-redraw t)))

;;; Various blocks

(defblock if 
  (type :initform :control)
  (result :initform nil)
  (schema :initform '(:block :block :block))
  (arguments :initform '(nil nil nil)))

(define-method execute if ()
  ^results)

(define-method execute-arguments if ()
  (with-fields (arguments results) self
    (destructuring-bind (predicate then else) arguments
      (if (run predicate)
	  (run then)
	  (run else)))))

;;; Get field value

(defblock my 
  (type :initform :variables)
  (schema :initform '(:block)))

(define-method execute my ()
  (with-fields (results) self
    (field-value (make-keyword (car results))
		 *target*)))

;;; Set field value

(defblock set
  (type :initform :variables)
  (schema :initform '(:symbol :anything))
  (arguments :initform '(:counter 1)))

;;; Talking 

(defblock emote 
  (type :initform :looks)
  (schema :initform '(:string)))

(define-method execute emote ()
  (send :emote *target* 
	(list (list (list (first ^results) :font *block-font*
			  :foreground "black")))
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

(define-method execute play-music ()
  (play-music *target* (first ^results) :loop t))

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

(define-method execute + ()
  (with-fields (results) self
    (when (every #'integerp results)
      (apply #'+ results))))

;;; system.lisp ends here
