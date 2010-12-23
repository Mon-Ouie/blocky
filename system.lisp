;;; system.lisp --- blocks library for basic ioforms operations

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
  (dolist (block *blocks*)
    (/step block)))

(define-method initialize system ()
  #+linux (do-cffi-loading)
  (ioforms:initialize)
  (apply #'/parent/initialize self args)
  (setf *timestep-function* #'(lambda (&rest args)
				(apply #'/step self args)))
  (reset-message-function)
  (setf *project-package-name* nil
        *project-directories* (ioforms:base-directories)
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
	   ;; (unless (sdl:initialise-default-font sdl:*ttf-font-vera*)
	   ;;   (error "FATAL: Cannot initialize the default font."))
	   (load-user-init-file)	
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

;;; Block shell widget and command prompt

(define-prototype block-prompt (:parent =prompt=)
  output 
  (rows :initform 10))

(define-method initialize block-prompt (output)
  (/parent/initialize self)
  (setf <output> output))
  
(define-method do-sexp block-prompt (sexp)
  (with-fields (output rows) self
    (assert output)
    (let ((container (/get-parent output)))
      (when container
	(/accept container 
		 (let ((*make-block-package* (find-package :ioforms)))
		   (if (symbolp (first sexp))
		       (make-block-ext sexp)
		       (make-block-ext (first sexp)))))
	(when (> (/length container) rows)
	  (/pop container))))))

(defblock listener
  (type :initform :system))

(defparameter *minimum-listener-width* 200)

(define-method initialize listener ()
  (with-fields (widget) self
    (/parent/initialize self)
    (let ((prompt (clone =block-prompt= self)))
      (/resize prompt 
	       :width *minimum-listener-width*
	       :height (+ (* 2 *dash*) 
			  (font-height *default-font*)))
      (setf widget prompt))))

(defblock shell
  (block :initform nil 
	  :documentation "The IOFORMS:=BLOCK= object being edited.")
  (selection :initform ()
  	     :documentation "Subset of selected blocks.")
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

(define-method initialize shell ()
  (/parent/initialize self)
  (with-fields (block) self
    (setf block (clone =block=))))

(define-method select shell (block)
  (with-fields (selection blocks) self
    (pushnew block selection)))

(define-method select-if shell (predicate)
  (with-fields (selection blocks) self
    (setf selection (remove-if predicate blocks))))

(define-method unselect shell (block)
  (with-fields (selection) self
    (setf selection (delete block selection))))

(define-method handle-key shell (keys)
  (with-fields (selection needs-redraw) self
    (when (= 1 (length selection))
      (when (first selection)
	(/handle-key (first selection) keys)
	(setf needs-redraw t)))))

(define-method resize shell (&key width height)
  (with-fields (buffer prompt image) self
    (when (null buffer)
      (setf buffer (create-image width height)))
    (unless (and (= <width> width)
		 (= <height> height))
      (/parent/resize self :width width :height height)
      (when buffer
	(sdl:free buffer))
      (setf buffer (create-image width height)))))

(define-method redraw shell ()
  (with-fields (block buffer selection needs-redraw width height) self
    (with-fields (blocks) block
      (draw-box 0 0 width height 
		:color *background-color*
		:stroke-color *background-color*
		:destination buffer)
      (dolist (block blocks)
	(/layout block))
      (dolist (block blocks)
	(when (find block selection)
	  (/draw-border block buffer))
	(/draw block buffer))
      (setf needs-redraw nil))))

(define-method begin-drag shell (mouse-x mouse-y block)
  (with-fields (drag block drag-start ghost drag-offset) self
    (setf drag block)
    (when (/is-member block block)
      (/delete block block))
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
  (with-fields (block) self
    (when block 
      (with-fields (blocks) block
	(labels ((hit (b)
		   (/hit b x y)))
	  (let ((parent (find-if #'hit blocks :from-end t)))
	    (when parent
	      (/hit parent x y))))))))

(define-method render shell ()
  (with-fields 
      (block needs-redraw image buffer drag-start selection
      drag modified hover ghost prompt) self
    (dolist (block selection)
      (let ((widget (/get-widget block)))
	(when widget 
	  (/render widget)
	  (/draw block image))))
    (labels ((copy ()
	       (draw-image buffer 0 0 :destination image)))
      (when block
	(when needs-redraw 
	  (/redraw self)
	  (copy))
	(when drag 
	  (copy)
	  (/layout drag)
	  (/draw-ghost ghost image)
	  (/draw drag image)
	  (when hover 
	    (/draw-hover hover image)))))))

(define-method mouse-down shell (x y &optional button)
  (with-fields (block) self 
    (let ((block (/hit-blocks self x y)))
      (when block
	(case button
	  (1 (/begin-drag self x y block))
	  (3 (/run block block)))))))

(define-method mouse-move shell (mouse-x mouse-y)
  (with-fields (block hover drag-offset drag-start drag) self
    (setf hover nil)
    (when drag
      (destructuring-bind (ox . oy) drag-offset
	(let ((target-x (- mouse-x ox))
	      (target-y (- mouse-y oy)))
	  (setf hover (/hit-blocks self target-x target-y))
	  (/move drag target-x target-y))))))

(define-method mouse-up shell (x y &optional button)
  (with-fields 
      (block needs-redraw drag-offset drag-start hover
	      selection drag modified) 
      self
    (with-fields (blocks) block
      (when drag
	(let ((drag-parent (/get-parent drag)))
	  (when drag-parent
	    (/unplug-from-parent drag))
	  (let ((target hover))
	    (if target
		;; dropping on another block
		(unless (/accept target drag)
		  (/add block drag))
		;; dropping on background
		(/add block drag)))))
      (setf selection nil)
      (when drag (/select self drag))
      (setf drag-start nil
	    drag-offset nil
	    drag nil
	    needs-redraw t))))

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

;;; system.lisp ends here
