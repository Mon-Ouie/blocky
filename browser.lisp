;;; browser.lisp --- the system menu

;; Copyright (C) 2010, 2011, 2012 David O'Toole

;; Author: David O'Toole <dto@ioforms.org>
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

;;; Commentary:

;;; Code:

(in-package :blocky)

;;; A basic action button

(define-block (button :super :list)
  (category :initform :button)
  (target :initform nil)
  (method :initform nil)
  (arguments :initform nil)
  (label :initform nil))

(define-method initialize button 
    (&key target method arguments label)
  (when target (setf %target target))
  (when method (setf %method method))
  (when label (setf %label label))
  (when arguments (setf %arguments arguments)))

(define-method layout button ()
  (with-fields (height width) self
    (setf width (+ (* 13 (dash))
		   (font-text-width %label
				    *block-bold*))
	  height (+ (font-height *block-bold*) (* 4 (dash))))))

(define-method draw button ()
  (with-fields (x y height width label) self
    (draw-patch self x y (+ x width) (+ y height))
    (draw-image "colorbang" 
		    (+ x (dash 1))
		    (+ y (dash 1)))
    (draw-string %label (+ x (dash 9)) (+ y (dash 2))
		 :color "white"
		 :font *block-bold*)))

(define-method tap button (x y)
  (apply #'send %method %target %arguments))

;;; Message output widget

(define-block messenger :category :terminal :messages nil)

(define-method initialize messenger (&optional messages)
  (cond 
    ((stringp messages)
     (setf %messages (list messages)))
    ((consp messages)
     (setf %messages messages))))

(define-method add-message messenger (message-string)
  (assert (stringp message-string))
  (push message-string %messages))

(defparameter *messenger-columns* 80)
(defparameter *messenger-rows* 7)

(define-method get-messages messenger ()
  (or %messages *message-history*))

(define-method layout messenger ()
  (setf %height (+ (* (font-height *font*) *messenger-rows*)
		   (dash 4)))
  (let ((width 0))
    (block measuring
      (dotimes (n *messenger-rows*)
	(if (<= (length (get-messages self)) n)
	    (return-from measuring nil)
	    (setf width 
		  (max width 
		       (font-text-width 
			(nth n (get-messages self))
			*block-font*))))))
    (setf %width (+ width (dash 5)))))
			     
(define-method draw messenger ()
  (draw-background self)
  (with-fields (x y width height) self
      (let ((y0 (+ y height (- 0 (font-height *font*) (dash 2))))
	    (x0 (+ x (dash 3))))
	(dotimes (n *messenger-rows*)
	  (unless (<= (length (get-messages self)) n)
	    (draw-string (nth n (get-messages self))
			 x0 y0
			 :color "gray70"
			 :font *block-font*)
	    (decf y0 (font-height *font*)))))))

;;; A block representing the current system and project universe

(defparameter *browser-menu-entries*
  '((:label "Project"
     :inputs
     ((:label "Create a new project" :action :create-project)
      (:label "Save current project" :action :save-project)
      (:label "Load a project" :action :load-project)
      ;; (:label "Show current changes without saving" :action :show-changes)
      ;; (:label "Export as archive" :action :export-archive)
      ;; (:label "Export as application" :action :export-application)
      ;; (:label "Publish to web" :action :publish-web)
      ;; (:label "Publish to community site" :action :publish-community)
      ;; (:label "Publish to FTP" :action :publish-ftp)
      (:label "Settings" :action :settings)
      (:label "Quit Blocky" :action :quit-blocky)))
    (:label "Edit"
     :inputs
     ((:label "Cut" :action :cut)
      (:label "Copy" :action :copy)
      (:label "Paste" :action :paste)
      (:label "Paste in new workspace" :action :paste-as-new-workspace)
      (:label "Select all" :action :select-all)
      (:label "Clear selection" :action :clear-selection)))
    (:label "Blocks"
     :inputs
     ((:label "Define a block" :action :open-define-block-dialog)
      (:label "Define a method" :action :open-define-method-dialog)
      (:label "Inspect" :action :inspect)
      (:label "Clone" :action :do-clone)
      (:label "Copy" :action :do-copy)
      (:label "Destroy" :action :destroy)))
    (:label "Resources"
     :inputs
     ((:label "Import new resource" :action :import-resources)
      (:label "Edit resource" :action :edit-resource)
      (:label "Search resources" :action :search-resources)
      (:label "Export resource(s)" :action :export-resources)
      (:label "Browse resources" :action :browse-resources)))
    ;; (:label "Tools" 
    ;;  :inputs
    ;;  ((:label "Create a Lisp listener" :action :create-listener)
    ;;   (:label "Create a text box" :action :create-text)
    ;;   (:label "Create a trash can" :action :create-trash)))
    (:label "Workspace" :inputs
     ((:label "Switch to workspace" :inputs
    	      ((:label "Workspace 1" :action :workspace-1)
    	       (:label "Workspace 2" :action :workspace-2)
    	       (:label "Workspace 3" :action :workspace-3)
    	       (:label "Workspace 4" :action :workspace-4)))
      (:label "Go back to the previous workspace" :action :previous-workspace)
      (:label "Create a new workspace" :action :create-workspace)
      (:label "Rename this workspace" :action :rename-workspace)
      (:label "Delete this workspace" :action :delete-workspace)
      (:label "Workspace settings" :action :configure-workspaces)))
    ;; (:label "Windows"
    ;;  :inputs
    ;;  ((:label "Create a new window" :action :create-window)
    ;;   (:label "Switch to the next window" :action :next-window)
    ;;   (:label "Switch to window" :action :switch-window)
    ;;   (:label "Close this window" :action :dismiss-window)))
    ;; (:label "Devices"
    ;;  :inputs
    ;;  ((:label "Browse available devices" :action :browse-devices)
    ;;   (:label "Scan for devices" :action :scan-devices)
    ;;   (:label "Configure joystick" :action :configure-joystick)
    ;;   (:label "Configure camera" :action :configure-camera)
    ;;   (:label "Configure microphone" :action :configure-microphone)
    ;;   (:label "Configure dance pad" :action :configure-dance-pad)))
    (:label "Help"
     :inputs
     ((:label "Copyright notice" :action :show-copyright-notice)
      (:label "General help" :action :general-help)
      (:label "Examples" :action :show-examples)
      (:label "Language Reference" :action :language-reference)))))

;;; Headline 

(define-block headline title)

(defparameter *blocky-title-string* "Blocky 0.92a")

(define-method initialize headline (&optional (title *project*))
  (initialize%super self)
  (setf %title title))

(define-method layout headline ()
  (resize self 
	  (+ (dash 2) *logo-height*)
	  *logo-height*))

(define-method draw headline ()
  (with-fields (x y) self
    (draw-image "blocky" (+ x (dash 0.5)) (- y (dash 0.5)) :height *logo-height* :width *logo-height*)
    (draw-string %title
		 (+ x *logo-height* (dash 2))
		 (+ y (dash 1))
		 :color "white"
		 :font *block-bold*)))

;;; Generic window titlebar utility

(define-block (window :super list)
  (centered :initform nil)
  (tags :initform '(:window))
  (category :initform :system))

(defun windowp (thing)
  (and (blockyp thing)
       (has-tag thing :window)))

(define-method initialize window (&key child (title "*untitled-window*"))
  (assert child)
  (initialize%super self)
  (setf %inputs (list (new 'headline title) child))
  (update-parent-links self)
  (mapc #'pin %inputs))

(define-method layout window ()
  (layout-vertically self)
  (align-to-pixels self))

(define-method can-pick window ()
  t)

(define-method pick window ()
  self)

(define-method center window ()
  (layout self)
  (center%super self))

(define-method accept window (thing))

(define-method draw-hover window ())

(define-method after-unplug-hook window (thing)
  (destroy self))

;;; The system menu itself

(define-block-macro browser 
    (:super :list 
     :fields 
     ((category :initform :system))
     :inputs 
     (:headline (new 'headline)
      :menu 
      (new 'tree 
	   :label *blocky-title-string*
	   :pinned t
	   :category :system
	   :expanded t
	   :inputs
	   (list 
	    (new 'listener)
	    (new 'menu :label "Menu" 
		      :inputs 
		 (flet ((process (entry)
			  (make-menu entry :target self)))
		   (mapcar #'process *browser-menu-entries*))
		      :category :menu
		      :expanded t)
	    (new 'tree :label "Messages"
		 :expanded nil
		 :inputs (list (new 'messenger)))))))
  (unfreeze self)
  (expand %%menu)
  (mapc #'pin %inputs)
  (mapc #'pin (%inputs %%menu))
  (setf %locked nil))

(defun make-browser ()
  (find-uuid 
   (new '"BLOCKY:BROWSER")))

(define-method alternate-tap browser (x y))
(define-method scroll-tap browser (x y))

(define-method destroy browser ()
  (destroy%super self)
  (setf (%browser (world)) nil))

(define-method menu-items browser ()
  (%inputs %%menu))

(define-method get-listener browser ()
  (first (menu-items self)))
        
(define-method layout browser ()
  (layout-vertically self)
  ;; adjust header for project name
  (setf %width 
	(max %width
	     (+ *logo-height* (dash 5) 
		(font-text-width *project*
				 *block-bold*)))))

;; (define-method can-pick browser ()
;;   t)

;; (define-method pick browser ()
;;   self)

(define-method draw browser ()
  (with-fields (x y width height) self
    (draw-patch self x y (+ x width) (+ y height))
    (mapc #'draw %inputs)))

(define-method close-menus browser ()
  (let ((menus (menu-items self)))
    (when (some #'expandedp menus)
      (mapc #'unexpand menus))))

(define-method drop-dialog browser (dialog)
  (multiple-value-bind (x y) (right-of self)
    (add-block (world) dialog x y)))

;; Don't allow anything to be dropped on the menus, for now.

(define-method draw-hover browser () nil)

(define-method accept browser (thing)
  (declare (ignore thing))
  nil)

(define-method tap browser (x y)
  (declare (ignore x y))
  (grab-focus (get-listener self)))

;;; Creating a project

(define-block-macro create-project-dialog 
    (:super :list
     :fields ((category :initform :system))
     :inputs (:name (new 'string :label "Project name:")
	      :parent (new 'string :label "Create in folder:" 
				   :value (namestring (projects-directory)))
	      :folder-name (new 'string :label "Project folder name (optional):")
	      :messenger (new 'messenger "Use the text entry fields above to name your new project.")
	      :buttons (new 'hlist
			    (new 'button :label "Create project"
				 :target self :method :create-project)
			    (new 'button :label "Dismiss"
				 :target self :method :dismiss)))))

(define-method create-project create-project-dialog ()
  (with-input-values (name parent folder-name) self
    (add-message 
     %%messenger    
     (if (create-project-image 
	  name :folder-name folder-name :parent parent)
	 "Successfully created new project."
	 "Could not create project."))))

(define-method create-project browser ()
  (let ((dialog (new 'window 
		     :title "Create a new project"
		     :child (new 'create-project-dialog))))
    (drop-dialog self dialog)))

;;; Loading a project

(define-block-macro load-project-dialog 
    (:super :list
     :fields ((category :initform :system))
     :inputs (:filename 
	      (new 'string :label "Load from folder:" 
			   :value (namestring (projects-directory)))
	      :messenger (new 'messenger "Use the text entry fields above to name your project.")
	      :buttons (new 'hlist
			    (new 'button :label "Load project"
				 :target self :method :load-project)
			    (new 'button :label "Dismiss"
				 :target self :method :dismiss)))))

(define-method load-project load-project-dialog ()
  (with-input-values (filename) self
    (add-message 
     %%messenger    
     (if (load-project-image filename)
	 "Successfully loaded new project."
	 "Could not load project."))))

(define-method load-project browser ()
  (let ((dialog (new 'window 
		     :title "Load a project"
		     :child (new 'load-project-dialog))))
    (drop-dialog self dialog)))

;;; Saving a project

(define-block-macro save-project-dialog 
    (:super :list
     :fields ((category :initform :system))
     :inputs (:messenger (new 'messenger "Save current project?")
	      :buttons (new 'hlist
			    (new 'button :label "Save project"
				 :target self :method :save-project)
			    (new 'button :label "Dismiss"
				 :target self :method :dismiss)))))

(define-method save-project save-project-dialog ()
  (add-message 
   %%messenger    
   (if (save-project-image :force)
       "Successfully saved project."
       "Could not save project!")))

(define-method save-project browser ()
  (let ((dialog (new 'window 
		     :title "Save current project"
		     :child (new 'save-project-dialog))))
    (assert (windowp dialog))
    (drop-dialog self dialog)))

;;; Splash screen

(defparameter *splash-screen-text*
  "Welcome to the Blocky multimedia programming language.
Copyright (C) 2006-2012 by David T O'Toole <dto@ioforms.org> 
This program is free software: you can redistribute it and/or 
modify it under the terms of the GNU General Public License.
For more information, see the included file 'COPYING',
or visit the language's home page: http://blocky.io

You may press F1 for help at any time, 
or press Alt-X to bring up the system menu.")

;; (define-block splash-logo)

;; (define-method update splash-logo ()
;;   (change-image self "blocky-big"))

;; (define-block-macro splash-screen
;;     (:super :list
;;      :fields 
;;      ((category :initform :system))
;;      :inputs
;;      ((new 'splash-logo)
;;       (new 'text *splash-screen-text*))
;;      :initforms 
;;      ((later 5.0 (destroy self)))))

;; (define-method update splash-screen ()
;;   (mapc #'update %inputs)
;;   (center self)
;;   (layout-vertically self))

    
;;; browser.lisp ends here

