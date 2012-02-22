;;; system.lisp --- the system menu

;; Copyright (C) 2010, 2011, 2012  David O'Toole

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

;;; Message output window

(define-block messenger :category :terminal)

(defparameter *messenger-columns* 80)
(defparameter *messenger-rows* 7)

(define-method layout messenger ()
  (setf %height (+ (* (font-height *font*) *messenger-rows*)
		   (dash 4)))
  (let ((width 0))
    (block measuring
      (dotimes (n *messenger-rows*)
	(if (<= (length *message-history*) n)
	    (return-from measuring nil)
	    (setf width 
		  (max width 
		       (font-text-width 
			(nth n *message-history*)
			*block-font*))))))
    (setf %width (+ width (dash 5)))))
			     
(define-method draw messenger ()
  (draw-background self)
  (with-fields (x y width height) self
      (let ((y0 (+ y height (- 0 (font-height *font*) (dash 2))))
	    (x0 (+ x (dash 3))))
	(dotimes (n *messenger-rows*)
	  (unless (<= (length *message-history*) n)
	    (draw-string (nth n *message-history*)
			 x0 y0
			 :color "gray70"
			 :font *block-font*)
	    (decf y0 (font-height *font*)))))))

;;; A block representing the current system and project universe

(defvar *system* nil)

(define-block system
  (type :initform :system)
  (shell :initform nil)
  (running :initform nil))

(define-method show-copyright-notice system ()
  (let ((box (new 'text *copyright-notice*)))
    (add-block *world* box 80 80)
    (resize-to-scroll box 80 24)
    (end-of-line box)))

(define-method save-before-exit system ())

;; Creating a project

(define-block-macro create-project-dialog 
    (:super :list
     :fields ((category :initform :system))
     :inputs (:name (new 'string :label "Project name:")
	      :parent (new 'string :label "Create in folder:" 
				   :value (namestring (projects-directory)))
	      :folder-name (new 'string :label "Project folder name:")
	      :messenger (new 'messenger)
	      :buttons (new 'hlist
			    (new 'button :label "Create project"
				 :target self :method :create-project)
			    (new 'button :label "Dismiss"
				 :target self :method :discard)))))

(define-method create-project create-project-dialog ()
  (with-input-values (name parent folder-name) self
    (unless (create-project-image name :folder-name folder-name :parent parent)
      (message "Could not create project."))))

(define-method create-project system-menu ()
  (let ((dialog (new 'create-project-dialog)))
    (add-block (world) dialog)
    (center-as-dialog dialog)))

;; (define-method open-existing-project system ((project-name string :default " "))
	     
(defparameter *system-menu-entries*
  '((:label "Project"
     :inputs
     ((:label "Create a new project" :action :create-project)
      (:label "Open an existing project" :action :open-existing-project)
      (:label "Save current changes" :action :save-changes)
      (:label "Save as new project" :action :save-new)
      ;; (:label "Show current changes without saving" :action :show-changes)
      ;; (:label "Export as archive" :action :export-archive)
      ;; (:label "Export as application" :action :export-application)
      ;; (:label "Publish to web" :action :publish-web)
      ;; (:label "Publish to community site" :action :publish-community)
      ;; (:label "Publish to FTP" :action :publish-ftp)
      ;;(:label "Edit preferences" :action :edit-preferences)
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
      (:label "Discard" :action :destroy)))
    (:label "Resources"
     :inputs
     ((:label "Import new resource" :action :import-resources)
      (:label "Edit resource" :action :edit-resource)
      (:label "Search resources" :action :search-resources)
      (:label "Export resource(s)" :action :export-resources)
      (:label "Browse resources" :action :browse-resources)))
    (:label "Tools" 
     :inputs
     ((:label "Create a Lisp listener" :action :create-listener)
      (:label "Create a text box" :action :create-text)
      (:label "Create a trash can" :action :create-trash)))
    ;; (:label "Workspace" :inputs
    ;;  ((:label "Switch to workspace" :inputs
    ;; 	      ((:label "Workspace 1" :action :workspace-1)
    ;; 	       (:label "Workspace 2" :action :workspace-2)
    ;; 	       (:label "Workspace 3" :action :workspace-3)
    ;; 	       (:label "Workspace 4" :action :workspace-4)))
    ;;   (:label "Go back to the previous workspace" :action :previous-workspace)
    ;;   (:label "Create a new workspace" :action :create-workspace)
    ;;   (:label "Rename this workspace" :action :rename-workspace)
    ;;   (:label "Delete this workspace" :action :delete-workspace)
    ;;   (:label "Workspace settings" :action :configure-workspaces)))
    ;; (:label "Windows"
    ;;  :inputs
    ;;  ((:label "Create a new window" :action :create-window)
    ;;   (:label "Switch to the next window" :action :next-window)
    ;;   (:label "Switch to window" :action :switch-window)
    ;;   (:label "Close this window" :action :close-window)))
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

;;; Headline 

(define-block system-headline)

(defparameter *logo-height* 24)

(defparameter *blocky-title-string* "Blocky 0.92a")

(define-method layout system-headline ()
  (resize self 
	  (+ (dash 2) *logo-height*)
	  *logo-height*))

(define-method draw system-headline ()
  (with-fields (x y) self
    (draw-image "blocky" (+ x (dash 0.5)) y :height *logo-height* :width *logo-height*)
    (draw-string *project*
		 (+ x *logo-height* (dash 2))
		 (+ y (dash 2))
		 :color "white"
		 :font *block-bold*)))

;;; The system menu itself

(define-block-macro system-menu 
    (:super :list 
     :fields 
     ((category :initform :system))
     :initforms 
     ((expand (second %inputs))
      (pin (second %inputs))
      (unfreeze self)
      (setf %locked t))
     :inputs 
     (:headline (new 'system-headline)
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
		      :inputs (mapcar #'make-menu *system-menu-entries*)
		      :target self
		      :category :menu
		      :expanded t)
	    (new 'tree :label "Messages"
		 :expanded nil
		 :inputs (list (new 'messenger))))))))


(defun make-system-menu ()
  (find-uuid 
   (new '"BLOCKY:SYSTEM-MENU")))

(define-method discard system-menu ()
  (discard%super self)
  (setf (%system-menu (world)) nil))

(define-method menu-items system-menu ()
  (%inputs %%menu))

(define-method get-listener system-menu ()
  (first (menu-items self)))
        
(define-method layout system-menu ()
  (layout-vertically self)
  ;; adjust header for project name
  (setf %width 
	(max %width
	     (+ *logo-height* (dash 5) 
		(font-text-width *project*
				 *block-bold*)))))

(define-method can-pick system-menu ()
  t)

(define-method pick system-menu ()
  self)

(define-method draw system-menu ()
  (with-fields (x y width height) self
    (draw-patch self x y (+ x width) (+ y height))
    (mapc #'draw %inputs)
    (draw-focus (get-prompt (get-listener self)))))

(define-method close-menus system-menu ()
  (let ((menus (menu-items self)))
    (when (some #'expandedp menus)
      (mapc #'unexpand menus))))

;; (define-method tap system-menu (x y)
;;   (declare (ignore x y))
;;   (close-menus self))

;; Don't allow anything to be dropped on the menus, for now.

(define-method draw-hover system-menu () nil)

(define-method accept system-menu (thing)
  (declare (ignore thing))
  nil)



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
;;      ((later 5.0 (discard self)))))

;; (define-method update splash-screen ()
;;   (mapc #'update %inputs)
;;   (center self)
;;   (layout-vertically self))

    
;;; system.lisp ends here

