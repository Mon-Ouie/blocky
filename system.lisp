;;; system.lisp --- one block to rule them all

;; Copyright (C) 2010, 2011  David O'Toole

;; Author: David O'Toole %dto@gnu.org
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

(in-package :ioforms)

(defvar *system* nil)

(defparameter *system-menu*
  '((:name "Project"
     :subtree
     ((:name "Create a new project" :action :create-project)
      (:name "Open an existing project" :action :open-existing-project)
      (:name "Save current changes" :action :save-changes)
      (:name "Show current changes without saving" :action :show-changes)
      (:name "Export as archive" :action :export-archive)
      (:name "Export as application" :action :export-application)
      (:name "Publish to web" :action :publish-web)
      (:name "Publish to community cloud" :action :publish-community)
      (:name "Publish to FTP" :action :publish-ftp)
      (:name "Edit preferences" :action :edit-preferences)
      (:name "Quit IOFORMS" :action :quit-ioforms)))
    (:name "Edit"
     :subtree
     ((:name "Cut" :action :cut)
      (:name "Copy" :action :copy)
      (:name "Paste" :action :paste)
      (:name "Paste as new workspace" :action :paste-as-new-workspace)
      (:name "Select all" :action :select-all)
      (:name "Clear selection" :action :clear-selection)))
    (:name "Resources"
     :subtree
     ((:name "Import new resource" :action :import-resources)
      (:name "Edit resource" :action :edit-resource)
      (:name "Search resources" :action :search-resources)
      (:name "Export resource(s)" :action :export-resources)
      (:name "Browse resources"
       :subtree
       ((:name "Browse objects" :action :browse-objects)
	(:name "Browse blocks" :action :browse-code)
	(:name "Browse images" :action :browse-images)
	(:name "Browse sounds" :action :browse-sounds)
	(:name "Browse music" :action :browse-music)
	(:name "Browse fonts" :action :browse-fonts)
	(:name "Browse code" :action :browse-code)))))
    (:name "Tools" 
     :subtree
     ((:name "Create a command prompt" :action :create-prompt)
      (:name "Create a note" :action :create-note)
      (:name "Version control" :action :version-control)))
    (:name "Workspace" :subtree
     ((:name "Switch to workspace" :subtree
	      ((:name "Workspace 1" :action :workspace-1)
	       (:name "Workspace 2" :action :workspace-2)
	       (:name "Workspace 3" :action :workspace-3)
	       (:name "Workspace 4" :action :workspace-4)))
      (:name "Go back to the previous workspace" :action :previous-workspace)
      (:name "Create a new workspace" :action :create-workspace)
      (:name "Rename this workspace" :action :rename-workspace)
      (:name "Delete this workspace" :action :delete-workspace)
      (:name "Workspace settings" :action :configure-workspaces)))
    (:name "Windows"
     :subtree
     ((:name "Create a new window" :action :create-window)
      (:name "Switch to the next window" :action :next-window)
      (:name "Switch to window" :action :switch-window)
      (:name "Close this window" :action :close-window)))
    (:name "Devices"
     :subtree
     ((:name "Browse available devices" :action :browse-devices)
      (:name "Scan for devices" :action :scan-devices)
      (:name "Configure joystick" :action :configure-joystick)
      (:name "Configure camera" :action :configure-camera)
      (:name "Configure microphone" :action :configure-microphone)
      (:name "Configure dance pad" :action :configure-dance-pad)))
    (:name "Help"
     :subtree
     ((:name "General help" :action :general-help)
      (:name "Examples" :action :show-examples)
      (:name "Language Reference" :action :language-reference)
      (:name "Licensing information" :action :licensing-information)))))
    
(defblock system
  (type :initform :system)
  (shell :initform nil)
  (running :initform nil))

(define-method initialize system ()
  (setf *system* self))

(define-method create-prompt system ()
  (add-block *script* (new listener) 100 100))

(define-method create-project system ())

(define-method open-existing-project system ())
  ;; TODO how to get arguments?
;  (open-project project))

(define-method save-changes system ()
  (save-project))

(define-method save-everything system ()
  (save-project :force))

(define-method ticks system ()
  (get-ticks))

(define-method quit-ioforms system ()
  ;; TODO destroy textures
  (ioforms:quit t))


;; (define-method get-blocks system ()
;;   %children)

;; (define-method count-blocks system ()
;;   (with-fields (children) self
;;     (apply #'+ (mapcar #'/count children))))

;; (define-method start system ()
;;   (setf %running t))

;; (define-method stop system ()
;;   (setf %running nil))

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


;; (define-method draw system (destination)
;;   FIXME)

;;; system.lisp ends here

