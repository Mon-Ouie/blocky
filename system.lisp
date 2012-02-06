;;; system.lisp --- one block to rule them all

;; Copyright (C) 2010, 2011  David O'Toole

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

(defvar *system* nil)

(define-block system
  (type :initform :system)
  (shell :initform nil)
  (running :initform nil))

(define-method show-copyright-notice system ()
  (let ((box (new text *copyright-notice*)))
    (add-block *shell* box 80 80)
    ;; (center box)
    (resize-to-scroll box 80 24)
    (end-of-line box)))

(define-method save-before-exit system ())

;; Creating a project

;; (define-visual-macro create-project 
;;     (:super list

(define-method create-project system ())

;; (define-method open-existing-project system ((project-name string :default " "))
	     
(define-method save-changes system ()
  (save-project))

(define-method save-everything system ()
  (save-project :force))

(defun shell ()
  (symbol-value '*shell*))

(define-method initialize system ()
  (setf *system* (find-uuid self)))

(define-method create-trash system ()
  (add-block (shell) (new trash) 100 100))

(define-method create-text system ()
  (add-block (shell) (new text) 100 100))

(define-method create-listener system ()
  (add-block (shell) (new listener) 100 100))

(define-method ticks system ()
  (get-ticks))

(define-method quit-blocky system ()
  ;; TODO destroy textures
  (blocky:quit t))

(defparameter *system-menu*
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
      (:label "Paste as new workspace" :action :paste-as-new-workspace)
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
    
;;; system.lisp ends here

