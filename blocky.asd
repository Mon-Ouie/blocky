;;; -*- Mode: Lisp; -*-

;; ASDF Manual: http://constantly.at/lisp/asdf/

(defpackage :blocky-asd)

(in-package :blocky-asd)

(asdf:defsystem blocky
  :name "blocky"
  :version "0.98"
  :maintainer "David T O'Toole <dto@ioforms.org>"
  :author "David T O'Toole <dto@ioforms.org>"
  :license "General Public License (GPL) Version 3"
  :description "BLOCKY is a visual programming language for Common Lisp."
  :serial t
  :depends-on (:lispbuilder-sdl 
	       :lispbuilder-sdl-image 
	       :lispbuilder-sdl-gfx
	       :lispbuilder-sdl-ttf
	       :lispbuilder-sdl-mixer
	       :uuid
	       ;; :quicklisp
	       ;; :buildapp
	       :cl-fad
	       :cl-opengl)
  :components ((:file "blocky")
	       (:file "rgb" :depends-on ("blocky"))
	       (:file "keys" :depends-on ("blocky"))
	       (:file "math" :depends-on ("blocky"))
	       (:file "logic" :depends-on ("blocky"))
	       (:file "prototypes" :depends-on ("blocky"))
	       (:file "console" :depends-on ("prototypes"))
	       (:file "blocks" :depends-on ("console"))
	       (:file "halo" :depends-on ("blocks"))
	       (:file "text" :depends-on ("blocks"))
	       (:file "windows" :depends-on ("blocks"))
	       (:file "trees" :depends-on ("blocks"))
	       (:file "listener" :depends-on ("blocks"))
	       (:file "system" :depends-on ("blocks"))
	       (:file "worlds" :depends-on ("blocks"))
	       (:file "shell" :depends-on ("trees" "windows" "listener" "system" "text" "halo"))
	       (:file "vmacs" :depends-on ("trees" "listener" "system" "shell"))
	       (:file "library" :depends-on ("worlds" "shell" "vmacs"))))
;;	       (:file "path")
	       
	       
