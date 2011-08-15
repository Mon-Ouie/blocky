;;; -*- Mode: Lisp; -*-

;; ASDF Manual: http://constantly.at/lisp/asdf/

(defpackage :blocky-asd)

(in-package :blocky-asd)

(asdf:defsystem blocky
  :name "blocky"
  :version "0.98"
  :maintainer "David T O'Toole <dto1138@gmail.com>"
  :author "David T O'Toole <dto1138@gmail.com>"
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
	       (:file "language" :depends-on ("console"))
	       (:file "widgets" :depends-on ("language"))
	       (:file "trees" :depends-on ("language"))
	       (:file "listener" :depends-on ("language"))
	       (:file "system" :depends-on ("language"))
	       (:file "things" :depends-on ("language"))
	       (:file "worlds" :depends-on ("things"))
	       (:file "shell" :depends-on ("trees" "listener" "system"))
	       (:file "vmacs" :depends-on ("trees" "listener" "widgets" "system"))
	       (:file "library" :depends-on ("worlds" "shell" "vmacs"))))
;;	       (:file "path")
	       
	       
