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
	       (:file "quadtree" :depends-on ("blocky"))
	       (:file "prototypes" :depends-on ("blocky"))
	       (:file "console" :depends-on ("prototypes" "quadtree"))
	       (:file "blocks" :depends-on ("console"))
	       (:file "library" :depends-on ("blocks"))
	       (:file "dance" :depends-on ("blocks"))
	       (:file "halo" :depends-on ("library"))
	       (:file "text" :depends-on ("library"))
;	       (:file "windows" :depends-on ("library"))
	       (:file "trees" :depends-on ("library"))
	       (:file "listener" :depends-on ("library"))
	       (:file "system" :depends-on ("library"))
	       (:file "worlds" :depends-on ("library"))
	       (:file "universe" :depends-on ("worlds"))
	       (:file "shell" :depends-on ("library"))
	       (:file "meta" :depends-on ("library"))))

;;	       (:file "path")
	       
	       
