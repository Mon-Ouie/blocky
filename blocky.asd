;;; -*- Mode: Lisp; -*-

;; ASDF Manual: http://constantly.at/lisp/asdf/

(defpackage :blocky-asd)

(in-package :blocky-asd)

(asdf:defsystem blocky
  :name "blocky"
  :version "0.991"
  :maintainer "David T O'Toole <dto@ioforms.org>"
  :author "David T O'Toole <dto@ioforms.org>"
  :license "General Public License (GPL) Version 3"
  :description "BLOCKY is a visual programming language for Common Lisp."
  :serial t
  :depends-on (:lispbuilder-sdl 
	       :lispbuilder-sdl-image 
	       :lispbuilder-sdl-ttf
	       :lispbuilder-sdl-mixer
	       :uuid
	       :cl-fad
	       :cl-opengl)
  :components ((:file "blocky")
	       (:file "rgb" :depends-on ("blocky"))
	       (:file "keys" :depends-on ("blocky"))
	       (:file "math" :depends-on ("blocky"))
	       (:file "logic" :depends-on ("blocky"))
	       (:file "prototypes" :depends-on ("blocky"))
	       (:file "quadtree" :depends-on ("blocky"))
	       (:file "console" :depends-on ("prototypes" "quadtree"))
	       (:file "blocks" :depends-on ("console" "prototypes"))
	       (:file "words" :depends-on ("blocks"))
	       (:file "halo" :depends-on ("blocks"))
	       (:file "trees" :depends-on ("blocks"))
	       (:file "basic" :depends-on ("blocks"))
	       (:file "text" :depends-on ("blocks"))
	       (:file "minibuffer" :depends-on ("basic"))
	       (:file "program" :depends-on ("basic"))
	       (:file "buffers" :depends-on ("blocks"))))
	       ;; (:file "syntax" :depends-on ("blocks"))))
	       
