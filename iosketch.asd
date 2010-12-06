;;; -*- Mode: Lisp; -*-

;; ASDF Manual: http://constantly.at/lisp/asdf/

(defpackage :iosketch-asd)

(in-package :iosketch-asd)

(asdf:defsystem iosketch
  :name "iosketch"
  :version "1.99"
  :maintainer "David O'Toole"
  :author "David O'Toole"
  :license "General Public License (GPL) Version 3"
  :description "An object-oriented graphical 2D game engine."
  :serial t
  :depends-on (;; :trivial-features :trivial-features-tests 
	       :lispbuilder-sdl 
	       :lispbuilder-sdl-image 
	       :lispbuilder-sdl-gfx
	       :lispbuilder-sdl-ttf
	       :lispbuilder-sdl-mixer)
  :components ((:file "iosketch")
	       (:file "prototypes")
	       (:file "math")
	       (:file "rgb")
	       (:file "keys")
	       (:file "console")
	       (:file "widgets")
	       (:file "viewport")
	       (:file "cells")
	       (:file "gcells")
	       (:file "gsprites")
	       (:file "narration")
	       (:file "browser")
	       (:file "mission")
	       (:file "forms")
	       (:file "blocks")
	       (:file "worlds")
;;	       (:file "path")
	       (:file "util")))
	       
	       
