;;; -*- Mode: Lisp; -*-

;; ASDF Manual: http://constantly.at/lisp/asdf/

(defpackage :ioforms-asd)

(in-package :ioforms-asd)

(asdf:defsystem ioforms
  :name "ioforms"
  :version "0.3"
  :maintainer "David T O'Toole <dto1138@gmail.com>"
  :author "David T O'Toole <dto1138@gmail.com>"
  :license "General Public License (GPL) Version 3"
  :description "IOFORMS is a visual game creation tool."
  :serial t
  :depends-on (:lispbuilder-sdl 
	       :lispbuilder-sdl-image 
	       :lispbuilder-sdl-gfx
	       :lispbuilder-sdl-ttf
	       :lispbuilder-sdl-mixer
	       :cl-fad
	       :cl-opengl)
  :components ((:file "ioforms")
	       (:file "rgb")
	       (:file "keys")
	       (:file "math")
	       (:file "logic")
	       (:file "prototypes")
	       (:file "console")
	       (:file "blocks")
	       (:file "widgets")
	       (:file "shell")
	       (:file "things")
	       (:file "worlds")))
;;	       (:file "path")
	       
	       
