;;; make-rlx-win32.lisp --- build script for win32

;; Copyright (C) 2009 Shawn Betts

;; Authors: Shawn Betts <sabetts@emmett.ca>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-user)

(require :asdf)

;;; Customize these to build different games

(defvar *game* "invader")
(defvar *version* "alpha")

;;; Hopefully you don't need to touch anything after this

(defvar *base-pathname* (make-pathname :version nil :name nil :type nil :defaults *load-pathname*))
(defvar *dll-pathname* (translate-pathname *base-pathname* "**/" "**/dlls/"))
(defvar *executable* (translate-pathname *base-pathname* "**/" (format nil "**/~a-~a-win32/~a.exe" *game* *version* *game*)))

(pushnew (translate-pathname *base-pathname* "**/" "**/site/cffi_0.10.3/") asdf:*central-registry*)
(pushnew (translate-pathname *base-pathname* "**/" "**/site/babel_0.3.0/") asdf:*central-registry*)
(pushnew (translate-pathname *base-pathname* "**/" "**/site/alexandria/") asdf:*central-registry*)
(pushnew (translate-pathname *base-pathname* "**/" "**/site/trivial-features_0.4/") asdf:*central-registry*)
(pushnew (translate-pathname *base-pathname* "**/" "**/site/rt-20040621/") asdf:*central-registry*)

(pushnew (translate-pathname *base-pathname* "**/" "**/site/lispbuilder/lispbuilder-sdl/") asdf:*central-registry*)
(pushnew (translate-pathname *base-pathname* "**/" "**/site/lispbuilder/lispbuilder-sdl-image/") asdf:*central-registry*)
(pushnew (translate-pathname *base-pathname* "**/" "**/site/lispbuilder/lispbuilder-sdl-mixer/") asdf:*central-registry*)

(pushnew (translate-pathname *base-pathname* "**/" "**/clon/") asdf:*central-registry*)
(pushnew (translate-pathname *base-pathname* "**/" "**/rlx/") asdf:*central-registry*)

(asdf:oos 'asdf:load-op :cffi)
(require 'sb-posix)
(sb-posix:chdir *dll-pathname*)
(setf *default-pathname-defaults* (make-pathname :directory '(:relative)))
(asdf:oos 'asdf:load-op :rlx)
(pop cffi:*foreign-library-directories*)
(pushnew (make-pathname :directory '(:relative)) rlx:*module-directories*)

(defun main ()
  (rlx:play *game*)
  0)

(ensure-directories-exist *executable*)
(sb-ext:save-lisp-and-die *executable* :toplevel #'main :executable t)
