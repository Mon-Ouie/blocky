;;; util.lisp --- handy utilities for gluon users

;; Copyright (C) 2010  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :gluon)

(defmacro defgame (module-name 
		   (&key title description
			 (prompt-prototype =prompt=)
			 timestep physics-function
			 held-keys 
			 splash-image splash-function splash-music
			 screen-width screen-height
			 keybindings pages
			 &allow-other-keys)
 		   &body startup-forms)
  `(progn
     (gluon:set-screen-height ,screen-height)
     (gluon:set-screen-width ,screen-width)
     (setf gluon:*physics-function* ,physics-function)
     (setf gluon:*dt* ,timestep)
     ,@startup-forms))

;;; util.lisp ends here
