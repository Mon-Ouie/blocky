;;; example3.lisp --- something more interesting

;; Copyright (C) 2011  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
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

;;; Preamble

(defpackage :example3 
    (:use :ioforms :common-lisp))
  
(in-package :example3)

(setf *screen-width* 640)
(setf *screen-height* 480)
(setf *window-title* "blocky.io")
(enable-key-repeat 9 3)

(defparameter *font* "sans-bold-12")

;;; Blocky

(defresource (:name "blocky" :type :image :file "blocky.png"))

(defsprite blocky
  :image "blocky"
  :default-events
  '(((:up) (move :north 5 :pixels))
    ((:down) (move :south 5 :pixels)) 
    ((:right) (move :east 5 :pixels)) 
    ((:left) (move :west 5 :pixels))
    ((:space) (:talk))))

(defparameter *phrases* 
  '("What lovely TTF font rendering!"
    "My name is Blocky and I'm gonna stomp on you."
    "Pardon me. Would you have any Grey Poupon?"
    "Stare deeply into my pixels."
    "May I help you?"
    "How art thou feeling, Avatar?"))
  
(define-method talk blocky ()
  (with-fields (x y) self
    (drop self (new balloon (random-choose *phrases*))
	  100 100)))

;;; Blocky's friend 

(defresource (:name "friend" :type :image :file "friend.png"))

(defsprite friend
  :image "friend")

(defun example3 ()
  (let ((script (new script :menu t))
	(shell (new shell)))
    (new system)
    (open-script shell script)
    (add-block shell)))


    	 ;; (new menu :label "outer menu" 
    	 ;;           :expanded t
    	 ;; 	   :inputs 
    	 ;; 	   (list (new menu :label "move north" :action :move-north)
    	 ;; 		 (new menu :label "move south" :action :move-south)
    	 ;; 		 (new menu :label "move east" :action :move-east)
    	 ;; 		 (new menu :label "move west" :action :move-west)
    	 ;; 		 (new menu :label "other" 
    	 ;; 			   :inputs
    	 ;; 			   (list (new menu :label "move north" :action :move-north)
    	 ;; 				 (new menu :label "move south" :action :move-south)
    	 ;; 				 (new menu :label "move east" :action :move-east)
    	 ;; 					      (new menu :label "move west" :action :move-west))))))
	     
;;; example3.lisp ends here
