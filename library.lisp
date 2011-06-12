;;; library.lisp --- standard blocks library for ioforms

;; Copyright (C) 2011  David O'Toole

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

;;; Commentary:

;; 

;;; Code:

(in-package :ioforms)

(defblock +
  (type :initform :operators)
  (schema :initform '((:a . :number)
		      (:b . :number)))
  (inputs :initform '(0 0)))

(define-method execute + ()
  (with-fields (results) self
    (when (every #'integerp results)
      (apply #'+ results))))

;; (defblock move-toward
;;   (type :initform :motion)
;;   (schema :initform '(:symbol :integer :symbol))
;;   (inputs :initform '(:north 10 :pixels)))

;; (defblock move
;;   (type :initform :motion)
;;   (schema :initform '(:unit :integer :integer))
;;   (inputs :initform '(:space 0 0)))


;; (defblock move
;;   (type :initform :motion)
;;   (schema :initform '(:symbol :integer :symbol))
;;   (inputs :initform '(:north 10 :pixels)))

;;; Other blocks

;; (defblock say 
;;   (type :initform :message)
;;   (schema :initform '(:string))
;;   (inputs :initform '("Hello!")))

;; (defblock move-toward
;;   (type :initform :motion)
;;   (schema :initform '(:symbol :integer :symbol))
;;   (inputs :initform '(:north 10 :pixels)))

;; (defblock move
;;   (type :initform :motion)
;;   (schema :initform '(:unit :integer :integer))
;;   (inputs :initform '(:space 0 0)))

;; (defblock joystick-button
;;   (type :initform :sensing)
;;   (schema :initform '(:integer :symbol))
;;   (inputs :initform '(1 :down)))

;; (defblock visible?
;;   (type :initform :variables)
;;   (schema :initform nil)
;;   (inputs :initform nil))

;; (defblock set-variable 
;;   (type :initform :variables)
;;   (schema :initform '(:symbol :block))
;;   (inputs :initform '(:n nil)))

;; (defblock animate 
;;   (type :initform :looks)
;;   (schema :initform '(:string))
;;   (inputs :initform '(nil)))

;; (defblock play-music 
;;   (type :initform :sound)
;;   (schema :initform '(:string))
;;   (inputs :initform '("fanfare")))

;; (define-method execute play-music ()
;;   (play-music *target* (first ^results) :loop t))

;; (defblock play-sound 
;;   (type :initform :sound)
;;   (schema :initform '(:string))
;;   (inputs :initform '("boing")))

;; (defblock when 
;;   (type :initform :control)
;;   (schema :initform '(:block :block))
;;   (inputs :initform '(nil nil)))

;; (defblock unless
;;   (type :initform :control)
;;   (schema :initform '(:block :block))
;;   (inputs :initform '(nil nil)))

;; (defblock fire
;;   (type :initform :control)
;;   (schema :initform '(:block))
;;   (inputs :initform '(:south)))

;; (defblock see-player
;;   (type :initform :sensing)
;;   (schema :initform nil)
;;   (inputs :initform nil))

;; (defblock player-direction
;;   (type :initform :sensing)
;;   (schema :initform nil)
;;   (inputs :initform nil))

;; (defblock closer-than
;;   (type :initform :sensing)
;;   (schema :initform '(:block :block :block :block))
;;   (inputs :initform '(10 spaces to player)))
  

;;; library.lisp ends here
