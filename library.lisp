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

;;; Code:

(in-package :ioforms)

(defblock sprite-id :sprite-uuid nil :category :structure)

(define-method initialize sprite-id (thing)
  (with-fields (sprite-uuid) self
    (parent/initialize self)
    (setf sprite-uuid (etypecase thing
			(string thing)
			(object (field-value :uuid thing))))))

(define-method header-height sprite-id ()
  (dash 1 (font-height *block-font*)))

(define-method get-sprite sprite-id ()
  (get-object-by-uuid ^sprite-uuid))

(define-method layout sprite-id ()
  (with-fields (height width) self
    (let ((sprite (get-sprite self)))
      (setf height (dash 1 (header-height self)
			 (field-value :height sprite)))
      (setf width (dash 1 (field-value :width sprite))))))

(define-method draw sprite-id ()
  (with-fields (inputs x y) self
    (let ((sprite (get-sprite self)))
      (draw-background self)
      (let ((header (header-height self)))
	(draw-string (get-some-object-name sprite)
		     (dash 1 x)
		     header
		     :color (find-color self :foreground))
	(draw-image (field-value :image sprite)
		    (dash 1 x)
		    (dash 1 y))))))

;;; Sending to an ID

(define-prototype send (:parent "IOFORMS:LIST")
  (category :initform :message)
  (schema :initform  '((:target :block)
		       (:body :list))))

(define-method header-height send ()
  (let ((id (input self :target)))
    (layout id)
    (dash 2 (field-value :height id))))

(define-method layout send () 
  (with-fields (height width x y) self
    (layout-as-list self)))

(define-method execute send ()
  (with-fields (inputs) self
    (destructuring-bind (id body) inputs
	(with-target (get-sprite id)
	  (execute body)))))

(defblock +
  (type :initform :operators))

;; (define-method execute + ()
;;   (with-fields (results) self
;;     (when (every #'integerp results)
;;       (apply #'+ results))))

(defblock move
  (type :initform :motion)
  (schema :initform '((:x . :integer)
		      (:y . :integer)))
  (inputs :initform '(:north 10 :pixels)))

;; (defblock move-toward
;;   (type :initform :motion)
;;   (schema :initform '(:symbol :integer :symbol))
;;   (inputs :initform '(:north 10 :pixels)))

;;; Other blocks

(defblock say 
  (type :initform :message)
  (schema :initform '((:text . :string)))
  (inputs :initform '("Hello!")))

(defblock change-image 
  (type :initform :looks)
  (schema :initform '((:image . :string)))
  (inputs :initform '("(new image)")))

(defblock play-music 
  (type :initform :sound)
  (schema :initform '(:string))
  (inputs :initform '("fanfare")))

(define-method execute play-music ()
  (play-music *target* (first ^results) :loop t))

(defblock play-sound 
  (type :initform :sound)
  (schema :initform '(:string))
  (inputs :initform '("boing")))


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
