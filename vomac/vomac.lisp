;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :vomac 
    (:use :blocky :common-lisp))
  
(in-package :vomac)

(defresource 
    (:name "bullet" :type :image :file "bullet.png")
    (:name "enemy" :type :image :file "enemy.png")
    (:name "player" :type :image :file "player.png")
    (:name "protostar" :type :image :file "protostar.png")
    (:name "starfield" :type :image :file "starfield.png"))

(defresource 
    (:name "zap" :type :sample :file "zap.wav")
    (:name "crunch" :type :sample :file "crunch.wav")
    (:name "corridor" :type :music :file "corridor.xm"))

(defun vomac ()
  (start (new 'world)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-block player 
  (image :initform "player")
  (default-events :initform
		  '(((:up) (move-toward :up))
		    ((:down) (move-toward :down))
		    ((:left) (move-toward :left))
		    ((:right) (move-toward :right))
		    ((:space) :fire))))

(define-method after-place-hook player ()
  (set-player (world) self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-block bullet :image "bullet" :speed 2)

(define-method update bullet ()
  (move-forward self %speed))

(define-method fire player ()
  (let ((bullet (new 'bullet)))
    (setf (field-value :heading bullet)
	  (radian-angle -90))
    (drop self bullet 20 10)))

(define-block enemy :image "enemy")

(define-method collide bullet (thing)
  (when (is-a 'enemy thing)
    (play-sample "crunch")
    (destroy thing)
    (destroy self)))

(define-method update enemy ()
  (percent-of-time 2 (setf %heading (random (* pi 2))))
  (move-forward self 0.8))

(define-block starfield :image "starfield")
