(defresource 
  (:name "cloud1" :type :image :file "cloud1.png")
  (:name "cloud2" :type :image :file "cloud2.png")
  (:name "cloud3" :type :image :file "cloud3.png")
  (:name "cloud4" :type :image :file "cloud4.png")
  (:name "flarestar" :type :image :file "flarestar.png"))

(defvar *cloud-images* (list "cloud1" "cloud2" "cloud3" "cloud4"))

(defsprite cloud 
  :image (random-choose *cloud-images*)
  :blend :additive ;; for nice transparency.
  :direction (random-direction)
  :x (+ 80 (random 500)) 
  :y (+ 80 (random 500)))

(define-method update cloud ()
  (with-fields (direction) self
    ;; move straight
    (move self direction 0.05) 
    ;; but with slight jitter
    (move self (random-direction)
	  (random 0.03)) 
    ;; occasionally change direction
    (percent-of-time 1 
      (setf direction (left-turn direction)))))

;;; We can define some stars as well; these should move all in the
;;; same direction, only very slightly.

(defresource
  (:name "aquastar" :type :image :file "aquastar.png")
  (:name "greenstar" :type :image :file "greenstar.png")
  (:name "bluestar" :type :image :file "bluestar.png")
  (:name "cosmos" :type :music :file "cosmos.ogg"))

(defvar *star-images* (list "flarestar" "greenstar" 
			    "bluestar" "aquastar"
			    "bluestar" "aquastar"
			    "bluestar" "aquastar"))

;; The repeated entries are to make the repeated star images more
;; common in the random selection.

(defsprite star 
  :image (random-choose *star-images*)
  :blend :additive
  :x (random 500)
  :y (random 500))

(define-method update star ()
  (move self :north 0.02))
