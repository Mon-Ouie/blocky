;; -*- Mode: lisp -*-
;;
;; This is the Blocky configuration file "blocky-init.lisp".
;; It is also a Lisp program.
;; You can edit this file to configure Blocky to your preference.
;; Comments begin with semicolons. 
;; "setf" is the standard way to set a variable.
;; The little "Mode" bit on the first line above is a hint for GNU Emacs.
;; It is not required for this file to work. 

(in-package :blocky) ;; don't remove this line. 

(setf *project-directories* (list #P"~/blocky/" #P"~/.blocky/" #P"~/"))
(setf *use-sound* t) ;; "t" means "true"
(setf *fullscreen* nil) ;; "nil" means "false"
(setf *message-logging* t)
(setf *user-keyboard-layout* :qwerty)

;; Joystick configuration

;; to get your button numbers, execute this in emacs:
;; (slime-toggle-trace-fdefinition 'blocky:make-event) 
;; and for your stick axis numbers, use:
;; (slime-toggle-trace-fdefinition 'blocky:update-joystick-axis)

;; (setf *user-joystick-profile* 
;;       '(:name "Generic USB Gamepad" :type :joystick
;; 	:left-analog-stick (0 1)
;; 	:right-analog-stick (3 2)
;; 	:buttons ((2 . :a)
;; 		  (1 . :b)
;; 		  (3 . :x)
;; 		  (0 . :y)
;; 		  (6 . :l2)
;; 		  (7 . :r2)
;; 		  (8 . :select)
;; 		  (9 . :start)
;; 		  (4 . :l1)
;; 		  (5 . :r1))))
      
