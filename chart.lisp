;;; 

(define-prototype chart (:parent =page=)
  (zoom :initform 2)
  (bars :initform 1)
  (beats-per-bar :initform 4))

(define-method make chart (&key zoom bars name)
  (/initialize self 
	       ;; reserve rightmost column for action buttons
	       :width (1+ (length *dance-arrows*))
	       :height (* <beats-per-bar> zoom bars)
	       :name name)
  (with-field-values (height width grid) self
    (setf <zoom> (or <zoom> zoom))
    (setf <bars> (or <bars> bars))
    (dotimes (row height)
      (vector-push-extend (clone =command-cell=)
      			  (aref grid row (1- width)))
      (dotimes (column (1- width))
	(let ((step (clone =step=)))
	  (/set step (nth column *dance-arrows*)) 
	  (/off step)
	  (vector-push-extend step (aref grid row column)))))))

(define-method row-steps chart (row)
  (with-field-values (width height) self
    (when (< row height)
      (let (steps)
	(dotimes (column (length *dance-arrows*))
	  (let ((step (/top-cell self row column)))
	    (when (and step (/is-on step))
	      (push (/get step) steps))))
	(sort steps #'string<)))))
  
(define-method row-list chart (row)
  (with-field-values (width height) self
    (when (< row height)
      (let (steps)
	(dotimes (column (length *dance-arrows*))
	  (let ((step (/top-cell self row column)))
	    (if (/is-on step)
		(push (/get step) steps)
		(push nil steps))))
	(nreverse steps)))))

(define-method row-command chart (row)
  (with-field-values (width grid) self
    (/top-cell self row (length *dance-arrows*))))

;;; Looping samples

(define-prototype looper (:parent =voice=)
  sample 
  (point :initform 0)
  playing)

(define-method play voice (&optional sample)
  (setf <sample> sample)
  (setf <point> 0)
  (setf <playing> t))

(define-method halt looper ()
  (with-field-values (output) self
    (unless (not <playing>)
      (setf <playing> nil)
      (dotimes (n (length output))
	(setf (aref output n) 0.0)))))

(define-method run looper ()
  (with-field-values (output playing) self
    (when (and <sample> playing)
      (let* ((input (get-sample-buffer <sample>))
	     (input-size (length input))
	     (output-size (length output))
	     (p <point>))
	(dotimes (n output-size)
	  (when (= p (- input-size 1))
	    (setf p 0)
	    (setf (aref output n)
		  (aref input p))
	    (incf p)))
	(setf <point> p)))))

;;; System status display widget

(defvar *status* nil)

(defparameter *margin-size* 16)

(defblock status 
  (height :initform *default-shell-height*)
  (width :initform (+  (* 3 *medium-arrow-width*)
			(* 2 *margin-size*))))

(define-method render status ()
  (with-fields (image width height) self
    (draw-box 0 0 width height :stroke-color ".black" :color ".black" 
	      :destination image)
    (let ((x 0) 
	  (y 0))
      (dolist (row '((:upleft :up :upright) 
		     (:left nil :right) 
		     (:downleft :down :downright)))
	(setf x 0)
	(dolist (button row)
	  (when button
	    (let ((icon (arrow-image button))
		  (index (get-button-index button)))
	      ;; draw a rectangle if the button is pressed
	      ;; TODO nice shaded glowing panels
	      (when (plusp (poll-joystick-button index))
		(draw-box x y *medium-arrow-height* *medium-arrow-height*
			  :stroke-color ".dark orange" :color ".dark orange" :destination image))
	      ;; draw the icon above the rectangle, if any
	      (draw-resource-image icon x y :destination image)))
	  (incf x *medium-arrow-width*))
	(incf y *medium-arrow-height*)))))

(defvar *engine* nil "Dance engine sound generator.")

;; (define-prototype dance-frame (:parent blocky:=split=))

;; (defparameter *qwerty-keybindings*
;;   '(;; arrow key cursor movement
;;     ("UP" nil :move-cursor-up)
;;     ("DOWN" nil :move-cursor-down)
;;     ("LEFT" nil :move-cursor-left)
;;     ("RIGHT" nil :move-cursor-right)
;;     ;; emacs-style cursor movement
;;     ("A" (:control) :beginning-of-line)
;;     ("E" (:control) :end-of-line)
;;     ("F" (:control) :move-cursor-right)
;;     ("B" (:control) :move-cursor-left)
;;     ;; editing keys
;;     ("HOME" nil :beginning-of-line)
;;     ("END" nil :end-of-line)
;;     ("PAGEUP" nil :move-beginning-of-column)
;;     ("PAGEDOWN" nil :move-end-of-column)
;;     ;; switching windows
;;     ("TAB" nil :switch-pages)
;;     ("TAB" (:control) :switch-pages)
;;     ("LEFT" (:control) :select-left-page)
;;     ("RIGHT" (:control) :select-right-page)
;;     ;; dropping commonly-used cells
;;     ("1" (:control) :drop-data-cell)
;;     ("2" (:control) :drop-command-cell)
;;     ("3" (:control) :drop-button-cell)
;;     ;; toggling arrows
;;     ("1" (:alt) :toggle-left)
;;     ("2" (:alt) :toggle-down)
;;     ("3" (:alt) :toggle-up)
;;     ("4" (:alt) :toggle-right)
;;     ;; performing operations like clone, erase
;;     ("UP" (:control) :apply-right)
;;     ("DOWN" (:control) :apply-left)
;;     ("LEFTBRACKET" nil :apply-left)
;;     ("RIGHTBRACKET" nil :apply-right)
;;     ;; marking and stuff
;;     ("SPACE" (:control) :set-mark)
;;     ("SPACE" (:alt) :clear-mark)
;;     ("SPACE" (:meta) :clear-mark)
;;     ;; numeric keypad
;;     ("KP8" nil :move-cursor-up)
;;     ("KP2" nil :move-cursor-down)
;;     ("KP4" nil :move-cursor-left)
;;     ("KP6" nil :move-cursor-right)
;;     ("KP8" (:control) :apply-right)
;;     ("KP2" (:control) :apply-left)
;;     ("KP4" (:control) :select-left-page)
;;     ("KP6" (:control) :select-right-page)
;;     ;; entering data and confirm/cancel
;;     ("RETURN" nil :enter-or-exit)
;; ;;    ("RETURN" (:control) nil :exit) ;; see also handle-key
;;     ("ESCAPE" nil :cancel)
;;     ("G" (:control) :cancel)
;;     ;; view mode
;;     ("F9" nil :image-view)
;;     ("F10" nil :label-view)
;;     ;; other
;;     ("X" (:control) :goto-prompt)
;;     ("X" (:alt) :goto-prompt)
;;     ("X" (:meta) :goto-prompt)
;;     ("T" (:control) :next-tool)))

;; (define-method install-keybindings dance-frame ()
;;   (dolist (binding (case *user-keyboard-layout*
;; 		     (:qwerty *qwerty-keybindings*)
;; 		     (otherwise *qwerty-keybindings*)))
;;     (/generic-keybind self binding)))

;;; Step charts are pages full of steps

(defcell step
  (arrow :initform :blank)
  (state :iniform nil))

(define-method get step () <arrow>)

(define-method set step (new-arrow)
  (assert (member new-arrow *dance-phrase-symbols*))
  (with-fields (arrow image) self
    (setf arrow new-arrow)
    (setf image (arrow-image new-arrow))))

(define-method is-on step ()
  <state>)

(define-method on step ()
  (setf <state> t)
  (setf <image> (arrow-image <arrow>)))

(define-method off step ()
  (setf <state> nil)
  (setf <image> (arrow-image :blank)))

(define-method toggle step ()
  (with-fields (state) self
    (if (/is-on self)
	(/off self)
	(/on self))))

(define-method activate step ()
  (/toggle self))

(define-method print step ()
  (format nil "~S" <arrow>))

(define-method read step (string)
  (let ((input (read-from-string string)))
    (assert (member input *dance-phrase-symbols*))
    (setf <arrow> input)))

(define-method initialize step (&optional (arrow :blank))
  (/set self arrow)
  (/off self))

;; (defun dance ()
;;   (blocky:message "Initializing DANCE...")
;;   (setf blocky:*window-title* "DANCE")
;;   (clon:initialize)
;;   (setf blocky:*dt* 20) ;; 20 millisecond timestep
;;   (setf blocky:*resizable* t) ;; We want the game window to be resizable.
;;   (blocky:set-screen-height *window-height*)
;;   (blocky:set-screen-width *window-width*)
;;   (let* ((prompt (clone =dance-prompt=))
;; 	 (help (clone =help-textbox=))
;; 	 (help-prompt (clone =help-prompt=))
;; 	 (quickhelp (clone =formatter=))
;; 	 (form (clone =form=))
;; 	 (form2 (clone =form= "*scratch*"))
;; 	 (engine (clone =tracker=))
;;  	 (status (clone =status=))
;; 	 (terminal (clone =narrator=))
;; 	 (frame (clone =dance-frame=))
;; 	 (stack (clone =stack=)))
;;     ;;
;;     (setf *form* form)
;;     (setf *engine* engine)
;;     (setf *physics-function* #'(lambda ()
;; 				 (when *engine*
;; 				   (/update-timers *engine*))))
;;     (setf *prompt* prompt)
;;     (setf *terminal* terminal)
;;     (setf *frame* frame)
;;     (labels ((resize-widgets ()
;; 	       (/say terminal "Resizing to ~S" (list :width *screen-width* :height *screen-height*))
;; 	       (/resize prompt :height *prompt-height* :width *screen-width*)
;; 	       (/resize form :height (- *screen-height* *terminal-height* 
;; 					*status-height*
;; 				       *prompt-height* *pager-height*) 
;; 		       :width (- *screen-width* *sidebar-width* 2))
;; 	       (/resize form2 :height (- *screen-height* 
;; 					 *status-height* 
;; 					 *terminal-height* 
;; 					 *prompt-height* 
;; 					 *pager-height*) 
;; 			:width (- *sidebar-width* 2))
;; 	       (/resize-to-scroll help :height (- *screen-height* *pager-height*) :width *screen-width*)
;; 	       (/resize frame :width (- *screen-width* 1) :height (- *screen-height* *pager-height* *prompt-height* *status-height* *terminal-height*))
;; 	       (/resize terminal :height *terminal-height* :width *screen-width*)
;; 	       (/resize quickhelp :height *quickhelp-height* :width *quickhelp-width*)
;; 	       ;;
;; 	       (/resize status :height *status-height* :width *window-width*)
;; 	       (/move status :x 0 :y 0)
;; 	       (/show status)
;; 	       (setf *status* status)
;; 	       (/resize engine :height 700 :width 500)
;; 	       (/move engine :x 0 :y 0)
;; 	       (/show engine)
;; 	       (/set-receiver engine engine)
;; 	       ;;	           (/set-receiver prompt engine)
;; 	       (/resize stack :width *screen-width* :height (- *screen-height* *pager-height* *prompt-height*))
;; 	       (/install-keybindings engine)
;; 	       (/move quickhelp :y (- *screen-height* *quickhelp-height* *pager-height*) :x (- *screen-width* *quickhelp-width* *quickhelp-spacer*))
;; 	       (/auto-position *pager*)))
;;       (add-hook 'blocky:*resize-hook* #'resize-widgets))
;;     ;;
;;     (/resize prompt :height *prompt-height* :width *screen-width*)
;;     (/move prompt :x 0 :y 0)
;;     (/show prompt)
;;     (/install-keybindings prompt)
;;     (/install-keybindings frame)
;;     (/say prompt "Welcome to DANCE. Press ALT-X to enter command mode, or F1 for help.")
;;     (/set-mode prompt :forward) ;; don't start with prompt on
;;     (/set-receiver prompt frame)
;;     ;; 
;;     (/resize form :height (- *screen-height* *terminal-height* 
;; 			    *prompt-height* *pager-height*) 
;; 	    :width (- *screen-width* *sidebar-width*))
;;     (/move form :x 0 :y 0)
;;     (/set-prompt form prompt)
;;     (/set-narrator form terminal)
;;     ;;
;;     (/resize-to-scroll help :height (- *screen-height* *pager-height*) :width *screen-width*)
;;     (/move help :x 0 :y 0)
;;     (setf (field-value :read-only help) t)
;;     (let ((text	(find-resource-object "dance-help-message")))
;;       (/set-buffer help text))
;;     ;;
;;     (/resize help-prompt :width 10 :height 10)
;;     (/move help-prompt :x 0 :y 0)
;;     (/hide help-prompt)
;;     (/set-receiver help-prompt help)
;;     ;;
;;     (/resize form2 :height (- *screen-height* *terminal-height* *prompt-height* *pager-height*) :width *sidebar-width*)
;;     (/move form2 :x 0 :y 0)
;;     (setf (field-value :header-style form2) nil)
;;     (/set-prompt form2 prompt)
;;     (/set-narrator form2 terminal)
;;     ;;
;;     (blocky:halt-music 1000)
;;     ;;
;;     ;; (/resize help :height 540 :width 800) 
;;     ;; (/move help :x 0 :y 0)
;;     (/resize-to-scroll help :height 540 :width 800) 
;;     (/move help :x 0 :y 0)
;;     (let ((text	(find-resource-object "dance-help-message")))
;;       (/set-buffer help text))
;;     ;;
;;     (/resize engine :height 500 :width 500)
;;     (/move engine :x 0 :y 0)
;;     (/show engine)
;;     ;;
;;     (/resize quickhelp :height *quickhelp-height* :width *quickhelp-width*)
;;     (/move quickhelp :y (- *screen-height* *quickhelp-height* *pager-height*) :x (- *screen-width* *quickhelp-width* *quickhelp-spacer*))
;;     (let ((text	(find-resource-object "dance-quickhelp-message")))
;;       (dolist (line text)
;;     	(dolist (string line)
;;     	  (funcall #'send nil :print-formatted-string quickhelp string))
;;     	(/newline quickhelp)))
;;     ;;
;;     (/resize stack :width *screen-width* :height (- *screen-height* *pager-height*))
;;     (/move stack :x 0 :y 0)
;;     (/set-children stack (list frame status terminal prompt))
;;     ;;
;;     (/resize frame :width *screen-width* :height (- *screen-height* *pager-height* *terminal-height* *prompt-height*))
;;     (/move frame :x 0 :y 0)
;;     (/set-children frame (list form form2))
;;     ;;
;;     (/resize terminal :height *terminal-height* :width *screen-width*)
;;     (/move terminal :x 0 :y (- *screen-height* *terminal-height*))
;;     (/set-verbosity terminal 0)
;;     ;;
;;     ;;
;;     (setf *pager* (clone =pager=))
;;     (/auto-position *pager*)
;;     ;;
;;     (/add-page *pager* :engine (list engine))
;;     (/add-page *pager* :chart (list prompt stack frame terminal status quickhelp))
;;     (/add-page *pager* :help (list help-prompt help))
;;     (/select *pager* :chart)
;;     (blocky:reset-joystick)
;;     (blocky:enable-classic-key-repeat 100 100)
;;     (/focus-left *frame*)
;;     (/label-view (/left-form *frame*))
;;     (run-hook 'blocky:*resize-hook*)
;; ;;    (play-music "electron")
;; ;;    (register-voice-mixer)
;; ))

;; (dance)

;;; Dance step scrolling display

;; 4 columns, use ABXY keys to select column, you can play a
;; whole phrase to loop stuff and/or interrupt loops at any time
;; left side is beat, right side is etc, up to 4 tracks.

 ;; to do the freestyle UI well, after making the step charts, can be
 ;; 4 sets of ddr-style arrows scrolling up in 4 big columns for the
 ;; beat for the pads for the bass or whatever the song requires then
 ;; you can start something looping after you complete dancing the
 ;; pattern successfully once and switch to another of the 4 tracks
 ;; using one of the corner buttons.  change relative volumes of each
 ;; track by holding corner button while pressing up/down left-right
 ;; could be Pan ok then where do you see the step options as you
 ;; freestyle? make an onscreen palette.

;; (defvar *commander* nil)

;; (define-prototype commander (:parent blocky:=formatter=)
;;   (display-current-line :initform t))

;; (define-method insert commander (arrow)
;;   (/print-formatted-string self (arrow-formatted-string arrow)))


(define-method play dance-frame (sample)
  (play-sample sample))

(define-method play-music dance-frame (music)
  (play-music music :loop t))

(define-method left-form dance-frame ()
  (nth 0 <children>))

(define-method right-form dance-frame ()
  (nth 1 <children>))

(define-method other-form dance-frame ()
  (ecase <focus>
    (0 (/right-form self))
    (1 (/left-form self))))

(define-method left-page dance-frame ()
  (field-value :page (/left-form self)))

(define-method right-page dance-frame ()
  (field-value :page (/right-form self)))

(define-method selected-form dance-frame ()
  (nth <focus> <children>))

(define-method left-selected-data dance-frame ()
  (/get-selected-cell-data (/left-form self)))

(define-method right-selected-data dance-frame ()
  (/get-selected-cell-data (/right-form self)))

(define-method focus-left dance-frame ()
  (/focus (/left-form self))
  (/unfocus (/right-form self)))

(define-method focus-right dance-frame ()
  (/focus (/right-form self))
  (/unfocus (/left-form self)))

(define-method refocus dance-frame ()
  (ecase <focus>
    (0 (/focus-left self))
    (1 (/focus-right self))))

(define-method select-left-page dance-frame ()
  "Select the left spreadsheet page."
  (/say self "Selecting left page.")
  (/focus-left self)
  (setf <focus> 0))

(define-method select-right-page dance-frame ()
  "Select the right spreadsheet page."
  (/say self "Selecting right page.")
  (/focus-right self)
  (setf <focus> 1))

(define-method switch-pages dance-frame ()
  (let ((newpos (mod (1+ <focus>) (length <children>))))
    (setf <focus> newpos)
    (ecase newpos
      (0 (/left-page self))
      (1 (/right-page self)))))

(define-method apply-left dance-frame ()
  "Move data LEFTWARD from right page to left page, applying current
left side tool to the right side data."
  (let* ((form (/left-form self))
	 (tool (field-value :tool form))
	 (data (/right-selected-data self)))
    (/say self (format nil "Applying LEFT tool ~S to data ~S in LEFT form." tool data))
    (/apply-tool form data)))

(define-method apply-right dance-frame ()
  "Move data RIGHTWARD from left page to right page, applying current
right side tool to the left side data."
  (let* ((form (/right-form self))
	 (tool (field-value :tool form))
	 (data (/left-selected-data self)))
    (/say self (format nil "Applying RIGHT tool ~S to data ~S in RIGHT form." tool data))
    (/apply-tool form data)))

(define-method paste dance-frame (&optional page)
  (let ((source (if page 
		    (find-page page)
		    (field-value :page (/other-form self))))
	(destination (field-value :page (/selected-form self))))
    (multiple-value-bind (top left bottom right) (/mark-region (/selected-form self))
      (multiple-value-bind (top0 left0 bottom0 right0) (/mark-region (/other-form self))
	(let ((source-height (field-value :height source))
	      (source-width (field-value :width source)))
	  (with-fields (cursor-row cursor-column) (/selected-form self)
	    (let* ((height (or (when top (- bottom top))
			       (when top0 (- bottom0 top0))
			       source-height))
		   (width (or (when left (- right left))
			      (when left0 (- right0 left0))
			      source-width))
		   (r0 (or top cursor-row))
		   (c0 (or left cursor-column))
		   (r1 (or bottom (- height 1)))
		   (c1 (or right (- width 1)))
		   (sr (or top0 0))
		   (sc (or left0 0)))
	      (/paste-region destination source r0 c0 sr sc height width))))))))

(define-method make-step-chart dance-frame (&rest parameters)
  (let ((chart (clone =chart=)))
    (/make-with-parameters chart parameters)
    (/visit (/left-form self) chart)))

(define-method view dance-frame (chart)
  (/visit (/left-form self) chart))

(define-method stop-music dance-frame ()
  (halt-music 0))
    
(define-method commands dance-frame ()
  "Syntax: command-name arg1 arg2 ...
Available commands: HELP EVAL SWITCH-PAGES LEFT-PAGE RIGHT-PAGE
NEXT-TOOL SET-TOOL APPLY-LEFT APPLY-RIGHT VISIT SELECT SAVE-ALL
SAVE-MODULE LOAD-MODULE IMAGE-VIEW LABEL-VIEW QUIT VISIT APPLY-TOOL
CLONE ERASE CREATE-PAGE PASTE QUIT ENTER EXIT"
 nil)

;;; Toggling arrows in the form editor

(define-method toggle-left dance-frame ()
  (/toggle (/cell-at self (field-value :cursor-row (/selected-form self)) 0)))

(define-method toggle-down dance-frame ()
  (/toggle (/cell-at self (field-value :cursor-row (/selected-form self)) 1)))

(define-method toggle-up dance-frame ()
  (/toggle (/cell-at self (field-value :cursor-row (/selected-form self)) 2)))

(define-method toggle-right dance-frame ()
  (/toggle (/cell-at self (field-value :cursor-row (/selected-form self)) 3)))

;;; Interactive command prompt

;; see also widgets.lisp

(define-prototype dance-prompt (:parent blocky:=prompt=))

(define-method say dance-prompt (&rest args)
  (apply #'send nil :say *terminal* args))

(define-method goto dance-prompt ()
  (/unfocus (/left-form *frame*))
  (/unfocus (/right-form *frame*))
  (setf <mode> :direct))

(define-method do-after-execute dance-prompt ()
  (/clear-line self)  
  (setf <mode> :forward))

(define-method exit dance-prompt ()
  (/parent>>exit self)
  (/refocus *frame*))
      
