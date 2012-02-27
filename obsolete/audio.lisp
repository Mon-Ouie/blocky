;;; Voice objects

;; (defvar *voices* nil)

;; (define-prototype voice () output)

;; (define-method initialize voice (&optional (size *output-chunksize*))
;;   (setf %output (make-array size :element-type 'float :initial-element 0.0))
;;   (register-voice self))

;; (define-method get-output voice ()
;;   %output)

;; ;;(define-method play voice (&rest parameters))
;; (define-method halt voice ())
;; (define-method run voice ())

;; (defun register-voice (voice)
;;   (pushnew voice *voices* :test 'eq))

;; (defun unregister-voice (voice)
;;   (setf *voices*
;; 	(delete voice *voices* :test 'eq)))

;; (defun cffi-sample-type (sdl-sample-type)
;;   (ecase sdl-sample-type
;;     (SDL-CFFI::AUDIO-U8 :uint8) ; Unsigned 8-bit samples
;;     (SDL-CFFI::AUDIO-S8 :int8) ; Signed 8-bit samples
;;     (SDL-CFFI::AUDIO-U16LSB :uint16) ; Unsigned 16-bit samples, in little-endian byte order
;;     (SDL-CFFI::AUDIO-S16LSB :int16) ; Signed 16-bit samples, in little-endian byte order
;;     ;; (SDL-CFFI::AUDIO-U16MSB nil) ; Unsigned 16-bit samples, in big-endian byte order
;;     ;; (SDL-CFFI::AUDIO-S16MSB nil) ; Signed 16-bit samples, in big-endian byte order
;;     (SDL-CFFI::AUDIO-U16 :uint16)  ; same as SDL(SDL-CFFI::AUDIO-U16LSB (for backwards compatability probably)
;;     (SDL-CFFI::AUDIO-S16 :int16) ; same as SDL(SDL-CFFI::AUDIO-S16LSB (for backwards compatability probably)
;;     (SDL-CFFI::AUDIO-U16SYS :uint16) ; Unsigned 16-bit samples, in system byte order
;;     (SDL-CFFI::AUDIO-S16SYS :int16) ; Signed 16-bit samples, in system byte order
;;     ))

;; (defun cffi-chunk-buffer (chunk)
;;   (sdl:fp chunk))

;; (defun buffer-length (buffer)
;;   (let ((type (cffi-sample-type *sample-format*)))
;;     (length (cffi:mem-ref buffer type))))

;; (defun convert-cffi-sample-to-internal (chunk)
;;   (let* ((input-buffer (cffi-chunk-buffer chunk))
;; 	 (type (cffi-sample-type *sample-format*))
;; 	 (size (length (cffi:mem-ref input-buffer type))))
;;     (assert (eq *sample-format* SDL-CFFI::AUDIO-S16LSB)) ;; for now
;;     (let ((output-buffer (make-array size)))
;; 	(prog1 output-buffer
;; 	  (dotimes (n size)
;; 	    (setf (aref output-buffer n)
;; 		  (/ (float (cffi:mem-aref input-buffer type n))
;; 		     32768.0)))))))

;; (defun convert-internal-sample-to-cffi (input output &optional limit)
;;   (let ((type (cffi-sample-type *sample-format*)))
;;     (dotimes (n 128)
;;       (setf (cffi:mem-aref output type n)
;; 	    (truncate (* (cffi:mem-aref input type n)
;; 			 32768.0))))))

;; (defvar *buffer* (make-array 10000 :element-type 'float :initial-element 0.0))

;; (defvar *sample-generator* nil)

;; (defvar *foo* nil)

;; ;; (defun music-mixer-callback (user output size)
;; ;;   (setf *foo* t)
;; ;;   (format t "XXXX ~S" *foo*))

;;   ;; (let ((type (cffi-sample-type *sample-format*)))
;;   ;;   (dotimes (n size)
;;   ;;     (setf (cffi:mem-aref output type n) 0))))

;;   ;; (when *sample-generator*
;;   ;;   (message "Generating samples")
;;   ;;   (funcall generator *buffer*)
;;   ;;   (message "Converting samples to output format...")
;;   ;;   (convert-internal-sample-to-cffi *buffer* output size)
;;   ;;   ))

;; ;; (defun register-sample-generator (generator)
;; ;;   (message "Registering sample generator...")
;; ;;   (setf *sample-generator* generator)
;; ;;   (sdl-mixer:register-music-mixer #'music-mixer-callback))

;; (defun mix-voices (output)
;;   (message "Mixing voices...")
;;   ;; create silence
;;   (dotimes (n *output-chunksize*)
;;     (setf (aref output n) 0.0))
;;   ;; mix in voices
;;   (dolist (voice *voices*)
;;     (run voice)
;;     (let ((input (get-output voice)))
;;       (dotimes (n *output-chunksize*)
;; 	(incf (aref output n)
;; 	      (aref input n))))))

;; ;; (defun register-voice-mixer () 
;; ;;   (message "Registering voice mixer...")
;; ;;   (setf *voices* nil)
;; ;;   (register-sample-generator #'mix-voices))

;; (defvar *buffer-cache* nil)

;; (defun initialize-buffer-cache ()
;;   (setf *buffer-cache* (make-hash-table :test 'eq)))

;; (defun get-sample-buffer (sample)
;;   (let ((chunk (if (stringp sample)
;; 		   (find-resource-object sample)
;; 		   sample)))
;;     ;; (when (null *buffer-cache*)
;;     ;;   (initialize-buffer-cache))
;;     ;; ;; is it cached?
;;     ;; (or (gethash chunk *sample-buffers*)
;;     ;; 	(setf (gethash chunk *sample-buffers*)
;; 	      (convert-cffi-sample-to-internal chunk)))

;; ;;; Regular music/sample functions

;; ;; (defvar *sample-callback* nil)

;; ;; (defun set-sample-callback (func)
;; ;;   (assert (functionp func))
;; ;;   (setf *sample-callback* func))

;; ;; (defvar *music-callback* nil)

;; ;; (defun set-music-callback (func)
;; ;;   (assert (functionp func))
;; ;;   (setf *music-callback* func))
