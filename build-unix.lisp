(require 'sb-posix)

(push (merge-pathnames "lib/" *default-pathname-defaults*)
      asdf:*central-registry*)

;; (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
;;                                        (user-homedir-pathname))))
;;   (when (probe-file quicklisp-init)
;;     (load quicklisp-init)))
(ql:quickload '(:lispbuilder-sdl-mixer :lispbuilder-sdl-ttf :lispbuilder-sdl-image :uuid :cl-opengl :cl-fad))
(asdf:load-system :blocky)


(push #p"/home/dto/2x0ng/" asdf:*central-registry*)

(asdf:oos 'asdf:load-op '2x0ng)

(sb-ext:save-lisp-and-die "2x0ng.bin"
			  :toplevel (lambda ()
				      (sb-posix:putenv
				       (format nil "SBCL_HOME=~A" 
				      	       #.(sb-ext:posix-getenv "SBCL_HOME")))
				      (setf blocky::*executable* t)
				      (2x0ng:2x0ng)
				      0)
			  :executable t)

;; Close terminal window...
;; (with-open-file (exe #p"path/to/game.exe" :direction :io :element-type '(unsigned-byte 8))
;;   (file-position exe #x3c)
;;   (let* ((b0 (read-byte exe))
;; 	 (b1 (read-byte exe))
;; 	 (b2 (read-byte exe))
;; 	 (b3 (read-byte exe))
;; 	 (pe-header (dpb (dpb b3 (byte 8 8) b2)
;; 			 (byte 16 16)
;; 			 (dpb b1 (byte 8 8) b0))))
;;     (file-position exe (+ pe-header #x5c))
;;     (write-byte 3 exe)))

