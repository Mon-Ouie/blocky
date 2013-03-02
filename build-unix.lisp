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

(sb-ext:save-lisp-and-die "2x0ng.exe"
			  :toplevel (lambda ()
				      (sb-posix:putenv
				       (format nil "SBCL_HOME=~A" 
				      	       #.(sb-ext:posix-getenv "SBCL_HOME")))
				      (setf blocky::*executable* t)
				      (2x0ng:2x0ng)
				      0)
			  :executable t)

