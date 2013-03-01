(require 'sb-posix)

(push (merge-pathnames "lib/" (values *default-pathname-defaults*))
      asdf:*central-registry*)

(push #p"~/2x0ng/" asdf:*central-registry*)

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

