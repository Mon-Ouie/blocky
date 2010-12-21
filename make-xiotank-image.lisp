(require 'sb-posix)

(push (merge-pathnames "lib/" (values *default-pathname-defaults*))
      asdf:*central-registry*)

(asdf:oos 'asdf:load-op 'ioforms)

(sb-ext:save-lisp-and-die "run-xiotank"
			  :toplevel (lambda ()
				      (sb-posix:putenv
				       (format nil "SBCL_HOME=~A" 
					       #.(sb-ext:posix-getenv "SBCL_HOME")))
				      (ioforms:play "xiotank")
				      0)
			  :executable t)

