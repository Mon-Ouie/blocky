(in-package :ioforms)

(define-prototype bloop () a b c)

(define-method doof bloop ()
  (list :doof :bloop))

(define-method initialize bloop ()
  (list :init))

(define-prototype clap (:parent bloop)
  d e f)

(define-method doof clap ()
  (append (list :doof :clap)
	  (next/doof self)))

(define-method initialize clap ()
  (append (list :init)
	  (next/clap self)))
	  
(defparameter *bloop* (new bloop))

(defparameter *clap* (new clap))

(doof *bloop*)
(doof *clap*)
(definer :initialize *clap*)

(definer :initialize *clap*) ;; can be nil! !!!!

(definer :doof *clap*)
(definition :doof *clap*)
(find-parent (definer :doof *clap*))
(next-definer :doof *clap*)
(next-definition :doof *clap*)

(definer :doof *clap*)


(definer 

(find-parent (find-parent "IOFORMS:CLAP"))

