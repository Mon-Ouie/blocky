(in-package :blocky)

(define-prototype aaa () a b c)
(define-method foo aaa ()
  (list :foo :aaa))
;; define foo, bar, baz
(define-method bar aaa ()
  (list :bar :aaa))
(define-method baz aaa ()
  (list :baz :aaa))
(define-method quux aaa ()
  (list :quux :aaa))

(define-prototype bbb (:parent aaa)
  d e f)
;; do not define "bar"
(define-method foo bbb ()
  (append (list :foo :bbb)
	  (super%foo self)))
(define-method quux bbb ()
  (list :quux :bbb))

(define-prototype ccc (:parent bbb)
  h i j)
;; do not define "foo"
(define-method bar ccc ()
  (append (list :bar :ccc)
	  (super%bar self)))
(define-method quux ccc ()
  (list :quux :ccc))

(defparameter *aaa* (new aaa))
(defparameter *bbb* (new bbb))
(defparameter *ccc* (new ccc))

;; foo: *ccc* -> BLOCKY:CCC -> BLOCKY:BBB* -> BLOCKY:AAA* 
;; baz: *ccc* -> BLOCKY:CCC -> BLOCKY:BBB -> BLOCKY:AAA*

;; qux: *ccc* -> BLOCKY:CCC* -> BLOCKY:BBB* -> BLOCKY:AAA*
;; bar: *ccc* -> BLOCKY:CCC* -> BLOCKY:BBB -> BLOCKY:AAA*

(foo *aaa*)
(foo *bbb*)
(foo *ccc*)

(bar *aaa*)
(bar *bbb*)
(bar *ccc*)

(baz *aaa*)
(baz *bbb*)
(baz *ccc*)

(quux *aaa*)
(quux *bbb*)
(quux *ccc*)

(definer :foo *bbb*)

(next-definer :foo *aaa*)
(next-definer :foo *bbb*)
(next-definer :foo *ccc*)

(next-definer :bar *aaa*)
(next-definer :bar *bbb*)
(next-definer :bar *ccc*)

(next-definer :baz *aaa*)
(next-definer :baz *bbb*)
(next-definer :baz *ccc*)

(next-definer :quux *aaa*)
(next-definer :quux *bbb*)
(next-definer :quux *ccc*)

(next-definition :foo *aaa*)
(next-definition :foo *bbb*)
(next-definition :foo *ccc*)

(next-definition :bar *aaa*)
(next-definition :bar *bbb*)
(next-definition :bar *ccc*)

(next-definition :baz *aaa*)
(next-definition :baz *bbb*)
(next-definition :baz *ccc*)

(next-definition :quux *aaa*)
(next-definition :quux *bbb*)
(next-definition :quux *ccc*)

