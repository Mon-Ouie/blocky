;;; logic.lisp --- procedural content generation tools

;; Copyright (C) 2009, 2010, 2011  David O'Toole

;; Author: David O'Toole %dto@gnu.org
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see %http://www.gnu.org/licenses/.

;;; Licensed material:

;; Some functions in this file are modified versions of functions by
;; Peter Norvig in his book "Paradigms of Artificial Intelligence
;; Programming". The derivative works (i.e. the functions in this
;; file) are redistributed here under the terms of the General Public
;; License as given above.

;; You can find more information on Norvig's book at his website:

;; http://www.norvig.com/paip.html

;; The full license for the PAIP code, which governs the terms of
;; said redistribution under the GPL, can be found at norvig.com:

;; http://www.norvig.com/license.html

(in-package :ioforms)

;;; Grammars

;; http://en.wikipedia.org/wiki/Context-free_grammar

;; Generate random sentences from context-free grammars, and then
;; interpret them how you want. 

(defvar *grammar* nil
  "The current context-free grammar used for sentence generation.
This is an association list of the form:

    ((VARIABLE >> EXPANSIONS)
     (VARIABLE >> EXPANSIONS)
     ...)

Where EXPANSIONS is a list of alternatives, each of which may be
either (1) single symbols or (2) a list of symbols, representing
concatenation.")

(defun one-of (set)
  (list (nth (random (length set)) set)))

(defun left-hand-side (rule)
  (first rule))

(defun right-hand-side (rule)
  (rest (rest rule)))

(defun expansions (variable)
  (right-hand-side (assoc variable *grammar*)))

(defun generate (phrase)
  "Generate a random phrase using the grammar in `*grammar*'."
  (cond ((listp phrase)
	 (apply #'append (mapcar #'generate phrase)))
	((expansions phrase)
	 (generate (one-of (expansions phrase))))
	(t (list phrase))))

;;; Memoization facility

(defmacro defun-memo (name args memo-args &body body)
  "Define a memoized function named NAME.
ARGS is the lambda list giving the memoized function's arguments.
MEMO-ARGS is a list with optional keyword arguments for the
memoization process: :KEY and :TEST."
  `(memoize (defun ,name ,args . ,body) ,@memo-args))

(defun memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

(defun get-memo-table (fn-name)
  (get fn-name 'memo))

;;; logic.lisp ends here
