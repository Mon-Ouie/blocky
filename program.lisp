;;; program.lisp --- interactive multimedia forthlike repl/editor

;; Copyright (C) 2011, 2012, 2013  David O'Toole

;; Author: David O'Toole <dto@ioforms.org>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :blocky)

(defparameter *program-keybindings*
  '(
    ;; (:a (:control) :beginning-of-line)
    ;; (:e (:control) :end-of-line)

    (:f (:alt) :forward-word)
    (:b (:alt) :backward-word)
    (:k (:control) :clear-line)
    (:backspace (:alt) :backward-delete-word)
    (:delete (:alt) :delete-word)
    (:d (:alt) :delete-word)
    (:return nil :enter)
    (:x (:control) :exit)
    (:g (:control) :exit)
    (:escape nil :exit)
    (:p (:control) :previous-line)
    (:n (:control) :next-line)
    (:p (:alt) :previous-line)
    (:n (:alt) :next-line)
    (:up nil :previous-line)
    (:down nil :next-line)
    (:left (:alt) :backward-word)
    (:right (:alt) :forward-word)
    (:home nil :beginning-of-line)
    (:end nil :end-of-line)))

(defparameter *cursor-blink-time* 10)

(define-block program
  ;; a list of lists of blocks
  (lines :initform nil)
  ;; "point" is the location of the selected block.
  (point-row :initform 0) 
  (point-column :initform 0)
  ;; the cursor shows where point is
  (cursor-color :initform "yellow")
  (cursor-blink-color :initform "magenta")
  (cursor-blink-clock :initform 0)
  (cursor-alpha :initform 0.6)
  ;;
  (spacing :initform 2)
  (row-spacing :initform 1 :documentation "Number of pixels to add between rows.")
  (mark-row :initform nil)
  (mark-column :initform nil)
  (alignment :initform nil))

(define-method set-mark program ()
  (setf %mark-row %point-row
	%mark-column %point-column))
   
(define-method clear-mark program ()
  (setf %mark-row nil %mark-column nil))

(define-method mark-region program ()
  (with-fields (mark-row mark-column point-row point-column) self
    (if (and (integerp mark-row) (integerp mark-column))
	(values (min mark-row point-row)
		(min mark-column point-column)
		(max mark-row point-row)
		(max mark-column point-column))
	(values nil nil nil nil))))

(define-method initialize program ()
  (initialize%super self)
  ;; start out with a simple blank
  (setf %lines (list (list (new 'word))))
  ;; see command implementations below
  (install-text-keybindings self *program-keybindings*)
  (setf %point-row 0)
  (setf %point-column 0)
  (clear-mark self))

(define-method enter program ()
  (newline self))

(define-method pick program () self)

(define-method hit program (x y)
  (with-fields (lines) self
    (labels ((try (it)
	       (hit it x y)))
      (block trying
	(dolist (line lines)
	  (let ((result (some #'try line)))
	    (when result
	      (return-from trying result))))))))

;;; Emulate the feel of emacs text properties buffers

;; Here we move over complete blocks, not characters.
;; Individual character movement is accomplished by the focused block methods.

(define-method end-of-line program ()
  (setf %point-column (length (nth %point-row %lines))))

(define-method beginning-of-line program ()
  (setf %point-column 0))

(define-method beginning-of-buffer program ()
  (setf %point-row 0 %point-column 0))

(define-method end-of-buffer program ()
  (setf %point-row (1- (length %lines)))
  (end-of-line self))

(define-method current-line program ()
  (nth %point-row %lines))

(define-method thing-at-point program ()
  (nth %point-column (current-line self)))

(define-method forward-word program ()
  (with-fields (lines point-row point-column) self
    (setf point-column (min (1+ point-column)
			    (length (nth point-row lines))))))

(define-method backward-word program ()
  (with-fields (lines point-row point-column) self
    (setf point-column (max 0 (1- point-column)))))

(define-method next-line program ()
  (with-fields (lines point-row point-column) self
    (setf point-row (min (1+ point-row)
			 (1- (length lines))))
    (setf point-column (min point-column 
			    (length (nth point-row lines))))))

(define-method previous-line program ()
  (with-fields (lines point-row point-column) self
    (setf point-row (max 0 (1- point-row)))
    (setf point-column (min point-column
			    (length (nth point-row lines))))))

(define-method newline program ()
  (with-fields (lines point-row point-column) self
    (if (null lines)
	(push (list (new 'word)) lines)
	(if (and (= point-row (1- (length lines)))
		 (= point-column (1- (length (nth point-row lines)))))
	    ;; at end of content
	    (progn (setf lines (append lines (list (list (new 'word)))))
		   (incf point-row)
		   (setf point-column 0))
	    ;; insert line break
	    (let* ((line (nth point-row lines))
		   (line-remainder 
		     (or (subseq line point-column)
			 (list (new 'word))))
		   (lines-remainder (nthcdr (1+ point-row) lines)))
	      ;; truncate current line
	      (setf (nth point-row lines) 
		    (subseq line 0 point-column))
	      ;; insert new line
	      (if (= 0 point-row)
		  (setf (cdr lines)
			(cons line-remainder (cdr lines)))
		  (setf (cdr (nthcdr (- point-row 1) lines))
			(cons (nth point-row lines)
			      (cons line-remainder lines-remainder))))
	      ;; move to new line
	      (incf point-row)			
	      (setf point-column 0))))))

(define-method backward-delete-word program ()
  (with-fields (lines point-row point-column) self
    (if (and (= 0 point-column) 
	     (not (= 0 point-row)))
	(progn 
	  ;;
	  ;; we need to remove a line break.
	  (let ((line (nth (- point-row 1) lines))
		(next-line (nth (+ point-row 1) lines))
		(len (length lines)))
	    (setf lines (append (subseq lines 0 (- point-row 1))
				 (list (concatenate 'string line (nth point-row lines)))
				 (subseq lines (min len (+ point-row 1)))))
	    ;; (setf (cdr (nthcdr (- point-row 1) lines))
	    ;; 	  (nth (+ point-row 1) lines))
	    ;;
	    ;; move cursor too
	    (decf point-row)
	    (setf point-column (length line))))
	;; otherwise, delete within current line.
	(when (not (= 0 point-column))
	  (let* ((line (nth point-row lines))
		 (remainder (subseq line point-column)))
	    (setf (nth point-row lines)
		  (concatenate 'string 
			       (subseq line 0 (- point-column 1))
			       remainder))
	    (decf point-column))))))
    
(define-method end-of-line-p program ()
  (= %point-column
     (1- (length (current-line self)))))

(define-method beginning-of-line-p program ()
  (= %point-column 0))

(define-method top-of-lines-p program ()
  (= %point-row 0)) 

(define-method bottom-of-lines-p program ()
  (= %point-row
     (1- (length %lines))))

(define-method beginning-of-lines-p program ()
  (and (beginning-of-line-p self)
       (top-of-lines-p self)))

(define-method end-of-lines-p program ()
  (and (end-of-line-p self)
       (bottom-of-lines-p self)))

(define-method delete-word program ()
  (with-fields (lines point-row point-column) self
    (if (end-of-line-p self)
	;; just remove line break between words
	(unless (bottom-of-lines-p self)
	  (next-line self)
	  (beginning-of-line self)
	  (backward-delete-word self))
	;; remove a word
	(progn 
	  (forward-word self)
	  (backward-delete-word self)))))

(define-method handle-event program (event)
  (let ((thing (thing-at-point self)))
    (let ((result 
	    (or (and thing (handle-event thing event))
		(block%handle-event self event))))
      (prog1 result 
	(when result (grab-focus thing))))))

;;; Drawing the program

(define-method layout program ()
  (with-fields (width height x y lines spacing) self
    (setf width 0 height 0)
    (let ((y0 y)
	  (x0 x)
	  (h0 0))
      (dolist (line lines)
	(dolist (thing line)
	  (move-to thing x0 y0)
	  (layout thing)
	  (incf x0 (+ (%width thing) spacing))
	  (setf h0 (max h0 (%height thing))))
	(setf width (max width (- x0 x)))
	(incf height h0)
	(incf y0 h0)
	(setf x0 x)
	(setf h0 0)))))

(define-method draw-cursor program (x y width height)
  (with-fields (cursor-color cursor-blink-color cursor-blink-clock focused) self
    (decf cursor-blink-clock)
    (when (minusp cursor-blink-clock)
      (setf cursor-blink-clock *cursor-blink-time*))
    (let ((color (if (or (null focused)
			 (< (truncate (/ *cursor-blink-time* 2))
			    cursor-blink-clock))
		     cursor-color
		     cursor-blink-color)))
      (draw-rectangle x y width height :color color :destination %image))))

(define-method draw-mark program (x y width height)
  (draw-rectangle x y width height :color "white" :destination %image))

(define-method draw-region program (x y width height)
  (draw-rectangle x y width height :color "cyan" :destination %image))

(define-method draw program ()
  ;; (with-style :rounded (draw-background self :color "white"))
  (dolist (line %lines)
    (mapc #'draw line)))

(define-method draw-focus program ()
  (draw-focus (thing-at-point self)))

(define-method tap program (x y)
  (grab-focus self))
  
(define-method update program ()
  (dolist (line %lines)
    (mapc #'update line))
  (layout self))
  
;;; program.lisp ends here
