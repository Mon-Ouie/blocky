;;; blocky.el --- Emacs tools for blocky

;; Copyright (C) 2006, 2007, 2008, 2009, 2010 David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: lisp, oop, extensions
;; Version: 0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'rx)

;;; Grabbing UUIDs and inspecting the corresponding objects

(defvar blocky-uuid-regexp 
  "[0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F]")

(defun blocky-inspect-uuid (uuid)
  (interactive "sInspect blocky UUID: ")
  (if (null uuid)
      (message "No UUID provided.")
      (progn 
	(assert (stringp uuid))
	(slime-inspect
	 (format "(blocky::find-object %S)" uuid)))))

(defun blocky-uuid-at-point ()
  (let ((thing (thing-at-point 'word)))
    (when (and (not (null thing))
	       (string-match blocky-uuid-regexp thing))
      thing)))
	  
(defun blocky-uuid-on-this-line ()
  (string-match blocky-uuid-regexp
		(buffer-substring-no-properties
		 (point-at-bol)
		 (point-at-eol))))

(defun blocky-inspect ()
  (interactive)
  (blocky-inspect-uuid (or (blocky-uuid-at-point)
			   (blocky-uuid-on-this-line))))

    

;;; Font-locking

;; Put this in your emacs initialization file to get the highlighting:
;; (add-hook 'emacs-lisp-mode-hook #'blocky-do-font-lock)

(defvar blocky-font-lock-keywords
  `((,(rx (sequence "(" (group "define-method")
		   (one-or-more space)
		   (group (one-or-more (not (any space))))
		   (one-or-more space)
		   (group (one-or-more (not (any space))))))
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face) ;; this still doesn't work
				      ;; properly.
     (3 font-lock-type-face))
    (,(rx (sequence "(" (group "define-prototype")
		   (one-or-more space)
		   (group (one-or-more (not (any space))))))
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
    (,(rx (sequence "(" (group "defmacro%")
		   (one-or-more space)
		   (group (one-or-more (not (any space))))))
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
    (,(rx (sequence "(" (group "defwidget")
		   (one-or-more space)
		   (group (one-or-more (not (any space))))))
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
    (,(rx (sequence "(" (group "defsprite")
		   (one-or-more space)
		   (group (one-or-more (not (any space))))))
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
    (,(rx (sequence "(" (group "defgsprite")
		   (one-or-more space)
		   (group (one-or-more (not (any space))))))
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
    (,(rx (sequence "(" (group "defblock")
		   (one-or-more space)
		   (group (one-or-more (not (any space))))))
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
;    ("\\<\\(\<[^<>]*\>\\)\\>" (1 font-lock-preprocessor-face))
    ("(.*\\(\>\>\\>\\)" (1 font-lock-type-face))))

(defun blocky-do-font-lock ()
  (interactive)
  "Highlight the keywords used in prototype-oriented programming."
  (font-lock-add-keywords nil blocky-font-lock-keywords))

(provide 'blocky)
;;; blocky.el ends here
