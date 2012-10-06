;;; glass.el --- transparent panes of emacs glass, augmenting apps

;; Copyright (C) 2012  David O'Toole

;; Author: David O'Toole <dto@ioforms.org>
;; Keywords: lisp, frames

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

;;; Code:

;; Emacs glass frame is transparent

(defvar glass-transparent-alpha 80)
(defvar glass-opaque-alpha 100)

(defun glass-transparent ()
  (set-frame-parameter nil 'alpha glass-transparent-alpha))

(defun glass-opaque ()
  (set-frame-parameter nil 'alpha glass-opaque-alpha))

(defvar glass-font nil)

(defvar glass-use-themes nil)

(defun glass-theme ()
  (when glass-use-themes
    (unless (custom-theme-p 'glass)
      (load-theme 'glass :no-confirm :no-enable)
      (enable-theme 'glass))))

(defun glass-font ()
  (when glass-font 
    (set-frame-font glass-font)))

;;; Glass frame can be fixed on top of other windows

(defvar glass-wm-toggle 2)
(defvar glass-wm-add 1)
(defvar glass-wm-remove 0)

(defun* glass-set-on-top-property (&optional frame (state glass-wm-toggle))
  (x-send-client-message
   frame 0 frame "_NET_WM_STATE" 32
   (list state "_NET_WM_STATE_ABOVE" 0 1)))

(defun glass-on-top (&optional frame)
  (glass-set-on-top-property frame glass-wm-add))

(defun glass-off-top (&optional frame)
  (glass-set-on-top-property frame glass-wm-remove))

;;; Without window-borders

(defun make-hinted-frame (hints)
   (let ((frame (make-frame '((visibility . nil)))))
     (prog1 frame
       (x-change-window-property 
	"_MOTIF_WM_HINTS" hints 
	frame
	"_MOTIF_WM_HINTS" 32 t)
       (make-frame-visible frame))))

(defvar glass-wm-without-decoration '(2 0 0 0 0))

(defun make-frame-without-decoration ()
  (interactive)
  (make-hinted-frame glass-wm-without-decoration))

(defvar glass-frame nil)

(defvar glass-use-special-frame nil)

(defun* make-glass-frame (&key width height)
  (let ((frame (make-frame-without-decoration)))
    (prog1 frame
      (select-frame frame)
      (when width (set-frame-width frame width))
      (when height (set-frame-height frame height))
      (menu-bar-mode -1)
      (glass-transparent)
      (glass-font)
      (glass-on-top))))
  
(defvar glass-showing nil)

(defun glass-live-p ()
  (and glass-frame (frame-live-p glass-frame)))

(defvar glass-scroll-bar-mode nil)

(defun glass-raise (&optional frame)
  (setf glass-scroll-bar-mode scroll-bar-mode)
  (set-scroll-bar-mode 'nil)
  (redirect-frame-focus frame)
  (raise-frame frame)
  (make-frame-visible frame)
  (select-frame frame)
  (glass-on-top)
  (select-frame-set-input-focus frame))

(make-variable-buffer-local 
 (defvar glass-local-mode-line-format nil))
  
(defun* glass-show (&key x y (buffer (current-buffer)) (width 80) (height 12))
  (interactive)
  (when (not (glass-live-p))
    (setf glass-frame (make-glass-frame :width width :height height)))
  (when (and (numberp x) (numberp y))
    (set-frame-position glass-frame (+ 40 x) (+ 40 y)))
  (glass-theme)
  (glass-raise glass-frame)
  (switch-to-buffer buffer)
  (setq indicate-buffer-boundaries 'left)
  ;; (setq glass-local-mode-line-format mode-line-format)
  ;; (setq mode-line-format nil)
  (setf glass-showing t))

(defun* glass-hide ()
  (interactive)
  (when (glass-live-p)
    ;; (when (null mode-line-format)
    ;;   (setq mode-line-format glass-local-mode-line-format))
    ;; lower all frames
    (mapc #'lower-frame (frame-list))
    (when (buffer-narrowed-p) (widen))
    (glass-off-top)
    ;; restore previous scroll bars, if any
    (set-scroll-bar-mode 'glass-scroll-bar-mode)
    (setf glass-showing nil)))

(defun glass-toggle ()
  (interactive)
  (when 
  (if glass-showing (glass-hide) (glass-show)))

(defun glass-toggle-play ()
  (interactive)
  (eval-in-cl "(blocky:toggle-play)"))

(global-set-key [f12] 'glass-toggle)
(global-set-key [pause] 'glass-toggle-play)

(defun* glass-destroy ()
  (interactive)
  (when (glass-live-p)
    (glass-hide)
    (delete-frame glass-frame))
  (setf glass-frame nil))

(defun glass-show-definition (name &rest params)
  (slime-edit-definition name)
  (delete-other-windows)
  (let ((mouse-autoselect-window nil))
    (select-frame-set-input-focus (selected-frame)))
  (when glass-use-special-frame 
    (narrow-to-defun)
    (apply #'glass-show params)
    (let ((height (min 16 (max 8 (count-lines (point-min) (point-max))))))
      (set-frame-height glass-frame height))))

(provide 'glass)
;;; glass.el ends here
