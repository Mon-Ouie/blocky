;;; ioforms.lisp --- retro-esque 2d game engine for Common Lisp
               
;; Copyright (C) 2006, 2007, 2008, 2009  David O'Toole

;; Author: David O'Toole ^dto@gnu.org
;; Keywords: multimedia, games
;; Version: 2.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; This program is dedicated to our beloved Yogi, who died 2006-10-06.

;;; Requirements:

;; This program requires a Common Lisp implementation and the
;; LISPBUILDER-SDL packages. See the included file INSTALL.

;; ASDF users can do the following:
;;
;;   (require :asdf)
;;   (require :asdf-install)
;;   (asdf-install:install :lispbuilder-sdl) 
;;   (asdf-install:install :lispbuilder-sdl-image)

;;; Code:

(defpackage :ioforms
    (:documentation "A 2D game engine in Common Lisp.")
  (:use :common-lisp) 
  (:export *default-frame-width* *default-frame-height* =null=
	   null-block =viewport= *frequency* *output-chunksize*
	   *output-channels* halt-sample *dt* defproject run
	   *update-function* =equipment= *default-world-axis-size*
	   defsprite generic-keybind *target* *blocks* *script*
	   *default-world-z-size* =browser= install-blocks =balloon=
	   =form= keyboard-held-p keyboard-pressed-p
	   keyboard-released-p keyboard-time-in-current-state
	   keyboard-time-in-previous-state *updates* keyboard-down-p
	   keyboard-keys-down keyboard-modifier-down-p
	   keyboard-modifiers draw-filled-circle draw-aa-circle =my=
	   =say= =animate= =hide= =show= =set-variable= =variable=
	   =visible?= =joystick-button= get-keys
	   *project-package-name* project-package-name =set=
	   make-block =integer= =string= =float= =symbol=
	   *form-command-handler-function* =data-cell= =var-cell=
	   =option-cell= =toggle-cell= =event-cell= =buffer-cell=
	   =comment-cell= install-block uninstall-block =button-cell=
	   =image-cell= *initialization-hook* initialize-engine =fire=
	   =see-player= hit-blocks =player-direction= =closer-than=
	   =block-prompt= =listener= =list= split-string-on-lines
	   message *prompt-sweden-keybindings*
	   *prompt-qwerty-keybindings* *screen-width*
	   transform-method-body roll-under make-stat =formatter= make-universe
	   initialize-colors *standard-categories* *left-turn* bind-event
	   *right-turn* *default-action-points* =world= roll
	   bind-event-to-method *colors* enable-classic-key-repeat
	   disable-classic-key-repeat get-color =prompt= =menu-item=
	   =direction-chooser= define-method *default-font* 
	   field-value set-field-value object-fields dispatch-event
	   run-project *user-init-file-name* distance icon-resource icon-image
	   *compass-directions* *compass-opposites*
	   find-resource-property compose-blank-fields font-width
	   font-height *browser* browser set-browser find-object
	   *windows* transform-field-reference defblock
	   *screen-height* =inventory= formatted-line-width
	   *last-event* formatted-line-height formatted-string-height
	   formatted-string-width get-color create-image draw-image
	   ioforms edit =frame= define-prototype has-field *target*
	   with-target defcell defworld *choose-direction-menu*
	   set-field-options field-option-value index-resource
	   find-project-path index-project load-image-resource
	   load-lisp-resource *executable* *function-buttons*
	   *corner-buttons* *dance-arrows* *punctuation*
	   *dance-phrase-symbols* *dance-keybindings*
	   *energy-dance-pad-mapping* *message-function*
	   *dark-target-arrow-images* get-button-index arrow-image
	   message-to-standard-output reset-message-function
	   arrow-formatted-string ticks-per-beat event-time
	   event-arrow default-project-directories *step-tolerance*
	   *resource-handlers* load-resource find-resource
	   find-resource-object *colors* *world* make-directory-maybe
	   load-user-init-file *project-directories* resource-to-plist
	   *osx* *linux* make-resource make-object-resource make-event
	   =block= *blocks* bind-event-to-prompt-insertion
	   make-field-initializer clone make-field-initializer-body
	   make-key-modifier-symbol make-key-string normalize-event
	   make-keyword make-object queue-head queue-max queue-count
	   *sender* make-special-variable-name field-reference-p
	   null-parent *message-send-symbol-suffix* *x11-color-data*
	   object-name object-parent send send-super send-queue self
	   opposite-direction object-address-string object
	   step-in-direction direction-to =cell= plasma-rect
	   subdivide-rect render-plasma add-hook run-hook queue-tail
	   make-resource-link save-resource save-project
	   save-everything *export-formats* export-archive
	   export-application export-project make-queue queue unqueue
	   queue-message queued-messages-p unqueue-message send-queue
	   field-value random-direction *resources* load-font-resource
	   save-object-resource /parent/initialize parent/initialize
	   /queue/initialize queue/initialize draw-string-solid
	   read-iof initialize-resource-table percent-of-time
	   render-formatted-paragraph make-formatted-string
	   draw-string-shaded render-formatted-string
	   render-formatted-line resource font-text-extents
	   write-sexp-to-file with-message-sender *message-sender*
	   =textbox= read-sexp-from-file with-fields with-field-values
	   write-iof *grammar* one-of left-hand-side right-hand-side
	   expansions generate send-event-to-blocks play-music
	   halt-music seek-music *joystick-mapping*
	   *generic-joystick-mapping* *ps3-joystick-mapping*
	   *joystick-button-symbols* draw-resource-image
	   *event-handler-function* *use-sound* trace-rectangle
	   trace-row trace-column trace-octagon trace-line midpoint
	   =asterisk= send-event =gray-asterisk= self *project-blocks*
	   defsprite =sprite= get-some-object-name
	   transform-declaration-field-descriptor show-blocks
	   no-such-field =narrator= find-projects-in-directory goal
	   =mission= =gateway= =launchpad= =environment=
	   directory-is-project-p find-all-projects *project*
	   transform-tree *after-initialization-hook* stat-value
	   draw-line *default-message-verbosities*
	   *message-verbosities* add-overlay set-message-verbosities
	   operation-symbol message-symbol play-sample
	   set-music-volume add-message-verbosities with-message-queue
	   =minimap= draw-pixel *user-keyboard-layout* *fullscreen*
	   draw-circle =emote= set-field-option-value =blockr=
	   =blockr-prompt= open-project =sprite-special= field-options
	   world set-frame-rate *frame-rate* =stack= *workbook*
	   set-resource-modified-p *iof-file-extension* load-project
	   *project* *project-path* *window-title* *window-position*
	   *default-shell-width* *default-shell-height* =split=
	   =shell= =system= *system* set-timer-interval =gcell=
	   defgcell =block= *message-logging* overlay
	   poll-joystick-axis poll-joystick-button reset-joysticks
	   *joystick-device-identifiers* set-screen-width =universe=
	   *universe* *play-args* set-screen-height genseq
	   *zoom-factor* zoom-image is-zoomed-resource
	   *timer-interval* save-objects enable-timer disable-timer
	   while defmission send-to-blocks enable-held-keys
	   disable-held-keys do-cells draw-box *resizable* achieve
	   *resize-hook* draw-rectangle *quitting*
	   *after-open-project-hook* *mission* mission-variable
	   set-mission-variable with-mission-locals =mission=
	   *background-color* set-sample-callback set-music-callback
	   cffi-chunk-buffer =command-cell= convert-cffi-sample
	   get-sample-buffer register-sample-generator =voice=
	   register-voice unregister-voice register-voice-mixer
	   mix-voices convert-cffi-sample-to-internal *block* *script*
	   =script= =editor= =block= =move= =move-to= =play-music=
	   =when= =play-sound= =unless= =if= =start= =stop= =+= =if=
	   =do= convert-internal-sample-to-cffi get-ticks
	   block-variable with-block-variables set-block-variable
	   with-blocks with-mission-locals *project* quit reset
	   seek-music make-keyword make-special-variable-name object
	   field-value make-queue set-field-value set-field-options
	   field-options field-documentation set-field-option-value
	   field-option-value *lookup-failure* no-such-field has-field
	   has-method send send-queue send-parent serialize
	   deserialize initialize-method-cache *send-parent-depth*
	   initialize-documentation-tables null-parent queue unqueue
	   empty-queue *message-queue* queue-message make-non-keyword
	   with-fields queue-count queue-head method-documentation
	   set-method-documentation method-arglist
	   method-arglist-for-swank set-method-arglist
	   queued-messages-p with-field-values with-fields-ex
	   unqueue-message unqueue-and-send-message with-message-queue
	   message-symbol operation-symbol *sender* message-reader
	   transform-tree field-reference-p transform-field-reference
	   transform-method-body object-parent object-name $
	   object-fields define-method define-prototype new object-p
	   self transform-declaration-field-descriptor is-a
	   compose-blank-fields make-field-initializer initialize
	   initialize-ioforms object-address-string
	   draw-string-blended =block= *token-types*
	   *block-categories* *block-colors* *block-text-colors*
	   defblock =beep= =program=))

;;; ioforms.lisp ends here
