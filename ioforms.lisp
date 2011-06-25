;;; ioforms.lisp --- a free game engine for common lisp
               
;; Copyright (C) 2006, 2007, 2008, 2009  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
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

;;; Code:

(defpackage :ioforms
    (:documentation "Visual Common Lisp.")
  (:use :common-lisp) 
  (:export *default-frame-width* *default-frame-height* null-block
*frequency* *output-chunksize* *output-channels* halt-sample *dt*
defproject run start stop *update-function* *default-world-axis-size*
defsprite *target* *blocks* *script* *default-world-z-size*
install-blocks seconds->frames keyboard-held-p keyboard-pressed-p
*use-nominal-screen-size* keyboard-released-p
keyboard-time-in-current-state make-menu pretty-symbol-string
*pointer-x* *pointer-y* keyboard-time-in-previous-state *updates*
keyboard-down-p keyboard-keys-down keyboard-modifier-down-p
keyboard-modifiers draw-filled-circle draw-aa-circle get-keys
*project-package-name* project-package-name make-block
*form-command-handler-function* add-block remove-block
*initialization-hook* initialize-engine hit-blocks
split-string-on-lines message *prompt-sweden-keybindings*
*prompt-qwerty-keybindings* *screen-width* transform-method-body
roll-under make-stat make-universe initialize-colors
*standard-categories* *left-turn* bind-event *right-turn*
*default-action-points* left-turn right-turn roll bind-event-to-method
*colors* enable-key-repeat disable-key-repeat get-color define-method
*default-font* field-value set-field-value object-fields
dispatch-event run-project *user-init-file-name* distance
icon-resource icon-image *compass-directions* *compass-opposites*
find-resource-property compose-blank-fields font-width font-height
*browser* browser set-browser find-object *windows*
transform-field-reference defblock *screen-height*
formatted-line-width *last-event* formatted-line-height
formatted-string-height formatted-string-width get-color create-image
draw-image ioforms edit define-prototype has-field *target*
with-target defcell defworld *choose-direction-menu* set-field-options
field-option-value index-resource find-project-path index-project
load-image-resource load-lisp-resource *executable* *function-buttons*
*corner-buttons* *dance-arrows* *punctuation* *screen-height*
*screen-width* *nominal-screen-width* *nominal-screen-height*
*gl-screen-width* *gl-screen-height* *dance-phrase-symbols*
*dance-keybindings* *energy-dance-pad-mapping* *message-function*
*dark-target-arrow-images* get-button-index arrow-image
message-to-standard-output reset-message-function
arrow-formatted-string ticks-per-beat event-time event-arrow
default-project-directories *step-tolerance* *resource-handlers*
load-resource find-resource find-resource-object *colors* *world*
make-directory-maybe load-user-init-file *project-directories*
resource-to-plist *osx* *linux* make-resource make-object-resource
make-event *blocks* bind-event-to-prompt-insertion
make-field-initializer clone make-field-initializer-body
make-key-modifier-symbol make-key-string normalize-event make-keyword
make-object queue-head queue-max queue-count *sender*
field-reference-p null-next object-eq *message-send-symbol-suffix*
*x11-color-data* object-name object-parent send send-super send-queue
self opposite-direction object-address-string object step-in-direction
direction-to plasma-rect subdivide-rect render-plasma add-hook
run-hook queue-tail make-resource-link save-resource save-project
*defined-resources* save-everything *export-formats* export-archive
defresource export-application export-project make-queue queue unqueue
queue-message queued-messages-p unqueue-message send-queue field-value
random-direction random-choose *resources* load-font-resource
save-object-resource next%initialize next%initialize /queue/initialize
queue/initialize draw-string-solid read-iof initialize-resource-table
percent-of-time render-formatted-paragraph make-formatted-string
draw-string-shaded set-blending-mode render-formatted-string
render-formatted-line resource font-text-extents write-sexp-to-file
with-message-sender *message-sender* read-sexp-from-file with-fields
with-field-values write-iof *grammar* one-of left-hand-side
right-hand-side expansions generate send-event-to-blocks play-music
halt-music seek-music *joystick-mapping* play initialize-sound
*generic-joystick-mapping* *ps3-joystick-mapping*
*joystick-button-symbols* draw-resource-image *event-handler-function*
*use-sound* trace-rectangle trace-row trace-column trace-octagon
trace-line midpoint send-event self *project-blocks* defsprite
get-some-object-name transform-declaration-field-descriptor
show-blocks no-such-field find-projects-in-directory goal
directory-is-project-p find-all-projects *project* transform-tree
*after-startup-hook* stat-value draw-line
*default-message-verbosities* *message-verbosities* add-overlay
set-message-verbosities operation-symbol message-symbol play-sample
set-music-volume add-message-verbosities with-message-queue draw-pixel
*user-keyboard-layout* *fullscreen* draw-circle set-field-option-value
open-project field-options world set-frame-rate *frame-rate*
*workbook* set-resource-modified-p *iof-file-extension* load-project
*project* *project-path* *window-title* *window-position* restartably
*default-shell-width* *default-shell-height* *system*
set-timer-interval defgcell *message-logging* overlay
initialize-console poll-joystick-axis poll-joystick-button
reset-joysticks *joystick-device-identifiers* set-screen-width
*universe* *play-args* set-screen-height genseq *zoom-factor*
zoom-image is-zoomed-resource *timer-interval* save-objects
enable-timer disable-timer while defmission send-to-blocks
enable-held-keys disable-held-keys do-cells draw-box *resizable*
achieve *resize-hook* draw-rectangle *quitting*
*after-open-project-hook* *mission* mission-variable
set-mission-variable with-mission-locals *background-color*
set-sample-callback set-music-callback cffi-chunk-buffer
convert-cffi-sample get-sample-buffer register-sample-generator
register-voice unregister-voice register-voice-mixer mix-voices st
convert-cffi-sample-to-internal *block* *script*
convert-internal-sample-to-cffi get-ticks block-variable
with-block-variables set-block-variable with-blocks
with-mission-locals *project* quit reset seek-music make-keyword
object field-value make-queue find-parent set-field-value
set-field-options field-options field-documentation
set-field-option-value field-option-value *lookup-failure*
no-such-field has-field has-method send send-queue send-next serialize
deserialize initialize-method-cache *send-next-depth*
initialize-documentation-tables null-parent queue unqueue empty-queue
*message-queue* queue-message make-non-keyword with-fields queue-count
queue-head method-documentation set-method-documentation
method-arglist method-arglist-for-swank set-method-arglist
queued-messages-p with-field-values with-fields-ex unqueue-message
unqueue-and-send-message with-message-queue message-symbol
operation-symbol *sender* message-reader transform-tree
field-reference-p transform-field-reference transform-method-body
object-parent object-name $ object-fields define-method
define-prototype new object-p self
transform-declaration-field-descriptor is-a compose-blank-fields
make-field-initializer initialize initialize-prototypes
initialize-ioforms object-address-string draw-string
draw-string-blended make-menu find-text-image make-text-image
clear-text-image-cache *token-types* *block-categories* *block-colors*
input *block-text-colors* defblock ))

;;; ioforms.lisp ends here
