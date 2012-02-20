;;; blocky.lisp --- a visual dialect of Common Lisp inspired by Squeak
               
;; Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011  David O'Toole

;; Author: David O'Toole <dto@ioforms.org>
;; Keywords: multimedia, games
;; Version: 2.11

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

;; See the included file INSTALL.

;;; Code:

(defpackage :blocky
    (:documentation "A visual multimedia programming language for Common Lisp.")
  (:use :common-lisp) 
  (:export *default-frame-width* *default-frame-height* null-block
*frequency* *output-chunksize* *output-channels* halt-sample *dt*
*copyright-notice* *author* *message-hook-functions* add-to-list
modify-joystick-profile defproject run start stop *update-function*
*default-world-axis-size* defsprite *target* *blocks* *buffer*
*default-world-z-size* install-blocks shut-down later later-at
later-when start-up seconds->frames keyboard-held-p keyboard-pressed-p
holding-control *scale-output-to-window* keyboard-released-p *edit*
callf with-font *font* find-heading keyboard-time-in-current-state
pretty-symbol-string *pointer-x* *pointer-y* is-joystick-event
is-raw-joystick-event keyboard-time-in-previous-state *updates*
keyboard-down-p keyboard-keys-down keyboard-modifier-down-p
keyboard-modifiers draw-filled-circle draw-aa-circle get-keys
*project-package-name* project-package-name make-block
*form-command-handler-function* add-block remove-block
*initialization-hook* initialize-engine hit-blocks quadtree-delete
quadtree-insert build-quadtree quadtree-collide quadtree-show
*quadtree* split-string-on-lines message *prompt-sweden-keybindings*
*prompt-qwerty-keybindings* *screen-width* transform-method-body
*edit* roll-under make-stat make-universe initialize-colors *style*
create-project *standard-categories* *left-turn* bind-event
*right-turn* left-turn right-turn roll bind-event-to-method *colors*
enable-key-repeat disable-key-repeat get-color define-method
*default-font* field-value set-field-value object-fields
dispatch-event run-project *user-init-file-name* distance
icon-resource icon-image *directions* *clear-cached-images-on-resize*
*opposites* find-resource-property compose-blank-fields font-width
font-height *browser* browser set-browser find-object *windows* edit
create transform-field-reference define-block *screen-height*
formatted-line-width *last-event* formatted-line-height
formatted-string-height formatted-string-width get-color create-image
draw-image blocky edit define-prototype has-field *target* with-target
defcell define-world *choose-direction-menu* set-field-options
*user-joystick-profile* field-option-value index-resource
*default-joystick-profile* joystick-profile find-project-path
index-project load-image-resource load-lisp-resource *executable*
*function-buttons* *corner-buttons* *dance-arrows* *punctuation*
*screen-height* player *screen-width* blockyp *nominal-screen-width*
*nominal-screen-height* *gl-screen-width* *gl-screen-height*
*dance-phrase-symbols* *dance-keybindings* *energy-dance-pad-mapping*
*message-function* dash holding-shift *dark-target-arrow-images*
get-button-index arrow-image message-to-standard-output
reset-message-function *orthogonal-arrows* *diagonal-arrows*
*function-buttons* *punctuation* *dance-arrows* *dance-pad-symbols*
*make-prototype-id-package* arrow-formatted-string lturn rturn
ticks-per-beat event-time event-arrow radian-angle
draw-textured-rectangle default-project-directories *step-tolerance*
*resource-handlers* load-resource find-resource find-resource-object
*colors* *world* make-directory-maybe load-user-init-file
*project-directories* resource-to-plist *osx* *linux* make-resource
make-object-resource make-event *blocks* bind-event-to-text-insertion
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
save-object-resource super%initialize super%initialize
/queue/initialize queue/initialize draw-string-solid read-iof
initialize-resource-table percent-of-time render-formatted-paragraph
make-formatted-string draw-string-shaded set-blending-mode
render-formatted-string render-formatted-line resource font-text-width
write-sexp-to-file with-message-sender *message-sender*
read-sexp-from-file with-fields with-field-values write-iof *grammar*
one-of left-hand-side right-hand-side expansions generate
send-event-to-blocks play-music halt-music seek-music
*joystick-mapping* play initialize-sound *generic-joystick-mapping*
*ps3-joystick-mapping* *joystick-button-symbols* draw-resource-image
*event-handler-function* *use-sound* trace-rectangle trace-row
trace-column trace-octagon trace-line midpoint send-event self
*project-blocks* defsprite get-some-object-name
transform-declaration-field-descriptor show-blocks no-such-field
find-projects-in-directory goal directory-is-project-p
find-all-projects *project* transform-tree *after-startup-hook*
stat-value draw-line *default-message-verbosities*
*message-verbosities* add-overlay set-message-verbosities
operation-symbol message-symbol play-sample set-music-volume
add-message-verbosities with-message-queue draw-pixel
*user-keyboard-layout* *fullscreen* draw-circle set-field-option-value
open-project field-options world set-frame-rate *frame-rate*
*workbook* set-resource-modified-p *iof-file-extension* load-project
*project* *project-path* *window-title* *window-position* restartably
*default-shell-width* *default-shell-height* *system*
set-timer-interval defgcell *message-logging* overlay
joystick-axis-pressed-p joystick-axis-value joystick-axis-raw-value
analog-stick-heading find-heading analog-stick-pressure
*joystick-axis-size* *joystick-axis-dead-zone* *event-hook*
left-analog-stick-heading left-analog-stick-pressure *message-history*
right-analog-stick-heading joystick-button-pressed-p
analog-stick-pressed-p left-analog-stick-pressed-p
right-analog-stick-pressed-p right-analog-stick-pressure
initialize-console joystick-axis-value poll-joystick-button
joystick-button-state reset-joysticks *device-profiles*
button-to-symbol symbol-to-button find-device-profile set-screen-width
*universe* *play-args* set-screen-height genseq *zoom-factor*
zoom-image is-zoomed-resource *timer-interval* save-objects
enable-timer disable-timer while defmission send-to-blocks
enable-held-keys disable-held-keys do-cells draw-box *resizable*
achieve *resize-hook* draw-rectangle *quitting*
*after-open-project-hook* *mission* mission-variable find-bounding-box
combine-worlds stack-vertically set-mission-variable horizontal-extent
vertical-extent flip-horizontally flip-vertically mirror-horizontally
mirror-vertically world with-mission-locals with-empty-world
define-turtle stack-horizontally *background-color* combine-beside
combine-below set-sample-callback set-music-callback cffi-chunk-buffer
convert-cffi-sample get-sample-buffer register-sample-generator
register-voice unregister-voice register-voice-mixer mix-voices
convert-cffi-sample-to-internal *block* *buffer* define-turtle
convert-internal-sample-to-cffi get-ticks block-variable *block-font*
with-block-variables set-block-variable with-blocks
with-mission-locals *project* quit reset seek-music make-keyword
object field-value make-queue find-parent set-field-value find-super
*font* set-field-options field-options field-documentation
set-field-option-value field-option-value *lookup-failure*
no-such-field has-field has-method send send-queue send-super
serialize deserialize initialize-method-cache *send-super-depth*
initialize-documentation-tables null-parent queue unqueue empty-queue
*message-queue* queue-message make-non-keyword with-fields queue-count
queue-head method-documentation set-method-documentation
method-arglist method-arglist-for-swank set-method-arglist
queued-messages-p with-field-values with-fields-ex unqueue-message
unqueue-and-send-message with-message-queue message-symbol
operation-symbol *sender* message-reader transform-tree
field-reference-p transform-field-reference transform-method-body
object-parent object-name $ object-fields define-method
*joystick-dead-zone* define-prototype new object-p self percent-gray
percent-grey *indicators* find-indicator-texture draw-indicator
font-text-width transform-declaration-field-descriptor is-a
compose-blank-fields make-field-initializer initialize
initialize-prototypes initialize-blocky object-address-string
draw-string make-tree draw-string-blended make-menu find-text-image
make-text-image find-texture *default-super* clear-text-image-cache
*token-types* verify *serif* *use-antialiased-text* *sans* *monospace*
toggle-debug *debug-on-error* *block-categories* *block-colors* input
paste arrange-beside arrange-below load-variable-resource translate
save-variable-resource *persistent-variables* with-new-world
with-border with-blank-world with-world-prototype with-world
remove-trailing-space *world-prototype* step-coordinates
make-field-accessor-forms save-excursion
*persistent-variables-file-name* duplicate persistent-variables-file
combine save-variables indicator-size draw-indicator load-variables
*block-text-colors* defblock *block-bold* *bold* *italic*
*block-italic* define-block-macro))
