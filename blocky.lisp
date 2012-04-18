;;; blocky.lisp --- a visual dialect of Common Lisp inspired by Squeak
               
;; Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011, 2012  David O'Toole

;; Author: David O'Toole <dto@blxorms.org>
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
  (:export null-block *frequency* *output-chunksize* *output-channels*
halt-sample *dt* *copyright-notice* *author* *message-hook-functions*
add-to-list modify-joystick-profile defproject start stop selection
clear-clipboard copy-to-clipboard at-next-update *next-update-hook*
*already-serialized* browse back *update-function* *target* *blocks*
shut-down later later-at later-when start-up seconds->frames
keyboard-held-p keyboard-pressed-p holding-control transform-window
*scale-output-to-window* keyboard-released-p *edit* with-font *font*
find-heading keyboard-time-in-current-state pretty-string ugly-symbol
*pointer-x* *pointer-y* is-joystick-event *self* is-raw-joystick-event
keyboard-time-in-previous-state *updates* keyboard-down-p *buffers*
keyboard-keys-down keyboard-modifier-down-p find-buffer find-world
*socket-size* keyboard-modifiers draw-filled-circle draw-aa-circle
get-keys *project-package-name* project-package-name make-block
add-block remove-block with-quadtree *initialization-hook* hit-blocks
quadtree-delete quadtree-insert build-quadtree quadtree-collide
quadtree-show *quadtree* *quadtree-depth* split-string-on-lines
message *prompt-sweden-keybindings* *prompt-qwerty-keybindings*
*screen-width* transform-method-body roll-under initialize-colors
*style* load-project-image create-project-image *standard-categories*
*left-turn* bind-event *right-turn* left-turn right-turn roll
bind-event-to-method *colors* enable-key-repeat disable-key-repeat
get-color define-method *default-font* field-value set-field-value
object-fields dispatch-event *user-init-file-name* distance
icon-resource icon-image *directions* *opposites*
project-orthographically project-with-perspective open-viewport
find-resource-property compose-blank-fields font-width font-height
find-object *windows* edit create blocky transform-field-reference
define-block *screen-height* formatted-line-width find-world
*clipboard* formatted-line-height formatted-string-height
formatted-string-width get-color create-image draw-image blocky edit
define-prototype has-field *target* with-target define-world
set-field-options *user-joystick-profile* field-option-value
index-resource *default-joystick-profile* joystick-profile
find-project-path index-project load-image-resource load-lisp-resource
*executable* *screen-height* player *screen-width* blockyp
*nominal-screen-width* *nominal-screen-height* *gl-screen-width*
*gl-screen-height* *message-function* dash holding-shift
get-button-index message-to-standard-output reset-message-function
*make-prototype-id-package* lturn rturn ticks-per-beat radian-angle
draw-textured-rectangle default-project-directories
*resource-handlers* load-resource find-resource find-resource-object
*colors* *world* make-directory-maybe load-user-init-file
*project-directories* resource-to-plist *osx* *linux* make-resource
make-object-resource make-event *blocks* bind-event-to-text-insertion
make-field-initializer clone make-field-initializer-body
make-key-modifier-symbol make-key-string normalize-event make-keyword
make-object queue-head queue-max queue-count *sender*
field-reference-p null-next object-eq *x11-color-data* object-name
object-parent send send-super send-queue self opposite-direction
opposite-heading object-address-string object step-in-direction
define-resource direction-to plasma-rect subdivide-rect render-plasma
add-hook run-hook queue-tail make-resource-link save-resource
on-screen-p save-project-image *system* *defined-resources*
save-everything with-input-values with-inputs *export-formats*
export-archive *use-texture-blending* defresource export-application
*default-texture-filter* export-project make-queue queue unqueue
*font-texture-filter* queue-message queued-messages-p unqueue-message
send-queue field-value random-direction random-choose *resources*
load-font-resource save-object-resource initialize%super
draw-string-solid read-box initialize-resource-table percent-of-time
render-formatted-paragraph make-formatted-string draw-string-shaded
set-blending-mode render-formatted-string render-formatted-line
resource font-text-width write-sexp-to-file with-message-sender
*message-sender* read-sexp-from-file with-fields with-field-values
write-blx *grammar* one-of left-hand-side right-hand-side expansions
generate send-event-to-blocks play-music halt-music seek-music
*joystick-mapping* play initialize-sound *generic-joystick-mapping*
*joystick-button-symbols* draw-resource-image *event-handler-function*
*use-sound* midpoint send-event self get-some-object-name
transform-declaration-field-descriptor no-such-field
find-projects-in-directory goal directory-is-project-p
find-directories find-all-projects *project* transform-tree
*after-startup-hook* draw-line operation-symbol message-symbol
play-sample set-music-volume draw-pixel *user-keyboard-layout*
*fullscreen* draw-circle set-field-option-value load-project
field-options world *frame-rate* set-resource-system-p
*blx-file-extension* *project* *project-path* *window-title*
*window-position* restartably *message-logging*
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
*play-args* set-screen-height genseq save-objects enable-timer
draw-box *resizable* *resize-hook* draw-rectangle playerp
*after-load-project-hook* *mission* mission-variable find-bounding-box
combine stack-vertically set-mission-variable horizontal-extent
vertical-extent flip-horizontally flip-vertically mirror-horizontally
mirror-vertically world with-mission-locals with-empty-world
define-turtle stack-horizontally *background-color* combine-beside
combine-below get-ticks *block-font* quit reset seek-music
make-keyword object field-value make-queue find-parent set-field-value
find-super *font* set-field-options field-options field-documentation
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
object-parent object-name object-fields define-method
*joystick-dead-zone* define-prototype new object-p self percent-gray
percent-grey *indicators* find-indicator-texture draw-indicator
font-text-width transform-declaration-field-descriptor is-a
compose-blank-fields make-field-initializer initialize pause play
*image-opacity* rewind stop initialize-prototypes initialize-blocky
play-project update-future object-address-string draw-string make-tree
draw-string-blended make-menu find-text-image make-text-image
find-texture *default-super* clear-text-image-cache *token-types*
verify *serif* *use-antialiased-text* *sans* *monospace* toggle-debug
*debug-on-error* *block-categories* *block-colors* input paste
arrange-beside arrange-below load-variable-resource translate
save-variable-resource *persistent-variables* with-new-world
with-border with-blank-world with-world-prototype with-world
remove-trailing-space *world-prototype* step-coordinates
*default-frame-rate* make-field-accessor-forms save-excursion
make-input-accessor-forms *persistent-variables-file-name* duplicate
persistent-variables-file combine save-variables indicator-size
draw-indicator load-variables *block-text-colors* defblock capture
make-input-accessor-macrolet-clause make-input-accessor-defun-forms
input-reference-p input-block input-value *block-bold* *bold* *italic*
*block-italic* define-block-macro))
