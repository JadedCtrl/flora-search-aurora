;;;; Copyright © 2023, Jaidyn Ann <jadedctrl@posteo.at>
;;;;
;;;; This program is free software: you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation, either version 3 of
;;;; the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;;; This contains package definitions for Flora Serĉ’ Kolora.

(defpackage :flora-search-aurora.util
  (:nicknames :fsa.utl :util :…)
  (:use :cl)
  (:export #:linewrap-string #:fit-lines
           #:plist=
           #:incf-0
           #:at-least #:at-most
           #:string->symbol
           #:system-language #:langcode->keysym #:getf-lang
           :*language*))

(defpackage :flora-search-aurora.input
  (:use :cl)
  (:nicknames :fsa.imp :input :⌨)
  (:export #:read-char-plist #:read-gamefied-char-plist
           #:normalize-char-plist #:gameify-char-plist
           #:plist-char-p
           #:pressed-enter-p
   :control :meta :shift
   +qwerty-layout+ +dvorak-layout+
   +arrows-game-layout+ +wasd-game-layout+ +ijkl-game-layout+
   :↑ :← :→ :↓ :↰ :↱ :↲ :↳ :🆗 :❎
   :*keyboard* :*controls*))

(defpackage :flora-search-aurora.display
  (:nicknames :fsa.dsp :display :✎)
  (:use :cl)
  (:export #:make-screen-matrix #:print-screen-matrix #:matrix-delta
           #:render-line #:render-string #:render-string-verbatim #:render-string-partially
           #:render-fill-rectangle
           #:hide-cursor #:show-cursor #:clear-screen))

(defpackage :flora-search-aurora.menu
  (:nicknames :fsa.mnu :menu :📋)
  (:use :cl)
  (:export #:menu-state #:menu-state-update #:menu-state-draw #:make-menu-function
           #:selected-menu-item
           :label :selection :selected))

(defpackage :flora-search-aurora.settings
  (:nicknames :fsa.set :settings :🔧)
  (:use :cl)
  (:export #:make-settings-menu-function #:settings-menu))


(defpackage :flora-search-aurora.dialogue
  (:nicknames :fsa.dia :dialogue :💬)
  (:use :cl)
  (:export #:dialogue-state #:make-dialogue-state
           #:start-dialogue #:face #:say #:mumble #:move
   :normal-face :talking-face))

(defpackage :flora-search-aurora.inventory
  (:nicknames :fsa.inv :inventory :🎒)
  (:use :cl)
  (:export #:inventory-state #:make-inventory-function))

(defpackage :flora-search-aurora.intermission
  (:nicknames :fsa.int :intermission :🎭)
  (:use :cl)
  (:export
   :make-intermission-function))

(defpackage :flora-search-aurora.overworld.util
  (:nicknames :fsa.ovr.… :overworld.util :🌍.…)
  (:use :cl)
  (:export #:coords->symbol #:symbol->coords
           #:world-coords->screen-coords
           #:world-coords-chunk
           #:map->plist #:plist->map
           #:save-map-to-file
           :*language*))

(defpackage :flora-search-aurora.overworld
  (:nicknames :fsa.ovr :overworld :🌍)
  (:use :cl
   :flora-search-aurora.overworld.util)
  (:export #:make-overworld-state #:make-overworld-function
           #:overworld-state #:overworld-state-draw
           #:merge-maps
           #:world-coords->screen-coords
           #:getf-entity #:getf-entity-data #:removef-entity
           #:entities-near-entity
           #:aget-item #:getf-act #:getf-know
           #:move-entity-to #:move-entity
           #:plist->map
           :left :right))

(defpackage :flora-search-aurora.engine
  (:nicknames :fsa.eng :engine :⚙)
  (:export #:state-loop #:main)
  (:use :cl))

(defpackage :flora-search-aurora
  (:nicknames :fsa :✿)
  (:export #:main
   :player)
  (:use :cl
   :flora-search-aurora.input :flora-search-aurora.display
   :flora-search-aurora.overworld :flora-search-aurora.dialogue
   :flora-search-aurora.menu))
