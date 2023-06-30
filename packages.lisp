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
  (:export #:split-string-by-length
           #:plist=
           #:incf-0
           #:at-least #:at-most
           #:system-language #:langcode->keysym #:getf-lang))

(defpackage :flora-search-aurora.input
  (:use :cl)
  (:nicknames :fsa.imp :input :⌨)
  (:export #:read-char-plist #:read-gamefied-char-plist
           #:normalize-char-plist #:gameify-char-plist
           #:plist-char-p
   :control :meta :shift
   +qwerty-layout+ +dvorak-layout+
   +arrows-game-layout+ +wasd-game-layout+ +ijkl-game-layout+
   :↑ :← :→ :↓ :🆗 :❎))

(defpackage :flora-search-aurora.display
  (:nicknames :fsa.d :display :✎)
  (:use :cl)
  (:export #:make-screen-matrix #:print-screen-matrix #:matrix-delta
           #:render-line #:render-string #:render-string-verbatim #:render-string-partially
           #:hide-cursor #:show-cursor #:clear-screen))

(defpackage :flora-search-aurora.ui
  (:nicknames :fsa.u :ui :📋)
  (:use :cl)
  (:export #:menu-state #:make-menu-state
           :label :selection :selected))

(defpackage :flora-search-aurora.dialogue
  (:nicknames :fsa.dia :dialogue :💬)
  (:use :cl)
  (:export #:dialogue-state #:make-dialogue-state
           #:start-dialogue #:face #:say #:mumble #:move
   :normal-face :talking-face))

(defpackage :flora-search-aurora.inventory
  (:nicknames :fsa.inv :inventory :🎒)
  (:use :cl)
  (:export #:inventory-state #:make-inventory-state))

(defpackage :flora-search-aurora.intermission
  (:nicknames :fsa.int :intermission :🎭)
  (:use :cl)
  (:export :*standard-font* :*small-font*))

(defpackage :flora-search-aurora.overworld.util
  (:nicknames :fsa.o.u :overworld.util :🌍.…)
  (:use :cl)
  (:export #:coords->symbol #:symbol->coords
           #:world-coords->screen-coords
           #:world-coords-chunk
           #:map->plist #:plist->map
           #:string->symbol
           #:save-map-to-file))

(defpackage :flora-search-aurora.overworld
  (:nicknames :fsa.o :overworld :🌍)
  (:use :cl
   :flora-search-aurora.overworld.util)
  (:export #:overworld-state #:make-overworld-state #:overworld-state-draw
           #:merge-maps
           #:world-coords->screen-coords
           #:getf-entity #:getf-entity-data #:removef-entity
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
   :flora-search-aurora.ui))
