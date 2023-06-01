;;
;; Copyright 2023, Jaidyn Ann <jadedctrl@posteo.at>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;

;; Made for the text-flavoured LibreJam of 2023-06!
;; https://jamgaroo.xyz/jams/2
;; 72x20

(ql:quickload :cl-tiled)


(defun main ()
  (clear-screen)
  (draw-map "/home/jaidyn/.local/src/games/flower/res/map.tmx"))


(defun move-cursor (row column &key (stream *standard-output*))
  "Moves cursor to desired position.
Borrowed from https://github.com/gorozhin/chlorophyll/
© 2022 Mikhail Gorozhin — MIT license"
  (format stream "~C[~A;~AH" #\Esc row column))


(defun clear-screen (&key (stream *standard-output*))
  "Completely clear the terminal screen."
  (move-cursor 0 0 :stream stream)
  (format stream "~C[J" #\Esc))


(defun draw-map (map-path)
  "Draw a Tiled-format tilemap to the screen."
  (mapcar #'draw-tile-layer
       (cl-tiled:map-layers (cl-tiled:load-map map-path))))


(defun draw-tile-layer (tile-layer)
  "Draw a Tiled tile-layer to the screen."
  (mapcar #'draw-cell
       (cl-tiled:layer-cells tile-layer)))


(defun draw-cell (cell)
  "Draw a specific cell of a tile-layer to the screen."
  (move-cursor (+ (cl-tiled:cell-row cell) 1)
               (+ (cl-tiled:cell-column cell) 1))
  (write-char (tile-character
               (cl-tiled:cell-tile cell))))


(defun tile-character (tile)
  "Given a tileset's tile, return it's corresponding text character,
assuming that the tileset is a bitmap font starting with char-code 32
with 15 characters-per-line."
  (code-char
   (+ (* (cl-tiled:tile-row tile) 15)
      (cl-tiled:tile-column tile)
      32)))


(main)
