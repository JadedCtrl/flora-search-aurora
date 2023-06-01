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
  (draw-map "/home/jaidyn/.local/src/games/flower/res/mom.tmx"))


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
  (mapcar #'draw-tile-layer
       (cl-tiled:map-layers (cl-tiled:load-map map-path))))


(defun draw-tile-layer (tile-layer)
  (mapcar #'draw-cell
       (cl-tiled:layer-cells tile-layer)))


(defun draw-cell (cell)
  (move-cursor (cl-tiled:cell-row cell)
               (cl-tiled:cell-column cell))
  (write-char #\A))


(main)
