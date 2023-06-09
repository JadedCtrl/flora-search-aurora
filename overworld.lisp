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

;;;; FLORA-SEARCH-AURORA.OVERWORLD
;;;; All game-functions and data relating to the “overworld” (that is,
;;;; the primary gameplay, the RPG-ish-ish bits).

(defpackage :flora-search-aurora.overworld
  (:use :cl
   :flora-search-aurora.input :flora-search-aurora.display
   :flora-search-aurora.ui)
  (:export #:overworld-state))

(in-package :flora-search-aurora.overworld)



;;; ———————————————————————————————————
;;; Overworld loop
;;; ———————————————————————————————————
(defun overworld-state (matrix map)
  "Render the given map to the matrix and take user-input — for one frame.
A state-function for use with #'state-loop."
  (let ((map (if (or (stringp map) (pathnamep map))
                 (cl-tiled:load-map map)
                 map)))
    (sleep .02)
    (overworld-state-draw matrix map)
    (overworld-state-update map)))


(defun overworld-state-draw (matrix map)
  "Draw the overworld map to the given matrix.
A core part of #'overworld-state."
  (matrix-write-tiled-map matrix map))


(defun overworld-state-update (map)
  "Do nothing, lol.
Core part of #'overworld-state."
  't)



;;; ———————————————————————————————————
;;; Mapping & map-rendering
;;; ———————————————————————————————————
(defun matrix-write-tiled-cell (matrix cell)
  "Set a matrice's (2d array's) element corresponding to
a Tiled cell's character-value, using it's column and row."
  (setf (aref matrix
              (cl-tiled:cell-row cell)
              (cl-tiled:cell-column cell))
        (tiled-tile-character
         (cl-tiled:cell-tile cell))))


(defun matrix-write-tiled-map (matrix map)
  "Draw a Tiled-format tilemap to the 2D array."
  (mapcar (lambda (layer) (matrix-write-tiled-map-layer matrix layer))
          (cl-tiled:map-layers map))
  matrix)


(defun matrix-write-tiled-map-layer (matrix tile-layer)
  "Set an array's elements to those corresponding the given Tiled
tile-layer's cells. a Tiled tile-layer to the screen."
  (mapcar (lambda (cell) (matrix-write-tiled-cell matrix cell))
          (cl-tiled:layer-cells tile-layer))
  matrix)


(defun tiled-tile-character (tile)
  "Given a tileset's tile, return it's corresponding text character,
assuming that the tileset is a bitmap font starting with char-code 32
with 15 characters-per-line."
  (code-char
   (+ (* (cl-tiled:tile-row tile) 15)
      (cl-tiled:tile-column tile)
      32)))
