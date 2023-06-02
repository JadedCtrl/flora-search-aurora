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

(ql:quickload '(alexandria cl-tiled str))


(defun move-cursor (row column &key (stream *standard-output*))
  "Moves cursor to desired position.
Borrowed from https://github.com/gorozhin/chlorophyll/
© 2022 Mikhail Gorozhin — MIT license"
  (format stream "~C[~A;~AH" #\Esc row column))


(defun clear-screen (&key (stream *standard-output*))
  "Completely clear the terminal screen."
  (move-cursor 0 0 :stream stream)
  (format stream "~C[J" #\Esc))


(defun matrix-delta (a b)
  "Given two 2D matrices, return a listcontaining on the cells that change between a→b in the following
format:
only the cells that change between a→b — all others are nil."
  (let* ((dimensions (array-dimensions a))
         (delta (make-array dimensions :initial-element nil))
         (max-i (car dimensions))
         (max-j (cadr dimensions))
         (i 0)  (j 0))
    (loop
     (cond
       ((< i max-i)
        (cond
          ((< j max-j)
           (if (not (eq (aref a i j)
                        (aref b i j)))
               (setf (aref delta i j)
                     (aref b i j)))
           (incf j))
          ((eq j max-j)
           (setf j 0)
           (incf i))))
       ((eq i max-i)
        (return))))
     delta))


(defun print-screen-matrix (array)
  "Given a matrix of characters, print each element to standard output."
  (let* ((dimensions (array-dimensions array))
         (max-i (car dimensions))
         (max-j (cadr dimensions))
         (i 0)  (j 0))
    (loop
     (cond
       ((< i max-i)
        (cond
          ((< j max-j)
           (if (characterp (aref array i j))
               (progn
                 (move-cursor i j)
                 (write-char (aref array i j))))
           (incf j))
          ((eq j max-j)
           (setf j 0)
           (incf i))))
       ((eq i max-i)
        (return))))))


(defun screen-matrix-set-map (array map-path)
  "Draw a Tiled-format tilemap to the 2D array."
  (mapcar (lambda (layer) (screen-matrix-set-map-layer array layer))
          (cl-tiled:map-layers (cl-tiled:load-map map-path)))
  array)


(defun screen-matrix-set-map-layer (array tile-layer)
  "Set an array's elements to those corresponding the given Tiled
tile-layer's cells. a Tiled tile-layer to the screen."
  (mapcar (lambda (cell) (screen-matrix-set-char-cell array cell))
          (cl-tiled:layer-cells tile-layer))
  array)


(defun screen-matrix-set-char-cell (array cell)
  "Set a 2D array's element corresponding to a Tiled cell's
character-value, using it's column and row."
  (setf (aref array
              (cl-tiled:cell-row cell)
              (cl-tiled:cell-column cell))
        (tile-character
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
