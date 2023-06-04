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

;;;; FLORA-SEARCH-AURORA.DISPLAY
;;;; All display-related curses go here.

(defpackage :flora-search-aurora.display
  (:use :cl)
  (:export #:make-screen-matrix #:print-screen-matrix #:screen-matrix-set-map))

(in-package :flora-search-aurora.display)


(defmacro do-for-cell (matrix &body body)
  "Given a 2d-array (matrix), execute the body for every cell.
The body has access to 4 variables:
  * i/j — The current row/column.
  * dimensions — Dimensions of the given matrix.
  * cell — The value of the current cell."
  `(let* ((dimensions (array-dimensions ,matrix))
          (max-i (car dimensions))
          (max-j (cadr dimensions))
          (i 0)  (j 0))
     (loop
      (let ((cell (ignore-errors (aref ,matrix i j))))
        (cond
          ((< i max-i)
           (cond
             ((< j max-j)
              ,@body
              (incf j))
             ((eq j max-j)
              (setf j 0)
              (incf i))))
          ((eq i max-i)
           (return)))))))


(defun matrix-delta (a b)
  "Given two 2D matrices, return a matrix containing only the cells
that change between a→b (favouring those in b) — all others are nil."
  (let ((delta (make-array (array-dimensions a))))
    (do-for-cell a
      (when (not (eq cell
                     (aref b i j)))
          (setf (aref delta i j)
                (aref b i j))))
    delta))


(defun print-screen-matrix (matrix)
  "Given a matrix of characters, print each element to standard output."
  (do-for-cell matrix
    (when (characterp cell)
      (move-cursor (+ i 1) (+ j 1))
      (write-char cell))))


(defun make-screen-matrix ()
  "Create a “screen matrix” — that is, a 2D array representing the
72x20 grid of characters we can print to the terminal."
  (make-array '(20 72) :initial-element #\space))


(defun screen-matrix-set-char-cell (matrix cell)
  "Set a matrice's (2d array's) element corresponding to
a Tiled cell's character-value, using it's column and row."
  (setf (aref matrix
              (cl-tiled:cell-row cell)
              (cl-tiled:cell-column cell))
        (tile-character
         (cl-tiled:cell-tile cell))))


(defun screen-matrix-set-map (matrix map-path)
  "Draw a Tiled-format tilemap to the 2D array."
  (mapcar (lambda (layer) (screen-matrix-set-map-layer matrix layer))
          (cl-tiled:map-layers (cl-tiled:load-map map-path)))
  matrix)


(defun screen-matrix-set-map-layer (matrix tile-layer)
  "Set an array's elements to those corresponding the given Tiled
tile-layer's cells. a Tiled tile-layer to the screen."
  (mapcar (lambda (cell) (screen-matrix-set-char-cell matrix cell))
          (cl-tiled:layer-cells tile-layer))
  matrix)


(defun tile-character (tile)
  "Given a tileset's tile, return it's corresponding text character,
assuming that the tileset is a bitmap font starting with char-code 32
with 15 characters-per-line."
  (code-char
   (+ (* (cl-tiled:tile-row tile) 15)
      (cl-tiled:tile-column tile)
      32)))


;;; ~ Utilities ~

(defun move-cursor (row column &key (stream *standard-output*))
  "Moves cursor to desired position.
Borrowed from https://github.com/gorozhin/chlorophyll/
Copyright © 2022 Mikhail Gorozhin — MIT license"
  (format stream "~C[~A;~AH" #\Esc row column))


(defun clear-screen (&key (stream *standard-output*))
  "Completely clear the terminal screen."
  (move-cursor 0 0 :stream stream)
  (format stream "~C[J" #\Esc))
