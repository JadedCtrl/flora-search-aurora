;;;; A simple TUI-game made for the text-flavoured LibreJam of 2023-06!
;;;; https://jamgaroo.xyz/jams/2

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


(defmacro do-for-cell (matrix &body body)
  "Given a 2d-array (matrix), execute the body for every cell.
The body has access to 4 variables:
  * i/j — The current row/column.
  * dimensions — Dimensions of the given matrix.
  * cell — The value of the current cell."
  `(let* ((dimensions (array-dimensions matrix))
          (max-i (car dimensions))
          (max-j (cadr dimensions))
          (i 0)  (j 0))
     (loop
      (let ((cell (ignore-errors (aref matrix i j))))
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
  "Given two 2D matrices, return a listcontaining on the cells that change between a→b in the following
format:
only the cells that change between a→b — all others are nil."
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


(defun screen-matrix-set-char-cell (matrix cell)
  "Set a matrice's (2d array's) element corresponding to
a Tiled cell's character-value, using it's column and row."
  (setf (aref matrix
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
