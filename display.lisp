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
that change between A→B (favouring those in B) — all others are nil."
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



;;; ———————————————————————————————————
;;; “Rendering” strings to matrix
;;; ———————————————————————————————————
(defun render-line (matrix text coords)
  "Apply a one-line string to the matrix at the given coordinates."
  (let ((dims (array-dimensions matrix))
        (x (getf coords :x))
        (y (getf coords :y)))
    (if (and (stringp text)
             (> (length text) 0))
        (progn
          (ignore-errors (setf (aref matrix y x) (char text 0)))
          (render-line matrix (subseq text 1)
                       (list :x (+ x 1) :y y)))
        matrix)))


(defun render-string-verbatim (matrix string coords)
  "Apply a STRING to a MATRIX at the precise COORDS, preserving newlines.
No word-wrapping is done, even if the line exceeds the MATRIX’es size!"
  (let ((y (- (getf coords :y) 1))
        (x (getf coords :x)))
    (mapcar (lambda (line) (✎:render-line matrix line (list :x x :y (incf y))))
            (str:lines string))))


(defun render-string-partially (matrix text coords &key (char-count 0) (max-column 72) (max-row 20))
  "Partially render the given string to a matrix of characters. Will render only
a portion of the string, dictated by the CHAR-COUNT.
See the similar RENDER-STRING function."
  (let* ((x (getf coords :x))
         (y (getf coords :y))
         (dimensions (array-dimensions matrix))
         (max-column (…:at-most (cadr dimensions) max-column))
         (row-width (- max-column x))
         (max-write-row (…:at-most (…:at-most (car dimensions) max-row)
                                   (floor (/ char-count row-width))))
         (row-width-at-max-write-row
           (…:at-most row-width
                      (- char-count (* max-write-row row-width))))
         (substrings (…:split-string-by-length text row-width))
         (row 0))
    (loop while (and (<= (+ y row) max-row)
                     substrings)
          do (cond ((< row max-write-row)
                    (render-line matrix (pop substrings)
                                 (list :x x :y (+ y row))))
                   ;; At the last line, write only up til the :CHAR-COUNT
                   ((eq row max-write-row)
                    (render-line
                     matrix
                     (subseq (pop substrings) 0 row-width-at-max-write-row)
                     (list :x x :y (+ y row))))
                   ('t
                    (pop substrings)))
             (incf row)))
  matrix)


(defun render-string (matrix text coords &key (max-column 72) (max-row 20))
  "Render the given string to the matrix of characters, character-by-character.
Will line-break or truncate as appropriate and necessary to not exceed the
positional arguments nor the dimensions of the matrix."
  (render-string-partially matrix text coords :max-column max-column :max-row max-row
                                              :char-count (length text)))



;;; ———————————————————————————————————
;;; Misc. utils
;;; ———————————————————————————————————
(defun hide-cursor ()
  (cl-charms/low-level:curs-set 0))


(defun show-cursor ()
  (cl-charms/low-level:curs-set 1))


(defun move-cursor (row column &key (stream *standard-output*))
  "Moves cursor to desired position.
Borrowed from https://github.com/gorozhin/chlorophyll/
Copyright © 2022 Mikhail Gorozhin — MIT license"
  (format stream "~C[~A;~AH" #\Esc row column))


(defun clear-screen (&key (stream *standard-output*))
  "Completely clear the terminal screen."
  (move-cursor 0 0 :stream stream)
  (format stream "~C[J" #\Esc))
