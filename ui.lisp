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

;;;; FLORA-SEARCH-AURORA.UI
;;;; Generic menu-making, displaying, and management.
;;;; Let's get to it, we're on a deadline!

(defpackage :flora-search-aurora.ui
  (:use :cl :flora-search-aurora.display))

(in-package :flora-search-aurora.ui)


(defun render-line (matrix text x y)
  "Apply a one-line string to the matrix at the given coordinates."
  (if (and (stringp text)
           (> (length text) 0))
      (progn
        (setf (aref matrix y x)
              (char text 0))
        (render-line matrix (subseq text 1)
                     (+ x 1) y))
      matrix))


(defun render-string (matrix text x y &key (max-column 72) (max-row 20))
  "Render the given string to the matrix of characters, character-by-character.
Will line-break or truncate as appropriate and necessary to not exceed the
positional arguments nor the dimensions of the matrix."
  (let* ((dimensions (array-dimensions matrix))
         (max-column (at-most (cadr dimensions) max-column))
         (max-row (at-most (car dimensions) max-row))
         (substrings (split-string-by-length text (- max-column x)))
         (row 0))
    (loop while (and (< (+ y row) max-row)
                     substrings)
          do
             (print (+ y row))
             (render-line matrix (pop substrings)
                          x (+ y row))
             (incf row)))
  matrix)


(defun split-string-by-length (string line-length &key (substrings '()))
  "Given a string, split it into a list of substrings all with lengths
equal or lower to the given length."
  (if (> (length string) line-length)
      (split-string-by-length
       (subseq string line-length)
       line-length
       :substrings (append substrings
                           `(,(subseq string 0 line-length))))
      (append substrings `(,string))))



;;"---{=============   -------------------"
;; | Kill your mom |    Give into despair
;; ---{=============   -------------------

