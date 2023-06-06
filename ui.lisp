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
  (:use :cl :flora-search-aurora.display)
  (:export #:ui-loop #:render-menu-strip))

(in-package :flora-search-aurora.ui)


;;; ———————————————————————————————————
;;; Menu loops
;;; ———————————————————————————————————
(defun ui-loop (last-matrix menu-alist)
  "The state loop to be used for displaying/processing/input-managing
with menus."
  (let* ((matrix (make-screen-matrix))
         (new-menu (ui-update matrix menu-alist)))
    (ui-draw matrix last-matrix)
    (sleep 1)
    (ui-loop matrix new-menu)))


(defun ui-draw (matrix last-matrix)
  "The draw loop for menus."
  (print-screen-matrix (matrix-delta last-matrix matrix))
  (finish-output))


(defun ui-update (matrix menu-alist)
  "The update loop for menus. It processes all input, state, etc, and
returns the new state of the menu."
  (render-menu-strip matrix menu-alist 0 0)
  (cdr menu-alist))



;;; ———————————————————————————————————
;;; Menu display
;;; ———————————————————————————————————
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


(defun render-menu-item
    (matrix text x y &key (width (+ (length text) 2)) (height 3) (selected 0))
  "Render a “menu-item” — that is, text surrounded by a box with an optional
'selected' form. If selected is a non-zero number below 100, then that percent
of the box will be displayed as selected/highlighted. This percent is from
left-to-right, unless negative — in which case, right-to-left."
  (render-string matrix text (+ x 1) (+ 1 y)
                 :max-column (- (+ x width) 1)
                 :max-row (- (+ y height) 2))

  ;; Render the normal top and bottom bars.
  (dotimes (i width)
    (setf (aref matrix y (+ x i)) #\-)
    (setf (aref matrix (+ y (- height 1)) (+ x i)) #\-))

  ;; Render the weird “selected” top and bottom bars. A menu item might be
  ;; only partially-selected…
  (if (and selected
           (not (eq selected 0)))
      (let* ((bar-width
               (at-most width (ceiling (* width (* (abs selected)
                                                   .01)))))
             (bar-start (if (> 0 selected) (- width bar-width) 0)))
        (dotimes (i bar-width)
          (setf (aref matrix y (+ x bar-start i)) #\=)
          (setf (aref matrix (+ y (- height 1)) (+ x bar-start i)) #\=))))

  ;; Render the horizontal “earmuffs” for helping the selected item stand out.
  (when (and selected
             (not (eq selected 0)))
    (dotimes (i (- height 2))
      (setf (aref matrix (+ y i 1) x) #\|)
      (setf (aref matrix (+ y i 1) (+ x width -1)) #\|))
   matrix))


(defun render-menu-strip (matrix items x y &key (max-item-width 12) (height 3))
  "Render several menu items to the matrix, starting at the given x/y coordinates,
maximum width for any given item, and the height of all items. If given a selected
item, than selected-% percent of it is displayed as highlighted/selected."
  (let ((x x))
    (mapcar
     (lambda (item)
       (let* ((label (car item))
              (selected (or (assoc 'selected (cdr item))
                            0))
              (width (at-most max-item-width
                              (+ (length label) 2))))
         (render-menu-item matrix label x y
                           :width width
                           :height height
                           :selected selected)
         (setf x (+ x width 1))))
     items))
  matrix)


(defun render-string (matrix text x y &key (max-column 72) (max-row 20))
  "Render the given string to the matrix of characters, character-by-character.
Will line-break or truncate as appropriate and necessary to not exceed the
positional arguments nor the dimensions of the matrix."
  (let* ((dimensions (array-dimensions matrix))
         (max-column (at-most (cadr dimensions) max-column))
         (max-row (at-most (car dimensions) max-row))
         (substrings (split-string-by-length text (- max-column x)))
         (row 0))
    (loop while (and (<= (+ y row) max-row)
                     substrings)
          do
             (render-line matrix (pop substrings)
                          x (+ y row))
             (incf row)))
  matrix)



;;; ———————————————————————————————————
;;; Misc. utils
;;; ———————————————————————————————————
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


(defun at-most (maximum num)
  "This function returns at most every hope and dream you've ever had, and at
minimum returns your more pitiful of moments."
  (if (> num maximum)
      maximum
      num))


(defun at-least (minimum num)
  "This function returns at least every hope and dream you've ever had, and at
maximum returns your more pitiful of moments."
  (if (< num minimum)
      minimum
      num))


(defun ± (num add)
  "Either add to a number, or subtract from it; whichever brings it closer to zero.
In addition, the resultant value shall not “pass” zero."
  (cond
    ((< num 0)
     (at-most 0 (+ num add)))
    ((> num 0)
     (at-minimum 0 (- num add)))
    ('t
     num)))
