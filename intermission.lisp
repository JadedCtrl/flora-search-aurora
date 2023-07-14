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

;;;; FLORA-SEARCH-AURORA.INTERMISSION
;;;; Used to render flashy little transitions in-between scenes/maps/etc.

(in-package :flora-search-aurora.intermission)


;;; ———————————————————————————————————
;;; Intermission loop logic
;;; ———————————————————————————————————
(defun intermission-state-update (title subtitle side-text progress return)
  "The input-taking/logic-handling component of the inventory state-function.
Part of INVENTORY-STATE."
  (if (and (⌨:pressed-enter-p) (> progress 60))
      return
      (list :parameters
            (list :title title
                  :subtitle subtitle
                  :side-text side-text
                  :return return
                  :progress (…:at-most 107 (+ progress .5))))))



;;; ———————————————————————————————————
;;; Intermission loop drawing
;;; ———————————————————————————————————
(defun render-centered-line (matrix string &key (y 0))
  "Given a STRING and a Y-position, render it to the MATRIX centered horizontally."
  (✎:render-line
   matrix
   string
   (list :y y
         :x (floor (/ (- (second (array-dimensions matrix))
                         (length string))
                      2)))))


(defun intermission-state-draw (matrix title subtitle side-text progress)
  "The drawing component of the inventory state-function.
Part of INVENTORY-STATE."
  (let* ((title (…:getf-lang title))
         (title-border (subseq (make-string (length title) :initial-element #\=)
                               0 (…:at-least 0 (…:at-most (length title)
                                                          (floor (- progress 35)))))))
    ;; Add a bit of a “blank” between scenes.
    ;; Why? Pacing, of course!
    (when (> progress 35)
      ;; Render the title
      (render-centered-line matrix title :y 1)
      ;; Render the borders surrounding the title
      (render-centered-line matrix title-border :y 0)
      (render-centered-line matrix title-border :y 2)
      ;; Now the sub-title…
      (render-centered-line matrix (…:getf-lang subtitle) :y 4)
      ;; And the side-text…!
      (✎:render-string matrix (…:getf-lang side-text) '(:x 15 :y 10) :width 47)
      ;; A little touch; a simple animation-ish line down the middle.
      (✎:render-line
       matrix
       (subseq (make-string (second (array-dimensions matrix))
                            :initial-element #\~)
               0 (…:at-least 0 (floor (- progress 35))))
       '(:x 0 :y 9)))))



;;; ———————————————————————————————————
;;; Intermission loop
;;; ———————————————————————————————————
(defun intermission-state (matrix &key title subtitle side-text progress return)
  "A state-function for use with STATE-LOOP."
  (sleep .02)
  (intermission-state-draw matrix title subtitle side-text progress)
  (intermission-state-update title subtitle side-text progress return))


(defun make-intermission-function (title subtitle side-text return)
  "Return a state-function for intermission, for use with STATE-LOOP."
  (lambda (matrix &key (title title) (subtitle subtitle) (side-text side-text) (return return) (progress 0))
    (funcall #'intermission-state
             matrix :title title :subtitle subtitle :side-text side-text :progress progress :return return)))
