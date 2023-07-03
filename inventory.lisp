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

;;;; FLORA-SEARCH-AURORA.INVENTORY
;;;; The menu for inventory selection/management.

(in-package :flora-search-aurora.inventory)


;;; ———————————————————————————————————
;;; Menu-creation! How fun!
;;; ———————————————————————————————————
(defun subseq-head (length sequence)
  "Get a subsequence of SEQUENCE containing its first LENGTH elements. If LENGTH
exceeds the size of SEQUENCE, the returned subsequence is equivalent to SEQUENCE."
  (subseq sequence 0 (…:at-most (length sequence) length)))


(defun items->menu-plist (item-plists &key (row-width 72))
  "Convert a list of items’ plists (that one might get from OVERWORLD’s map
:ITEMS) into a pretty and presentable menu-grid of items, for use with
📋:MENU-STATE(-*)."
  (format *error-output* "ITEMS ~A~%" item-plists)
  (let* ((label-width 7)
         (border-width 2)
         ;; Weird bounding, whoops! I’m so clumsy, it’s endearing instead of just annoying! … Right? =w=”
         (column (- 0 label-width border-width))
         (row 0))
    (mapcar (lambda (item)
              (incf column (+ label-width border-width))
              (when (>= column row-width)
                (incf row)
                (setf column 0))
              (list :en (subseq-head label-width (or (getf (cdr item) :name-en) (getf (cdr item) :id)))
                    :eo (subseq-head label-width (getf (cdr item) :name-eo))
                    :selected (and (eq row 0) (eq column 0))
                    :selection (if (and (eq row 0) (eq column 0)) 100 0)
                    :return nil
                    :row row))
            item-plists)))




;;; ———————————————————————————————————
;;; Inventory loop logic
;;; ———————————————————————————————————
(defun inventory-state-update (map inventory-menu)
  (if (📋:menu-state-update inventory-menu)
      (list :map map :inventory inventory-menu)
      (values nil
             (list :map map))))



;;; ———————————————————————————————————
;;; Inventory loop drawing
;;; ———————————————————————————————————
(defun inventory-state-draw (matrix items)
  (📋:menu-state-draw matrix items))



;;; ———————————————————————————————————
;;; Inventory loop
;;; ———————————————————————————————————
(defun inventory-state (matrix &key map inventory)
  "A state-function for use with STATE-LOOP."
  (sleep .02)
  (inventory-state-draw matrix inventory)
  (inventory-state-update map inventory))


(defun make-inventory-state (map)
  "Return a state-function for inventory-listing, for use with STATE-LOOP."
  (lambda (matrix &key (map map) (inventory (items->menu-plist (gethash :items map))))
    (apply #'🎒:inventory-state
           (list matrix :map map :inventory inventory))))

