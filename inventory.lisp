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
;;; Inventory loop logic
;;; ———————————————————————————————————
(defun inventory-state-update (map)
  (if (and (listen)
           (let ((input (getf (⌨:read-gamefied-char-plist) :semantic)))
             (or (eq input '⌨:🆗)
                 (eq input '⌨:❎))))
    (values nil
            (list :map map))
    't))



;;; ———————————————————————————————————
;;; Inventory loop drawing
;;; ———————————————————————————————————
(defun inventory-state-draw (matrix items)
  (📋:render-string matrix (format nil "~A" items) '(:x 0 :y 0)))



;;; ———————————————————————————————————
;;; Inventory loop
;;; ———————————————————————————————————
(defun inventory-state (matrix &key map)
  "A state-function for use with STATE-LOOP."
  (sleep .02)
  (inventory-state-draw matrix (gethash :items map))
  (inventory-state-update map))


(defun make-inventory-state (map)
  "Return a state-function for inventory-listing, for use with STATE-LOOP."
  (lambda (matrix &key (map map))
    (apply #'🎒:inventory-state
           (list matrix :map map))))

