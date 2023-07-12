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
;;; Misc. Utils
;;; ———————————————————————————————————
(defun subseq-head (length sequence)
  "Get a subsequence of SEQUENCE containing its first LENGTH elements. If LENGTH
exceeds the size of SEQUENCE, the returned subsequence is equivalent to SEQUENCE."
  (subseq sequence 0 (…:at-most (length sequence) length)))



;;; ———————————————————————————————————
;;; Menu-creation! How fun!
;;; ———————————————————————————————————
(defun items->menu-plist (item-plists &key (row-width 72))
  "Convert a list of items’ plists (that one might get from OVERWORLD’s map
:ITEMS) into a pretty and presentable menu-grid of items, for use with
📋:MENU-STATE(-*)."
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
              (append (list :en (subseq-head label-width (or (getf (cdr item) :inv-name-en)
                                                             (getf (cdr item) :name-en)
                                                             (getf (cdr item) :id)))
                            :eo (subseq-head label-width (or (getf (cdr item) :inv-name-eo)
                                                             (getf (cdr item) :name-eo)))
                            :selected (and (eq row 0) (eq column 0))
                            :selection (if (and (eq row 0) (eq column 0)) 100 0)
                            :submenu 't
                            :row row)
                      (cdr item)))
            item-plists)))



;;; ———————————————————————————————————
;;; Inventory loop logic
;;; ———————————————————————————————————
(defun submenu (item)
  "Create a ITEM’s submenu, from its plist."
  (nconc (when (getf item :use)
           (list
            (list :en "Use it!"
                  :eo  "Uzi ĝin!"
                  :selected 't :selection 100
                  :use 't)))
         (list
           (list :en "Lose it!"
                 :eo "Ne!"
                 :use nil))))


(defun inventory-state-update (map inventory-menu submenu)
  "The input-taking/logic-handling component of the inventory state-function.
Part of INVENTORY-STATE."
  (let ((menu-return (📋:menu-state-update (if submenu submenu inventory-menu)))
        (selected-item (📋:selected-menu-item inventory-menu)))
    (cond
      ;; Display the pop-up menu when an item is selected.
      ((and (listp menu-return) (getf menu-return :submenu))
       (list :parameters (list :map map :inventory inventory-menu
                               :submenu (submenu selected-item))))
      ;; User decided to use the item, let’s go!
      ((and (listp menu-return) (getf menu-return :use) (getf selected-item :use))
       (funcall (…:string->symbol (getf selected-item :use)) map selected-item))
      ;; User decided not to use the item.
      ((and (listp menu-return) (member :use menu-return))
       (list :parameters (list :map map :inventory inventory-menu)))
      ;; Return the menu itself, if non-nil.
      (menu-return
       menu-return)
      ;; If menu-return was non-nil, the user left the menu. Let’s leave inventory!
      ('t
       (list :drop 1 :parameters (list :map map))))))



;;; ———————————————————————————————————
;;; Inventory loop drawing
;;; ———————————————————————————————————
(defun render-selected-item (matrix items)
  "Draw the title, avatar, and description of the currently-selected item to
the bottom of the screen."
  (let* ((item (menu:selected-menu-item items))
         (name (list :en (or (getf item :name-en) (getf item :inv-name-en) (getf item :id))
                     :eo (or (getf item :name-eo) (getf item :inv-name-eo))))
         (desc (list :en (getf item :desc-en)
                     :eo (getf item :desc-eo))))
    (display:render-string-verbatim matrix (str:concat ":" (…:getf-lang name) ":  "
                                                       (getf item :avatar))
                                    '(:x 1 :y 17))
    (display:render-string matrix (…:getf-lang desc)
                          '(:x 1 :y 18) :width 70)))


(defun inventory-state-draw (matrix items submenu)
  "The drawing component of the inventory state-function.
Part of INVENTORY-STATE."
  (📋:menu-state-draw matrix items)
  (render-selected-item matrix items)
  (when submenu
    (📋:menu-state-draw matrix submenu)))



;;; ———————————————————————————————————
;;; Inventory loop
;;; ———————————————————————————————————
(defun inventory-state (matrix &key map inventory (submenu nil))
  "A state-function for use with STATE-LOOP."
  (sleep .02)
  (inventory-state-draw matrix inventory submenu)
  (inventory-state-update map inventory submenu))


(defun make-inventory-state (map)
  "Return a state-function for inventory-listing, for use with STATE-LOOP."
  (lambda (matrix &key (map map) (submenu nil) (inventory (items->menu-plist (gethash :items map))))
    (apply #'🎒:inventory-state
           (list matrix :map map :inventory inventory :submenu submenu))))
