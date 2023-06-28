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

(in-package :flora-search-aurora.ui)


;;; ———————————————————————————————————
;;; Menu loops
;;; ——————————————————————————————————
(defun make-menu-state (menu-list)
  "Return a state-function for the game’s main menu, for use with STATE-LOOP."
  (lambda (matrix)
    (📋:menu-state matrix menu-list)))


(defun menu-state (matrix menu-alist)
  "Render a menu in menu-alist format to the given matrix, and process user-input.
A state-function for use with the #'state-loop."
  (sleep .02)
  (menu-state-draw matrix menu-alist)
  (menu-state-update menu-alist))


(defun menu-state-draw (matrix menu-alist)
  "Render a menu in menu-alist format to the given matrix.
A core part of #'menu-state."
  (render-menu-strip matrix menu-alist 0 0))


(defun menu-state-update (menu-alist)
  "Update a menu — that is, take user input and modify the menu’s alist appropriately.
A core part of #'menu-state."
  (progress-menu-items menu-alist)
  (process-menu-input menu-alist))



;;; ———————————————————————————————————
;;; Menu display
;;; ———————————————————————————————————
(defun render-line (matrix text x y)
  "Apply a one-line string to the matrix at the given coordinates."
  (let ((dims (array-dimensions matrix)))
    (if (and (stringp text)
             (> (length text) 0))
        (progn
          (ignore-errors (setf (aref matrix y x) (char text 0)))
          (render-line matrix (subseq text 1)
                       (+ x 1) y))
        matrix)))


(defun render-menu-item
    (matrix text x y &key (width (+ (length text) 2)) (height 3) (selection 0) (selected nil))
  "Render a “menu-item” — that is, text surrounded by a box with an optional
'selected' form. If selected is a non-zero number below 100, then that percent
of the box will be displayed as selected/highlighted. This percent is from
left-to-right, unless negative — in which case, right-to-left."
  (render-string matrix text (list :x (+ x 1) :y (+ 1 y))
                 :max-column (- (+ x width) 1)
                 :max-row (- (+ y height) 2))
  ;; Render the normal top and bottom bars.
  (dotimes (i width)
    (setf (aref matrix y (+ x i)) #\-)
    (setf (aref matrix (+ y (- height 1)) (+ x i)) #\-))

  ;; Render the weird “selected” top and bottom bars. A menu item might be
  ;; only partially-selected…
  (if (and selection
           (not (eq selection 0)))
      (let* ((bar-width
               (…:at-most width (ceiling (* width (* (abs selection)
                                                     .01)))))
             (bar-start (if (> 0 selection) (- width bar-width) 0)))
        (dotimes (i bar-width)
          (setf (aref matrix y (+ x bar-start i)) #\=)
          (setf (aref matrix (+ y (- height 1)) (+ x bar-start i)) #\=))))

  ;; Render the horizontal “earmuffs” for helping the selected item stand out.
  (when selected
    (dotimes (i (- height 2))
     (setf (aref matrix (+ y i 1) x) #\|)
     (setf (aref matrix (+ y i 1) (+ x width -1)) #\|))
   matrix))


(defun render-menu-strip (matrix items x y &key (max-item-width 12) (height 3))
  "Render several menu items to the matrix, starting at the given x/y coordinates,
maximum width for any given item, and the height of all items.
The item list should be an alist of the following format:
   (((LABEL :en “BIRD” :eo “BIRDO”)(SELECTED . T)(SELECTION . 100))
    ((LABEL :en “BAR” :eo “BARO”)(SELECTION . -20)) ⋯)"
  (let ((x x))
    (mapcar
     (lambda (item)
       (let* ((label (…:getf-lang (cdr (assoc 'label item))))
              (selection (or (cdr (assoc 'selection item))
                             0))
              (width (…:at-most max-item-width
                              (+ (length label) 2))))
         (render-menu-item matrix label x y
                           :width width
                           :height height
                           :selection selection
                           :selected (cdr (assoc 'selected item)))
         (setf x (+ x width 1))))
     items))
  matrix)


(defun render-string (matrix text coords &key (max-column 72) (max-row 20))
  "Render the given string to the matrix of characters, character-by-character.
Will line-break or truncate as appropriate and necessary to not exceed the
positional arguments nor the dimensions of the matrix."
  (render-string-partially matrix text coords :max-column max-column :max-row max-row
                           :char-count (length text)))


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
                                 x (+ y row)))
                   ;; At the last line, write only up til the :CHAR-COUNT
                   ((eq row max-write-row)
                    (render-line
                     matrix
                     (subseq (pop substrings) 0 row-width-at-max-write-row)
                     x (+ y row)))
                   ('t
                    (pop substrings)))
             (incf row)))
  matrix)



;;; ———————————————————————————————————
;;; Menu logic
;;; ———————————————————————————————————
(defun progress-menu-items (menu-alist)
  "Given an associative list of menu-items, decrement or increment each
item's “selected-percentage”, so that they converge at the right percent.
That is, 0 for non-selected items and 100 for selected items."
  (mapcar
   (lambda (item)
     (let* ((selection (assoc 'selection item))
            (selection-num (or (cdr selection) 0))
            (selectedp (cdr (assoc 'selected item))))
       (if selection
           (setf (cdr selection)
                 (gravitate-toward
                  (cond ((and selectedp (< selection-num 0) 0)
                         -100)
                        (selectedp 100)
                        ('t 0))
                  (cdr selection) 10)))))
   menu-alist)
  menu-alist)


(defun process-menu-input (menu-alist)
  "Get and process any keyboard input, modifying the menu alist as necessary."
  (if (listen)
      (let* ((input (⌨:read-gamefied-char-plist))
             (selected-item (nth (selected-menu-item-position menu-alist)
                                 menu-alist))
             (func (cdr (assoc 'function selected-item)))
             (return-val (assoc 'return selected-item)))
        (case (getf input :semantic)
          ('⌨:→ (progn (select-right-menu-item menu-alist)
                       't))
          ('⌨:← (progn (select-left-menu-item menu-alist)
                       't))
          ('⌨:❎
           nil)
          ('⌨:🆗
           (cond ((and func return-val)
                  (apply func '())
                  (cdr return-val))
                 (func
                  (apply func '()))
                 (return-val
                  (cdr return-val))
                 ('t
                  't)))
          (otherwise 't)))
      't))


(defun select-menu-item (menu-alist position)
  "Given a menu associative list, select the menu-item at the given position."
  (let ((old-position (selected-menu-item-position menu-alist)))
    ;; The “polarity” (direction of selection) depends on the relative
    ;; direction of the previous selection.
    (setf (aget (nth position menu-alist) 'selection)
      (if (< old-position position) 10 -10))
    (setf (aget (nth position menu-alist) 'selected) 't)
    ;; Likewise for the previously-selected item.
    (setf (aget (nth old-position menu-alist) 'selection)
          (if (< old-position position) -90 90))
    (setf (aget (nth old-position menu-alist) 'selected) nil))
  menu-alist)


(defun select-right-menu-item (menu-alist)
  "Select the item to the right of the currenty-selected item."
  (let ((new-selection (+ (selected-menu-item-position menu-alist) 1)))
    (if (< new-selection (length menu-alist))
      (select-menu-item menu-alist new-selection))))


(defun select-left-menu-item ( menu-alist)
  "Select the item to the left of the currenty-selected item."
  (let ((new-selection (- (selected-menu-item-position menu-alist) 1)))
    (if (>= new-selection 0)
      (select-menu-item menu-alist new-selection))))


(defun selected-menu-item-position (menu-alist)
  "Returns the index of the menu alist's selected item."
  (or (position
       't menu-alist
       :test (lambda (ignore list-item)
               (cdr (assoc 'selected list-item))))
      0))



;;; ———————————————————————————————————
;;; Misc. utils
;;; ———————————————————————————————————
(defun gravitate-toward (goal num delta)
  "Either add to a number, or subtract from it; whichever brings it closer to zero.
In addition, the resultant value shall not “pass” zero."
  (cond
    ((< num goal)
     (…:at-most goal (+ num delta)))
    ((> num goal)
     (…:at-least goal (- num delta)))
    ('t
     goal)))


;;"---{=============   -------------------"
;; | Kill your mom |    Give into despair
;; ---{=============   -------------------
