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

;;;; FLORA-SEARCH-AURORA.OVERWORLD
;;;; All game-functions and data relating to the “overworld” (that is,
;;;; the primary gameplay, the RPG-ish-ish bits).

(defpackage :flora-search-aurora.overworld
  (:use :cl
   :flora-search-aurora.input :flora-search-aurora.display
   :flora-search-aurora.ui)
  (:export #:overworld-state))

(in-package :flora-search-aurora.overworld)



;;; ———————————————————————————————————
;;; Overworld loop
;;; ———————————————————————————————————
(defun overworld-state
    (matrix &key (map-path nil) (map (load-map-chunks map-path))
              (entities-alist
               '((player :coords (:x 3 :y 3) :face "uwu" :direction right))))
  "Render the given map to the matrix and take user-input — for one frame.
A state-function for use with STATE-LOOP."
  (sleep .02)
  (overworld-state-draw matrix map entities-alist)
  (overworld-state-update map entities-alist))


(defun overworld-state-draw (matrix map entities)
  "Draw the overworld map to the given matrix.
A core part of OVERWORLD-STATE."
  (let* ((player-data (cdr (assoc 'player entities)))
         (chunk (getf (world-coords->screen-coords (getf player-data :coords)) :chunk)))
    (matrix-write-tiled-map-chunk matrix map chunk)
    (matrix-write-entities matrix entities)))


(defun overworld-state-update (map entities-alist)
  "Do nothing, lol. Core part of OVERWORLD-STATE.
Returns parameters to be used in the next invocation of OVERWORLD-STATE."
  (process-overworld-input map entities-alist)
  (list :map map :entities-alist entities-alist))



;;; ———————————————————————————————————
;;; Overworld logic
;;; ———————————————————————————————————
(defun process-overworld-input (map entities)
  "Get and process any keyboard input, modifying the map or entities as necessary."
  (if (listen)
      (let* ((input (normalize-char-plist (read-char-plist))))
        (cond
          ((plist= input '(:modifier nil :char #\→))
           (move-entity 'player entities :x 1))
          ((plist= input '(:modifier nil :char #\←))
           (move-entity 'player entities :x -1))
          ((plist= input '(:modifier nil :char #\↑))
           (move-entity 'player entities :y -1))
          ((plist= input '(:modifier nil :char #\↓))
           (move-entity 'player entities :y 1))))))


(defun move-entity (entity entities-alist &key (x 0) (y 0))
  "Move an entity relative to its current position."
  (let ((entity-plist (cdr (assoc entity entities-alist))))
    (when (< x 0)
      (setf (getf entity-plist :direction) 'left))
    (when (> x 0)
      (setf (getf entity-plist :direction) 'right))
    (move-entity-to entity entities-alist
                    :x (+ x (getf (getf entity-plist :coords) :x))
                    :y (+ y (getf (getf entity-plist :coords) :y)))))


(defun move-entity-to (entity entities-alist &key (x 0) (y 0))
  "Move the given entity to the given coordinates."
  (let ((entity-plist (cdr (assoc entity entities-alist))))
    (setf (getf (getf entity-plist :coords) :x) x)
    (setf (getf (getf entity-plist :coords) :y) y)))



;;; ———————————————————————————————————
;;; Mapping & map-rendering
;;; ———————————————————————————————————
(defun load-map-chunks (map-file)
  (let ((cells (mapcar #'cl-tiled:layer-cells
                       (cl-tiled:map-layers (cl-tiled:load-map map-file)))))
    (collect-items-into-groups
     (cdar cells) ;; Only take the first layer, for now!
     (lambda (cell)
       (getf (world-coords->screen-coords (tiled-cell-world-coords cell))
             :chunk)))))


(defun matrix-write-tiled-map-chunk (matrix map-alist chunk
                                     &key (chunk-width 72) (chunk-height 20))
  (mapcar (lambda (cell)
            (matrix-write-tiled-cell matrix cell))
          (cdr (assoc chunk map-alist))))


(defun matrix-write-tiled-cell (matrix cell)
  "Set a matrice's (2d array's) element corresponding to
a Tiled cell's character-value, using it's column and row."
  (let ((coords (world-coords->screen-coords (tiled-cell-world-coords cell))))
    (setf (aref matrix
                (getf coords :y)
                (getf coords :x))
          (tiled-tile-character
           (cl-tiled:cell-tile cell)))))


(defun tiled-tile-character (tile)
  "Given a tileset's tile, return it's corresponding text character,
assuming that the tileset is a bitmap font starting with char-code 32
with 15 characters-per-line."
  (code-char
   (+ (* (cl-tiled:tile-row tile) 15)
      (cl-tiled:tile-column tile)
      32)))


(defun tiled-cell-world-coords (cell)
  (list :x (cl-tiled:cell-column cell) :y (cl-tiled:cell-row cell)))


(defun world-coords->screen-coords (world-coords &key (chunk-width 72) (chunk-height 20))
  (let* ((chunk-x (floor (/ (getf world-coords :x)
                            chunk-width)))
         (chunk-y (floor (/ (getf world-coords :y)
                            chunk-height)))
         (x (- (getf world-coords :x) (* chunk-x chunk-width)))
         (y (- (getf world-coords :y) (* chunk-y chunk-height))))
    (list :x x
          :y y
          :chunk (coords->symbol chunk-x chunk-y))))


(defun coords->symbol (x y)
  (intern (format nil "~A,~A" x y)))


(defun symbol->coords (coords-symbol)
  (str:split #\, (symbol-name coords-symbol)))



;;; ———————————————————————————————————
;;; Entity magic (AKA player, NPCs)
;;; ———————————————————————————————————
(defun matrix-write-entities (matrix entities-alist)
  "Draw all entities from an alist of entities to the matrix."
  (mapcar (lambda (entity-assoc)
;;            (ignore-errors
             (matrix-write-entity matrix (cdr entity-assoc)))
   entities-alist))


(defun matrix-write-entity (matrix entity-plist)
  "Render an entity-plist to the matrix."
  (matrix-write-entity-head matrix entity-plist)
  (matrix-write-entity-legs matrix entity-plist))


(defun matrix-write-entity-head (matrix entity-plist)
  "Draw an entity’s head. There aren't any Mami Tomoes in this game, dang it!"
  (let* ((screen-coords (world-coords->screen-coords (getf entity-plist :coords)))
         (direction (getf entity-plist :direction))
         (face (getf entity-plist :face))
         (width (+ (length face) 2)) ;; Face + |borders|
         (y (- (getf screen-coords :y) 1))
         (x (if (eq direction 'right)
                (- (getf screen-coords :x) (floor (/ width 2)) 0)
                (- (getf screen-coords :x) (floor (/ width 2)) 0))))
    (render-line matrix face (+ x 1) y)
    (ignore-errors (setf (aref matrix y x) #\|))
    (ignore-errors (setf (aref matrix y (+ width x -1))
                         #\|))))


(defun matrix-write-entity-legs (matrix entity-plist)
  "Draw a bipdel entity’s legs — a surprisingly in-depth task!"
  (let* ((screen-coords (world-coords->screen-coords (getf entity-plist :coords)))
         (x (getf screen-coords :x))
         (y (getf screen-coords :y))
         (direction (getf entity-plist :direction)))
    (cond ((eq direction 'right)
           (ignore-errors (setf (aref matrix y x) #\|))
           (ignore-errors (setf (aref matrix y (- x 1)) #\|)))
          ((eq direction 'left)
           (ignore-errors (setf (aref matrix y x) #\|))
           (ignore-errors (setf (aref matrix y (+ x 1)) #\|))))))



;;; ———————————————————————————————————
;;; Misc. utility
;;; ———————————————————————————————————
(defun collect-items-into-groups (list key-function)
  "Given a LIST of items and a function categorizing an individual item
(returning a “category” symbol for any given item), return an sorted
associative list."
  (let ((groups-alist '()))
    (loop for item in list
          do (let ((key (apply key-function (list item))))
               (setf (assoc-utils:aget groups-alist key)
                     (append (assoc-utils:aget groups-alist key)
                             (list item)))))
    groups-alist))


(defun every-other-element (list)
  "Collect every-other-element of a list. E.g., (1 2 3 4) → (1 3)."
  (when list
    (cons (car list)
          (every-other-element (cddr list)))))


(defun plist= (a b &key (test #'eql))
  "Return whether or not two property lists are equal, by comparing values of each pair.
Uses the keys of plist a."
  (let ((keys (every-other-element a)))
    (loop for key in keys
          do (when (not (apply test (list (getf a key)
                                          (getf b key))))
                 (return nil))
          finally (return 't))))
