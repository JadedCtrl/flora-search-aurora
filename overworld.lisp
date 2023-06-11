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
               '((player . (:x 0 :y 0 :face "uwu" :direction right)))))
  "Render the given map to the matrix and take user-input — for one frame.
A state-function for use with STATE-LOOP."
  (sleep .02)
  (overworld-state-draw matrix map entities-alist)
  (overworld-state-update map entities-alist))


(defun overworld-state-draw (matrix map entities-alist)
  "Draw the overworld map to the given matrix.
A core part of OVERWORLD-STATE."
  (matrix-write-tiled-map-chunk matrix map 0 0)
  (matrix-write-entities matrix entities-alist))


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
                    :x (+ x (getf entity-plist :x))
                    :y (+ y (getf entity-plist :y)))))


(defun move-entity-to (entity entities-alist &key (x 0) (y 0))
  "Move the given entity to the given coordinates."
  (let ((entity-plist (cdr (assoc entity entities-alist))))
    (setf (getf entity-plist :x) x)
    (setf (getf entity-plist :y) y)))



;;; ———————————————————————————————————
;;; Mapping & map-rendering
;;; ———————————————————————————————————
(defun load-map-chunks (map-file)
  (let ((cells (mapcar #'cl-tiled:layer-cells
                       (cl-tiled:map-layers (cl-tiled:load-map map-file)))))
    (collect-items-into-groups
     (car cells)
     (lambda (cell)
       (apply #'coords->symbol (map-chunk-of-tiled-cell cell))))))


(defun matrix-write-tiled-map-chunk (matrix map-alist x y
                                     &key (chunk-width 72) (chunk-height 20))
  (mapcar (lambda (cell)
            (matrix-write-tiled-cell matrix cell
                                     :x-offset (* x chunk-width)
                                     :y-offset (* y chunk-height)))
          (cdr (assoc (coords->symbol x y) map-alist))))


(defun matrix-write-tiled-cell (matrix cell &key (x-offset 0) (y-offset 0))
  "Set a matrice's (2d array's) element corresponding to
a Tiled cell's character-value, using it's column and row."
  (setf (aref matrix
              (- (cl-tiled:cell-row cell) y-offset)
              (- (cl-tiled:cell-column cell) x-offset))
        (tiled-tile-character
         (cl-tiled:cell-tile cell))))


(defun tiled-tile-character (tile)
  "Given a tileset's tile, return it's corresponding text character,
assuming that the tileset is a bitmap font starting with char-code 32
with 15 characters-per-line."
  (code-char
   (+ (* (cl-tiled:tile-row tile) 15)
      (cl-tiled:tile-column tile)
      32)))


(defun map-chunk-of-tiled-cell (cell &key (chunk-width 72) (chunk-height 20))
  "Given a Tiled cell, return a corresponding map chunk it resides in."
  (map-chunk-of-coords (cl-tiled:cell-column cell)
                       (cl-tiled:cell-row cell)
                       :chunk-width chunk-width :chunk-height chunk-height))


(defun map-chunk-of-coords (x y &key (chunk-width 72) (chunk-height 20))
  "Given a pair of coordinates, return the map chunk they reside within."
  (list (floor (/ x chunk-width)) (floor (/ y chunk-height))))


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
            (matrix-write-entity matrix (cdr entity-assoc)))
   entities-alist))


(defun matrix-write-entity (matrix entity-plist)
  "Render an entity-plist to the matrix."
  (let ((x (getf entity-plist :x))
        (y (getf entity-plist :y))
        (face (getf entity-plist :face)))
    (setf (aref matrix y x) #\|)
    (setf (aref matrix y (+ (length face) x 1))
          #\|)
    (render-line matrix face (+ x 1) y)
    (matrix-write-entity-legs matrix entity-plist)))


(defun matrix-write-entity-legs (matrix entity-plist)
  "Draw an entity's legs — a surprisingly in-depth task!"
  (let ((x (getf entity-plist :x))
        (y (+ (getf entity-plist :y) 1))
        (width (+ (length (getf entity-plist :face)) 2))
        (direction (getf entity-plist :direction)))
    (cond ((eq direction 'right)
           (setf (aref matrix y (+ x 1)) #\|)
           (setf (aref matrix y (+ x 2)) #\|))
          ((eq direction 'left)
           (setf (aref matrix y (+ x width -2)) #\|)
           (setf (aref matrix y (+ x width -3)) #\|)))))



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
