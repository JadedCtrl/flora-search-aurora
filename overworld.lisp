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
(defun overworld-state (matrix &key (map-path nil)
                                 (map (cl-tiled:load-map map-path))
                                 (entities-alist '((player . (:x 0 :y 0 :icon #\@)))))
  "Render the given map to the matrix and take user-input — for one frame.
A state-function for use with #'state-loop."
  (sleep .02)
  (overworld-state-draw matrix map entities-alist)
  (overworld-state-update map entities-alist))


(defun overworld-state-draw (matrix map entities-alist)
  "Draw the overworld map to the given matrix.
A core part of #'overworld-state."
  (matrix-write-tiled-map matrix map)
  (matrix-write-entities matrix entities-alist))


(defun overworld-state-update (map entities-alist)
  "Do nothing, lol. Core part of #'overworld-state.
Returns parameters to be used in the next invocation of #'overworld-state."
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
(defun matrix-write-tiled-cell (matrix cell)
  "Set a matrice's (2d array's) element corresponding to
a Tiled cell's character-value, using it's column and row."
  (setf (aref matrix
              (cl-tiled:cell-row cell)
              (cl-tiled:cell-column cell))
        (tiled-tile-character
         (cl-tiled:cell-tile cell))))


(defun matrix-write-tiled-map (matrix map)
  "Draw a Tiled-format tilemap to the 2D array."
  (mapcar (lambda (layer) (matrix-write-tiled-map-layer matrix layer))
          (cl-tiled:map-layers map))
  matrix)


(defun matrix-write-tiled-map-layer (matrix tile-layer)
  "Set an array's elements to those corresponding the given Tiled
tile-layer's cells. a Tiled tile-layer to the screen."
  (mapcar (lambda (cell) (matrix-write-tiled-cell matrix cell))
          (cl-tiled:layer-cells tile-layer))
  matrix)


(defun tiled-tile-character (tile)
  "Given a tileset's tile, return it's corresponding text character,
assuming that the tileset is a bitmap font starting with char-code 32
with 15 characters-per-line."
  (code-char
   (+ (* (cl-tiled:tile-row tile) 15)
      (cl-tiled:tile-column tile)
      32)))



;;; ———————————————————————————————————
;;; Entity magic (AKA player, NPCs)
;;; ———————————————————————————————————
(defun matrix-write-entities (matrix entities-alist)
  "Draw all entities from an alist of entities to the matrix."
  (mapcar (lambda (entity-assoc)
            (print entity-assoc)
            (force-output)
            (matrix-write-entity matrix (cdr entity-assoc)))
   entities-alist))


(defun matrix-write-entity (matrix entity-plist)
  "Render an entity-plist to the matrix."
  (setf (aref matrix (getf entity-plist :y) (getf entity-plist :x))
        (getf entity-plist :icon)))



;;; ———————————————————————————————————
;;; Misc. utility
;;; ———————————————————————————————————
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
