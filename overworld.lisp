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
  (:export #:overworld-state #:player))

(in-package :flora-search-aurora.overworld)



;;; ———————————————————————————————————
;;; Overworld loop
;;; ———————————————————————————————————
(defun overworld-state
    (matrix &key (map-path nil) (map (load-map map-path)))
  "Render the given map to the matrix and take user-input — for one frame.
A state-function for use with STATE-LOOP."
  (sleep .02)
  (overworld-state-draw matrix map)
  (overworld-state-update map))


(defun overworld-state-draw (matrix map)
  "Draw the overworld map to the given matrix.
A core part of OVERWORLD-STATE."
  (let* ((player-data (cdr (assoc 'PLAYER (getf map :entities))))
         (chunk (world-coords-chunk (getf player-data :coords))))
    (matrix-write-map-chunk matrix map chunk)
    (matrix-write-entities matrix map)))


(defun overworld-state-update (map)
  "Do nothing, lol. Core part of OVERWORLD-STATE.
Returns parameters to be used in the next invocation of OVERWORLD-STATE."
  (process-overworld-input map)
  (list :map map))



;;; ———————————————————————————————————
;;; Overworld logic
;;; ———————————————————————————————————
(defun process-overworld-input (map)
  "Get and process any keyboard input, modifying the map or entities as necessary."
  (if (listen)
      (let* ((input (normalize-char-plist (read-char-plist))))
        (cond
          ((plist= input '(:modifier nil :char #\→))
           (move-entity map 'player :x 1))
          ((plist= input '(:modifier nil :char #\←))
           (move-entity map 'player :x -1))
          ((plist= input '(:modifier nil :char #\↑))
           (move-entity map 'player :y -1))
          ((plist= input '(:modifier nil :char #\↓))
           (move-entity map 'player :y 1))))))


(defun move-entity (map entity &key (x 0) (y 0))
  "Move an entity relative to its current position."
  (let ((entity-plist (cdr (assoc entity (getf map :entities)))))
    (when (< x 0)
      (setf (getf entity-plist :direction) 'left))
    (when (> x 0)
      (setf (getf entity-plist :direction) 'right))
    (move-entity-to map entity
                    :x (+ x (getf (getf entity-plist :coords) :x))
                    :y (+ y (getf (getf entity-plist :coords) :y)))))


(defun move-entity-to (map entity &key (x 0) (y 0))
  "Move the given entity to the given coordinates."
  (let ((entity-plist (cdr (assoc entity (getf map :entities)))))
    (when (walkable-tile-p map x y)
      (setf (getf (getf entity-plist :coords) :x) x)
      (setf (getf (getf entity-plist :coords) :y) y))))



;;; ———————————————————————————————————
;;; Mapping & map-rendering
;;; ———————————————————————————————————
(defun load-map (map-file)
  "Parse a map-file into an plist of its data.)
At the moment, this consists solely of :TILE-CHUNKS, all visible cells sorted
into an alist by their “chunk” on the map."
  (let ((tile-chunks '())
        (bump-map '())
        (entities '()))
    (mapcar (lambda (layer)
              (typecase layer
                (cl-tiled.data-types:tile-layer
                 (when (gethash "colliding" (cl-tiled:properties layer) #'string-equal)
                   (setf bump-map (tiled-tile-layer-chunks layer bump-map)))
                 (setf tile-chunks (tiled-tile-layer-chunks layer tile-chunks)))
                (cl-tiled.data-types:object-layer
                 (setf entities (tiled-object-layer-entities layer entities)))))
            (cl-tiled:map-layers (cl-tiled:load-map map-file)))
    (list :tiles tile-chunks :bump-map bump-map :entities entities)))


(defun tiled-tile-layer-chunks (layer &optional (chunks '()))
  "Given a Tiled tile-layer (that is, graphics of the map), parse it into an
alist of Tiled cell “chunks”."
  (let ((cells (mapcar #'tiled-cell->cell (cl-tiled:layer-cells layer))))
    (collect-items-into-groups
     cells
     (lambda (cell)
       (world-coords-chunk (getf cell :coords)))
     :groups chunks)))


(defun tiled-object-layer-entities (layer &optional (entities '()))
  (append
   entities
   (mapcar
    (lambda (object)
      (tiled-object->entity object
                      (cl-tiled:layer-map layer)))
    (layer-objects layer))))


(defun tiled-object->entity (tiled-obj tiled-map)
  (let ((properties (cl-tiled:properties tiled-obj)))
    (list (intern (string-upcase (gethash "id" properties #'string-equal)))
          :coords (list :x (floor (/ (cl-tiled:object-x tiled-obj)
                                     (cl-tiled:map-tile-width tiled-map)))
                        :y (floor (/ (cl-tiled:object-y tiled-obj)
                                     (cl-tiled:map-tile-height tiled-map))))
          :face (gethash "face" properties #'string-equal)
          :direction (if (gethash "facing_right" properties #'string-equal)
                         'right
                         'left))))


(defun tiled-cell->cell (tiled-cell)
  (list :coords (list :x (cl-tiled:cell-column tiled-cell)
                      :y (cl-tiled:cell-row tiled-cell))
        :char (tiled-tile-character (cl-tiled:cell-tile tiled-cell))))


(defun matrix-write-map-chunk (matrix map chunk
                               &key (chunk-width 72) (chunk-height 20))
  "Draw a map’s specific chunk (by its ID) to the matrix."
  (mapcar (lambda (cell) 
            (matrix-write-cell matrix cell))
          (cdr (assoc chunk (getf map :tiles)))))


(defun matrix-write-cell (matrix cell)
  "Set a matrice's (2d array's) element corresponding to a “cell”; that is, an
alist containing a character (:CHAR) and :X & :Y coordinates."
  (let ((coords (world-coords->screen-coords (getf cell :coords))))
    (setf (aref matrix
                (getf coords :y)
                (getf coords :x))
          (getf cell :char))))


(defun layer-objects (layer)
  (slot-value layer 'cl-tiled.data-types::objects))


(defun tiled-tile-character (tile)
  "Given a tileset's tile, return it's corresponding text character,
assuming that the tileset is a bitmap font starting with char-code 32
with 15 characters-per-line."
  (code-char
   (+ (* (cl-tiled:tile-row tile) 15)
      (cl-tiled:tile-column tile)
      32)))


(defun cell-at-world-coords-p (map-chunks coords)
  "Return whether or not there is a cell at the given coordinates."
  (let ((chunk (world-coords-chunk coords)))
    (member 't (cdr (assoc chunk map-chunks))
            :test (lambda (ignored cell)
                    (plist= (getf cell :coords) coords)))))


(defun walkable-tile-p (map x y)
  "Return whether or not the given coordinates on the map are traversable for an entity."
  (not (cell-at-world-coords-p (getf map :bump-map)
                               (list :x x :y y))))


(defun world-coords->screen-coords (world-coords &key (chunk-width 72) (chunk-height 20))
  "Given a set of “world” coordinates, determine where this spot would be on the screen.
The world is split into screen-sized “chunks” to this end.
— Chester P. Runk"
  (let* ((chunk-x (floor (/ (getf world-coords :x)
                            chunk-width)))
         (chunk-y (floor (/ (getf world-coords :y)
                            chunk-height)))
         (x (- (getf world-coords :x) (* chunk-x chunk-width)))
         (y (- (getf world-coords :y) (* chunk-y chunk-height))))
    (list :x x
          :y y
          :chunk (coords->symbol chunk-x chunk-y))))


(defun world-coords-chunk (coords)
  (getf (world-coords->screen-coords coords) :chunk))


(defun coords->symbol (x y)
  (intern (format nil "~A,~A" x y)))


(defun symbol->coords (coords-symbol)
  (str:split #\, (symbol-name coords-symbol)))



;;; ———————————————————————————————————
;;; Entity magic (AKA player, NPCs)
;;; ———————————————————————————————————
(defun matrix-write-entities (matrix map)
  "Draw all entities from an alist of entities to the matrix."
  (mapcar (lambda (entity-assoc)
            (matrix-write-entity matrix (cdr entity-assoc)))
   (getf map :entities)))


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
(defun collect-items-into-groups (list key-function &key (groups '()))
  "Given a LIST of items and a function categorizing an individual item
(returning a “category” symbol for any given item), return an sorted
associative list."
  (loop for item in list
        do (let ((key (apply key-function (list item))))
             (setf (assoc-utils:aget groups key)
                   (append (assoc-utils:aget groups key)
                           (list item)))))
  groups)


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
