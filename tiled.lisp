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

(defpackage :flora-search-aurora.overworld.tiled
  (:use :cl)
  (:export #:load-map
           #:world-coords-chunk #:world-coords->screen-coords))

(in-package :flora-search-aurora.overworld.tiled)


;;; ———————————————————————————————————
;;; Tiled maps → Map lists
;;; ———————————————————————————————————
(defun load-map (map-file)
  "Parse a map-file into an plist of its data. This consists of:
  :BUMP-MAP, an alist of tiles (keyed by chunk) in a “collidable” layer
  :TILES, an alist of visible tiles (keyed by chunk).
  :ENTITIES, a list of entity plists."
  (let ((tile-chunks '())
        (bump-map '())
        (entities '())
        (hash (make-hash-table)))
    (mapcar (lambda (layer)
              (typecase layer
                (cl-tiled.data-types:tile-layer
                 (when (gethash "colliding" (cl-tiled:properties layer) #'string-equal)
                   (setf bump-map (tile-layer-chunks layer bump-map)))
                 (setf tile-chunks (tile-layer-chunks layer tile-chunks)))
                (cl-tiled.data-types:object-layer
                 (setf entities (object-layer-entities layer entities)))))
            (cl-tiled:map-layers (cl-tiled:load-map map-file)))
    (setf (gethash :tiles hash) tile-chunks)
    (setf (gethash :bump-map hash) bump-map)
    (setf (gethash :entities hash) entities)
    hash))


(defun object-layer-entities (layer &optional (entities '()))
  "Convert all objects in an object layer into entity plists."
  (append
   entities
   (mapcar
    (lambda (object)
      (tiled-object->entity object
                      (cl-tiled:layer-map layer)))
    (layer-objects layer))))


(defun tiled-object->entity (tiled-obj tiled-map)
  "Convert a Tiled object into an entity plist."
  (let ((properties (cl-tiled:properties tiled-obj)))
    (list (intern (string-upcase (gethash "id" properties #'string-equal)))
          :coords (list :x (floor (/ (cl-tiled:object-x tiled-obj)
                                     (cl-tiled:map-tile-width tiled-map)))
                        :y (floor (/ (cl-tiled:object-y tiled-obj)
                                     (cl-tiled:map-tile-height tiled-map))))
          :face (gethash "face" properties #'string-equal)
          :interact (gethash "interact" properties #'string-equal)
          :direction (if (gethash "facing_right" properties #'string-equal)
                         'right
                         'left))))


(defun tile-layer-chunks (layer &optional (chunks '()))
  "Given a Tiled tile-layer (that is, graphics of the map), parse it into an
alist of Tiled cell “chunks”."
  (let ((cells (mapcar #'tiled-cell->cell (cl-tiled:layer-cells layer))))
    (collect-items-into-groups
     cells
     (lambda (cell)
       (world-coords-chunk (getf cell :coords)))
     :groups chunks)))


(defun tiled-cell->cell (tiled-cell)
  "Convert a Tiled cell into a cell plist."
  (list :coords (list :x (cl-tiled:cell-column tiled-cell)
                      :y (cl-tiled:cell-row tiled-cell))
        :char (tile-character (cl-tiled:cell-tile tiled-cell))))


(defun layer-objects (layer)
  "Return all Tiled objects in the given object layer."
  (slot-value layer 'cl-tiled.data-types::objects))


(defun tile-character (tile)
  "Given a tileset's tile, return it's corresponding text character,
assuming that the tileset is a bitmap font starting with char-code 32
with 15 characters-per-line."
  (code-char
   (+ (* (cl-tiled:tile-row tile) 15)
      (cl-tiled:tile-column tile)
      32)))



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
