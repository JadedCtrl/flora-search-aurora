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

;;;; FLORA-SEARCH-AURORA.OVERWORLD.TILED
;;;; Import a Tiled-format (TMX) map into the hash-table/plist/alist format
;;;; used by the overworld.

(defpackage :flora-search-aurora.overworld.tiled
  (:nicknames :fsa.o.t :overworld.tiled)
  (:use :cl :anaphora-basic
   :flora-search-aurora.overworld.util)
  (:export #:load-map))

(in-package :flora-search-aurora.overworld.tiled)


;;; ———————————————————————————————————
;;; Misc. utility
;;; ———————————————————————————————————
(defun collect-items-into-groups (list key-function &key (groups '()))
  "Given a LIST of items and a KEY-FUNCTION categorizing an individual item
(returning a “category” symbol for any given item), return an sorted
associative list built upon GROUPS.
If NIL is returned from KEY-FUNCTION, the given item is thrown out."
  (loop for item in list
        do (let ((key (apply key-function (list item))))
             (when key
               (setf (assoc-utils:aget groups key)
                     (append (assoc-utils:aget groups key)
                             (list item))))))
  groups)


(defun tiled-coords->world-coords (x y tiled-map)
  "Given X & Y coordinates with a parsed Tiled map, return the appropriate
character-scale world coordinates in plist form."
  (list :x (floor (/ x (cl-tiled:map-tile-width tiled-map)))
        :y (floor (/ y (cl-tiled:map-tile-height tiled-map)))))



;;; ———————————————————————————————————
;;; Object-layer (Persons/Triggers)
;;; ———————————————————————————————————
(defun tiled-rectangle-p (tiled-obj)
  "Whether or not a Tiled object is a valid rectangle."
  (and (> (cl-tiled:rect-width tiled-obj) 0)
       (> (cl-tiled:rect-height tiled-obj) 0)))


(defun tiled-object->entity (tiled-obj tiled-map)
  "Convert a Tiled object into an entity plist."
  (let ((properties (cl-tiled:properties tiled-obj)))
    (when (not (tiled-rectangle-p tiled-obj))
      (list (intern (string-upcase (gethash "id" properties)))
            :coords (tiled-coords->world-coords (cl-tiled:object-x tiled-obj)
                                                (cl-tiled:object-y tiled-obj)
                                                tiled-map)
            :face (gethash "normal_face" properties)
            :normal-face (gethash "normal_face" properties)
            :talking-face (gethash "talking_face" properties)
            :interact (gethash "interact" properties)
            :direction (if (gethash "facing_right" properties)
                           'right
                           'left)))))



(defun tiled-object->trigger (tiled-obj tiled-map)
  "Convert a Tiled object into a “trigger” plist. That is, a rectangle with
a :FUNCTION to be triggered when it’s stepped upon."
  (when (tiled-rectangle-p tiled-obj)
    (let ((obj-x (cl-tiled:object-x tiled-obj))
          (obj-y (cl-tiled:object-y tiled-obj))
          (obj-width (cl-tiled:rect-width tiled-obj))
          (obj-height (cl-tiled:rect-height tiled-obj)))
      (list
       :coords (tiled-coords->world-coords obj-x obj-y tiled-map)
       :width obj-width
       :height obj-height
       :bottom-coords (tiled-coords->world-coords (+ obj-x obj-width) (+ obj-y obj-height) tiled-map)
       :function (gethash "function" (cl-tiled:properties tiled-obj))))))


(defun object-layer-entities (layer &optional (entity-chunks '()))
  "Convert all point objects in an object layer into entity plists."
  (let ((entities (mapcar (lambda (object) (tiled-object->entity object (cl-tiled:layer-map layer)))
                          (layer-objects layer))))
    (collect-items-into-groups
     entities
     (lambda (entity)
       (when entity
         (world-coords-chunk (getf (cdr entity) :coords))))
     :groups entity-chunks)))


(defun object-layer-triggers (layer &optional (trigger-chunks '()))
  "Convert all rectangle objects in an object layer into trigger plists."
  (let ((triggers (mapcar (lambda (object) (tiled-object->trigger object (cl-tiled:layer-map layer)))
                          (layer-objects layer))))
    (collect-items-into-groups
     triggers
     (lambda (trigger)
       (when trigger
         (world-coords-chunk (getf trigger :coords))))
     :groups trigger-chunks)))



;;; ———————————————————————————————————
;;; Tile-layer parsing (graphics)
;;; ———————————————————————————————————
(defun tiled-cell->cell (tiled-cell &key (language nil))
  "Convert a Tiled cell into a cell plist."
  (list :coords (list :x (cl-tiled:cell-column tiled-cell)
                      :y (cl-tiled:cell-row tiled-cell))
        :char (tile-character (cl-tiled:cell-tile tiled-cell))
        :lang language))


(defun tiled-layer-cells (layer)
  "Given a Tiled layer, return all of its cells in our custom cell plist-format."
  (let ((layer-lang
          (…:langcode->keysym
            (gethash "language" (cl-tiled:properties layer)))))
    (mapcar (lambda (tiled-cell)
              (tiled-cell->cell tiled-cell :language layer-lang))
            (cl-tiled:layer-cells layer))))


(defun tile-layer-chunks (layer &optional (chunks '()))
  "Given a Tiled tile-layer (that is, graphics of the map), parse it into an
alist of Tiled cell “chunks”."
  (collect-items-into-groups
    (tiled-layer-cells layer)
    (lambda (cell)
      (world-coords-chunk (getf cell :coords)))
    :groups chunks))


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
;;; Tiled maps → Map lists
;;; ———————————————————————————————————
(defun load-map (map-file)
  "Parse a map-file into an plist of its data. This consists of:
  :BUMP-MAP, an alist of tiles (keyed by chunk) in a “collidable” layer
  :TILES, an alist of visible tiles (keyed by chunk).
  :ENTITIES, a list of entity plists."
  (let ((tile-chunks '())
        (top-tiles '())
        (bump-map '())
        (entities '())
        (triggers '())
        (hash (make-hash-table)))
    (mapcar (lambda (layer)
              (typecase layer
                (cl-tiled.data-types:tile-layer
                 ;; Add to the bump-map if the layer is colliding
                 (when (gethash "colliding" (cl-tiled:properties layer))
                   (setf bump-map (tile-layer-chunks layer bump-map)))
                 (if (gethash "top-layer" (cl-tiled:properties layer))
                     (setf top-tiles   (tile-layer-chunks layer top-tiles))
                     (setf tile-chunks (tile-layer-chunks layer tile-chunks))))
                (cl-tiled.data-types:object-layer
                 (setf triggers (object-layer-triggers layer triggers))
                 (setf entities (object-layer-entities layer entities)))))
            (cl-tiled:map-layers (cl-tiled:load-map map-file)))
    (setf (gethash :tiles hash) tile-chunks)
    (setf (gethash :top-tiles hash) top-tiles)
    (setf (gethash :bump-map hash) bump-map)
    (setf (gethash :entities hash) entities)
    (setf (gethash :triggers hash) triggers)
    hash))
