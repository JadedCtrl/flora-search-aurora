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

;;;; FLORA-SEARCH-AURORA.OVERWORLD.UTIL
;;;; Utility functions used by multiple overworld packages (overworld.tiled & overworld).

(in-package :flora-search-aurora.overworld.util)


(defun coords->symbol (x y)
  (intern (format nil "~A,~A" x y) "KEYWORD"))


(defun symbol->coords (coords-symbol)
  (str:split #\, (symbol-name coords-symbol)))


(defun string->symbol (string)
  "Given a STRING with an optionally defined package (e.g., “package:symbol”),
return it as an appopriate symbol."
  (let* ((split (str:split ":" (string-upcase string)))
         (package (when (eq (length split) 2)
                    (car split)))
         (symbol (or (cadr split) (car split))))
    (if package
        (intern symbol package)
        (intern symbol))))


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


(defun map->plist (map-hash)
  "Convert a map(-HASH) into a friendly, property-list format!
Used by SAVE-MAP-TO-FILE."
  (alexandria:hash-table-plist map-hash))


(defun plist->map (plist)
  "Convert a map from a MAP->PLIST’ed PLIST into a normal
map hash-table, as used by the game."
  (let ((hash (make-hash-table)))
    ;; Add the core map-data…
    (setf (gethash :tiles hash) (getf plist :tiles))
    (setf (gethash :top-tiles hash) (getf plist :top-tiles))
    (setf (gethash :bump-map hash) (getf plist :bump-map))
    (setf (gethash :entities hash) (getf plist :entities))
    (setf (gethash :triggers hash) (getf plist :triggers))
    ;; And now the user’s data…
    (setf (gethash :seconds hash) (getf plist :seconds))
    (setf (gethash :day hash) (getf plist :day))
    (setf (gethash :acts hash) (getf plist :acts))
    (setf (gethash :knows hash) (getf plist :knows))
    (setf (gethash :items hash) (getf plist :items))
    hash))


(defun save-map-to-file (path map &optional (package ":FLORA-SEARCH-AURORA") (variable "*map*"))
  "Given a map, generate source-code that corresponds to it."
  (with-open-file (file-stream path :direction :output :if-exists :supersede)
    (format file-stream "(in-package ~A)~%(defparameter ~A~%  (🌍.…:plist->map~%    (QUOTE ~S)))"
            package variable (map->plist map))))
