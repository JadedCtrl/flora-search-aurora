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

(defpackage :flora-search-aurora.overworld.util
  (:nicknames :fsa.o.u :overworld.util)
  (:use :cl)
  (:export #:coords->symbol #:symbol->coords
          #:world-coords->screen-coords
          #:world-coords-chunk))

(in-package :flora-search-aurora.overworld.util)


(defun coords->symbol (x y)
  (intern (format nil "~A,~A" x y)))


(defun symbol->coords (coords-symbol)
  (str:split #\, (symbol-name coords-symbol)))


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
