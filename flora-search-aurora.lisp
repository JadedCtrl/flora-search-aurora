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

;; FLORA-SEARCH-AURORA
;; A simple TUI-game made for the text-flavoured LibreJam of 2023-06!
;; See: https://jamgaroo.xyz/jams/2

(ql:quickload '(alexandria cl-charms cl-tiled str))
(load "input.lisp")
(load "display.lisp")

(defpackage :flora-search-aurora
  (:use :cl :flora-search-aurora.input :flora-search-aurora.display)
  (:export #:main))

(in-package :flora-search-aurora)


(defun main ()
  "A pathetic fascimile of a main loop. Look, I'm still tinkering!"
  (let ((matrix (make-screen-matrix)))
    (screen-matrix-set-map
     matrix
     (str:concat (namestring (uiop:getcwd)) "res/map.tmx"))
    (cl-charms:with-curses ()
      (cl-charms:enable-raw-input :interpret-control-characters 't)
      (print-screen-matrix matrix)
      (loop (print (normalize-char-plist (read-char-plist))))
      (sleep 5))))

(main)
