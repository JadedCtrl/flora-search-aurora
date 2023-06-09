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

;;;; FLORA-SEARCH-AURORA
;;;; A simple TUI-game made for the text-flavoured LibreJam of 2023-06!
;;;; See: https://jamgaroo.xyz/jams/2

(ql:quickload '(alexandria assoc-utils cl-charms cl-tiled str))

(load "input.lisp")
(load "display.lisp")
(load "ui.lisp")

(defpackage :flora-search-aurora
  (:export #:main)
  (:use :cl
   :flora-search-aurora.input :flora-search-aurora.display
   :flora-search-aurora.ui))

(in-package :flora-search-aurora)


(defun state-loop (loops &optional (last-matrix (make-screen-matrix)) (matrix (make-screen-matrix)))
  (when loops
   (let ((loop-result
           (apply (car loops) (list matrix))))
     (print-screen-matrix (matrix-delta last-matrix matrix))
     (force-output)
     (state-loop
      (cond ((functionp loop-result)
             (cons loop-result loops))
            ((not loop-result)
             (cdr loops))
            ('t loops))
      matrix))))


(defun make-main-menu-loop ()
  (let ((main-menu
          `(((LABEL . "CRY OUT") (SELECTED . T) (FUNCTION . ,(lambda () (print "AAAAAA"))))
            ((LABEL . "RUN AWAY") (FUNCTION . ,(lambda () nil)))
            ((LABEL . "SUBMENU")
             (SELECTION . -100)
             (FUNCTION . ,(make-options-menu-loop))))))
    (lambda (matrix)
      (menu-loop matrix main-menu))))


(defun make-options-menu-loop ()
  (let ((options-menu
         `(((LABEL . "IDK") (SELECTION . 100) (SELECTED . T)
                            (FUNCTION . ,(lambda () (print "¯\_(ツ)_/¯"))))
           ((LABEL . "GO BACK") (FUNCTION . ,(lambda () nil))))))
    (lambda (matrix)
      (menu-loop matrix options-menu))))


(defun main ()
  "A pathetic fascimile of a main loop. Look, I'm still tinkering!"
  (cl-charms:with-curses ()
   (cl-charms:enable-raw-input :interpret-control-characters 't)
   (clear-screen)
   (state-loop (list (make-main-menu-loop)))))


;;    (print-screen-matrix
;      (render-menu-strip matrix items 2 5
;;                         :max-item-width 20 :height 3)))
;;    (screen-matrix-set-map
;;     matrix
;;      (print-screen-matrix matrix)
;;      (loop (print (normalize-char-plist (read-char-plist))))
;;      (sleep 5)))

(main)
