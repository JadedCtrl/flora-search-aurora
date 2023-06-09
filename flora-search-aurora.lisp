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
(load "overworld.lisp")

(defpackage :flora-search-aurora
  (:export #:main)
  (:use :cl
   :flora-search-aurora.input :flora-search-aurora.display
   :flora-search-aurora.overworld :flora-search-aurora.ui))

(in-package :flora-search-aurora)


(defun state-loop
    (states &key (last-matrix (make-screen-matrix)) (matrix (make-screen-matrix)) (state-params nil))
  "Begin the game’s loop, which executes (henceforthly called) state-functions over and over again
until Hell freezes over and a new king reigns supreme.
Given a list of state-functions, states, it will execute the first function.
Each state-function must take at least a single parameter, a matrix of characters. A state-function
should edit this matrix in-place, replacing its elements with characters that will later be printed
to the terminal.
If this function returns NIL, then it will be removed from state-function list and the next
function in the list will be run in the next iteration. If no more functions remain, the loop
terminates.
If this function returns a list, then the given list will be used as additional parameters to the
state-function in the next iteration, in addition to the aforementioned matrix.
If this function returns a function, then the returned function will go to the front of the
state-function list, and will be ran in the next iteration onward.
If this function returns anything else, then it’ll simply be run again in the next iteration.
Make note to add a delay to your state functions, or… well, y’know. Your computer will overheat,
or something ¯\_(ツ)_/¯"

  (when states
   (let ((state-result
           (apply (car states) (cons matrix state-params)))) ;; Run the latest-added update/draw loop
     (print-screen-matrix (matrix-delta last-matrix matrix)) ;; Print its results.
     (force-output)
     (state-loop
      (cond ((functionp state-result)
             (cons state-result states))
            ((not state-result)
             (cdr states))
            ('t states))
      :last-matrix matrix
      :state-params (when (listp state-result)
                      state-result)))))


(defun make-main-menu-state ()
  "Return a state-function for the game’s main menu, for use with #'state-loop."
  (let ((main-menu
          `(((LABEL . "PLAY")
             (SELECTION . 100) (SELECTED . T)
             (FUNCTION . ,(lambda () (make-main-overworld-state))))
            ((LABEL . "SUBMENU")
             (FUNCTION . ,(lambda () (make-options-menu-state))))
            ((LABEL . "QUIT") (FUNCTION . ,(lambda () nil))))))
    (lambda (matrix)
      (menu-state matrix main-menu))))


(defun make-options-menu-state ()
  "Return a state-function for the options menu, for use with #'state-loop."
  (let ((options-menu
          `(((LABEL . "IDK")
             (SELECTION . 100) (SELECTED . T)
             (FUNCTION . ,(lambda () (print "¯\_(ツ)_/¯"))))
            ((LABEL . "GO BACK")
             (FUNCTION . ,(lambda () nil))))))
    (lambda (matrix)
      (menu-state matrix options-menu))))


(defun make-main-overworld-state ()
  "Return a state-function for the game’s overworld (the majority of the game), for use with
#'state-loop."
  (lambda (matrix)
    (overworld-state matrix "/home/jaidyn/.local/src/games/flora search aurora/res/map.tmx")))


(defun main ()
  "A pathetic fascimile of a main loop. What does it do? WHAST DOES TI DODOO?"
  (cl-charms:with-curses ()
   (cl-charms:enable-raw-input :interpret-control-characters 't)
   (clear-screen)
   (state-loop (list (make-main-menu-state)))))


(main) ;; — Knock-knock
;; — Who’s there?
;; — Yo momma!
;; — “Yo momma” who?
;; — Yo momma’s a sweet lady, and I think she’s swell!
