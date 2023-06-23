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

;;;; FLORA-SEARCH-AURORA.ENGINE
;;;; The core of the game’s engine, the loop. Not much to see here other
;;;; than a loop. Honest!


(defpackage :flora-search-aurora.engine
  (:nicknames :fsa.eng :engine :⚙)
  (:export #:state-loop #:main)
  (:use :cl))

(in-package :flora-search-aurora.engine)


(defun state-loop
    (states &key (last-matrix (✎:make-screen-matrix)) (matrix (✎:make-screen-matrix)) (state-params nil))
  "Begin the game’s loop, which executes (henceforthly called) state-functions over and over again
until Hell freezes over and a new king reigns supreme.
Given a list of state-functions, STATES, it will execute the first function.
Each state-function must take at least a single parameter, a matrix of characters. A state-function
should edit this matrix in-place, replacing its elements with characters that will later be printed
to the terminal.
What the state-function returns is pretty important, having different repercussions:
  * NIL       —  The function is removed from STATES, and so the next function in STATES will start
                 getting executed instead.
  * NIL; List —  The function is popped off STATES and the list is used as the new parameters for
                 the next function in STATES.
  * Function  —  The function is pushed to the front of STATES, and so is executed instead of the
                 current function.
  * List      —  The current function (front of STATES) continues to be executed with the given
                 list as a parameters list.
Make note to add a delay w SLEEP to your state functions, or… well, y’know. Your computer will
overheat, or something ¯\_(ツ)_/¯"
  (when states
    (multiple-value-bind (state-result new-state-params)
        (apply (car states) (cons matrix state-params)) ;; Run the latest-added update/draw loop
      (✎:print-screen-matrix (✎:matrix-delta last-matrix matrix)) ;; Print its results.
      (force-output)
      (state-loop
          (cond ((functionp state-result)
                 (cons state-result states))
                ((not state-result)
                 (cdr states))
                ('t states))
          :last-matrix matrix
          :state-params
          (cond ((not state-result)
                 new-state-params)
                ((listp state-result)
                 state-result))))))


(defun main (states)
  "A toplevel-worthy function that configures rendering and kicks off the
game’s loop. Let’s get started!"
  (cl-charms:with-curses ()
    (cl-charms:enable-raw-input :interpret-control-characters 't)
    (✎:hide-cursor)
    (✎:clear-screen)
    (state-loop states)))
