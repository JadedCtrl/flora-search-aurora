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

;;;; FLORA-SEARCH-AURORA.ENGINE ⚙
;;;; The core of the game’s engine, the loop. Not much to see here other
;;;; than a loop. Honest!

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
  * T         —  The function will continue to be run with the same parameters.
  * (:FUNCTION FUNCTION :PARAMETERS LIST :DROP NUMBER)
Make note to add a delay w SLEEP to your state functions, or… well, y’know. Your computer will
overheat, or something ¯\_(ツ)_/¯"
  (when states
    (let ((state-result
            (apply (car states) (cons matrix state-params)))) ;; Run the last-added state-loop.
      (✎:print-screen-matrix (✎:matrix-delta last-matrix matrix)) ;; Print its results.
;;      (format *error-output* "S::~S~%D::~S~%" states state-result)
      (force-output)
      (state-loop
       (cond ((listp state-result)
              (nconc (if (getf state-result :function)
                         (list (getf state-result :function)))
                     (nthcdr (or (getf state-result :drop) 0) states)))
             ((not state-result)
              (cdr states))
             ('t states))
       :state-params
       (cond ((listp state-result)
              (getf state-result :parameters))
             ((not state-result)
              nil)
             ('t state-params))
       :last-matrix matrix))))


(defun main (states)
  "A toplevel-worthy function that configures rendering and kicks off the
game’s loop. Let’s get started!"
  (cl-charms:with-curses ()
    (cl-charms:enable-raw-input :interpret-control-characters 't)
    (✎:hide-cursor)
    (✎:clear-screen)
    (state-loop states)))
