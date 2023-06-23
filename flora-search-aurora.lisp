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

(ql:quickload '(alexandria anaphora assoc-utils cl-charms cl-tiled str))

(load "util.lisp")
(load "input.lisp")
(load "display.lisp")
(load "ui.lisp")
(load "overworld.util.lisp")
(load "overworld.tiled.lisp")
(load "overworld.lisp")
(load "dialogue.lisp")
(load "engine.lisp")

(defpackage :flora-search-aurora
  (:nicknames :fsa :✿)
  (:export #:main)
  (:use :cl
   :flora-search-aurora.input :flora-search-aurora.display
   :flora-search-aurora.overworld :flora-search-aurora.dialogue
   :flora-search-aurora.ui))

(in-package :flora-search-aurora)


(defparameter *literary-girl-dialogue*
  (💬:start-dialogue
   (💬:mumble "literary-girl" :en "...")
   (💬:say "player"        :eo "Kielas apud la mar'?"
                           :en "How's the view?")
   (💬:face "player" "<.<")
   (💬:say "literary-girl" :eo "Kielas apud la ruinoj de via viv'?"
                           :en "How's your trainwreck of a life?")
   (💬:face "player" '💬:normal-face)))


(defun literary-girl-dialogue (map)
  (make-dialogue-state map *literary-girl-dialogue*))


(defparameter *submenu* `(((LABEL :en "IDK") (selection . 100) (selected t))
                          ((LABEL :en "GO BACK") (return . nil))))


(defparameter *main-menu* `(((LABEL :en "PLAY" :eo "EKLUDI")
                             (selection . 100) (selected . t)
                             (return . ,(🌍:make-overworld-state
                                         (format nil "~Ares/map.tmx" (uiop:getcwd)))))
                            ((LABEL :en "SUBMENU" :eo "SUBMENUO")
                             (return . ,(📋:make-menu-state *submenu*)))
                            ((LABEL :en "QUIT" :eo "REZIGNI")
                             (return . nil))))


(defun main ()
  "A pathetic fascimile of a main loop. What does it do? WHAST DOES TI DODOO?
What a mysteryyy! You’ll have to check out the engine to uncover it.
engine.lisp, that is. Cheers! :D"
    (⚙:main (list (📋:make-menu-state *main-menu*))))


(main) ;; — Knock-knock
;; — Who’s there?
;; — Yo momma!
;; — “Yo momma” who?
;; — Yo momma’s a sweet lady, and I’d like to take her out for some tea!
