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


(defmacro getf-act (map act)
  `(getf (gethash :acts ,map) ,act))


(defmacro getf-know (map idea)
  `(getf (gethash :knows ,map) ,idea))


(defun childhood-friend-greetings (map)
  (if (getf-act map :sasha-greetings)
    (incf (getf-act map :sasha-greetings))
    (setf (getf-act map :sasha-greetings) 0))
  (let ((sasha "childhood-friend"))
    (case (getf-act map :sasha-greetings)
       (0
        (💬:start-dialogue
         (💬:mumble sasha    :en "...")
         (💬:say    "player" :eo "Kielas apud la mar'?"
                             :en "How's the view?")
         (💬:face   "player" "<.<")
         (💬:say    sasha    :eo "Kielas apud la ruinoj de via viv'?"
                             :en "How's your trainwreck of a life?")
         (💬:face   "player" '💬:normal-face)))
       (1
        (start-dialogue
         (mumble "player"  :en "...")
         (face   "player"  "<w<")
         (say    sasha     :eo "Kial vi restas? Ĉu tiom solecas ke nur ideas ĝeni min?"
                           :en "Why are you still here? Are you so lonely you've only got me to bother?")
         (mumble "player"  :eo "(Ŝi parolas pri si mem, ĉu ne?)"
                           :en "(She's projecting, isn't she?)"
                           :face ":w:")))
       (2
        (start-dialogue
         (say    "player"  :eo "Nu... Vi staris tie ĉi senmove dum la pastintaj tri tagoj..."
                           :en "So... You've stood around here for three days already, you've hardly moved..."
                           :face ":o:")
         (face   "player"  ":w:")
         (say    sasha     :eo "Pŝ! Do?! Mi simple havas multajn pripensindaĵojn! Mi tiom multe okupiĝas!"
                           :en "Pff! So what?! My mind's just busy! I've got a lot going on right now!"
                           :face "vov")
         (say    sasha     :eo "Ne ŝajnigu vin supera al mi, dum vi mem senespere sencelas!!"
                           :en "Don't act all haughty when you're such an aimless loser yourself!!"
                           :face ">o<")
         (mumble "player"  :eo "Eee.. pardonu."
                           :en "Well... sorry.")))
       (3
        (start-dialogue
         (say    "player"  :eo "Nu, vere, mia celo sufiĉe klaras al mi. Jam baldaŭ redungiĝos."
                           :en "I'm not too aimless, actually. I've got good job prospects, right about now."
                           :face "<w<")
         (say    sasha     :eo "Mi tute ne prizorgas."
                           :en "I really don't care."))))))


(defun childhood-friend-partings ()
  (let ((partings
          '((:eo "Nu? Ĝis! Adiaŭ!"
             :en "Well? Bye! Ta-ta!")
            (:eo "Ve! Eĉ via rigardo sentas strange!"
             :en "God! The way you look at me gives me the creeps!")
            (:eo "Lasu! Min! Sooooola!"
             :en "Leave me! The hell! Alooooone!")
            (:eo "Subvermo!"
             :en "Worm!"))))
    (start-dialogue
      (apply #'say (append '("childhood-friend")
                           (nth (random (length partings)) partings))))))


(defun childhood-friend-dialogue (map)
  (let ((greetings (getf-act map :sasha-greetings)))
    (cond ((or (not greetings)
               (< greetings 3))
           (childhood-friend-greetings map))
          ('t
           (childhood-friend-partings)))))


(defun childhood-friend-interact (map)
  (make-dialogue-state map (childhood-friend-dialogue map)))


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
