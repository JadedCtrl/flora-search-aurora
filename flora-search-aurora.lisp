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

(defpackage :flora-search-aurora
  (:nicknames :fsa :✿)
  (:export #:main)
  (:use :cl
   :flora-search-aurora.input :flora-search-aurora.display
   :flora-search-aurora.overworld :flora-search-aurora.dialogue
   :flora-search-aurora.ui))

(in-package :flora-search-aurora)

(defvar *casino-map* nil)
(defvar *outdoors-map* nil)



;;; ———————————————————————————————————
;;; Trans-map entity interactions
;;; ———————————————————————————————————
(defun take-item (map entity-id)
  "Take an entity from the MAP :ENTITIES and place it in the player’s pockets.
That is, into MAP’s :ITEMS."
  (let ((item-plist (cdr (getf-entity map entity-id))))
    (when item-plist
      (setf (aget-item map entity-id) item-plist)
      (removef-entity map entity-id))))


(defun take-item-dialogue (item-plist)
  "Return some dialogue expressing surprise/dread or whatever at the collection
of a new item. The attributes set for the entity item should be:
  ID
  NAME-[EO|EN]
  DESC-[EO|EN]
  REMARK-[EO|EN]
  REACTION-FACE
  REACTION-TALKING
All are optional, save ID."
  (start-dialogue
   (face   'player (or (getf item-plist :reaction-face) "^_^")
                   (or (getf item-plist :reaction-talking) "^o^"))
   (mumble 'player :en (format nil "(Hey, it's a ~A! ~A!)"
                        (or (getf item-plist :name-en) (getf item-plist :id))
                        (or (getf item-plist :remark-en) "Nice!"))
                   :eo (format nil "(Ho, jen ~A! ~A)"
                               (or (getf item-plist :name-eo) (getf item-plist :id))
                               (or (getf item-plist :remark-eo) "Interese!")))
   (mumble 'player :en (if (getf item-plist :desc-en)
                           (format nil "~A" (getf item-plist :desc-en))
                           "(I'm glad I found it.)")
                   :eo (if (getf item-plist :desc-eo)
                           (format nil "~A" (getf item-plist :desc-eo))
                           "(Kia bonŝanco!)"))))


(defun take-item-interact (map interactee-id)
  "Try to “pick up” the interactee entity, then have the player react to the
pickup. See TAKE-ITEM-DIALOGUE for customizing the reaction dialogue.
Should be the `interact` function for takeable items."
  (let ((item-plist (cdr (getf-entity map interactee-id))))
    (when (take-item map interactee-id)
      (make-dialogue-state map (take-item-dialogue item-plist)))))


(defun description-interact (map interactee-id)
  "Try to “pick up” the interactee entity, then have the player react to the
pickup. See TAKE-ITEM-DIALOGUE for customizing the reaction dialogue.
Should be the `interact` function for takeable items."
  (let ((item-plist (cdr (getf-entity map interactee-id))))
    (make-dialogue-state
     map
     (start-dialogue
      (mumble 'player :en (or (getf item-plist :desc-en) "...")
                      :eo (getf item-plist :desc-eo))))))



;;; ———————————————————————————————————
;;; The Outside World™
;;; ———————————————————————————————————
(defun casino-entrance-trigger (&optional map)
    (list :map (🌍:merge-maps map *casino-map*)))



;;; ———————————————————————————————————
;;; Childhood friend (Sasha) arc
;;; ———————————————————————————————————
(defun childhood-friend-greetings (map)
  (…:incf-0 (getf-act map :sasha-greetings))
  (let ((sasha 'childhood-friend))
    (case (getf-act map :sasha-greetings)
       (0
        (💬:start-dialogue
         (💬:mumble sasha   :en "...")
         (💬:say    'player :eo "Kielas apud la mar'?"
                            :en "How's the view?")
         (💬:face   'player "<.<" "<o<")
         (💬:say    sasha   :eo "Kielas apud la ruinoj de via viv'?"
                            :en "How's your trainwreck of a life?")))
       (1
        (start-dialogue
         (mumble 'player  :en "...")
         (face   'player  "<w<")
         (say    sasha    :eo "Kial vi restas? Ĉu tiom solecas ke nur ideas ĝeni min?"
                          :en "Why are you still here? Are you so lonely you've only got me to bother?")
         (face   'player  ":w:" ":u:")
         (mumble 'player  :eo "(Ŝi parolas pri si mem, ĉu ne?)"
                          :en "(She's projecting, isn't she?)")))
       (2
        (start-dialogue
         (face   'player  ":w:" ":o:")
         (say    'player  :eo "Nu... Vi staris tie ĉi senmove dum la pastintaj tri tagoj..."
                          :en "So... You've stood around here for three days already, you've hardly moved...")
         (say    sasha    :eo "Pŝ! Do?! Mi simple havas multajn pripensindaĵojn! Mi tiom multe okupiĝas!"
                          :en "Pff! So what?! My mind's just busy! I've got a lot going on right now!"
                          :face "vov")
         (say    sasha    :eo "Ne ŝajnigu vin supera al mi, dum vi mem senespere sencelas!!"
                          :en "Don't act all haughty when you're such an aimless loser yourself!!"
                          :face ">o<")
         (face   'player  "=w=" "=u=")
         (mumble 'player  :eo "Eee.. pardonu."
                          :en "Well... sorry.")))
       (3
        (start-dialogue
         (say    'player  :eo "Nu, vere, mia celo sufiĉe klaras al mi. Jam baldaŭ redungiĝos."
                          :en "I'm not too aimless, actually. I've got good job prospects, right about now."
                          :face "<w<")
         (say    sasha    :eo "Mi tute ne prizorgas."
                          :en "I really don't care."))))))


(defun childhood-friend-partings ()
  (let ((partings
          '((:eo "Nu? Ĝis! Adiaŭ!"
             :en "Well? Bye! Ta-ta!")
            (:eo "Ve! Eĉ via rigardo malkomfortigas!"
             :en "God! You're such a creep!")
            (:eo "Lasu! Min! Sooooola!"
             :en "Leave me! The hell! Alooooone!")
            (:eo "Subvermo!"
             :en "Worm!"))))
    (start-dialogue
      (apply #'say (append '(childhood-friend)
                           (nth (random (length partings)) partings))))))


(defun childhood-friend-dialogue (map)
  (let ((greetings (getf-act map :sasha-greetings)))
    (cond ((or (not greetings)
               (< greetings 3))
           (childhood-friend-greetings map))
          ('t
           (childhood-friend-partings)))))


(defun childhood-friend-interact (map &optional interactee-id)
  (make-dialogue-state map (childhood-friend-dialogue map)))



;;; ———————————————————————————————————
;;; School prologue: Childhood friend
;;; ———————————————————————————————————
(defun flashback-childhood-friend-interact (map &optional sasha)
  (make-dialogue-state
   map
   (start-dialogue
    (face   'player "` `" "`o`")
    (say    'player :eo "Ĉu ĉio enordas, Saŝa? Iom malfruas, ĉu ne?"
                    :en "Is everything OK, Sasha? It's a bit late, isn't it?")
    (face   sasha   ">->" ">o>")
    (say    sasha   :eo "Jes, jes! Mi nur iom distriĝis."
                    :en "Yea, yea! I just got a bit distracted is all.")
    (say    'player :eo "Prelegoj finiĝis antaŭ du horoj nun..."
                    :en "School ended two hours ago...")
    (say    sasha   :eo "Nu, mi ĵus eliris klubkunvenon! De klubo!"
                    :en "Well, I just got out of a meeting! ... Of a club!")
    (say    sasha   :eo "Do, kial VI restas? Ĉu ankoraŭ havas ne amikojn?"
                    :en "And what're YOU doing here? Still a loner?")
    (say    sasha   :eo "Nesurprize, tutatendite!"
                    :en "I'm sure not surprised.")
    (say    'player :eo "Ĉu ne ni estas amikoj, Saŝa?"
                    :en "Well, aren't we friends, Sasha?")
    (face   sasha   ":v:" ":o:")
    (mumble sasha   :en "...")
    (say    sasha   :eo "Kun vi? Ni? Amikoj..?"
                    :en "You? Me? Friends..?")
    (mumble sasha   :en "...")
    (face   sasha   "<-<" "<o<")
    (face   'player ":w:" ":u:")
    (say    sasha   :eo "Pŝ! Ne metu ŝercojn tiom stultaj, Dio mia!"
                    :en "Pssh! Don't make such bad jokes, Jesus.")
    (face   sasha   ">->" ">o>")
    (say    sasha   :eo "Kvazaŭ ni povus esti tiel!"
                    :en "As if!")
    (face   sasha   "<-<" "<o<")
    (say    sasha   :eo "Mi ne TIOM subigos miajn minimumojn!"
                    :en "I'd never lower my standards THAT much!")
    (say    'player :eo "... ĉu iu ajn atingas viajn minimumojn?"
                    :en "... does anyone meet your standards?")
    (say    sasha   :eo "Fermu la buŝon, stulta vermo!"
                    :en "Shut up, you damn loser!"
                    :face ">O<"))))


;;; ———————————————————————————————————
;;; Casino!
;;; ———————————————————————————————————
(defun casino-exit-trigger (&optional map)
  (list :map (🌍:merge-maps map *outdoors-map*)))



;;; ———————————————————————————————————
;;; Random casino NPCs
;;; ———————————————————————————————————
(defun boozy-lady-dialogue ()
  (let ((messages
          '((:eo "SaaaAAAl' belul', ĉu vjifik...vekfj/?"
             :en "HeeeEey sweet-cheeks, u wan sum..?")
            (:eo "Iu prenu bieron por tiu ĉi BELULO--!"
             :en "Someone get this HUNK a beer--!")
            (:eo "Kara, ĉu vi volus iri... min?"
             :en "Baby, wanna take a ride on my... me?")
            (:eo "Restu kun miii! <3"
             :en "Stay a whileee! <3")
            (:eo "Mia sino malvarmassss! Hlepu min, karulo~"
             :en "My lap's coldd! Gimem some help, sweetie.")
            (:eo "Lasu panjon instrui vin *hik* boooone"
             :en "Let momma show you a good *hic* timeee")
            (:eo "kaj mi diris RIKAŜIKA! DIN-DON!"
             :en "and I said BAZINGA! CA-CHOW!"))))
    (start-dialogue
      (apply #'say (append '(boozy-lady)
                           (nth (random (length messages)) messages))))))


(defun boozy-lady-interact (map &optional interactee-id)
  (make-dialogue-state map (boozy-lady-dialogue)))


(defun boozy-friend-interact (map &optional interactee-id)
  (make-dialogue-state
   map
   (start-dialogue
    (say 'boozy-friend :eo "Kial ŝi ĉiufoje tiom ebriiĝas?"
                       :en "I swear, she gets like this every time.")
    (say 'boozy-friend :eo "Kia ĝeno, tiom hontindas...!"
                       :en "It's so embarrasing..."))))


(defun casino-attendant-interact (map &optional interactee-id)
  (make-dialogue-state
   map
   (start-dialogue
    (say 'casino-attendant :eo "Bonvenon, estimata sinjoro!"
                           :en "Welcome in, good sir!")
    (say 'casino-attendant :eo "Ĝuu la ludadon, kaj sorto ridetu al vi!"
                           :en "Have fun; may lady luck blow you a kiss!"))))


(defun casino-bartender-interact (map &optional interactee-id)
  (make-dialogue-state
   map
   (start-dialogue
    (say 'casino-bartender :eo "Pffff, ŝi tiom amuzas!"
                           :en "Pffff, this broad's a riot!"
                           :face "xD ~")
    (say 'casino-bartender :eo "Fojfoje mi ja ŝatas mian fakon, mdr."
                           :en "Sometimes I really do like my job. lol"
                           :face "=w=~"))))



;;; ———————————————————————————————————
;;; Destitute Gambler arc
;;; ———————————————————————————————————
(defun bad-gambler-greetings (map)
  (…:incf-0 (getf-act map :gambler-greetings))
  (let ((gambler 'bad-gambler))
    (case (getf-act map :gambler-greetings)
       (0
        (start-dialogue
         (face   gambler "xD ")
         (mumble gambler :en "Hahaha... haha.")
         (say    gambler :eo "Kia spektalo! Hahaha!"
                         :en "Good one! Hahaha!"
                         :face "xDD")
         (say    gambler :en "Hahahaha! Hahahahaha!"
                         :face "x'D")
         (face   'player "^^'")
         (face   gambler ">V<" ">O<")
         (say    gambler :eo "Tiom amuze! Bona ŝerco!"
                         :en "Shit that's funny!"
                         :face ">V<")
         (face   'player "^^\"")
         (say    gambler :eo "Mi tute ruinigis mian vivon! MDR!"
                         :en "I totally fucked my life! LMAO!"
                         :face ">V<")
         (face   'player "o-o" "ouo")
         (say    gambler :en "HAHAHAHAHAHAHAHAHAHAAHAHAHAHAHAHAHAHAHAHAHAHAHAHAHAHAHAHAHAHAAHAHAHAHAHAHAHAHAHAHAHAHA")
         (face   gambler "=w=" "=w=")
         (mumble gambler :en "...")
         (mumble gambler :eo "Fek'."
                         :en "Fuck.")))
       (1
        (start-dialogue
         (say 'player :eo "Ĉu... ĉio enordas, samideano?"
                      :en "Everything alright, friend..?")
         (say gambler :eo "Jes! Tutorde! Bonas!"
                      :en "Yup! For sure! All good!")
         (say gambler :eo "Mi simple trafis iujn monproblemojn, ne problemas."
                      :en "I've just hit a slight snag financially, it's no problem.")
         (say gambler :eo "Nur povas ne repagi monprunton."
                      :en "I just can't repay my mortgage.")
         (say gambler :eo "... kiun mi pruntis por repagi alian monprunton."
                      :en "... that I took out to pay back a loan.")
         (say gambler :eo "... kiun mi pruntis por subteni la kompanion de mia frato."
                      :en "... that I borrowed to support my bro's company.")
         (say gambler :eo "... kiu estas senpaga lernejo por handikapuloj."
                      :en "... that's a non-profit school for disabled kids.")
         (say gambler :eo "... danke al kiu mia filino povas edukiĝi bone."
                      :en "... thanks to which my daughter can receive a good education.")
         (say gambler :eo "Kial mi forĵetis la monon tie ĉi?! FEK'!!"
                      :en "Why'd I waste it all here?! FUCK!!"
                      :face ">O<")))
       (2
        (start-dialogue
         (say gambler :eo "Nu, ĉio enordas! Ni simple perdos la domon, jen ĉio."
                      :en "Anyways, all good! We'll just lose the house, that's all.")
         (say gambler :eo "Kompreneble, perdinte la domon, mia edzino lasos min, kunprenante la gefilojn..."
                      :en "Obviously, after losing the house, my wife'll take leave with the kids...")
         (say gambler :eo "Dommastrino sen domo ja ne sencas, do!"
                      :en "A house-mistress without a house is no mistress at all, of course!")
         (say gambler :eo "Kaj kromvirino sen edzino ja ne sencas, do lasos min ankaŭ ŝi..."
                      :en "And a side-piece without a 'main-piece' is no side-piece at all, so she'll leave me too...")
         (say gambler :eo "Neniu mono, neniu domo, neniu filo, neniu edzino, neniu kromvirino!"
                      :en "No money, no house, no children, no wife, no mistress!")
         (say gambler :eo "Neniu vivo!"
                      :en "No life!")))
       (3
        (start-dialogue
         (say  gambler :eo "Mi cedu al la vakuo. Tre komfortas ĉe la fundo, kamarado."
                       :en "I'm giving into the void. It's quite peaceful down here, pal.")
         (say  gambler :eo "Mi lasu min falu entute, ĉu ne, kara amiko? Ĉu neee?"
                       :en "I should let go, right, pal? Righttt?")
         (face 'player ";w;" ";o;")
         (say  'player :eo "Ne tro senesperu -- sinjoro, restas al vi fuĝvojo, sendube!"
                       :en "Guy, there's still hope for you, somewhere!")
         (face gambler "=v=" "=v=")
         (say  gambler :eo "Mi ideas IAN fuĝvojon..."
                       :en "Five feet under, maybe..."))))))


(defun bad-gambler-partings (&optional map)
  (let ((messages
          '((:eo "... kaj ŝiaj mamoj tiom belis..."
             :en "... her titties were so nice, too...")
            (:eo "... vakuo, nenio, detruo..."
             :en "... emptiness, nothing, destruction...")
            (:eo "... vivo, bruo. morto, paco..."
             :en "... life, noise. death, peace...")
            (:eo "... laco, kaco, maĉo-veneno..."
             :en "... tired, weary, dreary...")
            (:eo "... tempo, tik-tok, malfruo..."
             :en "... time, tick-tock, too late...")
            (:eo "... finiĝo, ĉesiĝo, paciĝo..."
             :en "... endings, partings, peace-ings..."))))
    (start-dialogue
     (apply #'say (append '(bad-gambler)
                          (nth (random (length messages)) messages))))))


(defun bad-gambler-dialogue (map)
  (let ((greetings (getf-act map :gambler-greetings)))
    (cond ((or (not greetings)
               (< greetings 3))
           (bad-gambler-greetings map))
          ('t
           (bad-gambler-partings)))))


(defun bad-gambler-interact (map &optional interactee-id)
  (make-dialogue-state map (bad-gambler-dialogue map)))



;;; ———————————————————————————————————
;;; Main-menu data
;;; ———————————————————————————————————
(defun submenu ()
  `(((LABEL :en "IDK") (selection . 100) (selected t))
    ((LABEL :en "GO BACK") (return . nil))))


(defun main-menu ()
  `(((LABEL :en "PLAY" :eo "EKLUDI")
     (selection . 100) (selected . t)
     (return . ,(🌍:make-overworld-state *flashback-school-map*)))
    ((LABEL :en "SUBMENU" :eo "SUBMENUO")
     (return . ,(📋:make-menu-state (submenu))))
    ((LABEL :en "QUIT" :eo "REZIGNI")
     (return . nil))))



;;; ———————————————————————————————————
;;; Invocation station! 🚆
;;; ———————————————————————————————————
(defun main ()
  "A pathetic fascimile of a main loop. What does it do? WHAST DOES TI DODOO?
What a mysteryyy! You’ll have to check out the engine to uncover it.
engine.lisp, that is. Cheers! :D"
  (⚙:main (list (📋:make-menu-state (main-menu)))))


;; — Who’s there?
;; — Yo momma!
;; — “Yo momma” who?
;; — Yo momma’s a sweet lady, and I’d like to take her out for some tea!
