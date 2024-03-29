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

;;;; FLORA-SEARCH-AURORA ✿
;;;; A simple TUI-game made for the text-flavoured LibreJam of 2023-06!
;;;; See: https://jamgaroo.xyz/jams/2

(in-package :flora-search-aurora)


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


(defun remove-item (map item-id)
  "Given an item’s id, remove it from the user’s inventory."
  (…:remove-from-alistf item-id (gethash :items map)))


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
   (💬:face   'player (or (getf item-plist :reaction-face) "^_^")
              (or (getf item-plist :reaction-talking) "^o^"))
   (💬:mumble 'player :en (format nil "(Hey, it's a ~A! ~A!)"
                                  (or (getf item-plist :name-en) (getf item-plist :id))
                                  (or (getf item-plist :remark-en) "Nice!"))
                      :eo (format nil "(Ho, jen ~A! ~A)"
                                  (or (getf item-plist :name-eo) (getf item-plist :id))
                                  (or (getf item-plist :remark-eo) "Interese!")))
   (💬:mumble 'player :en (if (getf item-plist :desc-en)
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
  "An interact-function that can be used for displaying simple descriptions of
items on the map. These descriptions can be set by the :DESC-EN and :DESC-EO
properties of an entity (editable from Tiled)."
  (let ((item-plist (cdr (getf-entity map interactee-id))))
    (make-dialogue-state
     map
     (apply #'start-dialogue
            (loop for en-line in (str:lines (getf item-plist :desc-en))
                  for eo-line in (str:lines (getf item-plist :desc-eo))
                  collect (💬:mumble 'player :en en-line :eo eo-line))))))


(defun monologue-interact (map interactee-id)
  "An interact-function that can be used for displaying a monologue of a the
interactee — that is, they say the dialogue defined in the interactee’s
:SPEECH-EN and :SPEECH-EO properties."
  (let ((item-plist (cdr (getf-entity map interactee-id))))
    (make-dialogue-state
     map
     (apply #'start-dialogue
            (loop for en-line in (str:lines (getf item-plist :speech-en))
                  for eo-line in (str:lines (getf item-plist :speech-eo))
                  collect (💬:say interactee-id :en en-line :eo eo-line))))))


(defun random-monologue-interact (map interactee-id)
  "An interact-function that can be used for displaying a random piece of
dialogue as a monologue of a the interactee — that is, they say the dialogue
defined in the interactee’s :SPEECH-EN and :SPEECH-EO properties.
Each line in :SPEECH-EN or :SPEECH-EO is treated as a seperate line of
dialogue, among which one will be selected randomly."
  (let* ((item-plist (cdr (getf-entity map interactee-id)))
         (eo-line (alexandria:random-elt (str:lines (getf item-plist :speech-eo))))
         (en-line (alexandria:random-elt (str:lines (getf item-plist :speech-en)))))
    (make-dialogue-state
     map (start-dialogue (💬:say interactee-id :en en-line :eo eo-line)))))


(defun move-trigger (map trigger-plist)
  "A trigger function that forces the player to move either relatively or
absolutely, depending on the properties of TRIGGER-PLIST (:ΔX & ΔY or X & Y).
It will also display a monologue of the :DESC-* properties before movement.
Useful for making barriers the player character refuses to traverse."
  (make-dialogue-state
   map
   (apply
    #'start-dialogue
    (append
     (loop for en-line in (str:lines (getf trigger-plist :desc-en))
          for eo-line in (str:lines (getf trigger-plist :desc-eo))
          collect (💬:mumble 'player :en en-line :eo eo-line))
     (list (move 'player (list :Δx (getf trigger-plist :Δx)
                               :Δy (getf trigger-plist :Δy)
                               :x (getf trigger-plist :x)
                               :y (getf trigger-plist :y))))))))


(defun entrance-trigger (map trigger-plist)
  "A trigger that can be used to move the user from one MAP to another, via the
:MAP property in a trigger’s Tiled entity."
  (list :parameters
        (list :map (🌍:merge-maps
                    map (symbol-value (read-from-string (getf trigger-plist :map)))))))


(defun avatar-change-trigger (map &optional trigger-plist)
  "A somewhat weird trigger that changes the player's face or avatar."
  (setf (getf-entity-data map 'player :talking-face) (getf trigger-plist :talking-face))
  (setf (getf-entity-data map 'player :face) (getf trigger-plist :face))
  (setf (getf-entity-data map 'player :avatar) (getf trigger-plist :avatar))
  (list :parameters (list :map map)))


(defun entrance-interact (map interactee)
  "An interact function that can be used to move the user from one MAP to another,
via the :MAP property in the INTERACTEE’s Tiled entity."
  (let ((new-parameters
          (list :map
                (merge-maps
                 map (symbol-value (read-from-string (getf-entity-data map interactee :map)))))))
    (if (getf-entity-data map interactee :desc-en)
        (make-dialogue-state
         map
         (apply
          #'start-dialogue
          (append
           (loop for en-line in (str:lines (getf-entity-data map interactee :desc-en))
                for eo-line in (str:lines (getf-entity-data map interactee :desc-eo))
                collect (💬:mumble 'player :en en-line :eo eo-line))
           `(((:parameters ,new-parameters))))))
        `(:parameters ,new-parameters))))


(defun item-refusal-lines (string item)
  "Given an ITEM’s symbol ID, return all speech lines according it.
These are encoded in an entity’s :ITEM-SPEECH-[EN|EO] properties, with the ID at
the start of each line followed by a tab and the speech line."
  (mapcar #'cadr
          (remove-if-not
           (lambda (pair)
             (eq (…:string->symbol (car pair)) item))
           (mapcar
            (lambda (line)
              (str:split #\tab line))
            (str:lines string)))))


(defun refusal-use (map item-plist entity)
  "A generic use function for entities that can be used to display refusal
messages embedded in the Tiled map data."
  (let* ((refusal-en-lines (item-refusal-lines (getf-entity-data map entity :item-speech-en)
                                               (…:string->symbol (getf item-plist :id))))
         (refusal-eo-lines (item-refusal-lines (getf-entity-data map entity :item-speech-eo)
                                               (…:string->symbol (getf item-plist :id))))
         (en-lines (if (str:emptyp (str:unlines refusal-en-lines))
                       (str:lines (or (getf-entity-data map entity :item-default-en) "No thanks."))
                       refusal-en-lines))
         (eo-lines (if (str:emptyp (str:unlines refusal-eo-lines))
                       (str:lines (or (getf-entity-data map entity :item-default-eo) "Ne, dankon."))
                       refusal-eo-lines)))
    (make-dialogue-state
     map
     (apply #'start-dialogue
            (loop for eo-line in eo-lines
                  for en-line in en-lines
                  collect (💬:say entity :en en-line :eo eo-line))))))


(defun to-person-use (&optional map item-plist)
  "A function ran by INVENTORY 🎒 when an item is “used”, which will try to
run the :USE function of the nearest entity, if it has any."
  (let ((nearby (car (entities-near-entity (getf-entity map 'player)
                                           (gethash :entities map)))))
    (if (and nearby (getf (cdr nearby) :use))
        (let ((use-result (funcall (…:string->symbol (getf (cdr nearby) :use))
                                   map item-plist (car nearby))))
          (typecase use-result
            (list (nconc (list :drop 1) use-result))
            (t use-result)))
        (nconc
         (list :drop 1)
         (make-dialogue-state
          map
          (start-dialogue (💬:mumble 'player :en "(They don't seem to want it.)"
                                            :eo "(Ĝi verŝajne ne volas tion.)")))))))



;;; ———————————————————————————————————
;;; The Outside World™
;;; ———————————————————————————————————

;;; ———————————————————————————————————
;;; Random outdoors NPCs
;;; ———————————————————————————————————
(defun kvincent-greetings (map)
  (case (…:incf-0 (getf-act map :kvincent-greetings))
    (0
     (start-dialogue
      (💬:face 'kvincent "@__@" ">OO<")
      (💬:face 'player   ":w:" ":o:")
      (💬:say  'kvincent :eo "AIIII! Ve! Ve! Diablo! Jen diablo!"
                         :en "AIIII! Woe is me! The Devil has come!")
      (💬:say  'player   :eo "Kvincent, Kvincent! Trankviliĝu, trankviliĝu! Estas mi!"
                         :en "Kvincent, Kvincent! Calm down, it's just me!")
      (💬:say  'kvincent :eo "... bedaŭron?"
                         :en "... pardon?")
      (💬:face 'kvincent "@w@" "@o@")
      (💬:say  'kvincent :eo "Hooo, vi tute ne estas diablo! Vi estas nura homo!"
                         :en "Ooooh, you're not the Devil! You're just a person!"
                         :face "@v@")
      (💬:say  'player   :eo "Kompreneble!"
                         :en "Obviously not!")))
    (otherwise
     (start-dialogue
      (💬:face 'player   "=w='" "=o='")
      (💬:face 'kvincent "@__@" ">OO<")
      (💬:say  'kvincent :eo "AJJJJ! Ve! Ve! Dia..."
                         :en "AIIII! Woe is me! Dev...")
      (💬:say  'player   :eo "Mi ankoraŭ ne estas diablo!!"
                         :en "I'm still no demon!!")
      (💬:face 'kvincent "@w@" "@o@")
      (💬:say  'kvincent :eo "Ha, jes. Pardonu."
                         :en "Oh, right. Sorry.")))))


(defun kvincent-dialogue (map)
  (append (kvincent-greetings map)
          (start-dialogue
           (💬:face 'player   "` `" "`o`")
           (💬:say  'player   :eo "Ĉu ĉio enordas, Kvincent?"
                              :en "Everything alright, Kvincent?")
           (💬:say  'kvincent :eo "Mi iom anksias, freŝe... mi aŭdas strangajn, metalajn sonojn proksime en la montoj!"
                              :en "I'm a bit anxious, lately... I hear strange, metalic noises nearby in the mountains!")
           (💬:say 'kvincent :eo "Estas sendube diabloj!"
                             :en "Without a doubt, the sounds of devils!")
           (💬:say  'kvincent :eo "Kaj ankaŭ mi apenaŭ trovas fungojn, hodiaŭ... la dioj malbenis min!"
                              :en "Not to mention that I'm hardly finding any mushrooms... I've been cursed!")
           (💬:say  'player   :eo "Nek mi povas trovi florojn! Kia malfacila tago."
                              :en "I can't find any flowers, either! Today sucks."
                              :face "vov\'"))))


(defun kvincent-interact (map &optional interactee-id)
  (make-dialogue-state map (kvincent-dialogue map)))



;;; ———————————————————————————————————
;;; Childhood friend (Sasha) arc
;;; ———————————————————————————————————
(defun childhood-friend-greetings (map)
  (…:incf-0 (getf-act map :sasha-greetings))
  (let ((sasha 'childhood-friend))
    (case (getf-act map :sasha-greetings)
      (0
       (start-dialogue
        (💬:mumble sasha   :en "...")
        (💬:say    'player :eo "Kielas apud la mar'?"
                           :en "How's the view?")
        (💬:face   'player "<.<" "<o<")
        (💬:say    sasha   :eo "Kielas apud la ruinoj de via viv'?"
                           :en "How's your trainwreck of a life?")))
      (1
       (start-dialogue
        (💬:mumble 'player  :en "...")
        (💬:face   'player  "<w<")
        (💬:say    sasha    :eo "Kial vi restas? Ĉu tiom solecas ke nur ideas ĝeni min?"
                           :en "Why are you still here? Are you so lonely you've only got me to bother?")
        (💬:face   'player  ":w:" ":u:")
        (💬:mumble 'player  :eo "(Ŝi parolas pri si mem, ĉu ne?)"
                           :en "(She's projecting, isn't she?)")))
      (2
       (start-dialogue
        (💬:face   'player  ":w:" ":o:")
        (💬:say    'player  :eo "Nu... Vi staris tie ĉi senmove dum la pastintaj tri tagoj..."
                           :en "So... You've stood around here for three days already, you've hardly moved...")
        (💬:say    sasha    :eo "Pŝ! Do?! Mi simple havas multajn pripensindaĵojn! Mi tiom multe okupiĝas!"
                           :en "Pff! So what?! My mind's just busy! I've got a lot going on right now!"
                           :face "vov")
        (💬:say    sasha    :eo "Ne ŝajnigu vin supera al mi, dum vi mem senespere sencelas!!"
                           :en "Don't act all haughty when you're such an aimless loser yourself!!"
                           :face ">o<")
        (💬:face   'player  "=w=" "=u=")
        (💬:mumble 'player  :eo "Eee.. pardonu."
                           :en "Well... sorry.")))
      (3
       (start-dialogue
        (💬:say    'player  :eo "Nu, vere, mia celo sufiĉe klaras al mi. Jam baldaŭ redungiĝos."
                           :en "I'm not too aimless, actually. I've got good job prospects, right about now."
                           :face "<w<")
        (💬:say    sasha    :eo "Mi tute ne prizorgas."
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


(defun childhood-friend-dialogue-bracelet-intro (sasha)
  (start-dialogue
   (💬:face 'player "=w=" "=v=")
   (💬:say 'player  :eo "Nu, mi ankoraŭ opinias ke ĉi tio akordas vian stilon tre bone."
                    :en "I still think this suits you, you know.")
   (💬:face sasha   ";w:" ":o:")
   (💬:say sasha    :en "...")
   (💬:say sasha    :eo "... vi ankoraŭ havas tion?"
                    :en "... you still have that?")
   (💬:say 'player  :eo "Kompreneble mi plu havas!"
                    :en "Of course I do!")
   (💬:say 'player  :eo "Sed mi ne plu; ĝi ja estas via."
                    :en "But I shouldn't; it's yours, after all.")
   (💬:say sasha    :en "...")))


(defun childhood-friend-dialogue-bracelet-bad-end (sasha)
  (start-dialogue
   (💬:face sasha   "v-v" "vov")
   (💬:face 'player ":w;" ":o;")
   (💬:say  sasha   :eo "Pŝ!"
                    :en "Psh!")
   (💬:say  sasha   :eo "Ankoraŭ estas la malĝusta koloro, do..!"
                    :en "It's still the wrong colour, so..!"
                    :face "<o<")
   (💬:say  sasha   :eo "Lasu min!"
                    :en "Leave me alone!"
                    :face ">o<")
   (💬:say  'player :eo "Kial vi ankoraŭ forpuŝas min ĉi tiom?"
                    :en "Why do you still push me away this much?")
   (💬:say  sasha   :eo "Puŝas? Bona ideo, mi falpuŝu vin al la maro!"
                    :en "Push? Good idea, I'll push you off this cliff!")
   (💬:say  'player :eo "Vi ankoraŭ kompenas nenion ajn, Saŝa."
                    :en "You still don't get it at all, Sasha.")
   (💬:say  'player :eo "... kaj ne puŝu min, bonvole!!"
                    :en "... and please don't push me!!"
                    :face "@o@")))


(defun childhood-friend-dialogue-bracelet-good-end (sasha)
  (start-dialogue
   (💬:face   sasha   "=v=\"" "=v=\"")
   (💬:say    sasha   :eo "Nu, dankon..."
                      :en "Well, thanks...")
   (💬:say    sasha   :eo "[SAŜA prenas ĈIRKAŬMANON de vi.]"
                      :en "[SASHA takes BRACELET from you.]")
   (💬:face   sasha  "<w<" "<w<")
   (💬:say    sasha   :eo "Verdire, mi mensogetis tiam..."
                      :en "To be honest, I lied a bit, back then...")
   (💬:say    sasha   :eo "Ĉi tiu koloro fakte tre plaĉas al mi."
                      :en "I actually really like this colour."
                      :face ">w>")
   (💬:say    'player :eo "Mi ĝojas, ke finfine estos via."
                      :en "I'm glad it's finally yours.")
   (💬:say    sasha   :eo "Mi suportos ĝin ĉiutage!"
                      :en "I'll wear it every day!"
                      :face "^o^")
   (💬:say    sasha   :eo "... ho. Fek! Nu!"
                      :en "... wait. Shit!"
                      :face "<w<\"")
   (💬:say    sasha   :eo "Simple pro la aspekto! Ne gravas al mi, ĉu estas donaco aŭ ne!!"
                      :en "Just because it's pretty! I don't care that it's a gift or whatever!!"
                      :face ">o<")
   (💬:face   'player "` `" "`o`")
   (💬:mumble 'player :eo "(Mi supozas, ke la kerno de homo ne tro multe sanĝiĝas...)"
                      :en "(At the end of the day, I guess people can't change too much...)")
   (💬:mumble 'player :eo "(Sed tamen."
                      :en "(But y'know what? This is good enough for me.)")
   (💬:face   'player "^w^" "^o^")
   (💬:mumble 'player :eo "(Ĉi tio sufiĉas al mi!)"
                      :en "(This is good enough for me!)")))


(defun childhood-friend-dialogue-edelweiss (sasha)
  (start-dialogue
   (say sasha   :eo "Ĝi iom belas, mi supozas."
                :en "It's pretty, I guess.")
   (say 'player :eo "Saŝa, mi komprenas vin finfine."
                :en "Sasha, I think I understand you now.")
   (say 'player :eo "Vi fermiĝas en vi mem, timante ke aliuloj vin vundos kaj malaprobos."
                :en "You close up in yourself, afraid that others will reject you.")
   (say 'player :eo "Saŝa--! Vi bezonas pli kuraĝi!"
                :en "Sasha... you need to be brave!")
   (say sasha   :eo "... Vi afektemas, laŭkutime."
                :en "... You're pretentious, as per usual.")))


(defun childhood-friend-dialogue-lavendula (sasha)
  (start-dialogue
   (say sasha :eo "Ee... dankon?"
              :en "Uh... thanks?")
   (say sasha :eo "Kion vi eĉ alcelas per ĉi tio? Je dio."
              :en "What're you even getting at? Jesus.")
   (say 'player :eo "Saŝa, mi pensas ke lavendo perfekte akordas vin."
                :en "Sasha, I think lavender fits you perfectly.")
   (say 'player :eo "Vi malfidas ĉiun, eĉ tiujn kiuj plej fidas je vi."
                :en "You distrust and alienate everyone, even those who are most loyal to you.")
   (say 'player :eo "Mi eble ne diris rekte ĝis nun, do jen:"
                :en "Maybe I haven't said it directly before, so I'll go ahead now:")
   (face sasha ";w:" ":w:")
   (say 'player :eo "Mi zorgas pri vi multe, vi ĉiam estis mia plej kara amiko."
                :en "I care about you a lot, Sasha, and you've always been my dearest friend.")
   (say 'player :eo "Malgraŭ via mistraktado, mi neniam foriris, ĉu ne?"
                :en "Despite your pushing me away -- violently -- I never did leave, right?")
   (say 'player :eo "Mi konscias ke vi \"testadis\" nian amikecon, sed vi ne devas tion fari plu."
                :en "I know you try to \"test\" our friendship, but you don't have to do that anymore.")
   (say sasha   :en "...")
   (say sasha   :eo "... ĉu vere bonas?"
                :en "... is it really alright?")
   (say 'player :eo "Jes. Friends?"
                :en "Yea. Friends?")
   (face sasha "<w<" "^o^")
   (say sasha   :eo "Amikoj."
                :en "Friends.")))


(defun childhood-friend-dialogue-bracelet (map sasha)
  (append (childhood-friend-dialogue-bracelet-intro sasha)
          (if (getf-act map :encourage-friendship)
              (childhood-friend-dialogue-bracelet-good-end sasha)
              (childhood-friend-dialogue-bracelet-bad-end sasha))))


(defun childhood-friend-use (map item-plist &optional entity-id)
  (let ((item-id (…:string->symbol (getf item-plist :id))))
    (cond ((eq item-id 'bracelet)
           (if (getf-act map :encourage-friendship)
               (setf (getf-act map :perfect-friendship) 't))
           (make-dialogue-state
            map (childhood-friend-dialogue-bracelet map entity-id)))
          ((eq item-id 'neĝfloro)
           (remove-item map entity-id)
           (make-dialogue-state map (childhood-friend-dialogue-edelweiss entity-id)))
          ((eq item-id 'lavendula)
           (setf (getf-act map :encourage-friendship) 't)
           (remove-item map entity-id)
           (make-dialogue-state map (childhood-friend-dialogue-lavendula entity-id)))
          ('t
           (refusal-use map item-plist entity-id)))))



;;; ———————————————————————————————————
;;; School prologue: Childhood friend
;;; ———————————————————————————————————
(defun flashback-school ()
  (list
   :title '(:eo "ANTAŬLUDO I" :en "PROLOGUE I")
   :subtitle '(:eo "Antaŭ kvar jaroj..." :en "Four years ago...")
   :side-bar '(:eo "Amikoj, ĉu? Mi ne scias, eble..." :en "Friends? I don't know, maybe...")
   :map *flashback-school-map*))


(defun flashback-school-trigger (map &optional trigger-plist)
  "This is triggered right as the player enters the map — they literally can't
avoid triggering this."
  (take-item map 'bracelet)
  't)


(defun flashback-childhood-friend-dialogue-intro (sasha)
  (start-dialogue
   (💬:face   'player "` `" "`o`")
   (💬:say    'player :eo "Ĉu ĉio enordas, Saŝa? Iom malfruas, ĉu ne?"
                      :en "Is everything OK, Sasha? It's a bit late, isn't it?")
   (💬:face   sasha   "=_=" "=o=")
   (💬:say    sasha   :eo "Ho, jes, mi simple ĵus eliris klubkunvenon."
                      :en "Yea, I just left a club-meeting, is all.")
   (💬:say    'player :eo "Hodiaŭ ne estas klubotago..."
                      :en "Today isn't club day...")
   (💬:say    sasha   :eo "Nu, estas escepte speciala klubo!"
                      :en "Well, whatever, it's a special club!"
                      :face "<o<")
   (💬:say    sasha   :eo "Nu, kial VI restas? Ĉu ankoraŭ havas ne amikojn?"
                      :en "And what're YOU doing here? Still no friends?")
   (💬:say    'player :eo "Ankoraŭ sole vin."
                      :en "Still just you.")
   (💬:face   sasha   ":v:" ":o:")
   (💬:mumble sasha   :en "...")
   (💬:say    sasha   :eo "Nu..."
                      :en "Well...")
   (💬:mumble sasha   :en "...")
   (💬:face   sasha   "<-<" "<o<")
   (💬:face   'player ":w:" ":u:")
   (💬:say    sasha   :eo "Pŝ! Fermu la buŝon, vermo!"
                      :en "Ugh! Just shut up, you loser!")
   (💬:face   sasha   ">->" ">o>")
   (💬:say    sasha   :eo "Kvazaŭ ni povus esti tiel!"
                      :en "As if!")
   (💬:face   sasha   "<-<" "<o<")
   (💬:say    sasha   :eo "Simple lasu min al paco, fek'!!"
                      :en "Just get the hell out of my face!!"
                      :face ">o<")
   (💬:say    'player :eo "Bedaŭron..."
                      :en "Sorry...")
   (move   'player '(:x 46 :y 11) :delay .05)
   (💬:face   'player "^_^" "^o^")
   (💬:say    'player :eo "Ho, jes!"
                      :en "Oh, yea!")
   (move   sasha  '(:x 36 :y 3) :delay .03)
   (move   'player '(:x 43 :y 4) :delay .05)
   (move   sasha  '(:x 37 :y 3))))


(defun flashback-childhood-friend-dialogue-bracelet (map sasha)
  (start-dialogue
   (💬:say    'player :eo "Mi freŝe trovis ĉi tion, ĝi ŝajnis akorda al via stilo."
                      :en "I found this a while back, I thought you'd like it.")
   (💬:face   sasha   ";w:" ";u:")
   (💬:mumble 'player :eo "[Vi donas al SAŜA ĉirkaŭmanon belbrilan.]"
                      :en "[You give SASHA a shiny bracelet.]")
   (💬:say    sasha   :eo "Ho, tio surprize afablis...."
                      :en "Oh, that's surprisingly nice...")
   (💬:face   sasha   "<w<" "<o<")
   (💬:face   'player ":w;" ":o;")
   (💬:say    sasha   :eo "... jen la sola koloro, kiun mi malamas."
                      :en "... this is literally the one color I hate.")
   (💬:say    sasha   :eo "Kial vi ne elektis bluan? Dio mia."
                      :en "You couldn't have gotten blue?")
   (💬:say    sasha   :eo "Mi jam bone sciu, ne atendi bonon de vi."
                      :en "I should know not to expect so much from you...")
   (💬:say    sasha   :eo "Bonaj \"amikoj\" ni estas, ja..."
                      :en "\"Friends,\" whatever...")
   (💬:face   'player "T_T" "ToT")
   (move   'player '(:x 41 :y 3) :delay .05)
   (💬:face   sasha   ":_;")
   (💬:mumble 'player :eo "[Vi prenas de SAŜA ĉirkaŭmanon belbrilan.]"
                      :en "[You take a shiny bracelet from SASHA.]")
   (move   'player '(:x 46 :y 5) :delay .05)
   (move   'player '(:x 51 :y 19) :delay .05)
   (list (make-flashforward-state map))))


(defun flashback-childhood-friend-dialogue (map sasha)
  (append (flashback-childhood-friend-dialogue-intro sasha)
          (flashback-childhood-friend-dialogue-bracelet map sasha)))


(defun flashback-childhood-friend-interact (map entity-id)
  (make-dialogue-state
   map (flashback-childhood-friend-dialogue map entity-id)))


(defun flashback-childhood-friend-use (map item-plist &optional entity-id)
  (let ((item-id (…:string->symbol (getf item-plist :id))))
    (cond ((eq item-id 'bracelet)
           ;; If player gives her the special bracelet, skip the dialogue intro
           (make-dialogue-state
            map (flashback-childhood-friend-dialogue-bracelet map entity-id)))
          ('t (childhood-friend-use map item-plist entity-id)))))



;;; ———————————————————————————————————
;;; Military-base!
;;; ———————————————————————————————————
(defun captain-snake-interact (map &optional entity-id)
  (make-dialogue-state
   map
   (start-dialogue
    (💬:say    'beatnick      :eo "Mi havas sagacan planon, sinjoro."
                              :en "I've got a cunning plan, sir.")
    (💬:say    'captain-snake :en "Of course you do."
                              :eo "Laŭkutime.")
    (💬:say    'captain-snake :en "And what is it this time, Beatnick?"
                              :eo "Do, kiom ĝi malbonas cî-foje, Balnik?")
    (💬:say    'beatnick      :en "Well, I figured that, as officers, our job is to catch baddies..."
                              :eo "Nu, mi rimarkis ke, kiel oficiroj, nia tasko estas kapti malbonulojn...")
    (💬:say    'captain-snake :en "Yes..?"
                              :eo "Kaj..?")
    (💬:say    'beatnick      :en "... and, well, if we were baddies, we'd have to catch ourselves, wouldn't we?"
                              :eo "... kaj, nu, se ni estus la malbonuloj, ni devus kapti nin mem, ĉu ne?")
    (💬:say    'captain-snake :en "Beatnick, is your plan for us to commit a minor crime so we get tossed in jail rather than shot for desertion?"
                              :eo "Balnik, ĉu via plano estas ke ni faru krimeton por ke ni malliberiĝu anstataŭ mortpafiĝu?")
    (💬:say    'beatnick      :en "Yes, sir, Captain Snake sir."
                              :eo "Jes, sinjoro, Kapitano Serpento.")
    (💬:mumble 'captain-snake :en "...")
    (💬:say    'captain-snake :en "It's no world-beater, but it's the only plan we've got."
                              :eo "Ĝia ne elstare bonas, sed ĝi elstaras inter la aliaj planoj... kiel nia sola plano.")
    (move      'captain-snake '(:x 75 :y 10) :delay .07)
    (💬:say    'beatnick      :eo "Ho, ni komencas nun, ĉu? Atendu min!"
                              :en "Oh, we're starting now? Wait for me!"
                              :face "ovo")
    (move      'beatnick '(:x 75 :y 10) :delay 0)
    (move      'captain-snake '(:x 150 :y 10) :delay 0)
    (move      'beatnick '(:x 150 :y 10) :delay 0))))


(defun sheriff-trigger-dialogue-intro ()
  (start-dialogue
   (face      'player  "o^o" "o*o")
   (face      'sheriff ">,,,<" ">;;;<")
   (💬:say    'sheriff :eo "TRUDANTO!"
                       :en "INTRUDER!")
   (move      'sheriff '(:x 29 :y 1) :delay 0)
   (move      'sheriff '(:x 14 :y 10) :delay 0)
   (face      'player  ">^<" ">o<")
   (💬:say    'sheriff :eo "MANOJN LEVU! MANOJN POSTDORSU! SURPLANKIĜU!"
                       :en "HANDS IN THE AIR! HANDS ON YOUR BACK! GET ON THE GROUND, NOW!")
   (💬:mumble 'sheriff :en "...")
   (face      'player  "o^<" "o.o")
   (face      'sheriff "u,,,u" "u,,,o")
   (💬:say    'sheriff :eo "Hooooooo, vi ne estas kontraŭulo!"
                       :en "Ohhhhh, you're no enemy!")))


(defun sheriff-trigger-dialogue-postintro (map)
  (if (getf-know map :is-intern)
      (start-dialogue
       (face 'player "=w=" "=v=")
       (say 'player  :eo "Kompreneble ke ne! Mi estas la nova staĝisto, sinjoro!"
                     :en "Of course not! I'm the new intern, sir!")
       (say 'sheriff :eo "Ha jes, bone do."
                     :en "Right right, very good."))
      (progn
        (setf (getf-know map :is-intern) 't)
        (start-dialogue
          (💬:say    'sheriff :eo "Vi estas la staĝanto nova, ĉu?"
                              :en "You're clearly the new intern.")
          (face      'player  "=w=" "=v=")
          (💬:say    'player  :eo "Eee... bone? Jes. Mi estas ja."
                              :en "Eee... sure? Yes, I sure am."
                              :face "=v='")
          (💬:say    'sheriff :eo "Vi volis diri \"Mi estas ja, SINJORO.\""
                              :en "You mean, \"I sure am, SIR.\"")
          (💬:say    'sheriff :eo "Vi alparolas la policĉefon, ne forgesu tion!"
                              :en "It's the sheriff you're talking to, and don't you forget it!")
          (face      'player  "o^o" "o*o")
          (💬:say    'sheriff :eo "Mi \"punos\" vin sekvafoje, ĉu ni interkompreniĝas?"
                              :en "I'll have you \"reprimanded\" next time, are we clear?")
          (💬:say    'player  :eo "Eeee!! Tutkomprenite, sinjoro! Pardonu min, sinjoro!"
                              :en "Eeee!! Crystal, sir! Forgive me, sir!"
                              :face ">o<")
          (💬:say    'sheriff :eo "Bone, bone."
                              :en "Good, then.")
          (face      'player  "=*=" "=v=")))))


(defun sheriff-trigger-dialogue-order (map)
  (start-dialogue
   (💬:say    'sheriff :eo "Nu, vi jam bone scias ke ne multe okupiĝos dum la sekvontaj kelkaj tagoj."
                       :en "Now, I'm sure you're well aware of how busy we'll be for the next few days.")
   (💬:say    'sheriff :eo "Mi mem okupiĝas multe kun tre prema planado kun la kapitanoj."
                       :en "I'm currently busy making some last-minute plans with the captains.")
   (💬:say    'sheriff :eo "Nu, ni komencu."
                       :en "Now then, down to brass tacks.")
   (💬:say    'sheriff :eo "Prenu por mi kafon de la plej proksima kafejo. Poste, petu Kapitanon Serpenton por viaj ordonoj."
                       :en "Fetch me coffee from the nearest coffee house. Then, see Captain Snake for your orders.")
   (if (getf-know map :is-intern)
       (💬:say 'player :eo "Jes, sinjoro; mi faros tuj, sinjoro!"
                       :en "Yes, sir; I'll do it right away, sir!")
       (💬:say    'player  :eo "Jes sinjoro!! Tuj, sinjoro!!"
                         :en "Sir yes sir!! Right away, sir!"
                         :face ">o<"))
   (move   'sheriff '(:x 29 :y 1))
   (move   'sheriff '(:x 44 :y 1))))


(defun sheriff-trigger-dialogue (map)
  (append (sheriff-trigger-dialogue-intro)
          (sheriff-trigger-dialogue-postintro map)
          (sheriff-trigger-dialogue-order map)))


(defun sheriff-trigger (map &optional trigger-plist)
 (declare (ignore trigger-plist))
 (if (not (getf-act map :sheriff-met))
     (progn
       (setf (getf-act map :sheriff-met) 't)
       (make-dialogue-state map (sheriff-trigger-dialogue map)))
     (list :parameters (list :map map))))


(defun scientist-dialogue-edelweiss (scientist)
  (start-dialogue
   (say 'player :eo "Jen, por vi."
                :en "Here, for you.")
   (say scientist :eo "... he?"
                  :en "... huh?")
   (say 'player :eo "Estas neĝfloro, ĉu vi konas ĝian signifon?"
                :en "It's an Edelweiss. Do you know what it means?")
   (say 'player :eo "Signifas kuraĝon kaj bravecon."
                :en "It means courage and bravery.")
   (say 'player :eo "La kuraĝo fidi je viaj juĝoj..."
                :en "The courage to trust your judgement...")
   (say 'player :eo "... kaj braveco por ilin esprimi."
                :en "... and the bravery to speak your mind.")
   (face scientist "8w8" "8v8")
   (say scientist :eo "Dankon, staĝanto."
                  :en "Thanks, intern.")
   (say scientist :eo "Ĉi tio fakte tre agrablas de vi, multan, multan dankon."
                  :en "This was actually very kind of you, thank you so much.")
   (say scientist :eo "Mi pravas... mi scias, kion mi devas fari!"
                  :en "I'm in the right... I know now what I have to do!")
   (say scientist :en "Farewell, intern."
                  :eo "Adiaŭ, S-ro staĝanto.")
   (move scientist '(:x 145 :y 10))))


(defun scientist-use (map item-plist &optional entity-id)
  (let ((item-id (…:string->symbol (getf item-plist :id))))
    (cond ((eq item-id 'neĝfloro)
           (setf (getf-act map :encourage-scientist) 't)
           (make-dialogue-state map (scientist-dialogue-edelweiss entity-id)))
          ('t
           ;; Otherwise, have her politely refuse. =w=
           (refusal-use map item-plist entity-id)))))



;;; ———————————————————————————————————
;;; Base prologue: Quiet scientist
;;; ———————————————————————————————————
(defun flashback-base-dialogue (map)
  (let ((sheriff 'flashback-sheriff)
        (scientist 'flashback-scientist))
    (start-dialogue
     (move   sheriff   '(:x 100 :y 26) :delay .1)
     (move   sheriff   '(:x 90 :y 29) :delay .1)
     (move   sheriff   '(:x 87 :y 29))
     (mumble sheriff   :en "Hm?")
     (move   scientist '(:x 112 :y 22) :delay 0)
     (say    scientist :eo "Atendu, sinjoro!!"
                       :en "Wait, sir!!")
     (move   scientist '(:x 95 :y 29) :delay 0.02)
     (say    scientist :eo "Momenton!"
                       :en "Just a moment!")
     (say    sheriff   :eo "Doktoro Tim?"
                       :en "Doctor Tim?")
     (say    sheriff   :eo "Kial la kurado, ĉu ĉio enordas?"
                       :en "Why the hussle, has something happened?")
     (say    scientist :eo "Mi pensas, ke vi bone konas la problemon, sinjoro."
                       :en "I think you know very well what the problem is, sir.")
     (say    sheriff   :eo "Pardonu, sed mi tute ne. Ĉu mi misdiris ion, dum la kunveno?"
                       :en "Sorry, but I have no idea. Did I say    something wrong in the meeting?")
     (say    scientist :eo "Eble mi misaŭdis, fakte..."
                       :en "Maybe I just misheard, but...")
     (say    scientist :eo "Ĉu vi vere volas TIOM da tankoj?"
                       :en "Do you really want THAT MANY tanks?")
     (say    sheriff   :eo "Ĉu tio problemas iel, doktoro?"
                       :en "Is that a problem, doctor?")
     (say    sheriff   :eo "Mi esperas, ke ne."
                       :en "I certainly hope not.")
     (say    scientist :eo "N-ne, ĉefo!"
                       :en "N-no sheriff!")
     (say    scientist :eo "Mi simple c-cerbumas..."
                       :en "I'm just w-wondering, is all...")
     (say    scientist :eo "Ĉu ni vere bezonas tiom multe da tankoj?"
                       :en "Do we really need so many tanks?")
     (say    scientist :eo "Nu, ha, mi simple iom prizorgas."
                       :en "Well, eh, I'm just worrying a bit.")
     (say    scientist :eo "Ĉu ne ni jam uzis tro da rimedoj por konstrui ĉi tiun bazon..?"
                       :en "Haven't we already gone through too many resources just building this place..?")
     (say    sheriff   :eo "He. Tial vi estas sciencisto, kaj nek kapitano nek kontisto."
                       :en "Heh. That's why you're a scientist, and not a captain nor an accountant.")
     (say    sheriff   :eo "Fidu je mi, Karla! Mi kontrolis la nombrojn, kaj ili balanciĝos bone."
                       :en "Trust me, Karla! I looked at the numbers personally, and it all checks out.")
     (say    scientist :eo "Bone, sinjoro. Kaj pardonu min pro la trudo, sinjoro, mi ne intencis malrespekton."
                       :en "Alright then, sir. I'm sorry for the intrusion, I meant no disrespect.")
     (say    sheriff   :eo "Ne zorgu, doktoro; mi ne dubas vian fidelecon."
                       :en "Don't worry, doctor; I don't doubt your loyalty.")
     (move   sheriff   '(:x 71 :y 29) :delay .1)
     (move   sheriff   '(:x 40 :y 29) :delay 0)
     (mumble scientist :en "...")
     (mumble scientist :eo "(Simple... tute ne sencas!)"
                       :en "(It just doesn't make any sense!)")
     (move   scientist '(:x 80 :y 29) :delay 0)
     (say    scientist :eo "S-sinjoro! Momenton plu, bonvolu!!"
                       :en "S-sir! Just one more thing, please!!")
     (move   scientist '(:x 71 :y 29) :delay 0)
     (move   'player   '(:Δx -3 :Δy 0) :delay 0)
     (move   scientist '(:x 56 :y 29) :delay 0)
     (say    scientist :eo "Vi diras, ke la nombroj balanciĝos, sed... kiel?"
                       :en "You say that the numbers'll balance out, but... how?")
     (say    scientist :eo "Kiel tioma malŝparo repagiĝos? Kiel tioma truo pleniĝos?"
                       :en "How could such a waste be paid back? Such a hole be filled?")
     (say    sheriff   :eo "Ĉu \"malŝparo,\" Klara?"
                       :en "\"Waste,\" you say?")
     (say    scientist :eo "E-eee! Ne tiel, ĉefo! V-volis diri, ke..."
                       :en "E-eee! Not like that, s-sir! I m-meant to say...")
     (say    sheriff   :eo "Trankviliĝu."
                       :en "Calm down.")
     (say    sheriff   :eo "Sole kapitanoj scias ĉi tion ĝuste nun, sed..."
                       :en "Only the captains know this right now, but...")
     (say    sheriff   :eo "La urbolimo removiĝos iom. Preter tiu de nia najbaro, precize."
                       :en "City limits are expanding somewhat. Beyond those of our neighbor, specifically.")
     (say    sheriff   :eo "Ĝia estraro rifuzis nin, kompreneble, do ni devos peni iom."
                       :en "Their city council refused our proposal, of course, so we'll have to put in some elbow grease.")
     (say    sheriff   :eo "La landa armeo jam promesis ne entrudiĝi; ni rajtas fari ĉion necesan."
                       :en "The national army's already promised to keep their nose out of this; we can do what needs to be done.")
     (say    sheriff   :eo "Per la impostoj kaj landopagoj de Etburgo, niaj kontoj balanciĝos."
                       :en "If we add in the all of Etteburg's revenue, our accounts will balance out.")
     (say    sheriff   :eo "Ĉu vi komprenas, nun? Kiel vi opinias?"
                       :en "Do you get it, now? What do you think?")
     (say    scientist :eo "Jes, ĉefo! Dankon!"
                       :en "Yes, sir! Thank you!")
     (say    scientist :eo "Ĝi estas t-tre bona ideo, gratulon! Genie!"
                       :en "It's a f-fantastic idea, sir! Perfect!")
     (say    scientist :eo "N-nu! Bonan tagon!"
                       :en "N-now! Good day!")
     (say    sheriff   :eo "Bonan tagon, doktoro."
                       :en "Good day, doctor.")
     (move   sheriff   '(:x -5 :y 29) :delay .07)
     (mumble scientist :eo "(... Li demandis min rekte, kial mi mensogis?)"
                       :en "(... He asked me directly, so why did I lie?)")
     (move   scientist '(:x 65 :y 29) :delay 0)
     (mumble scientist :eo "(Ni kunigos Etburgon kaj Egburo, ĉi tiel?)"
                       :en "(We'll merge Etteburg and Bigborough like that?)")
     (move   scientist '(:x 45 :y 29) :delay 0)
     (mumble scientist :eo "(Ĉi tio frenezas! Frenezas!)"
                       :en "(This is insane! Insane!)")
     (move   scientist '(:x 65 :y 29) :delay 0)
     (mumble scientist :eo "(Kaj la kapitanoj vere aprobas ĉi tion?)"
                       :en "(The captains really think this is a good idea?)")
     (mumble scientist :eo "(Dum la kunveno, ili sciis la veron, kaj diris nenion--!)"
                       :en "(During the meeting, they knew the truth, and said nothing--!")
     (move   scientist '(:x 45 :y 29) :delay 0)
     (mumble scientist :eo "(Fek'... Neeblas, ke ĉiu aprobas!)"
                       :en "(Shit... It's impossible that everyone agrees!)")
     (mumble scientist :eo "(Sed kio restas al mi?)"
                       :en "(But what can I do?)")
     (move   scientist '(:x 65 :y 29) :delay 0)
     (mumble scientist :eo "(Mi ne povas esti la sola kontraŭanto!)"
                       :en "(I couldn't argue against the sheriff alone!)")
     (move   scientist '(:x 45 :y 29) :delay 0)
     (mumble scientist :eo "(Fek'!)"
                       :en "(Damn!)")
     (move   scientist '(:x 65 :y 29) :delay 0)
     (mumble scientist :en "(...)")
     (mumble scientist :eo "(Mi malsatas...)"
                       :en "(I'm hungry...)")
     (mumble scientist :en "(...)")
     (move   scientist '(:x 56 :y 31) :delay .07)
     (move   scientist '(:x 56 :y 42) :delay .07)
     (list (make-flashforward-state map :was-dialogue 't)))))


(defun flashback-base ()
  (list
   :title '(:eo "ANTAŬLUDO III" :en "PROLOGUE III")
   :subtitle '(:eo "Antaŭ du monatoj..." :en "Two months ago...")
   :side-bar '(:eo "-- Li blokas vin sur la vojo. Kiel vi reagos al tio? -- Eee, mi afable ĉirkaŭmarŝos lin?"
               :en "-- He’s blocking your path. What are you gonna do about it? -- Um, politely walk around him?")
   :dialogue (flashback-base-dialogue *flashback-base-map*)
   :map *flashback-base-map*))



;;; ———————————————————————————————————
;;; Factory!
;;; ———————————————————————————————————
(defun factory-window-interact (&optional map interactee-id)
  (make-dialogue-state
   map
   (start-dialogue
    (💬:face 'player "` `" "`o`")
    (💬:mumble 'player :eo "(Al ĉi tiu fenesto tute mankas vitro!)"
                       :en "(This window's got no pane at all!)")
    (💬:mumble 'player :eo "(Mi kredeble povus grimpi tien, fakte...)"
                       :en "(I could probably fit my way in there, actually...)")
    (💬:mumble 'player :eo "(... sed ĉu vere farindas?)"
                       :en "(... but should I?)"
                       :face "`o`"))))



;;; ———————————————————————————————————
;;; Casino!
;;; ———————————————————————————————————
(defun boozy-lady-dialogue-ring ()
  (start-dialogue
   (💬:say 'boozy-lady   :eo "Vi volas edzinigi min, belulo?"
                         :en "Ya wanna marry me, hot stuff?")
   (💬:say 'boozy-lady   :eo "Booone, niiru tuj!"
                         :en "Shuree, lez go now!")
   (💬:say 'boozy-friend :eo "Vi NE iros tuj."
                         :en "You \"shure\" as hell WON'T.")))


(defun boozy-lady-dialogue-wallet ()
  (start-dialogue
   (💬:say 'boozy-lady   :eo "...? Monujo?"
                         :en "...? Wallet?")
   (💬:say 'boozy-lady   :eo "Monujo, mono! Mono, biero!!"
                         :en "Wallet, money! Money, booze!")
   (💬:say 'boozy-friend :eo "... kaj vi certe ne bezonas pli da TIO."
                         :en "You don't need any more of THAT, hon.")))


(defun boozy-lady-dialogue-bracelet ()
  (start-dialogue
   (💬:face 'player     "^_^" "^o^")
   (💬:say  'boozy-lady  :eo "Ooo, brile! Ĝi belasss"
                         :en "Ooo, shiny! It's prettyy")
   (💬:say  'boozy-lady  :eo "Ĉu por mji?"
                         :en "Is it for mi?")
   (💬:say  'player      :eo "Estos via, se vi ĵuras ne plu drinki ĉi-nokte."
                         :en "It's yours, if you promise to slow your roll a bit.")
   (💬:say  'boozy-lady  :eo "Jeŝ! Ne pluuu!"
                         :en "Okieeee!"
                         :face "= w =")
   (💬:say  'boozy-lady  :eo "[MAJA prenas ĈIRKAŬMANON de vi.]"
                         :en "[MAJA takes BRACELET from you.]"
                         :face "= w =")
   (💬:face 'player      ">_<" ">o<")
   (💬:say  'boozy-lady  :eo "Plia biero por mi, sinjoro!!"
                         :en "Jkjk. One more for me, barkeep!!")
   (💬:say  'casino-bartender
            :eo "Bone, momenton!!"
            :en "Sure thing, lady!"
            :face "xD ")))


(defun boozy-lady-use (map item-plist &optional entity-id)
  (let ((item-id (…:string->symbol (getf item-plist :id))))
    (cond ((eq item-id 'ring)
           (make-dialogue-state
            map
            (boozy-lady-dialogue-ring)))
          ((eq item-id 'bracelet)
           (make-dialogue-state
            map
            (boozy-lady-dialogue-bracelet)))
          ((eq item-id 'wallet)
           (make-dialogue-state
            map
            (boozy-lady-dialogue-wallet)))
          ('t
           (refusal-use map item-plist entity-id)))))


;;; ———————————————————————————————————
;;; Destitute Gambler arc
;;; ———————————————————————————————————
(defun bad-gambler-greetings (map)
  (…:incf-0 (getf-act map :gambler-greetings))
  (let ((gambler 'bad-gambler))
    (case (getf-act map :gambler-greetings)
      (0
       (start-dialogue
        (💬:face   gambler "xD ")
        (💬:mumble gambler :en "Hahaha... haha.")
        (💬:say    gambler :eo "Kia spektalo! Hahaha!"
                          :en "Good one! Hahaha!"
                          :face "xDD")
        (💬:say    gambler :en "Hahahaha! Hahahahaha!"
                          :face "x'D")
        (💬:face   'player "^^'")
        (💬:face   gambler ">V<" ">O<")
        (💬:say    gambler :eo "Tiom amuze! Bona ŝerco!"
                          :en "Shit that's funny!"
                          :face ">V<")
        (💬:face   'player "^^\"")
        (💬:say    gambler :eo "Mi tute ruinigis mian vivon! MDR!"
                          :en "I totally fucked my life! LMAO!"
                          :face ">V<")
        (💬:face   'player "o-o" "ouo")
        (💬:say    gambler :en "HAHAHAHAHAHAHAHAHAHAAHAHAHAHAHAHAHAHAHAHAHAHAHAHAHAHAHAHAHAHAAHAHAHAHAHAHAHAHAHAHAHAHA")
        (💬:face   gambler "=w=" "=w=")
        (💬:mumble gambler :en "...")
        (💬:mumble gambler :eo "Fek'."
                          :en "Fuck.")))
      (1
       (start-dialogue
        (💬:say 'player :eo "Ĉu... ĉio enordas, samideano?"
                       :en "Everything alright, man..?")
        (💬:say gambler :eo "Jes! Tutorde! Bonas!"
                       :en "Yup! For sure! All good!")
        (💬:say gambler :eo "Mi simple trafis iujn monproblemojn, ne problemas."
                       :en "I've just hit a slight snag financially, it's no problem.")
        (💬:say gambler :eo "Nur povas ne repagi monprunton."
                       :en "I just can't repay my mortgage.")
        (💬:say gambler :eo "... kiun mi pruntis por repagi alian monprunton."
                       :en "... that I took out to pay back a loan.")
        (💬:say gambler :eo "... kiun mi pruntis por subteni la kompanion de mia frato."
                       :en "... that I borrowed to support my bro's company.")
        (💬:say gambler :eo "... kiu estas senpaga lernejo por handikapuloj."
                       :en "... that's a non-profit school for disabled kids.")
        (💬:say gambler :eo "... danke al kiu mia filino povas edukiĝi bone."
                       :en "... thanks to which my daughter can receive a good education.")
        (💬:say gambler :eo "Kial mi forĵetis la monon tie ĉi?! FEK'!!"
                       :en "Why'd I waste it all here?! FUCK!!"
                       :face ">O<")))
      (2
       (start-dialogue
        (💬:say gambler :eo "Nu, ĉio enordas! Ni simple perdos la domon, jen ĉio."
                       :en "Anyways, all good! We'll just lose the house, that's all.")
        (💬:say gambler :eo "Kompreneble, perdinte la domon, mia edzino lasos min, kunprenante la gefilojn..."
                       :en "Obviously, after losing the house, my wife'll take leave with the kids...")
        (💬:say gambler :eo "Dommastrino sen domo ja ne sencas, do!"
                       :en "A house-mistress without a house is no mistress at all, of course!")
        (💬:say gambler :eo "Kaj kromvirino sen edzino ja ne sencas, do lasos min ankaŭ ŝi..."
                       :en "And a side-piece without a 'main-piece' is no side-piece at all, so she'll leave me too...")
        (💬:say gambler :eo "Neniu mono, neniu domo, neniu filo, neniu edzino, neniu kromvirino!"
                       :en "No money, no house, no children, no wife, no mistress!")
        (💬:say gambler :eo "Neniu vivo!"
                       :en "No life!")))
      (3
       (start-dialogue
        (💬:say  gambler :eo "Mi cedu al la vakuo. Tre komfortas ĉe la fundo, kara."
                        :en "I'm giving into the void. It's quite peaceful down here, buddy.")
        (💬:say  gambler :eo "Mi lasu min falu entute, ĉu ne, kara amiko? Ĉu neee?"
                        :en "I should let go, right, pal? Righttt?")
        (💬:face 'player ";w;" ";o;")
        (💬:say  'player :eo "Ne tro senesperu -- sinjoro, restas al vi fuĝvojo, sendube!"
                        :en "Guy, there's still hope for you, somewhere!")
        (💬:face gambler "=v=" "=v=")
        (💬:say  gambler :eo "Mi ideas IAN fuĝvojon..."
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
;;; Casino prologue: Bad gambler!
;;; ———————————————————————————————————
(defun flashback-casino ()
  (list
   :title '(:eo "ANTAŬLUDO II" :en "PROLOGUE II")
   :subtitle '(:eo "Antaŭ du jaroj..." :en "Two years ago...")
   :side-bar '(:eo "anyway whatever bruh" :en "sldkajsldkja")
   :map *flashback-casino-map*))


(defun flashback-bad-gambler-dialogue ()
  (let ((gambler 'flashback-bad-gambler))
    (start-dialogue
     (💬:say  gambler :eo "Kia spektalo! Hahaha!"
                     :en "How nice! Hahaha!")
     (💬:face gambler ">v<" ">u<")
     (💬:face 'player "o-o")
     (💬:say  gambler :en "HAHAHAHAHAHAHAHAHAHAHAHA!")
     (💬:face 'player "=w='")
     (💬:say  gambler :eo "Hehe-Heŭreka! Ĉi-nokte, Sorto estas mia kromvirino!"
                     :en "Haha-Hot damn, lady luck's my mistress tonight!")
     (💬:face gambler "^#^" "^O^")
     (💬:say  gambler :eo "ĉu vi sciis, kara?"
                     :en "Guess what, buddy?")
     (💬:say  gambler :eo "Por tiu ĉi ludo mi havas feke legendan vetaĵon!"
                     :en "I've got one hell of a chip for this game!")
     (💬:face 'player "^_^" "^o^"))))


(defun flashback-bad-gambler-interact (map &optional interactee-id)
  (make-dialogue-state map (flashback-bad-gambler-dialogue)))


(defun flashback-casino-dialogue-intro ()
  (let ((father 'flashback-casino-father)
        (mother 'flashback-casino-mother)
        (dealer 'flashback-casino-dealer)
        (geezer 'flashback-casino-geezer)
        (gambler 'flashback-bad-gambler))
    (start-dialogue
     (💬:say    dealer  :eo "Ĉu ĉiu pretas?"
                        :en "Everyone ready?")
     (💬:say    geezer  :eo "Pretis jam antaŭ vi eĉ naskiĝis!"
                        :en "I've been ready since before you were born!")
     (💬:say    gambler :eo "Ĉu necesas demandi?"
                        :en "Need you ask?")
     (💬:say    father  :eo "Ni pretas, ankaŭ!"
                        :en "We're ready, too!")
     (💬:say    'player :en "Eee...!"
                        :face ";w:")
     (💬:say    father  :eo "Ho, jes, vi ne scipovas ludi."
                        :en "Oh, right, you still don't know how to play.")
     (💬:say    father  :eo "Atendu momente, ĉiu, mi klarigos rapide!"
                        :en "Hold on, everyone, we'll be quick!")
     (💬:say    father  :eo "Bone: Vikĉjo, atentu! Mi ne min ripetos!"
                        :eo "Alright, Vic! Listen up, 'cuz I'm not gonna repeat myself!")
     (💬:say    father  :eo "La ludestro disdonas kartojn hazardkaŝe. La kartvaloro sekvas la numerojn, krom se estas kolora karto. Se estas kolora karto, la valoro sekvas la Epokon de la ludo. La unua Epoko estas Antaŭhistorio, kaj kompreneble plej valoras tiam la..."
                        :en "The dealer hands out cards randomly and discretely. The cards' value is based on their number, unless they're a coloured card, in which case the value is based on the Era of the game. The first Era is Pre-history, and of course the most imporant card in Pre-history is...")
     (💬:face   'player "=w='" "=o='")
     (💬:say    'player :eo "(Mi ĝojas festi kun ĉiu, sed ĉi tiu ludo sonas jam tro malsimpla por mi...)"
                        :en "(I'm glad to party like this with my folks, but this game sounds a bit complicated...)")
     (💬:say    'player :eo "(Ĉu ne ni povus simple ludi Hanafudan?)"
                        :en "(Why can't we just play Hanafuda?)"
                        :face "=w='")
     (💬:say    father  :eo "... kaj tiel oni venkas la markizon.")
     (💬:say    father  :eo "Bone, jen ĉio!")
     (💬:say    gambler :eo "Finfine!!"))))


(defun flashback-casino-dialogue-bet (map)
  (let ((father 'flashback-casino-father)
        (mother 'flashback-casino-mother)
        (dealer 'flashback-casino-dealer)
        (geezer 'flashback-casino-geezer)
        (gambler 'flashback-bad-gambler))
    (append
     (start-dialogue
      (💬:say    gambler :eo "Jen, mi vetas ĉi tion!"
                         :en "Now, I'm putting this down!")
      (💬:say    gambler :eo "Momenton..."
                         :en "Wait..."
                         :face "`o`")
      (💬:say    gambler :eo "He?! Mi perdis la geeziĝan ringon?!"
                         :en "Wait... what?! I lost my wedding ring?!"
                         :face "O_O"))
     (if (aget-item map 'ring)
         (start-dialogue
          (💬:face   'player "^_^" "^o^")
          (💬:say    'player :eo "Ho! jen!"
                             :en "Ĉu estas tia ĉi?")
          (💬:mumble 'player :eo "[Vi donas RINGON al HAZARDLUDEMULO]"
                             :en "[You give RING to GAMBLER]")
          (💬:face   gambler "=w=" "=w=")
          (💬:say    gambler :eo "..."
                             :en "...")
          (💬:face   gambler ":W:" ":O:")
          (💬:say    gambler :eo "Je dio!! Dankegon, kara!!"
                             :en "God damn!! Thank you, buddy!!")
          (💬:say    gambler :eo "Mi preskaŭ tiom senesperis!"
                             :en "I almost gave into despair!")
          (💬:face   gambler "^#^" "^O^")
          (💬:say    gambler :eo "NUN mi pretas ludi."
                             :en "NOW I'm ready to play.")
          (💬:mumble gambler :eo "[HAZARDLUDEMULO donas GEEDZIĜAN RINGON al TABLO]"
                             :en "[GAMBLER gives WEDDING RING to TABLE")
          (💬:say    'player :eo "Ĉu vere?! Kio misas je vi!?!"
                             :en "Seriously?! What's wrong with you!?!"
                             :face ">O<")
          (💬:face   'player ":w:'")
          (💬:say    gambler :en "Live fast die hard, brother!"
                             :eo "Vivu akre mortu frue!"
                             :face "B-)")
          (💬:say    mother  :eo "Vi provis, Vikĉjo..."
                             :en "You tried, Vic..."))
         (start-dialogue
          (💬:say    gambler :eo "... bone do, tio ĉi sufiĉos."
                             :en "... fine then, this'll have to do."
                             :face "<v<")
          (💬:mumble gambler :eo "[HAZARDLUDEMULO donas BIENPOSEDON al TABLO]"
                             :en "[GAMBLER gives LAND OWNERSHIP to TABLE]")
          (💬:face   'player ":w:" ":0:")
          (💬:say    'player :eo "Ĉu vi frenezas?! Ne tion faru!!"
                             :en "Are you crazy?! Don't do that!!")
          (💬:say    mother  :eo "Lasu lin, Vikĉjo."
                             :en "Leave him, Vic.")
          (💬:say    mother  :eo "La fonto de amuzo estas senzorga risko!"
                             :en "The root of all of joy is careless, unthinking risk!")
          (💬:say    father  :eo "Tielas la animokerno de la kartoj."
                             :en "Such is the nature of the cards."))))))


(defun flashback-casino-outro (map)
  (start-dialogue
   (💬:say 'flashback-casino-dealer
           :eo "Nu, ĉiu krom li, metu viajn vetaĵojn. Ni komencos je la Nula Epoko!"
           :en "As for the rest of you, place your bets. It's time for the Zeroth Era!")
   (list (make-flashforward-state map))))


(defun flashback-casino-dialogue (map)
  (append (flashback-casino-dialogue-intro)
          (flashback-casino-dialogue-bet map)
          (flashback-casino-outro map)))


(defun flashback-casino-seat-trigger (map &optional trigger-plist)
  (make-dialogue-state
   map
   (flashback-casino-dialogue map)))



;;; ———————————————————————————————————
;;; Flashbacks, generally
;;; ———————————————————————————————————
(defun flashbacks ()
  (list (flashback-school) (flashback-casino) (flashback-base)))


(defparameter *numerology-excerpts*
  '((:en "No. 1: \"VEGETATION: Azalea, Iris, Lilac. [...] Activate today's energetic, independent mood by choosing to wear a red tie or dress. Above all, do not be lazy.\""
     :eo "N-ro 1: \"KRESKAĴOJ: Azaleo, irido, siringo. [...] Konduku vian tagon al energia kaj sendependa etoso per ruĝa kravato aŭ robo. Plej grave, ne maldiligentu.\"")))


(defun make-flashback-state (flashback)
  "Given a FLASHBACK-plist, return a state plist suitable for use with STATE-LOOP.
The plist should have :TITLE, :SUBTITLE, and :SIDE-TEXT strings, alongside :MAP.
If you don’t want the player to be walking around (non-interactive flashback),
you can add :DIALOGUE with a dialogue-list for use with a dialogue state."
  (🎭:make-intermission-state
   (getf flashback :title)
   (getf flashback :subtitle)
   (getf flashback :side-text)
   (list :drop 1
         :function
         (if (getf flashback :dialogue)
             (💬:make-dialogue-function (getf flashback :map) (getf flashback :dialogue))
             (🌍:make-overworld-function (getf flashback :map))))))


(defun make-flashforward-state (map &key (was-dialogue nil))
  "Make a state-plist for use with STATE-LOOP that will give the player a smooth
transition from a flashback MAP into the *OUTDOORS-MAP*.
If the flashback was non-interactive (dialogue-only), WAS-DIALOGUE should be
set."
  (🎭:make-intermission-state
   '(:eo "NUNA TEMPO, NUNA DATO" :en "PRESENT DAY, PRESENT TIME")
   '(:eo "La 3a de junio, 2006" :en "June 3rd, 2006")
   (car *numerology-excerpts*)
   (list :drop 1
         :function (when was-dialogue (🌍:make-overworld-function *outdoors-map*))
         :parameters (list :map (merge-maps map *outdoors-map*)))))



;;; ———————————————————————————————————
;;; Main-menu data
;;; ———————————————————————————————————
(defun start-game-function ()
  "Returns a nameless function for use as a state function.
Initializes the current instance of the game, and such."
  ;; We’ve gotta make fresh copies of the maps, in case the user’s restarted the game.
  ;; metacopy, I love you <3 <3 <3
  (lambda (matrix)
    (defparameter *base-map*             (🌍:plist->map (metacopy:copy-thing *base-map-plist*)))
    (defparameter *casino-map*           (🌍:plist->map (metacopy:copy-thing *casino-map-plist*)))
    (defparameter *factory-map*          (🌍:plist->map (metacopy:copy-thing *factory-map-plist*)))
    (defparameter *flashback-base-map*   (🌍:plist->map (metacopy:copy-thing *flashback-base-map-plist*)))
    (defparameter *flashback-casino-map* (🌍:plist->map (metacopy:copy-thing *flashback-casino-map-plist*)))
    (defparameter *flashback-school-map* (🌍:plist->map (metacopy:copy-thing *flashback-school-map-plist*)))
    (defparameter *outdoors-map*         (🌍:plist->map (metacopy:copy-thing *outdoors-map-plist*)))
    (make-flashback-state (alexandria:random-elt (flashbacks)))))


(defun main-menu ()
  `((:en "Play" :eo "Ekludi"
     :selection 50 :selected t
     :function ,(start-game-function))
    (:en "Settings" :eo "Agordoj" :row 1
     :function ,(🔧:make-settings-menu-function))
    (:en "Give in" :eo "Cedi" :row 2
     :drop 1)))



;;; ———————————————————————————————————
;;; Invocation station! Choo-choo! 🚆
;;; ———————————————————————————————————
(defun main ()
  "A pathetic fascimile of a main loop. What does it do? WHAST DOES TI DODOO?
What a mysteryyy! You’ll have to check out the engine to uncover it.
engine.lisp, that is. Cheers! :D"
  (⚙:main (list (📋:make-menu-function (main-menu)))))


;; *Knock-knock*
;; — Who’s there?
;; — Yo momma!
;; — “Yo momma” who?
;; — Yo momma’s a sweet lady, and I’d like to take her out for coffee sometime!
