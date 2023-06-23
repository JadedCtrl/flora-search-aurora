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

;;;; FLORA-SEARCH-AURORA.OVERWORLD
;;;; All game-functions and data relating to the “overworld” (that is,
;;;; the primary gameplay, the RPG-ish-ish bits).

(defpackage :flora-search-aurora.dialogue
  (:nicknames :fsa.dia :dialogue :💬)
  (:use :cl)
  (:export #:dialogue-state
          #:start-dialogue #:face #:say #:mumble #:move))

(in-package :flora-search-aurora.dialogue)



;;; ———————————————————————————————————
;;; Misc. utilities
;;; ———————————————————————————————————
(defun pressed-enter-p ()
  "Whether or not the enter/return key has been pressed recently.
Man, today’s a good day. Well, it wasn’t great, too be honest. Kind of bad,
I slightly humiliated myself a tiny bit. But wow, I’m having such nice tea!
Programming with nice tea! What a nice day this is. If you happen to be
reading this, I hope your day is going well too!
If not, have some tea on me: I’m paying. =w="
  (and (listen)
       (eq (getf (⌨:normalize-char-plist (⌨:read-char-plist)) :char)
           #\return)))



;;; ———————————————————————————————————
;;; Dialogue-generation helpers
;;; ———————————————————————————————————
(defun start-dialogue (&rest dialogue-tree)
  (reduce (lambda (a b) (append a b))
          dialogue-tree))


(defun face (speaker face)
  (list
   (list :speaker speaker :face face)))


(defun say (speaker text)
  (list
   (list :speaker speaker :text text :face 'talking-face :progress 0)
   (car (face speaker 'normal-face))))


(defun mumble (speaker text)
  (list
   (list :speaker speaker :text text :progress 0)))


(defun move (speaker world-coords)
  (list
   (list :speaker speaker :coords world-coords)))



;;; ———————————————————————————————————
;;; Accessors
;;; ———————————————————————————————————
(defun dialogue-speaker (dialogue)
  "Get the DIALOGUE-speaker’s corresponding identifying symbol.
Because they’re stored in strings. So we gotta, like, unstringify. Ya dig?"
  (intern (string-upcase (getf dialogue :speaker))))



;;; ———————————————————————————————————
;;; Dialogue logic
;;; ———————————————————————————————————
(defun appropriate-face (map speaker face)
  "Return the face appropriate for the speaker.
If FACE is a string, used that.
If FACE is 'TALKING-FACE, then use their talking-face (if they have one).
If FACE is 'NORMAL-FACE, then use their normal-face (if they’ve got one).
If FACE is NIL… guess what that does. :^)"
  (let ((talking-face (🌍:getf-entity-data map speaker :talking-face))
        (normal-face (🌍:getf-entity-data map speaker :normal-face)))
    (cond ((and (eq face 'talking-face)
                talking-face)
           talking-face)
          ((and (eq face 'normal-face)
                normal-face)
           normal-face)
          ((stringp face)
           face))))


(defun update-speaking-face (map dialogue)
  "Given a line (plist) of DIALOGUE, change speaker’s face to either their
talking-face or the face given by the DIALOGUE."
  (let* ((speaker (intern (string-upcase (getf dialogue :speaker))))
         (new-face (appropriate-face map speaker (getf dialogue :face))))
    ;; Replace the face, when appropriate.
    (when new-face
      (setf (🌍:getf-entity-data map speaker :face) new-face))))


(defun progress-line-delivery (dialogue)
  "Progress the delivery of a line (plist) of DIALOGUE. That is, increment the
“said character-count” :PROGRESS, which dictates the portion of the message that
should be printed on the screen at any given moment."
  (let ((progress (getf dialogue :progress))
        (text (getf dialogue :text)))
    (when (and text
               (< progress (length text)))
      (incf (getf dialogue :progress) 1))))


(defun progress-movement (map dialogue)
  "Move the entity by one tile in the targeted position — that is, the
coordinates listed in the DIALOGUE’s :COORDS property. … If applicable, ofc."
  (let* ((speaker (dialogue-speaker dialogue))
         (target-coords (getf dialogue :coords))
         (speaker-coords (🌍:getf-entity-data map speaker :coords))
         (finished-moving-p (if target-coords (…:plist= speaker-coords target-coords) 't)))
    (when (not finished-moving-p)
     (🌍:move-entity
      map speaker
      :x (cond ((< (getf target-coords :x) (getf speaker-coords :x)) -1)
               ((> (getf target-coords :x) (getf speaker-coords :x)) 1)
               ('t 0))
      :y (cond ((< (getf target-coords :y) (getf speaker-coords :y)) -1)
               ((> (getf target-coords :y) (getf speaker-coords :y)) 1)
               ('t 0))))
    finished-moving-p))


(defun finished-printing-p (dialogue)
  "Whether or not a line of dialogue has been completely printed to the screen."
  (or (not (getf dialogue :text))
      (eq (length (getf dialogue :text))
          (getf dialogue :progress))))


(defun dialogue-state-update (map dialogue-list)
  "The logic/input-processing helper function for DIALOGUE-STATE.
Progress through the lines of dialogue when the user hits ENTER, etc.
Returns the state for use with STATE-LOOP, pay attention!"
  (update-speaking-face map (car dialogue-list))
  (progress-line-delivery (car dialogue-list))
  ;; Progress to the next line of dialogue as appropriate.
  (let* ((dialogue (car dialogue-list))
         (text (getf dialogue :text))
         (did-press-enter-p (pressed-enter-p))
         (did-finish-printing-p (finished-printing-p dialogue))
         (did-finish-moving-p (progress-movement map dialogue)))
    ;; Only show the cursor when rendering text!
    (if did-finish-moving-p
        (✎:show-cursor)
        (✎:hide-cursor))
    (cond
      ;; When enter’s hit and most everything is done (rendering text, etc),
      ;; progress the dialogue.
      ((or (and did-press-enter-p did-finish-printing-p did-finish-moving-p)
           (and (not text) did-finish-moving-p))
       (if (cdr dialogue-list)
          (list :dialogue (cdr dialogue-list) :map map)
          (progn
            (✎:hide-cursor)
            (values nil
                    (list :map map)))))
      ;; Allow interupting text-printing to end it!
      ((and did-press-enter-p (not did-finish-printing-p))
       (setf (getf (car dialogue-list) :progress) (length text))
       (list :dialogue dialogue-list :map map))
      ;; If no input, keep steady!
      ((or (not did-finish-printing-p)
           (not did-finish-moving-p)
           (cdr dialogue-list))
       (list :dialogue dialogue-list :map map)))))



;;; ———————————————————————————————————
;;; Dialogue drawing
;;; ———————————————————————————————————
(defun optimal-text-placement-horizontally (text coords &key (rightp nil) (width 72) (height 20))
  "Given a horizontal direction (RIGHTP defined or nil) and a focal point COORDS,
return the parameters of a text-box that can optimally fit the given TEXT in the
direction specified relative to the focal point. If a legible position can’t be
found, just give up! Return nil.
Otherwise, return a list list with the coordinates, max column, and max row — for
use with RENDER-STRING."
  (let* ((text-x-margin (if rightp
                            (+ (getf coords :x) 3)
                            0))
         (text-width (…:at-most (floor (* width 3/5)) ;; Not _too_ wide!
                              (if rightp
                                  (- width text-x-margin)
                                  (- (getf coords :x) 3))))
         (lines (ignore-errors (…:split-string-by-length text text-width)))
         (text-height (length lines)))
    ;; When this layout is valid…
    (when (and lines
               (>= height text-height) ;; If the text’ll fit on screen
               (> text-width 10))      ;; If the text is wide enough to be legible
      (let ((y (…:at-least 0 (- (getf coords :y)
                                (if (eq (length lines) 1)  ;; Align toward the speaker’s face
                                    1 0)
                               (floor (/ (length lines) 2)))))
            (x (if (and (not rightp)
                        (eq (length lines) 1))
                   (- text-width (length text))
                   text-x-margin)))
       (list (list :x x :y y) ;; Coords
             (+ x text-width) ;; Max column
             height)))))      ;; Max row


(defun optimal-text-placement-vertically (text coords &key (downp nil) (width 72) (height 20))
  "Given a vertical direction (DOWNP defined or nil) and a focal point COORDS,)
return the parameters of a text-box that can optimally fit the given TEXT in the
direction specified relative to the focal point. Return nil if no such placement
is found, otherwise return a list of the coordinates, max-column, and max-row
(for use with RENDER-STRING)."
  (let* ((text-y-margin (if downp
                            (+ (getf coords :y) 1)
                            (- (getf coords :y) 2)))
         (text-height (if downp
                          (- height text-y-margin)
                          (- text-y-margin 1)))
         (text-width (floor (* width 3/5))) ;; Too wide’s illegible! So ⅗-screen.
         (lines (ignore-errors (…:split-string-by-length text text-width))))
    ;; When the text can be printed with this layout…
    (when (and lines (>= text-height (length lines)))
      (let ((y (…:at-least
                 0
                 (if downp
                     text-y-margin
                     (- text-y-margin (length lines)))))
            (x (…:at-least
                 0
                 (- (getf coords :x)
                    (if (eq (length lines) 1)
                        (floor (/ (length (car lines)) 2))
                        (floor (/ text-width 2)))))))
        (list (list :x x :y y)       ;; Coords
              (+ x text-width)       ;; Max column
              (+ y text-height)))))) ;; Max row


(defun optimal-speech-layout (map dialogue &key (width 72) (height 20))
  "Given a line of DIALOGUE and MAP data, return the ideal “text-box” for the
text. This tries to place the text on the screen without covering up anything
important, if possible.
The data returned is a list of the box’es top-left coordinate, max-column,
and max-row; for use with RENDER-STRING. Like so:
  ((:x X :y Y) MAX-COLUMN MAX-ROW)"
  (let* ((speaker-id (dialogue-speaker dialogue))
         (direction (🌍:getf-entity-data map speaker-id :direction))
         (playerp (eq speaker-id 'player))
         (leftp (not (eq direction '🌍:right)))
         (text (getf dialogue :text))
         (coords (🌍:world-coords->screen-coords (🌍:getf-entity-data map speaker-id :coords))))
    ;; Ideally, place text-box behind the speaker; otherwise, place it above (NPC) or below (player).
    (or (optimal-text-placement-horizontally text coords :width width :height height
                                                        :rightp leftp)
        (optimal-text-placement-vertically text coords :width width :height height
                                                    :downp playerp)
        ;; … Worst-case scenario, just do whatever’ll fit :w:”
        (optimal-text-placement-vertically text coords :width width :height height
                                            :downp  (not playerp))
        (optimal-text-palcement-horizontally text coords :width width :height height
                                                        :rightp (not leftp)))))


(defun render-dialogue-block (matrix map dialogue)
  "Render a bit of DIALOGUE to the MATRIX, in an intelligent fashion; that is,
make it pretty, dang it! >O<
☆:.｡.o(≧▽≦)o.｡.:☆"
  (let* ((progress (getf dialogue :progress))
         (text (getf dialogue :text))
         (optimal-layout (when text (optimal-speech-layout map dialogue))))
    (when (and text optimal-layout)
      (📋:render-string-partially
       matrix text (first optimal-layout)
       :max-column (second optimal-layout)
       :max-row (third optimal-layout)
       :char-count progress))))


(defun dialogue-state-draw (matrix map dialogue-list)
  "Draw the dialogue where appropriate.
Helper function for DIALOGUE-STATE."
  (when (getf (car dialogue-list) :text)
    (✎:show-cursor)
    (render-dialogue-block matrix map (car dialogue-list))))



;;; ———————————————————————————————————
;;; Dialogue loop
;;; ———————————————————————————————————
(defun dialogue-state (matrix &key dialogue map)
  "Render a bit of DIALOGUE to the screen, using :FLORA-SEARCH-AURORA.OVERWORLD
entities as the speakers. Dialogue should be in the format:
  ((:text \"Hello, papa!\"
    :speaker \"son\"        ;; The entity’s ID (if applicable)
    :face \"owo\")         ;; If you want their face to change
   (:face \"=w=\" :speaker 'son) ;; change their face back when done talking
   (:text \"My dearest son, it’s been so long~!\"
    :speaker \"papa\"
   ...))
A state-function for use with STATE-LOOP."
  (sleep .05)
  (dialogue-state-draw matrix map dialogue)
  (dialogue-state-update map dialogue))


;; Split a banana in two, bisection-fruit,
;; Yummy-yummy, toot-toot~ 🎵
