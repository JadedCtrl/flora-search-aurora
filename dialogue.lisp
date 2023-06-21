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
          #:start-dialogue #:face #:say #:mumble))

(in-package :flora-search-aurora.dialogue)



;;; ———————————————————————————————————
;;; Dialogue-generation DSL (sorta)
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



;;; ———————————————————————————————————
;;; Accessors
;;; ———————————————————————————————————
(defun dialogue-speaker (dialogue)
  (intern (string-upcase (getf dialogue :speaker))))



;;; ———————————————————————————————————
;;; Dialogue logic
;;; ———————————————————————————————————
(defun pressed-enter-p ()
  "Whether or not the enter/return key has been pressed recently."
  (and (listen)
       (eq (getf (⌨:normalize-char-plist (⌨:read-char-plist)) :char)
           #\return)))


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
  "Given a line (plist) of dialogue, change speaker’s face to either their
talking-face or the face given by the dialogue."
  (let* ((speaker (intern (string-upcase (getf dialogue :speaker))))
         (new-face (appropriate-face map speaker (getf dialogue :face))))
    ;; Replace the face, when appropriate.
    (when new-face
      (setf (🌍:getf-entity-data map speaker :face) new-face))))


(defun progress-line-delivery (dialogue)
  "Progress the delivery of a line (plist) of dialogue. That is, increment the
“said character-count” :PROGRESS, which dictates the portion of the message that
should be printed on the screen at any given moment."
  (let ((progress (getf dialogue :progress))
        (text (getf dialogue :text)))
    (when (and text
               (< progress (length text)))
      (incf (getf dialogue :progress) 1))))


(defun dialogue-state-update (map dialogue-list)
  "The logic/input-processing helper function for DIALOGUE-STATE."
  (update-speaking-face map (car dialogue-list))
  (progress-line-delivery (car dialogue-list))
  ;; Progress to the next line of dialogue as appropriate.
  (let ((text (getf (car dialogue-list) :text)))
    (cond ((or (pressed-enter-p)
               (not text))
           (if (cdr dialogue-list)
               (list :dialogue (cdr dialogue-list) :map map)
               (progn
                 (✎:hide-cursor)
                 (values nil
                         (list :map map)))))
         ((cdr dialogue-list)
          (list :dialogue dialogue-list :map map)))))



;;; ———————————————————————————————————
;;; Dialogue drawing
;;; ———————————————————————————————————
(defun optimal-speech-layout-horizontally (text coords &key (right-p nil) (width 72) (height 20))
  (let* ((text-margin (if right-p
                          (+ (getf coords :x) 3)
                          0))
         (text-width (if right-p
                         (- width text-margin)
                         (- (getf coords :x) 3)))
         (lines (ignore-errors (…:split-string-by-length text text-width))))
    (format *error-output* "Margin: ~A Width: ~A Right: ~A" text-margin text-width right-p)
    (when (and (> text-width 0)
               lines)
      (let ((y (…:at-least 0 (- (getf coords :y)
                                (floor (/ (length lines) 2))
                                1)))
            (x (if (and (not right-p)
                        (eq (length lines) 1))
                   (- text-width (length text))
                   text-margin)))
        (list (list :x x :y y)
              (+ x text-width)
              height)))))


(defun optimal-speech-layout (map dialogue &key (width 72) (height 20))
  (let* ((speaker-id (dialogue-speaker dialogue))
         (direction (🌍:getf-entity-data map speaker-id :direction))
         (text (getf dialogue :text))
         (coords (🌍:world-coords->screen-coords (🌍:getf-entity-data map speaker-id :coords))))
    (optimal-speech-layout-horizontally text coords :right-p 't :width width :height height)))


(defun render-dialogue-block (matrix map dialogue)
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
  "Render a bit of dialogue to the screen, using :FLORA-SEARCH-AURORA.OVERWORLD
entities as the speakers. Dialogue should be in the format:
  ((:text \"Hello, papa!\"
    :speaker \"son\"        ;; The entity’s ID (if applicable)
    :face \"owo\")         ;; If you want their face to change
   (:face \"=w=\" :speaker 'son) ;; change their face back when done talking
   (:text \"Hello, you little gremlin! <3\"
    :speaker \"papa\"
   ...))
A state-function for use with STATE-LOOP."
  (sleep .05)
  (dialogue-state-draw matrix map dialogue)
  (dialogue-state-update map dialogue))
