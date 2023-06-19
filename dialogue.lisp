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
  (:nicknames :fsa.d :dialogue)
  (:use :cl
   :flora-search-aurora.overworld :flora-search-aurora.ui :flora-search-aurora.input)
  (:export #:dialogue-state #:say))

(in-package :flora-search-aurora.dialogue)



;;; ———————————————————————————————————
;;; Dialogue-generation DSL (sorta)
;;; ———————————————————————————————————
(defun dialogue (&rest dialogue-tree)
  (reduce (lambda (a b) (append a b))
          dialogue-tree))


(defun say (speaker text)
  (list
   (list :speaker speaker :text text :face 'talking-face)
   (car (face speaker 'normal-face))))


(defun mumble (speaker text)
  (list
   (list :speaker speaker :text text)))


(defun face (speaker face)
  (list
   (list :speaker speaker :face face)))



;;; ———————————————————————————————————
;;; Dialogue logic
;;; ———————————————————————————————————
(defun pressed-enter-p ()
  (and (listen)
       (eq (getf (normalize-char-plist (read-char-plist)) :char)
           #\return)))


(defun appropriate-face (map speaker face)
  (let ((talking-face (getf-entity-data map speaker :talking-face))
        (normal-face (getf-entity-data map speaker :normal-face)))
    (cond ((and (eq face 'talking-face)
                talking-face)
           talking-face)
          ((and (eq face 'normal-face)
                normal-face)
           normal-face)
          ((stringp face)
           face))))


(defun dialogue-state-update (dialogue-list map)
  "The logic/input-processing helper function for DIALOGUE-STATE."
  (let* ((speaker (intern (string-upcase (getf (car dialogue-list) :speaker))))
         (new-face (appropriate-face map speaker
                                     (getf (car dialogue-list) :face))))
    ;; Replace the face, when appropriate.
    (when new-face
      (setf (getf-entity-data map speaker :face) new-face)))
  ;; Progress the dialogue as appropriate.
  (let ((text (getf (car dialogue-list) :text)))
    (cond ((or (pressed-enter-p)
               (not text))
           (if (cdr dialogue-list)
              (list :dialogue (cdr dialogue-list) :map map)
              (values nil
                      (list :map map))))
         ((cdr dialogue-list)
          (list :dialogue dialogue-list :map map))
         ('t
          (values nil
                  (list :map map))))))



;;; ———————————————————————————————————
;;; Dialogue drawing
;;; ———————————————————————————————————
(defun dialogue-state-draw (matrix dialogue-list)
  "Draw the dialogue where appropriate.
Helper function for DIALOGUE-STATE."
  (let ((text (getf (car dialogue-list) :text)))
    (when text
      (render-line matrix text 0 0))))



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
  (sleep .02)
  (dialogue-state-draw matrix dialogue)
  (dialogue-state-update dialogue map))
