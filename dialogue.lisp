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

;;;; FLORA-SEARCH-AURORA.DIALOGUE 💬
;;;; The dialogue-scripting part of the game. This handles all dialogue!

(in-package :flora-search-aurora.dialogue)


;;; ———————————————————————————————————
;;; Dialogue-generation helpers
;;; ———————————————————————————————————
(defun start-dialogue (&rest dialogue-tree)
  (reduce #'append dialogue-tree))


(defun face (speaker face &optional (talking-face nil))
  (if talking-face
      (list
       (list :speaker speaker
             :face face
             :set :normal-face
             :to face)
       (list :speaker speaker
             :set :talking-face
             :to talking-face))
      (list
       (list :speaker speaker
             :face face
             :set :normal-face
             :to face))))


(defun say (speaker &rest keys)
  (list
   (list :speaker speaker :progress 0
         :face (or (getf keys :face) 'talking-face)
         :text (…:getf-lang keys))
   (list :speaker speaker :face 'normal-face)))


(defun mumble (speaker &rest keys)
  (list
   (list :speaker speaker :progress 0
         :text (…:getf-lang keys)
         :face (getf keys :face))))


(defun move (speaker coords &key (delay .05))
  (if (or (getf coords :Δx) (getf coords :Δy))
      (list
       (list :speaker speaker :relative-coords coords :delay delay))
      (list
        (list :speaker speaker :coords coords :delay delay))))



;;; ———————————————————————————————————
;;; Accessors
;;; ———————————————————————————————————
(defun dialogue-speaker (dialogue)
  "Get the DIALOGUE-speaker’s corresponding identifying symbol.
Because they’re stored in strings. So we gotta, like, unstringify. Ya dig?"
  (getf dialogue :speaker))



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
  (let* ((speaker (getf dialogue :speaker))
         (new-face (appropriate-face map speaker (getf dialogue :face))))
    ;; Replace the face, when appropriate.
    (when new-face
      (setf (🌍:getf-entity-data map speaker :face) new-face))))


(defun update-entity-data (map dialogue)
  "Given a plist of DIALOGUE, update an arbitrary bit of data in the speaker's
data, using :SET and :TO of the DIALOGUE."
  (let* ((speaker (getf dialogue :speaker))
         (key (getf dialogue :set))
         (data (getf dialogue :to)))
    (when (and key data)
      (setf (🌍:getf-entity-data map speaker key) data))))
;;      (format *error-output*  "[~A] ~A → ~A???~%" dialogue key data)

;;      (format *error-output* "~A!!!!~%" (🌍:getf-entity-data map speaker :normal-face)))))


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
      :Δx (cond ((< (getf target-coords :x) (getf speaker-coords :x)) -1)
                ((> (getf target-coords :x) (getf speaker-coords :x)) 1)
                ('t 0))
      :Δy (cond ((< (getf target-coords :y) (getf speaker-coords :y)) -1)
                ((> (getf target-coords :y) (getf speaker-coords :y)) 1)
                ('t 0)))
     (sleep (or (getf dialogue :delay) 0)))
    finished-moving-p))


(defun ensure-dialogue-movement (map dialogue-list)
  "Given a DIALOGUE-LIST, ensure that the first line of dialogue’s movement is
absolute rather than relative, if it contains any movement at all."
  (let ((dialogue (car dialogue-list)))
    (when (and (getf dialogue :relative-coords)
               (not (getf dialogue :coords)))
      (let ((speaker-coords (🌍:getf-entity-data map (dialogue-speaker dialogue) :coords))
            (relative-coords (getf dialogue :relative-coords)))
        (setf (getf (car dialogue-list) :coords)
              (list :x (+ (getf speaker-coords :x) (or (getf relative-coords :Δx) 0))
                    :y (+ (getf speaker-coords :y) (or (getf relative-coords :Δy) 0))))))))


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
  (update-entity-data map (car dialogue-list))
  (progress-line-delivery (car dialogue-list))
  (ensure-dialogue-movement map dialogue-list)
  ;; Progress to the next line of dialogue as appropriate.
  (let* ((dialogue (car dialogue-list))
         (text (getf dialogue :text))
         (did-press-enter-p (⌨:pressed-enter-p))
         (did-finish-printing-p (finished-printing-p dialogue))
         (did-finish-moving-p (progress-movement map dialogue)))
    ;; Only show the cursor when rendering text!
    (if (or did-finish-moving-p (not did-finish-printing-p))
        (✎:show-cursor)
        (✎:hide-cursor))
    (cond
      ;; When enter’s hit and most everything is done (rendering text, etc),
      ;; progress the dialogue.
      ((or (and did-press-enter-p did-finish-printing-p did-finish-moving-p)
           (and (not text) did-finish-moving-p))
       (if (cdr dialogue-list)
          (list :parameters (list :dialogue (cdr dialogue-list) :map map))
          (progn
            (✎:hide-cursor)
            (clear-input)
            (list :drop (1+ (or (getf dialogue :drop) 0))
                  :function (getf dialogue :function)
                  :parameters (if (member :parameters dialogue)
                                  (getf dialogue :parameters)
                                  (list :map map))))))
      ;; Allow interupting text-printing to end it!
      ((and did-press-enter-p (not did-finish-printing-p))
       (setf (getf (car dialogue-list) :progress) (length text))
       (list :parameters (list :dialogue dialogue-list :map map)))
      ;; If no input, keep steady!
      ('t
       (list :parameters (list :dialogue dialogue-list :map map))))))



;;; ———————————————————————————————————
;;; Dialogue drawing
;;; ———————————————————————————————————
(defun optimal-text-placement-horizontally (text coords &key (rightp nil) (width 72) (height 20))
  "Given a horizontal direction (RIGHTP defined or nil) and a focal point COORDS,
return the parameters of a text-box that can optimally fit the given TEXT in the
direction specified relative to the focal point. If a legible position can’t be
found, just give up! Return nil.
Otherwise, return a list list with the coordinates, textbox width, and textbox
height — all parameters for use with RENDER-STRING & co."
  (let* ((text-x-margin (if rightp
                            (+ (getf coords :x) 3)
                            0))
         (text-width (…:at-most (floor (* width 3/5)) ;; Not _too_ wide!
                              (if rightp
                                  (- width text-x-margin)
                                  (- (getf coords :x) 3))))
         (lines (ignore-errors (str:lines (…:linewrap-string text text-width))))
         (text-height (length lines)))
    (format *error-output* "HORIZ COORD: ~A HEIGHT: ~A WIDTH ~A LINES:~%~S~%" coords text-height text-width lines)
    ;; When this layout is valid…
    (when (and lines
               (>= height text-height) ;; If the text’ll fit on screen
               (> text-width 10))      ;; If the text is wide enough to be legible
      (let* ((y (…:at-least 0 (- (getf coords :y)
                                 (if (eq text-height 1)  ;; Align toward the speaker’s face
                                     1 0)
                                 (floor (/ text-height 2)))))
             (x (if (and (not rightp)
                         (eq (length lines) 1))
                    (- text-width (length text))
                    text-x-margin))
             (y-margin (if (> (+ y (length lines)) height) ;; How many lines are off-screen
                           (- (+ y (length lines)) height)
                           0)))
        (list
         ;; Coords of text-box’es top-left corner
         (list :x x :y (- y y-margin))
         text-width       ;; Width of text-box
         text-height))))) ;; Height of text-box


(defun optimal-text-placement-vertically (text coords &key (downp nil) (width 72) (height 20))
  "Given a vertical direction (DOWNP defined or nil) and a focal point COORDS,)
return the parameters of a text-box that can optimally fit the given TEXT in the
direction specified relative to the focal point. Return nil if no such placement
is found, otherwise return a list of the coordinates, textbox width, and textbox
height (for use as parameters with RENDER-STRING et al.)."
  (let* ((text-y-margin (if downp
                            (+ (getf coords :y) 2)
                            (- (getf coords :y) 2)))
         (text-height (if downp
                          (- height text-y-margin)
                          (- text-y-margin 1)))
         (text-width (floor (* width 3/5))) ;; Too wide’s illegible! So ⅗-screen.
         (lines (ignore-errors (str:lines (…:linewrap-string text text-width)))))
    (format *error-output* "VERT HEIGHT: ~A WIDTH ~A LINES: ~A~%" text-height text-width lines)
    ;; When the text can be printed with this layout…
    (when (and lines (>= text-height (length lines)))
      (let* ((y (…:at-least
                  0
                  (if downp
                      text-y-margin
                      (- text-y-margin (length lines)))))
             (x (…:at-least
                  0
                  (- (getf coords :x)
                     (if (eq (length lines) 1)
                         (floor (/ (length (car lines)) 2))
                         (floor (/ text-width 2))))))
             (x-margin (if (> (+ x text-width) width)
                           (- (+ x text-width) width)
                           0)))
        (list
         ;; Coords of text-box’es top-left corner
         (list :x (- x x-margin) :y y)
         text-width          ;; Width of the text-box
         (length lines)))))) ;; Height of text-box


(defun optimal-speech-layout (map dialogue &key (width 72) (height 20))
  "Given a line of DIALOGUE and MAP data, return the ideal “text-box” for the
text. This tries to place the text on the screen without covering up anything
important, if possible.
The data returned is a list of the box’es top-left coordinate, max-column,
and max-row; for use with RENDER-STRING. Like so:
  ((:x X :y Y) MAX-COLUMN MAX-ROW)"
  (let* ((speaker-id (dialogue-speaker dialogue))
         (playerp (eq speaker-id '✿:player))
         (leftp (not (🌍:getf-entity-data map speaker-id :facing-right)))
         (text (getf dialogue :text))
         (coords (🌍:world-coords->screen-coords (🌍:getf-entity-data map speaker-id :coords))))
    ;; Ideally, place text-box above/below (NPC/player); otherwise, place it behind speaker
    (or (optimal-text-placement-vertically text coords :width width :height height
                                                       :downp playerp)
        (optimal-text-placement-horizontally text coords :width width :height height
                                                        :rightp leftp)
        ;; … Worst-case scenario, just do whatever’ll fit :w:”
        (optimal-text-placement-horizontally text coords :width width :height height
                                                         :rightp (not leftp))
        (optimal-text-placement-vertically text coords :width width :height height
                                             :downp  (not playerp)))))


(defun ensure-dialogue-layout (map dialogue-list)
  "Given a DIALOGUE-LIST, ensure that the FIRST line of dialogue has a :layout
property — that is, a property detailing the optimal width and coordinates for
its display."
  (when (and (getf (car dialogue-list) :text)
             (not (getf (car dialogue-list) :layout)))
    (setf (getf (car dialogue-list) :layout)
          (optimal-speech-layout map (car dialogue-list)))))


(defun render-dialogue-block (matrix dialogue)
  "Render a bit of DIALOGUE to the MATRIX, in an intelligent fashion; that is,
make it pretty, dang it! >O<
☆:.｡.o(≧▽≦)o.｡.:☆"
  (let* ((progress (getf dialogue :progress))
         (text (getf dialogue :text))
         (optimal-layout (getf dialogue :layout))
         (coords (car optimal-layout)))
    (when (and text optimal-layout)
;;      (✎:render-fill-rectangle matrix #\space
;;                               (list :x (- (getf coords :x) 1)
;;                                     :y (- (getf coords :y) 1)
;;                               (+ (second optimal-layout) 2) ;; Width
;;                               (+ (third optimal-layout) 1)) ;; Height
      (✎:render-string
       matrix text (first optimal-layout)
       :width (second optimal-layout)
       :char-count progress))))


(defun dialogue-state-draw (matrix map dialogue-list)
  "Draw the dialogue where appropriate.
Helper function for DIALOGUE-STATE."
  (when (getf (car dialogue-list) :text)
    (✎:show-cursor)
    (ensure-dialogue-layout map dialogue-list)
    (render-dialogue-block matrix (car dialogue-list))))



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


(defun make-dialogue-function (map dialogue-list)
  "Return a state-function for a section of dialogue, for use with STATE-LOOP."
  (lambda (matrix &key (map map) (dialogue dialogue-list))
    (🌍:overworld-state-draw matrix map)
    (dialogue-state matrix :map map :dialogue dialogue)))


(defun make-dialogue-state (map dialogue-list)
  "Return a state-plist for a section of dialogue, for use with STATE-LOOP."
  (list :function (make-dialogue-function map dialogue-list)))


;; Split a banana in two, bisection-fruit,
;; Yummy-yummy, toot-toot~ 🎵
