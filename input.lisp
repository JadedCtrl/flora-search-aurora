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

;;;; FLORA-SEARCH-AURORA.INPUT ⌨
;;;; All input-related voodoo goes here: Input reading, translating, parsing, etc.

(in-package :flora-search-aurora.input)

;; Yup, they're hardcoded like this. Horrid, ain't it? ^_^
(defvar +qwerty-layout+
  '(#\q #\Q #\w #\W #\e #\E #\r #\R #\t #\T #\y #\Y #\u #\U #\i #\I #\o #\O #\p #\P #\[ #\{ #\] #\}
    #\a #\A #\s #\S #\d #\D #\f #\F #\g #\G #\h #\H #\j #\J #\k #\K #\l #\L #\; #\: #\' #\"
    #\z #\Z #\x #\X #\c #\C #\v #\V #\b #\B #\n #\N #\m #\M #\, #\< #\. #\> #\/ #\?))


(defvar +dvorak-layout+
  '(#\' #\" #\, #\< #\. #\> #\p #\P #\y #\Y #\f #\F #\g #\G #\c #\C #\r #\R #\l #\L #\/ #\? #\= #\+
    #\a #\A #\o #\O #\e #\E #\u #\U #\i #\I #\d #\D #\h #\H #\t #\T #\n #\N #\s #\S #\- #\_
    #\; #\: #\q #\Q #\j #\J #\k #\K #\x #\X #\b #\B #\m #\M #\w #\W #\v #\V #\z #\Z))


(defvar +numpad-game-layout+
  '((#\8 . ↑)(#\4 . ←)(#\2 . ↓)(#\6 . →)(#\7 . ↰)(#\9 . ↱)(#\1 . ↲)(#\3 . ↳)
    (#\5 . 🆗)(#\return . 🆗)
    (#\+ . ❎)(#\0 . ❎)))


(defvar +arrows-game-layout+
  (append '((#\↑ . ↑)(#\← . ←)(#\↓ . ↓)(#\→ . →)
            (#\a . 🆗)(#\z . 🆗)(#\space . 🆗)(#\return . 🆗)
            (#\s . ❎)(#\x . ❎)(#\Esc . ❎))
          +numpad-game-layout+))


(defvar +wasd-game-layout+
  (append '((#\w . ↑)(#\a . ←)(#\s . ↓)(#\d . →)
            (#\j . 🆗)(#\← . 🆗)(#\space . 🆗)(#\return . 🆗)
            (#\k . ❎)(#\↓ . ❎)(#\Esc . ❎))
          +numpad-game-layout+))


(defvar +ijkl-game-layout+
  (append '((#\i . ↑)(#\j . ←)(#\k . ↓)(#\l . →)
            (#\a . 🆗)(#\z . 🆗)(#\← . 🆗)(#\space . 🆗)(#\return . 🆗)
            (#\s . ❎)(#\x . ❎)(#\↓ . ❎)(#\Esc . ❎))
          +numpad-game-layout+))


(defparameter *keyboard* +qwerty-layout+)
(defparameter *controls* +arrows-game-layout+)


(defun read-char-plist (&optional (stream *standard-input*))
  "Reads a character directly from standard-input (sans buffering).
Simple terminal escape codes (like arrow-keys) are translated into
roughly-semantically-equal character representations — see the
docstring of #'escape-code-to-character for more info."
  (let* ((char-1 (read-char stream))
         (char-2 (if (eq char-1 #\ESC) (read-char-no-hang stream)))  ; Maybe escaped char or [.
         (char-3 (if (eq char-2 #\[)   (read-char-no-hang stream)))  ; Maybe end-of-sequence, or 1.
         (char-4 (if (eq char-3 #\1)   (read-char-no-hang stream)))  ; Maybe semicolon.
         (char-5 (if (eq char-4 #\;)   (read-char-no-hang stream)))  ; Maybe modifer-key.
         (char-6 (if (characterp char-5) (read-char-no-hang stream)))) ; Escaped char and EOS.
    ;; Let me explain! There are pretty much three input-cases we should care about:
    ;;   * character
    ;;   * ␛ character
    ;;   * ␛ [ character
    ;;   * ␛ [ 1 ; modifier character
    ;; This is by no means comprehensive, sorry: I didn't even try! But it suits my purposes. :-P
    (let ((the-char (or char-6 char-3 char-2 char-1))
          (modifier (case char-5
                      (#\2 'shift)
                      (#\3 'meta)
                      (#\5 'control)))
          (escaped (eq char-1 #\ESC)))
      (list :char the-char :modifier modifier :escaped escaped))))


(defun normalize-char-plist (char-plist &optional (layout *keyboard*))
  "Given a character input property list (as received from READ-CHAR-PLIST),
massage the output into parsable, deescaped, QWERTY-according format."
  (let ((normalized (deescape-char-plist char-plist)))
    (setf (getf normalized :char)
          (qwertyize-char (getf normalized :char)
                          layout))
    normalized))


(defun qwertyize-char (char layout)
  "Given a char input in some layout, return the corresponding character in QWERTY.
Not at all comprehensive, but probably-mostly-just-good-enough. ¯\_ (ツ)_/¯"
  (or (parallel-list-item char layout +qwerty-layout+)
      char))


(defun read-gamefied-char-plist
    (&optional (stream *standard-input*) (layout *keyboard*) (game-layout *controls*))
  "Read a character directly from standard-input, then translating its character into the QWERTY
equivalent, then parsing that character into semantic meaning. Results in a plist like so:
   (:char #\w :semantic ↑ :modifier nil :escaped nil)"
  (gameify-char-plist (normalize-char-plist (read-char-plist stream) layout)
                      game-layout))


(defun gameify-char-plist (char-plist &optional (game-layout *keyboard*))
  "Given a character input plist (as received by READ-CHAR-PLIST), return a
char plist containing a :FUNCTION property, which contains one of several
semantic symbols that match up for menus/gameplay: ↑, →, ←, ↓, 🆗, or ❎."
  (let ((semantic-value (cdr (assoc (getf char-plist :char) game-layout))))
    (if semantic-value
        (append (list :semantic semantic-value) char-plist)
        char-plist)))


(defun deescape-char-plist (char-plist)
  "Translate escaped characters into somewhat-semantically-adjacent
characters, like left arrow-key (escaped D) into ← (“LEFTWARDS ARROW”)."
  (list :modifier (getf char-plist :modifier)
        :char (if (getf char-plist :escaped)
                  (case (getf char-plist :char)
                    (#\A #\↑)
                    (#\B #\↓)
                    (#\C #\→)
                    (#\D #\←)
                    (otherwise (getf char-plist :char)))
                  (getf char-plist :char))))



;;; ———————————————————————————————————
;;; Misc. utils
;;; ———————————————————————————————————
(defun parallel-list-item (item-a list-a list-b &key (test #'eql))
  "Given two parallel lists and an item contained in the first list, return its
corresponding item in the other list, by index."
  (let ((index (position item-a list-a :test test)))
    (if index
        (nth index list-b))))


(defun pressed-enter-p ()
  "Whether or not the enter/return key has been pressed recently.
Man, today’s a good day. Well, it wasn’t great, too be honest. Kind of bad,
I slightly humiliated myself a tiny bit. But wow, I’m having such nice tea!
Programming with nice tea! What a nice day this is. If you happen to be
reading this, I hope your day is going well too!
If not, have some tea on me: I’m paying. =w="
  (and (listen)
       (let ((input (getf (⌨:read-gamefied-char-plist) :semantic)))
         (or (eq input '⌨:🆗)
             (eq input '⌨:❎)))))
