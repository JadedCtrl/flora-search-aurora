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

;;;; FLORA-SEARCH-AURORA.SETTINGS 🔧
;;;; Guess whatttt? Settings, that’s what. =w=
;;;; How riveting!

(in-package :flora-search-aurora.settings)


(defun make-controls-function (controls)
  "Simple state-function (for use with STATE-LOOP's state plists) that sets the
keyboard controls to the given CONTROLS."
  (lambda (matrix)
    (setq ⌨:*controls* controls)
    (list :drop 1)))


(defun make-keyboard-function (layout)
  "Simple state-function (for use with STATE-LOOP's state plists) that sets the
keyboard layout to the given LAYOUT."
  (lambda (matrix)
    (setq ⌨:*keyboard* layout)
    (list :drop 1)))


(defun make-language-function (language)
  "Simple state-function (for use with STATE-LOOP's state plists) that sets the
game language to the given LANGUAGE."
  (lambda (matrix)
    (setq …:*language* language)
    (list :drop 1)))


(defun keyboard-menu ()
  `((:en "QWERTY"
     :selection 50 :selected t
     :function ,(make-keyboard-function ⌨:+qwerty-layout+)
     :drop 1)
    (:en "Dvorak"
     :function ,(make-keyboard-function ⌨:+dvorak-layout+)
     :drop 1)
    (:en "Arrows" :eo "Sagoj"
     :function ,(make-controls-function ⌨:+arrows-game-layout+)
     :row 1 :drop 1)
    (:en "WASD"
     :function ,(make-controls-function ⌨:+wasd-game-layout+)
     :row 1 :drop 1)
    (:en "IJKL" :eo "IJKL"
     :function ,(make-controls-function ⌨:+ijkl-game-layout+)
     :row 1 :drop 1)))


(defun language-menu ()
  `((:en "Esperanto"
     :selected t :selection 50
     :function ,(make-language-function :eo)
     :drop 1)
    (:en "English"
     :function ,(make-language-function :en)
     :row 1 :drop 1)))


(defun settings-menu ()
  `((:en "Keyboard" :eo "Klavararanĝo"
     :selection 50 :selected t
     :function
     ,(📋:make-menu-function (keyboard-menu))
     :drop 1)
    (:en "Language" :eo "Lingvo"
     :function ,(📋:make-menu-function (language-menu))
     :drop 1)
    (:en "Back" :eo "Reiri"
     :drop 1)))


(defun make-settings-menu-function ()
  "Create a menu state-function for use with STATE-LOOP, displaying settings
and allowing the user to modify them. Pretty self-explanatory, tbh."
  (📋:make-menu-function (settings-menu)))
