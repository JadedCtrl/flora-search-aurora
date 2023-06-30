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

;;;; FLORA-SEARCH-AURORA.INTERMISSION — CLOCKS
;;;; Some ASCII clock-stuff, for use with INTERMISSION’s clock-rendering.

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)
;;(in-package :flora-search-aurora.intermission)

;; So that we can do #>eof>strings like this!eof
;; What a wonderful macro! :D


(defparameter *clock-base*
"
   ---------------
  /   ________   \\\"\
 / . /        \ ,.\\\"\
⁄   /          \   \\\"|
|  |            |  |\"|
| ,|     ()     | ,|\"|
|. |            |  |\"|
|   \          / , |\"|
| ,. \________/.   |\"|
| .            . . |\"|
| .,-  ., . .      |\"|
| ..  . . . . . .  |\"|
|__________________|\"|
"
)

(defparameter *calendar-base*
"
  :===================:
  :===================:|
  |\/      /          ||
  |                   ||\"
  |                   ||\"
  |                   ||\"'
  |                   ||\"'
  |                   ||\"'
  |                   ||\"''
  |                   ||\"''
  |                   ||\"''
  |___________________||
"
)

