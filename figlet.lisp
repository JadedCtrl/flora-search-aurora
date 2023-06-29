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

;;;; FIGLET
;;;; A package for parsing Figlet fonts into simple associative lists, for
;;;; devious text-rendering purposes.

(in-package :figlet)


(defun string->integer (string)
  "Convert a string to a number, potentially in 0x… hexadecimal form.
If no number is parsed out, return NIL."
  (let ((radix (if (str:starts-with-p "0x" string)
                   16 10))
        (string (if (str:starts-with-p "0x" string)
                    (subseq string 2) string)))
    (ignore-errors (parse-integer string :radix radix))))


(defun parse-lines (lines &optional (font-plist '()) (current-symbol 32))
  "Parse a list of lines from a Figlet font-file (.FLF) into a plist
associating a character with its respective string in the font-file.
  (#\A \"TEXT-ART-A\" #\B \"TEXT-ART-B\" …)"
  (if lines
    (let* ((line (car lines))
           (sans-@ (cl:string-trim "@" line))
           (last-of-symbol-p (str:ends-with-p "@@" line))
           (not-art-line (not (str:ends-with-p "@" line)))
           (first-word (car (str:split " " line)))
           (first-word-num (string->integer first-word))
           (new-symbol-header-p (and not-art-line first-word-num)))
      (cond (new-symbol-header-p
             (parse-lines (cdr lines) font-plist first-word-num))
            ((not not-art-line)
             (setf (getf font-plist (code-char current-symbol))
                   (format nil "~A~A~%"
                           (or (getf font-plist (code-char current-symbol))
                               "")
                           sans-@))
             (parse-lines (cdr lines) font-plist (if last-of-symbol-p
                                                     (+ current-symbol 1)
                                                     current-symbol)))
            ('t
             (parse-lines (cdr lines) font-plist current-symbol))))
    font-plist))


(defun figlet-font-plist (path)
  "Parse a Figlet font-file (.FLF) into a plist associating a character
with its respective string in the font-file.
  (#\A \"TEXT-ART-A\" #\B \"TEXT-ART-B\" …)"
  (parse-lines
   (str:lines
    (alexandria:read-file-into-string path))))
