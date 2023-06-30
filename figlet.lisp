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


;;; ———————————————————————————————————
;;; Misc. utilities
;;; ———————————————————————————————————
(defun contains-char-p (character string)
  "Whether or not a STRING contains the given CHARACTER."
  (str:containsp (string character) string))


(defun characters (string)
  "Return a list of a STRING’s characters."
  (loop for char across string
        collect char))


(defun string->integer (string)
  "Convert a string to a number, potentially in 0x… hexadecimal form.
If no number is parsed out, return NIL."
  (let ((radix (if (str:starts-with-p "0x" string)
                   16 10))
        (string (if (str:starts-with-p "0x" string)
                    (subseq string 2) string)))
    (ignore-errors (parse-integer string :radix radix))))


(defun unlines (strings)
  "Wrapper around STR:UNLINES that removes all non-strings from the STRINGS list."
  (str:unlines (remove-if-not #'stringp strings)))



;;; ———————————————————————————————————
;;; Font-parsing
;;; ———————————————————————————————————
(defun parse-lines (lines &optional (font-plist '()) (current-charcode 32))
  "Parse a list of lines from a Figlet font-file (.FLF) into a plist
associating a character with its respective string in the font-file.
  (#\A \"TEXT-ART-A\" #\B \"TEXT-ART-B\" …)"
  (if lines
    (let* ((line (car lines))
           (sans-@ (string-trim "@" line)) ;; Lines are terminated by ‘@’
           (last-of-symbol-p (str:ends-with-p "@@" line)) ;; Character-art is terminated by ‘@@’
           (not-art-line-p (not (str:ends-with-p "@" line))) ;; If no @ at all, line’s a comment or header
           (first-word-num (string->integer (car (str:words line)))) ;; If header line, this’ll be a character-code
           (current-art (ignore-errors (getf font-plist (code-char current-charcode)))))
      (cond
        ;; This is a header for a new char-art of specific char-code.
        ((and not-art-line-p first-word-num)
         (parse-lines (cdr lines) font-plist first-word-num))
        ;; If a line of char-art, amass it!
        ((not not-art-line-p)
         (setf (getf font-plist (code-char current-charcode))
               (unlines (list current-art
                              (str:replace-all (getf font-plist :space-char) " " sans-@))))
         (parse-lines (cdr lines) font-plist (if last-of-symbol-p
                                                (+ current-charcode 1)
                                                current-charcode)))
        ;; This is the first line of the file, the header line.
        ((str:starts-with-p "flf2a" line)
         (setf (getf font-plist :space-char)
               (subseq line 5 6)) ;; A char (often $) to substitute spaces.
         (parse-lines (cdr lines) font-plist current-charcode))
        ;; If none of the above, it’s a comment!
        ('t
         (setf (getf font-plist :comments)
               (unlines (list (getf font-plist :comments) line)))
         (parse-lines (cdr lines) font-plist current-charcode))))
    font-plist))


(defun figlet-font-plist (font-path)
  "Parse a Figlet font-file (.FLF) into a plist associating a character
with its respective string in the font-file.
  (#\A \"TEXT-ART-A\" #\B \"TEXT-ART-B\" …)"
  (parse-lines
   (str:lines
    (alexandria:read-file-into-string font-path))))



;;; ———————————————————————————————————
;;; Output of Figlet-style strings
;;; ———————————————————————————————————
(defun figlet-string (string &key (font-path nil) (font-plist (figlet-font-plist font-path)))
  (if (contains-char-p #\newline string)
      (mapcar (lambda (line) (figlet-string line :font-path font-path :font-plist font-plist))
              (str:lines string))
      (let* ((char-lines
               (mapcar (lambda (char)
                         (str:lines (getf font-plist char)))
                       (characters string)))
             (lines’-parts
               (loop for i to (- (length (car char-lines)) 1)
                    collect (mapcar (lambda (lines)
                                      (nth i lines))
                                    char-lines))))
        (str:unlines (mapcar (lambda (line-parts)
                               (reduce #'str:concat line-parts))
                             lines’-parts)))))
