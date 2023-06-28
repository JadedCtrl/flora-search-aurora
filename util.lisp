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

;;;; FLORA-SEARCH-AURORA.UTIL
;;;; Useful misc. utilities used in multiple packages.
;;;; Let's get to it, we're on a deadline!

(in-package :flora-search-aurora.util)


(defun split-string-by-length (string line-length &key (substrings '()))
  "Given a string, split it into a list of substrings all with lengths
equal or lower to the given length."
  (if (> (length string) line-length)
      (split-string-by-length
       (subseq string line-length)
       line-length
       :substrings (append substrings
                           `(,(subseq string 0 line-length))))
      (append substrings `(,string))))


(defun every-other-element (list)
  "Collect every-other-element of a list. E.g., (1 2 3 4) → (1 3)."
  (when list
    (cons (car list)
          (every-other-element (cddr list)))))


(defun plist= (a b &key (test #'eql))
  "Return whether or not two property lists are equal, by comparing values of each pair.
Uses the keys of plist a."
  (let ((keys (every-other-element a)))
    (loop for key in keys
          do (when (not (apply test (list (getf a key)
                                          (getf b key))))
                 (return nil))
          finally (return 't))))


(defmacro incf-0 (place &optional (Δ 1))
  "INCF the given PLACE, if it’s a number. If not a number, then set it to zero."
  `(if (numberp ,place)
       (incf ,place ,Δ)
       (setf ,place 0)))


(defun at-least (minimum num)
  "This function returns at least every hope and dream you've ever had, and at
maximum returns your more pitiful of moments."
  (if (< num minimum)
      minimum
      num))


(defun at-most (maximum num)
  "This function returns at most every hope and dream you've ever had, and at
minimum returns your more pitiful of moments."
  (if (> num maximum)
      maximum
      num))


(defun langcode->keysym (str)
  "Given a language’s code (es/cz/it/etc.), return a corresponding key symbol,
if the language is among the supported. Otherwise, nil."
  (when (stringp str)
    (let ((lang (string-downcase (subseq str 0 2))))
      (cond
        ((string-equal lang "eo") :eo)
        ((string-equal lang "en") :en)))))


(defun system-language ()
  "Return the system language, if among the supported; otherwise, EN-glish."
  (or (langcode->keysym (uiop:getenv "LANG"))
      :en))


(defun getf-lang (plist &optional (language (system-language)) (fallback-lang :en))
  "With a plist containing keys of language-codes, return the property either fitting the
preferred LANGUAGE, or the backup FALLBACK-LANG (if LANGUAGE’s is NIL)."
  (or (getf plist language)
      (getf plist fallback-lang)))
