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

;;;; FLORA-SEARCH-AURORA.UI
;;;; Generic menu-making, displaying, and management.
;;;; Let's get to it, we're on a deadline!

(defpackage :flora-search-aurora.util
  (:nicknames :fsa.ut :util :…)
  (:use :cl :assoc-utils)
  (:export #:split-string-by-length #:plist= #:at-least #:at-most))

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
