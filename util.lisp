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


;;; ———————————————————————————————————
;;; Linewrapping & its helpers
;;; ———————————————————————————————————
(defun search-all (subseq sequence &key (start 0))
  "Given a SUBSEQ to search for within a SEQUENCE, return every instance of
SUBSEQ in SEQUENCE."
  (let ((matches '()))
    (loop while (setf start (search subseq sequence :start2 start))
          do (progn (pushnew start matches)
                    (incf start)))
    (reverse matches))) ;; So they’re in ascending order!


(defun closest-below (num number-list)
  "Given a NUMBER-LIST, return a descending list of member numbers below NUM."
  (sort
   (remove-if-not (lambda (a) (and (numberp a) (<= a num))) number-list)
   #'>))


(defun fit-lines (string width &key (alignment :center))
  "Fit each line of a STING into a specific WIDTH, with ALIGNMENT to a specific
side (either :CENTER, :LEFT, or :RIGHT)."
  (str:unlines
   (mapcar (lambda (line)
             (str:fit width line :pad-side alignment))
           (str:lines string))))


(defun linewrap-string (string width)
  "Break a STRING into several lines, each one no larger than WIDTH. Uses
newlines and hypens (to break long words) as necessary."
  (let* ((string (str:replace-all (string #\newline) " " string))
         (spaces (append '(0) (search-all " " string)))
         (index width))
    (loop while (< index (length string))
          do (let ((closest-space (car (closest-below index spaces)))
                   (old-index (- index width)))
               (if (or (<= closest-space old-index)
                       (> closest-space index))
                   ;; Break up long words with a hyphen
                   (return
                     (linewrap-string
                      (str:insert "- " (- index 1) string)
                      width))
                   ;; Replace eligible spaces with newlines uwu
                   (progn
                     (setf (elt string closest-space) #\newline)
                     (setf index (+ closest-space width)))))
         finally (return string))))



;;; ———————————————————————————————————
;;; Listic affairs
;;; ———————————————————————————————————
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



;;; ———————————————————————————————————
;;; Numeric affairs
;;; ———————————————————————————————————
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



;;; ———————————————————————————————————
;;; Linguistic & symbolic affirs
;;; ———————————————————————————————————
(defmacro remove-from-alistf (key alist &key (test 'eql))
  "Remove the given item from an associative list destructively."
  `(alexandria:removef
    ,alist ,key
    :test (lambda (key item) (,test key (car item)))))


(defun string->symbol (string)
  "Given a STRING with an optionally defined package (e.g., “package:symbol”),
return it as an appopriate symbol."
  (let* ((split (str:split ":" (string-upcase string)))
         (package (when (eq (length split) 2)
                    (car split)))
         (symbol (or (cadr split) (car split))))
    (if package
        (intern symbol package)
        (intern symbol))))


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


(defun getf-lang (plist &key language (fallback-lang :en))
  "With a PLIST containing keys of language-codes, return the property either fitting the
preferred LANGUAGE, or the backup FALLBACK-LANG (if LANGUAGE’s is NIL)."
  (or (getf plist (or language (ignore-errors *language*) (system-language)))
      (getf plist fallback-lang)))


(defparameter *language* (…:system-language))
