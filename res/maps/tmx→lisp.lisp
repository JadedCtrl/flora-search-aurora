(ql:quickload '(alexandria cl-tiled assoc-utils str uiop))
(load "util.lisp")
(load "overworld.util.lisp")
(load "overworld.tiled.lisp")

(defpackage :flora-search-aurora
  (:nicknames :fsa :✿)
  (:export #:main)
  (:use :cl))


(defun local-path (subpath)
  (format nil "~A~A" (uiop:getcwd) subpath))


(mapcar
 (lambda (map-name)
   (🌍.…::save-map-to-file
    (local-path (format nil "res/maps/~A.tmx.lisp" map-name))
    (overworld.tiled:load-map (local-path (format nil "res/maps/~A.tmx" map-name)))
    ":FLORA-SEARCH-AURORA"
    (format nil "*~A-map*" map-name)))
 '("casino" "outdoors" "flashback-school"))
