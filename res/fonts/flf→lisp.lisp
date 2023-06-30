(ql:quickload '(alexandria str uiop))
(load "packages.lisp")
(load "figlet.lisp")


(defun local-path (subpath)
  (format nil "~A~A" (uiop:getcwd) subpath))


(mapcar
 (lambda (font-name)
   (figlet:save-font-to-file
    (local-path (format nil "res/fonts/~A.flf.lisp" font-name))
    (figlet:figlet-font-plist (local-path (format nil "res/fonts/~A.flf" font-name)))
    ":FLORA-SEARCH-AURORA.INTERMISSION"
    (format nil "*~A-font*" font-name)))
 '("standard" "small"))


(quit)
