;;;; A simple TUI-game made for the text-flavoured LibreJam of 2023-06!
;;;; https://jamgaroo.xyz/jams/2

(ql:quickload '(alexandria cl-charms cl-tiled str))


(defun move-cursor (row column &key (stream *standard-output*))
  "Moves cursor to desired position.
Borrowed from https://github.com/gorozhin/chlorophyll/
© 2022 Mikhail Gorozhin — MIT license"
  (format stream "~C[~A;~AH" #\Esc row column))


(defun clear-screen (&key (stream *standard-output*))
  "Completely clear the terminal screen."
  (move-cursor 0 0 :stream stream)
  (format stream "~C[J" #\Esc))


(defmacro do-for-cell (matrix &body body)
  "Given a 2d-array (matrix), execute the body for every cell.
The body has access to 4 variables:
  * i/j — The current row/column.
  * dimensions — Dimensions of the given matrix.
  * cell — The value of the current cell."
  `(let* ((dimensions (array-dimensions ,matrix))
          (max-i (car dimensions))
          (max-j (cadr dimensions))
          (i 0)  (j 0))
     (loop
      (let ((cell (ignore-errors (aref ,matrix i j))))
        (cond
          ((< i max-i)
           (cond
             ((< j max-j)
              ,@body
              (incf j))
             ((eq j max-j)
              (setf j 0)
              (incf i))))
          ((eq i max-i)
           (return)))))))


(defun matrix-delta (a b)
  "Given two 2D matrices, return a matrix containing only the cells
that change between a→b (favouring those in b) — all others are nil."
  (let ((delta (make-array (array-dimensions a))))
    (do-for-cell a
      (when (not (eq cell
                     (aref b i j)))
          (setf (aref delta i j)
                (aref b i j))))
    delta))


(defun print-screen-matrix (matrix)
  "Given a matrix of characters, print each element to standard output."
  (do-for-cell matrix
    (when (characterp cell)
      (move-cursor (+ i 1) (+ j 1))
      (write-char cell))))


(defun make-screen-matrix ()
  "Create a “screen matrix” — that is, a 2D array representing the
72x20 grid of characters we can print to the terminal."
  (make-array '(20 72) :initial-element #\space))


(defun screen-matrix-set-map (matrix map-path)
  "Draw a Tiled-format tilemap to the 2D array."
  (mapcar (lambda (layer) (screen-matrix-set-map-layer matrix layer))
          (cl-tiled:map-layers (cl-tiled:load-map map-path)))
  matrix)


(defun screen-matrix-set-map-layer (matrix tile-layer)
  "Set an array's elements to those corresponding the given Tiled
tile-layer's cells. a Tiled tile-layer to the screen."
  (mapcar (lambda (cell) (screen-matrix-set-char-cell matrix cell))
          (cl-tiled:layer-cells tile-layer))
  matrix)


(defun screen-matrix-set-char-cell (matrix cell)
  "Set a matrice's (2d array's) element corresponding to
a Tiled cell's character-value, using it's column and row."
  (setf (aref matrix
              (cl-tiled:cell-row cell)
              (cl-tiled:cell-column cell))
        (tile-character
         (cl-tiled:cell-tile cell))))


(defun tile-character (tile)
  "Given a tileset's tile, return it's corresponding text character,
assuming that the tileset is a bitmap font starting with char-code 32
with 15 characters-per-line."
  (code-char
   (+ (* (cl-tiled:tile-row tile) 15)
      (cl-tiled:tile-column tile)
      32)))


(defvar +qwerty-layout+
  '(#\q #\Q #\w #\W #\e #\E #\r #\R #\t #\T #\y #\Y #\u #\U #\i #\I #\o #\O #\p #\P #\[ #\{ #\] #\}
    #\a #\A #\s #\S #\d #\D #\f #\F #\g #\G #\h #\H #\j #\J #\k #\K #\l #\L #\; #\: #\' #\"
    #\z #\Z #\x #\X #\c #\C #\v #\V #\b #\B #\n #\N #\m #\M #\, #\< #\. #\> #\/ #\?))

(defvar +dvorak-layout+
  '(#\' #\" #\, #\< #\. #\> #\p #\P #\y #\Y #\f #\F #\g #\G #\c #\C #\r #\R #\l #\L #\/ #\? #\= #\+
    #\a #\A #\o #\O #\e #\E #\u #\U #\i #\I #\d #\D #\h #\H #\t #\T #\n #\N #\s #\S #\- #\_
    #\; #\: #\q #\Q #\j #\J #\k #\K #\x #\X #\b #\B #\m #\M #\w #\W #\v #\V #\z #\Z))


(defun parallel-list-item (item-a list-a list-b &key (test #'eql))
  "Given two parallel lists and an item contained in the first list, return its
corresponding item in the other list, by index."
  (let ((index (position item-a list-a :test test)))
    (if index
        (nth index list-b))))


(defun normalize-char (char-plist &optional (layout +qwerty-layout+))
  "Given a character input property list (as received from read-char-plist),
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


(defun main ()
  "A pathetic fascimile of a main loop. Look, I'm still tinkering!"
  (let ((matrix (make-screen-matrix)))
    (screen-matrix-set-map
     matrix
     (str:concat (namestring (uiop:getcwd)) "res/map.tmx"))
    (cl-charms:with-curses ()
      (cl-charms:enable-raw-input :interpret-control-characters 't)
      (print-screen-matrix matrix)
      (loop (print (deescape-char-plist (read-char-plist))))
      (sleep 5))))


(main)
