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

;;;; FLORA-SEARCH-AURORA.OVERWORLD
;;;; All game-functions and data relating to the “overworld” (that is,
;;;; the primary gameplay, the RPG-ish-ish bits).

(in-package :flora-search-aurora.overworld)


;;; ———————————————————————————————————
;;; Misc. Utils
;;; ———————————————————————————————————
(defun within-rectangle (point top-left-corner bottom-right-corner)
  "With three coordinate plists, determine whether or not POINT resides within a
rectangle as defined by its TOP-LEFT-CORNER & BOTTOM-RIGHT-CORNER."
  (and (<= (getf point :x) (getf bottom-right-corner :x))
       (>= (getf point :x) (getf top-left-corner :x))
       (<= (getf point :y) (getf bottom-right-corner :y))
       (>= (getf point :y) (getf top-left-corner :y))))


(defmacro remove-from-alistf (key alist)
  "Remove the given item from an associative list destructively."
  `(alexandria:removef
    ,alist ,key
    :test (lambda (key item) (eq key (car item)))))



;;; ———————————————————————————————————
;;; Accessors
;;; ———————————————————————————————————
(defmacro getf-entity (map entity-id)
  "Get an entity from the map-data, using its ID."
  `(mapcan (lambda (chunk) (assoc ,entity-id (cdr chunk)))
           (gethash :entities ,map)))


(defmacro getf-entity-data (map entity-id key)
  "Get a specific piece of data from the given entity's property-list."
  `(getf (cdr (mapcan (lambda (chunk) (assoc ,entity-id (cdr chunk)))
                      (gethash :entities ,map)))
         ,key))


(defun removef-entity (map entity-id)
  "Remove an entity of the given ID from the map entirely. Nuke ‘em!
Literally kill them, show no mercy, dig your sharp nails into their fleshy
stomache and PULL HARD, show NO REMORSE. RAAAAAA 🗡🩸"
  (mapcar (lambda (chunk-alist)
            (overworld::remove-from-alistf entity-id (cdr chunk-alist)))
          (gethash :entities map)))


(defmacro aget-item (map item)
  "Get an item from the MAP’s :ITEMS alist. That is, an item in user’s inventory.
Members of :ITEMS will not be persistent beween play-throughs; the user has to
get everything again."
  `(assoc-utils:aget (gethash :items ,map) ,item))


(defmacro getf-act (map act)
  "Get an ACT from the MAP’s :ACTS plist. That is, some marker indicating that
the user has done something. Just like :ITEMS, these are not persistent through
replays of the game."
  `(getf (gethash :acts ,map) ,act))


(defmacro getf-know (map idea)
  "Get an item from the MAP’s :KNOWS plist. That is, some marker indicating that
the user knows something. Unlike :ITEMS and :ACTS, these _are_ persistent through
replays of the game."
  `(getf (gethash :knows ,map) ,idea))



;;; ———————————————————————————————————
;;; Item searching/testing
;;; ———————————————————————————————————
(defun entities-near-coords (coords radius entities &key (x-radius radius) (y-radius radius))
  "Return a list of entity-plists that are near the given coordinates within the given RADIUS."
  (remove-if-not
   (lambda (test-entity)
     (let ((test-coords (getf (cdr test-entity) :coords)))
       (and (< (abs (- (getf coords :x)
                       (getf test-coords :x)))
               x-radius)
            (< (abs (- (getf coords :y)
                       (getf test-coords :y)))
               y-radius))))
   (cdr (assoc (world-coords-chunk coords) entities))))


(defun entities-near-entity (entity entities)
  "Return a new list of entities near the given ENTITY — that is, within touching-distance."
  (remove-if
   (lambda (test-entity)
     (…:plist= (cdr entity)
             (cdr test-entity)))
   (entities-near-coords (getf (cdr entity) :coords)
                         (+ (length (getf (cdr entity) :face)) 6)
                         entities
                         :y-radius 4)))


(defun cell-at-world-coords-p (map-chunks coords)
  "Return whether or not there is a cell at the given COORDS."
  (let ((chunk (world-coords-chunk coords)))
    (member 't (cdr (assoc chunk map-chunks))
            :test (lambda (ignored cell)
                    (…:plist= (getf cell :coords) coords)))))


(defun walkable-tile-p (map x y)
  "Return whether or not the given coordinates on the MAP are traversable for an entity."
  (not (cell-at-world-coords-p (gethash :bump-map map)
                               (list :x x :y y))))


(defun trigger-at-coords (map world-coords)
  "Return a “Trigger”-rectangle from MAP that’d be triggered at the given coords."
  (let ((chunk (world-coords-chunk world-coords)))
    (loop for trigger in (cdr (assoc chunk (gethash :triggers map)))
          do (when (within-rectangle world-coords
                                     (getf trigger :coords) (getf trigger :bottom-coords))
               (return trigger)))))



;;; ———————————————————————————————————
;;; Map conversions & manipulations
;;; ———————————————————————————————————
(defun merge-maps (map-a map-b)
  "Copy data that should be persistent between maps from map-a to map-b.
Used primarily in moving between different maps in an overworld state."
  ;; Copy over important game-data from map-a.
  (mapcar
   (lambda (map-key)
     (setf (gethash map-key map-b) (gethash map-key map-a)))
   '(:acts :knows :items :seconds :day))
  ;; Copy specific bits of player data from map-a’s :ENTITIES.
  (mapcar
   (lambda (player-key)
     (setf (getf-entity-data map-b '✿:player player-key)
           (getf-entity-data map-a '✿:player player-key)))
   '(:face :normal-face :talking-face))
  map-b)



;;; ———————————————————————————————————
;;; Overworld logic
;;; ———————————————————————————————————
(defun overworld-state-update (map Δt)
  "Do nothing, lol. Core part of OVERWORLD-STATE.
Returns parameters to be used in the next invocation of OVERWORLD-STATE."
  (process-overworld-time map Δt)
  (process-overworld-input map))


(defun seconds->game-datetime (seconds &key (game-day-length 240))
  "Convert real-world SECONDS into a datetime plist, calculating with
GAME-DAY-LENGTH as as the seconds-per-day.
Returns a plist of properties :DAY, :HOUR, and :MINUTE, all numbers."
  (let* ((game-days (floor (/ seconds game-day-length))) ;; Days passed in game-time
         (seconds (floor (- seconds (* game-days game-day-length)))) ;; Keep hour below 24!
         (real-day-length 1440)) ;; You know what I mean <w<
    (multiple-value-bind (hour minutes-fraction)
        (floor (/ (* seconds (/ real-day-length game-day-length))
                  60))
      (list :day game-days :hour hour
            :minute (floor (* 60 minutes-fraction))))))


(defun process-overworld-time (map Δt)
  "Do nothing, lol. Core part of OVERWORLD-STATE.
Returns parameters to be used in the next invocation of OVERWORLD-STATE."
  (let* ((time (…:incf-0 (gethash :seconds map) Δt))
         (game-datetime (seconds->game-datetime time)))
    ;; Go through the day-update procedures!
    (when (not (eq (getf game-datetime :day)
                   (gethash :day map)))
      (setf (gethash :day map) (getf game-datetime :day)))))


(defun process-overworld-input (map)
  "Get and process any keyboard input, modifying the map or entities as necessary."
  (if (listen)
      (let* ((input (⌨:read-gamefied-char-plist)))
        (case (getf input :semantic)
          ;; Interacting with nearby characters/entities
          ('⌨:🆗
           (let* ((player (getf-entity map '✿:player))
                  (interactee (car (entities-near-entity player (gethash :entities map))))
                  (interactee-id (car interactee))
                  (interaction (getf (cdr interactee) :interact)))
             (if interaction
                 (apply (string->symbol interaction) (list map interactee-id))
                 (list :map map))))
          ('⌨:❎
           (🎒:make-inventory-state map))
          ;; Simple up-down-left-right movements
          ('⌨:→
           (move-player map :Δx 1))
          ('⌨:←
           (move-player map :Δx -1))
          ('⌨:↑
           (move-player map :Δy -1))
          ('⌨:↓
           (move-player map :Δy 1))
          (otherwise
           (list :map map))))
      (list :map map)))


(defun move-player (map &key (Δx 0) (Δy 0))
  (move-entity map '✿:player :Δx Δx :Δy Δy)
  (let* ((coords (getf-entity-data map '✿:player :coords))
         (trigger (trigger-at-coords map (list :x (getf coords :x) :y (getf coords :y)))))
    (if (and trigger (getf trigger :function))
        (apply (string->symbol (getf trigger :function))
               (list map))
        (list :map map))))


(defun move-entity (map entity-id &key (Δx 0) (Δy 0))
  "Move an entity relative to its current position."
  (when (< Δx 0)
    (setf (getf-entity-data map entity-id :facing-right) nil))
  (when (> Δx 0)
    (setf (getf-entity-data map entity-id :facing-right) 't))
  (let ((coords (getf-entity-data map entity-id :coords)))
    (move-entity-to map entity-id
                    :x (+ Δx (getf coords :x))
                    :y (+ Δy (getf coords :y)))))


(defun move-entity-to (map entity &key (x 0) (y 0))
  "Move the given entity to the given coordinates."
  (let ((old-chunk (world-coords-chunk (getf-entity-data map entity :coords)))
        (new-chunk (world-coords-chunk (list :x x :y y))))
    ;; Change the entity’s world coordinates…
    (when (walkable-tile-p map x y)
      (setf (getf (getf-entity-data map entity :coords) :x) x)
      (setf (getf (getf-entity-data map entity :coords) :y) y))
    ;; If the entity’s moved into a different screen-chunk (and so into a different
    ;; sub-alist of MAP hash-table’s :entities), move its list into the new chunk’s.
    (when (not (eq old-chunk new-chunk))
      ;; Add it to the new chunk list…
      (setf (assoc-utils:aget (assoc-utils:aget (gethash :entities map) new-chunk) entity)
            (cdr (getf-entity map entity)))
      ;; Delete it from the old list…
      (alexandria:deletef (assoc-utils:aget (gethash :entities map) old-chunk) entity
                          :test (lambda (id alist) (eq id (car alist)))))))



;;; ———————————————————————————————————
;;; Overworld-drawing: Map-rendering
;;; ———————————————————————————————————
(defun overworld-state-draw (matrix map)
  "Draw the overworld map to the given matrix.
A core part of OVERWORLD-STATE."
  (let* ((chunk (world-coords-chunk (getf-entity-data map '✿:player :coords))))
    (matrix-write-tiles matrix (gethash :tiles map) chunk)
    (matrix-write-entities matrix map chunk)
    (when (gethash :seconds map)
      (matrix-write-datetime matrix (seconds->game-datetime (gethash :seconds map))))
    (matrix-write-tiles matrix (gethash :top-tiles map) chunk)))



;;; ———————————————————————————————————
;;; Overworld-drawing: Map-tiles
;;; ———————————————————————————————————
(defun matrix-write-tiles (matrix tiles chunk
                           &key (chunk-width 72) (chunk-height 20))
  "Draw a map’s specific chunk (by its ID) to the matrix."
  (mapcar (lambda (cell)
            (if (or (not (getf cell :lang))
                    (eq (getf cell :lang) (…:system-language)))
                (matrix-write-cell matrix cell)))
          (cdr (assoc chunk tiles))))


(defun matrix-write-cell (matrix cell)
  "Set a matrice's (2d array's) element corresponding to a “cell”; that is, an
alist containing a character (:CHAR) and :X & :Y coordinates."
  (let ((coords (world-coords->screen-coords (getf cell :coords))))
    (setf (aref matrix
                (getf coords :y)
                (getf coords :x))
          (getf cell :char))))



;;; ———————————————————————————————————
;;; Overworld-drawing: Person-rendering
;;; ———————————————————————————————————
(defun matrix-write-entities (matrix map chunk)
  "Draw all entities from an alist of entities to the matrix."
  (mapcar (lambda (entity-assoc)
            (matrix-write-entity matrix (cdr entity-assoc)))
   (cdr (assoc chunk (gethash :entities map)))))


(defun matrix-write-entity (matrix entity-plist)
  "Render an entity-plist to the matrix."
  (when (getf entity-plist :face)
    (matrix-write-entity-head matrix entity-plist)
    (matrix-write-entity-legs matrix entity-plist))
  (when (getf entity-plist :avatar)
    (matrix-write-entity-avatar matrix entity-plist)))


(defun matrix-write-entity-avatar (matrix entity-plist)
  "Draw an “avatar” entity; that is, not a person, but a random item."
  (let* ((screen-coords (world-coords->screen-coords (getf entity-plist :coords)))
         (avatar (getf entity-plist :avatar))
         (width (length avatar))
         (y (getf screen-coords :y))
         (x (- (getf screen-coords :x) (floor (/ width 2)))))
    (📋:render-line matrix avatar x y)))


(defun matrix-write-entity-head (matrix entity-plist)
  "Draw an entity’s head. There aren't any Mami Tomoes in this game, dang it!"
  (let* ((screen-coords (world-coords->screen-coords (getf entity-plist :coords)))
         (face (getf entity-plist :face))
         (width (+ (length face) 2)) ;; Face + |borders|
         (y (- (getf screen-coords :y) 1))
         (x (if (getf entity-plist :facing-right)
                (- (getf screen-coords :x) (floor (/ width 2)) 0)
                (- (getf screen-coords :x) (floor (/ width 2)) 0))))
    (📋:render-line matrix face (+ x 1) y)
    (ignore-errors (setf (aref matrix y x) #\|))
    (ignore-errors (setf (aref matrix y (+ width x -1))
                         #\|))))


(defun matrix-write-entity-legs (matrix entity-plist)
  "Draw a bipdel entity’s legs — a surprisingly in-depth task!"
  (let* ((screen-coords (world-coords->screen-coords (getf entity-plist :coords)))
         (x (getf screen-coords :x))
         (y (getf screen-coords :y)))
    (cond ((getf entity-plist :facing-right)
           (ignore-errors (setf (aref matrix y x) #\|))
           (ignore-errors (setf (aref matrix y (- x 1)) #\|)))
          ('t
           (ignore-errors (setf (aref matrix y x) #\|))
           (ignore-errors (setf (aref matrix y (+ x 1)) #\|))))))



;;; ———————————————————————————————————
;;; Overworld-drawing: The date
;;; ———————————————————————————————————
(defun game-datetime->string (date-plist &optional (year 2006))
  (format nil
          (…:getf-lang '(:en "~A ~A ~A ~2,'0d:~2,'0d"
                         :eo "~A ~A ~Aa ~2,'0d:~2,'0d"))
          year
          (…:getf-lang '(:en "Jun" :eo "Jun"))
          (+ (getf date-plist :day) 3)
          (getf date-plist :hour)
          (getf date-plist :minute)))


(defun matrix-write-datetime (matrix datetime)
  (let ((string (game-datetime->string datetime)))
    (📋:render-line matrix string (- 71 (length string)) 19)))



;;; ———————————————————————————————————
;;; Overworld loop
;;; ———————————————————————————————————
(defun overworld-state
    (matrix &key map (Δt .02))
  "Render the given map to the matrix and take user-input — for one frame.
A state-function for use with STATE-LOOP."
  (sleep Δt)
  (overworld-state-draw matrix map)
  (overworld-state-update map Δt))


(defun make-overworld-state (map)
  "Return a state-function for a a map, for use with STATE-LOOP."
  (lambda (matrix &key (map map))
    (apply #'🌍:overworld-state
           (list matrix :map map))))
