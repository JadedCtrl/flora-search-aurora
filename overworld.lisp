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

(defpackage :flora-search-aurora.overworld
  (:nicknames :fsa.o :overworld :🌍)
  (:use :cl
   :flora-search-aurora.overworld.tiled :flora-search-aurora.overworld.util)
  (:export #:overworld-state #:make-overworld-state #:overworld-state-draw
           #:merge-maps
           #:world-coords->screen-coords
           #:getf-entity #:getf-entity-data
           #:move-entity-to #:move-entity
           :left :right
           :player))

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
  "Return a list of entities near the given entity — that is, within touching-distance."
  (remove-if
   (lambda (test-entity)
     (…:plist= (cdr entity)
             (cdr test-entity)))
   (entities-near-coords (getf (cdr entity) :coords)
                         (+ (length (getf (cdr entity) :face)) 6)
                         entities
                         :y-radius 4)))


(defun cell-at-world-coords-p (map-chunks coords)
  "Return whether or not there is a cell at the given coordinates."
  (let ((chunk (world-coords-chunk coords)))
    (member 't (cdr (assoc chunk map-chunks))
            :test (lambda (ignored cell)
                    (…:plist= (getf cell :coords) coords)))))


(defun walkable-tile-p (map x y)
  "Return whether or not the given coordinates on the map are traversable for an entity."
  (not (cell-at-world-coords-p (gethash :bump-map map)
                               (list :x x :y y))))


(defun trigger-at-coords (map world-coords)
  (let ((chunk (world-coords-chunk world-coords)))
    (loop for trigger in (cdr (assoc chunk (gethash :triggers map)))
           do (when (within-rectangle world-coords
                                       (getf trigger :coords) (getf trigger :bottom-coords))
                (return trigger)))))



;;; ———————————————————————————————————
;;; Overworld logic
;;; ———————————————————————————————————
(defun overworld-state-update (map)
  "Do nothing, lol. Core part of OVERWORLD-STATE.
Returns parameters to be used in the next invocation of OVERWORLD-STATE."
  (process-overworld-input map))


(defun process-overworld-input (map)
  "Get and process any keyboard input, modifying the map or entities as necessary."
  (if (listen)
      (let* ((input (⌨:normalize-char-plist (⌨:read-char-plist))))
        (cond
          ;; Interacting with nearby characters/entities
          ((…:plist= input '(:modifier nil :char #\return))
           (let* ((player (getf-entity map 'player))
                  (interactee (car (entities-near-entity player (gethash :entities map))))
                  (interaction (getf (cdr interactee) :interact)))
             (if interaction
                 (apply (intern (string-upcase interaction)) (list map))
                 (list :map map))))
          ;; The pause-menu…
;;          ((plist = input '(:modifier nil :char #\Esc)))
          ;; Simple up-down-left-right movements
          ((…:plist= input '(:modifier nil :char #\→))
           (move-player map :Δx 1))
          ((…:plist= input '(:modifier nil :char #\←))
           (move-player map :Δx -1))
          ((…:plist= input '(:modifier nil :char #\↑))
           (move-player map :Δy -1))
          ((…:plist= input '(:modifier nil :char #\↓))
           (move-player map :Δy 1))
          ('t
           (list :map map))))
      (list :map map)))


(defun move-player (map &key (Δx 0) (Δy 0))
  (move-entity map 'player :Δx Δx :Δy Δy)
  (let* ((coords (getf-entity-data map 'player :coords))
         (trigger (trigger-at-coords map (list :x (getf coords :x) :y (getf coords :y)))))
    (if (and trigger (getf trigger :function))
        (apply (intern (string-upcase (getf trigger :function)))
               (list map))
        (list :map map))))



(defun move-entity (map entity-id &key (Δx 0) (Δy 0))
  "Move an entity relative to its current position."
  (when (< Δx 0)
    (setf (getf-entity-data map entity-id :direction) 'left))
  (when (> Δx 0)
    (setf (getf-entity-data map entity-id :direction) 'right))
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
  (let* ((chunk (world-coords-chunk (getf-entity-data map 'player :coords))))
    (matrix-write-tiles matrix (gethash :tiles map) chunk)
    (matrix-write-entities matrix map chunk)
    (matrix-write-tiles matrix (gethash :top-tiles map) chunk)))


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
         (direction (getf entity-plist :direction))
         (face (getf entity-plist :face))
         (width (+ (length face) 2)) ;; Face + |borders|
         (y (- (getf screen-coords :y) 1))
         (x (if (eq direction 'right)
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
         (y (getf screen-coords :y))
         (direction (getf entity-plist :direction)))
    (cond ((eq direction 'right)
           (ignore-errors (setf (aref matrix y x) #\|))
           (ignore-errors (setf (aref matrix y (- x 1)) #\|)))
          ('t
           (ignore-errors (setf (aref matrix y x) #\|))
           (ignore-errors (setf (aref matrix y (+ x 1)) #\|))))))



;;; ———————————————————————————————————
;;; Overworld loop
;;; ———————————————————————————————————
(defun overworld-state
    (matrix &key (map-path nil) (map (load-map map-path)))
  "Render the given map to the matrix and take user-input — for one frame.
A state-function for use with STATE-LOOP."
  (sleep .02)
  (overworld-state-draw matrix map)
  (overworld-state-update map))


(defun make-overworld-state (map-path)
  "Return a state-function for a a map, for use with STATE-LOOP."
  (lambda (matrix &rest args)
    (apply #'🌍:overworld-state
           (append (list matrix :map-path map-path)
                   args))))


(defun merge-maps (map-a map-b)
  "Copy data that should be persistent between maps from map-a to map-b."
  (setf (gethash :acts map-b) (gethash :acts map-a))
  (setf (gethash :knows map-b) (gethash :knows map-a))
  (mapcar
   (lambda (player-key)
     (setf (getf-entity-data map-b 'player player-key)
           (getf-entity-data map-a 'player player-key)))
   '(:face :normal-face :talking-face))
  map-b)
