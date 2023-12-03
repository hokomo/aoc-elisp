;; -*- lexical-binding: t; -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 's)

(defvar *game-regexp*
  (rx "Game " (group (+ digit)) ": " (group (+ nonl))))

(defvar *cube-colors*
  '("red" "green" "blue"))

(defvar *cube-regexp*
  (rx (group (+ digit)) " " (group (eval `(| ,@*cube-colors*)))))

(defun parse-round (string)
  (cl-loop
    with counts = (ht)
    for cube in (s-split ", " string t)
    for (n color) = (cdr (s-match *cube-regexp* cube))
    do (setf (h. counts color) (int n))
    finally (cl-return `[,@(--map (or (h. counts it) 0) *cube-colors*)])))

(defun parse-game (line)
  (seq-let [id body] (cdr (s-match *game-regexp* line))
    (let ((rounds (-map #'parse-round (s-split "; " body t))))
      (cons (int id) rounds))))

(defun read-02 (string)
  (-map #'parse-game (s-split "\n" string t)))

(definput *test-02* #'read-02
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(definput *input-02* #'read-02 "input-02.txt")

(defun minimal-cubes (game)
  (apply #'vmap #'max (cdr game)))

(defun solve-02-1 (games)
  (-sum (--map (if (v<= (minimal-cubes it) [12 13 14]) (car it) 0) games)))

(expect (solve-02-1 *test-02*) 8)
(expect (solve-02-1 *input-02*) 2528)

(defun solve-02-2 (games)
  (-sum (--map (cl-reduce #'* (minimal-cubes it)) games)))

(expect (solve-02-2 *test-02*) 2286)
(expect (solve-02-2 *input-02*) 67363)
