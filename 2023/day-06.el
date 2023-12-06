;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

(require 'aoc-util)
(require 'dash)
(require 's)

(defvar *stats-regexp*
  (rx "Time: " (group (+ nonl)) "
Distance: " (group (+ nonl))))

(defun parse-nums (line)
  (-map #'int (s-split " " line t)))

(defun read-06 (string)
  (seq-let [_ times distances] (s-match *stats-regexp* string)
    (list (parse-nums times) (parse-nums distances))))

(definput *test-06* #'read-06
  "Time:      7  15   30
Distance:  9  40  200")

(definput *input-06* #'read-06 "day-06-input.txt")

(defun/s race-win-range ([time distance])
  ;; s = v * (time - v) = time * v - v^2
  ;;
  ;; s > distance
  ;; -v^2 + time * v - distance > 0
  ;;
  ;; v = (-time +- sqrt(time^2 - 4 distance)) / -2
  ;; v = (time -+ sqrt(time^2 - 4 distance)) / 2
  (let ((d (sqrt (- (* time time) (* 4 distance)))))
    (list (floor (1+ (/ (- time d) 2)))
          (ceiling (1- (/ (+ time d) 2))))))

(defun race-margin (race)
  (seq-let [from to] (race-win-range race)
    (1+ (- to from))))

(defun solve-06-1 (stats)
  (-product (-map #'race-margin (apply #'-zip-lists stats))))

(expect (solve-06-1 *test-06*) 288)
(expect (solve-06-1 *input-06*) 500346)

(defun combine-digits (nums)
  (horner (--mapcat (digits it 10) nums) 10))

(defun solve-06-2 (stats)
  (race-margin (-map #'combine-digits stats)))

(expect (solve-06-2 *test-06*) 71503)
(expect (solve-06-2 *input-06*) 42515755)
