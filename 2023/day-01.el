;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

(require 'aoc-util)
(require 'dash)
(require 's)

(defun read-01 (string)
  (s-split "\n" string t))

(definput *test-01-1* #'read-01
  "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(definput *test-01-2* #'read-01
  "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(definput *input-01* #'read-01 "input-01.txt")

(defvar *digit-regexp*
  (rx-to-string
   `(or ,@(--map `(group ,it)
                 '("one" "two" "three" "four" "five"
                   "six" "seven" "eight" "nine"))
        digit)))

(defun calibration-value (regexp line)
  (let ((digits (--map (let ((idx (-find-index #'identity (cdr it))))
                         (if idx (1+ idx) (int (car it))))
                       (s-match-strings-all regexp line))))
    (horner (list (-first-item digits) (-last-item digits)) 10)))

(defun solve-01-1 (document)
  (-sum (--map (calibration-value (rx digit) it) document)))

(expect (solve-01-1 *test-01-1*) 142)
(expect (solve-01-1 *input-01*) 54239)

(defun solve-01-2 (document)
  (-sum (--map (calibration-value *digit-regexp* it) document)))

(expect (solve-01-2 *test-01-2*) 281)
(expect (solve-01-2 *input-01*) 55343)
