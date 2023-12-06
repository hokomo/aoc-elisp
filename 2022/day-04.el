;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 's)

(defun parse-range (range)
  (vmap #'int (s-split "-" range)))

(defun parse-pair (line)
  (-map #'parse-range (s-split "," line)))

(defun read-04 (string)
  (-map #'parse-pair (s-split "\n" string t)))

(definput *test-04* #'read-04
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(definput *input-04* #'read-04 "day-04-input.txt")

(defun/s complete-overlap-p ([x y])
  ;; NOTE: Assume the ranges are sorted.
  (with-vref (or (>= x.b y.b) (and (= y.a x.a) (>= y.b x.b)))))

(defun solve-04-1 (pairs)
  (->> pairs
       (--map (-sort #'range< it))
       (-count #'complete-overlap-p)))

(expect (solve-04-1 *test-04*) 2)
(expect (solve-04-1 *input-04*) 509)

(defun/s overlapp ([x y])
  ;; NOTE: Assume the ranges are sorted.
  (with-vref (<= y.a x.b)))

(defun solve-04-2 (pairs)
  (->> pairs
       (--map (-sort #'range< it))
       (-count #'overlapp)))

(expect (solve-04-2 *test-04*) 4)
(expect (solve-04-2 *input-04*) 870)
