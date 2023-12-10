;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 's)

(defun parse-nums (line)
  (-map #'int (s-split " " line t)))

(defun read-09 (string)
  (-map #'parse-nums (s-split "\n" string t)))

(definput *test-09* #'read-09
  "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(definput *input-09* #'read-09 "day-09-input.txt")

(defun adjacent-difference (nums)
  (--map (apply (-flip #'-) it) (-partition-in-steps 2 1 nums)))

(defun extrapolate-value (op history)
  (cl-labels ((rec (history)
                (if (-all-p #'zerop history)
                    0
                  (funcall op history (rec (adjacent-difference history))))))
    (rec history)))

(defun extrapolate-last (history)
  (extrapolate-value (lambda (history diff) (+ (-last-item history) diff))
                     history))

(defun solve-09-1 (histories)
  (-sum (-map #'extrapolate-last histories)))

(expect (solve-09-1 *test-09*) 114)
(expect (solve-09-1 *input-09*) 1969958987)

(defun extrapolate-first (history)
  (extrapolate-value (lambda (history diff) (- (-first-item history) diff))
                     history))

(defun solve-09-2 (histories)
  (-sum (-map #'extrapolate-first histories)))

(expect (solve-09-2 *test-09*) 2)
(expect (solve-09-2 *input-09*) 1068)
