;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (when (require 'aoc-emacs nil t) (aoc-mode 1)); -*-

(require 'aoc-util)
(require 'dash)
(require 's)

(defun parse-block (block)
  (-map #'int (s-split "\n" block t)))

(defun read-01 (string)
  (-map #'parse-block (s-split "\n\n" string t)))

(definput *test-01* #'read-01
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(definput *input-01* #'read-01 "input-01.txt")

(defun calorie-sum (n calories)
  (-sum (top-n n (-map #'-sum calories))))

(defun solve-01-1 (calories)
  (calorie-sum 1 calories))

(expect (solve-01-1 *test-01*) 24000)
(expect (solve-01-1 *input-01*) 69836)

(defun solve-01-2 (calories)
  (calorie-sum 3 calories))

(expect (solve-01-2 *test-01*) 45000)
(expect (solve-01-2 *input-01*) 207968)