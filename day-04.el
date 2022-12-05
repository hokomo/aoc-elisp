;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (aoc-mode 1); -*-

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'aoc-util)

(defun parse-range (range)
  (-map #'int (s-split "-" range)))

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

(definput *input-04* #'read-04 "input-04.txt")

(defun range< (x y)
  (funcall (cl-load-time-value
            (lexicographical-compare
             (compare-by #'< #'cl-first)
             (compare-by #'< #'cl-second)))
           x y))

(defun/s complete-overlap-p ([[a b] [c d]])
  ;; NOTE: Assume the pairs are sorted wrt each other.
  (or (>= b d) (and (= c a) (>= d b))))

(defun solve-04-1 (pairs)
  (->> pairs
       (--map (-sort #'range< it))
       (-count #'complete-overlap-p)))

(expect (solve-04-1 *test-04*) 2)
(expect (solve-04-1 *input-04*) 509)

(defun/s overlap-p ([[a b] [c d]])
  ;; NOTE: Assume the pairs are sorted wrt each other.
  (<= c b))

(defun solve-04-2 (pairs)
  (->> pairs
       (--map (-sort #'range< it))
       (-count #'overlap-p)))

(expect (solve-04-2 *test-04*) 4)
(expect (solve-04-2 *input-04*) 870)
