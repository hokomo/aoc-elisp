;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (aoc-mode 1); -*-

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'aoc-util)

(defun priority (char)
  (cond
   ((<= ?a char ?z)
    (1+ (- char ?a)))
   ((<= ?A char ?Z)
    (1+ (+ (- char ?A) 26)))))

(defun parse-rucksack (line)
  (seq-partition (-map #'priority line) (/ (length line) 2)))

(defun read-03 (string)
  (-map #'parse-rucksack (s-split "\n" string t)))

(definput *test-03* #'read-03
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(definput *input-03* #'read-03 "input-03.txt")

(defun/s find-shared ([x y])
  (cl-first (seq-intersection x y)))

(defun solve-03-1 (rucksacks)
  (-sum (-map #'find-shared rucksacks)))

(expect (solve-03-1 *test-03*) 157)
(expect (solve-03-1 *input-03*) 7917)

(defun find-badge (group)
  (cl-first (cl-reduce #'seq-intersection (-map #'join group))))

(defun solve-03-2 (rucksacks)
  (-sum (-map #'find-badge (seq-partition rucksacks 3))))

(expect (solve-03-2 *test-03*) 70)
(expect (solve-03-2 *input-03*) 2585)
