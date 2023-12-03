;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (when (require 'aoc-emacs nil t) (aoc-mode 1)); -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 's)

(defun read-06 (string)
  (s-trim string))

(definput *test-06* #'read-06
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(definput *input-06* #'read-06 "input-06.txt")

(defun find-n-different (n string)
  (cl-loop for w in (seq-windows n string)
           for i from 0
           when (= (length (cl-delete-duplicates w)) n)
             return i))

(defun start-of-packet (n signal)
  (+ (find-n-different n signal) n))

(defun solve-06-1 (signal)
  (start-of-packet 4 signal))

(expect (solve-06-1 *test-06*) 7)
(expect (solve-06-1 *input-06*) 1965)

(defun solve-06-2 (signal)
  (start-of-packet 14 signal))

(expect (solve-06-2 *test-06*) 19)
(expect (solve-06-2 *input-06*) 2773)
