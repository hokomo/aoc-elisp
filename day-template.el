;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (aoc-mode 1); -*-

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'aoc-util)

(defun read-<level> (string)
  )

(definput *test-<level>* #'read-<level>
  "")

(definput *input-<level>* #'read-<level> "input-<level>.txt")

(defun solve-<level>-1 (input)
  )

(expect (solve-<level>-1 *test-<level>*) nil)
(expect (solve-<level>-1 *input-<level>*) nil)

(defun solve-<level>-2 (input)
  )

(expect (solve-<level>-2 *test-<level>*) nil)
(expect (solve-<level>-2 *input-<level>*) nil)
