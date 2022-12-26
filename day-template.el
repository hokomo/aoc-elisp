;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (when (require 'aoc-emacs nil t) (aoc-mode 1)); -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 'heap)
(require 'ht)
(require 'queue)
(require 's)

(defun read-<level> (string)
  )

(definput *test-<level>* #'read-<level>
  "")

(definput *input-<level>* #'read-<level> "input-<level>.txt")

(defun solve-<level>-1 (thing)
  )

(expect (solve-<level>-1 *test-<level>*) nil)
(expect (solve-<level>-1 *input-<level>*) nil)

(defun solve-<level>-2 (thing)
  )

(expect (solve-<level>-2 *test-<level>*) nil)
(expect (solve-<level>-2 *input-<level>*) nil)
