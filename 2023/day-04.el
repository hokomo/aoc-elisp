;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 's)

(defun parse-card (line)
  (seq-let [_ body] (s-split ": " line t)
    (seq-let [winning owned] (s-split " | " body t)
      (list (vmap #'int (s-split " " winning t))
            (vmap #'int (s-split " " owned t))))))

(defun read-04 (string)
  (-map #'parse-card (s-split "\n" string t)))

(definput *test-04* #'read-04
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(definput *input-04* #'read-04 "input-04.txt")

(defun/s card-matches ([winning owned])
  (if-let ((winners (set-intersection (setify winning) (setify owned))))
      (set-size winners)
    0))

(defun card-value (card)
  (let ((matches (card-matches card)))
    (if (zerop matches) 0 (expt 2 (1- matches)))))

(defun solve-04-1 (cards)
  (-sum (-map #'card-value cards)))

(expect (solve-04-1 *test-04*) 13)
(expect (solve-04-1 *input-04*) 20855)

(defun solve-04-2 (cards)
  (let ((counts (make-vector (length cards) 1)))
    (for-do ([(i (:range 0 nil))
              (c cards)]
             (j (:range (card-matches c))))
      (cl-incf (v. counts (+ i j 1)) (v. counts i)))
    (cl-reduce #'+ counts)))

(expect (solve-04-2 *test-04*) 30)
(expect (solve-04-2 *input-04*) 5489600)
