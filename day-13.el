;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (when (require 'aoc-emacs nil t) (aoc-mode 1)); -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 's)

(defun parse-packet (line)
  (cl-flet ((rep (c)
              (pcase-exhaustive c
                ("," " ")
                ("[" "(")
                ("]" ")"))))
    (car (read-from-string (s-replace-regexp (rx (any ",[]")) #'rep line)))))

(defun read-13 (string)
  (--map (-map #'parse-packet (s-split "\n" it t)) (s-split "\n\n" string t)))

(definput *test-13* #'read-13
  "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(definput *input-13* #'read-13 "input-13.txt")

(defun packet-cmp (x y)
  (if (and (integerp x) (integerp y))
      (signum (- x y))
    (cl-loop for xs = (ensure-list x) then (cdr xs)
             for ys = (ensure-list y) then (cdr ys)
             when (or (not xs) (not ys))
               return (cond
                       ((and (not xs) (not ys)) 0)
                       ((not xs) -1)
                       (t 1))
             for cmp = (packet-cmp (car xs) (car ys))
             when (/= cmp 0)
               return cmp)))

(defun solve-13-1 (packets)
  (->> (--find-indices (= (apply #'packet-cmp it) -1) packets)
       (-map #'1+)
       -sum))

(expect (solve-13-1 *test-13*) 13)
(expect (solve-13-1 *input-13*) 5882)

(defvar *decoder-packets*
  '(((2)) ((6))))

(defun packet< (x y)
  (= (packet-cmp x y) -1))

(defun solve-13-2 (packets)
  (->> (append (-flatten-n 1 packets) *decoder-packets*)
       (-sort #'packet<)
       (--find-indices (member it *decoder-packets*))
       (-map #'1+)
       -product))

(expect (solve-13-2 *test-13*) 140)
(expect (solve-13-2 *input-13*) 24948)
