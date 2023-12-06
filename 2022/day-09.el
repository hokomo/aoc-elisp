;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 's)

;;; We use the point [i j] to represent the location in the i-th row and j-th
;;; column, starting from the origin [0 0] in the top-left.

(defun parse-rope-move (line)
  (--map (car (read-from-string it)) (s-split " " line)))

(defun read-09 (string)
  (-map #'parse-rope-move (s-split "\n" string t)))

(definput *test-09-1* #'read-09
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(definput *test-09-2* #'read-09
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(definput *input-09* #'read-09 "day-09-input.txt")

(defvar *rope-dirs*
  (ht ('U [-1 0]) ('D [1 0]) ('L [0 -1]) ('R [0 1])))

(defun make-rope (knots)
  (vecify (for ((i (:range knots))) (vector 0 0))))

(defun adjust-spine (rope)
  (cl-loop for k from 1 below (length rope)
           while (> (chess-distance (v. rope (1- k)) (v. rope k)) 1)
           for dir = (v2- (v. rope (1- k)) (v. rope k))
           for unit = (v2clamp [-1 -1] [1 1] dir)
           do (setf (v. rope k) (v2+ (v. rope k) unit))))

(defun simulate-rope (func rope moves)
  (prog1 rope
    (cl-loop for (sym n) in moves do
      (dotimes (_ n)
        (setf (v. rope 0) (v2+ (v. rope 0) (h. *rope-dirs* sym)))
        (adjust-spine rope)
        (funcall func rope)))))

(defun unique-tails (knots moves)
  (let* ((rope (make-rope knots))
         (seen (st (v. rope -1))))
    (simulate-rope (lambda (rope)
                     (set-add seen (v. rope -1)))
                   rope moves)
    (set-size seen)))

(defun solve-09-1 (moves)
  (unique-tails 2 moves))

(expect (solve-09-1 *test-09-1*) 13)
(expect (solve-09-1 *input-09*) 6011)

(defun solve-09-2 (moves)
  (unique-tails 10 moves))

(expect (solve-09-2 *test-09-1*) 1)
(expect (solve-09-2 *test-09-2*) 36)
(expect (solve-09-2 *input-09*) 2419)
