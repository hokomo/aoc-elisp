;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 'queue)
(require 's)

(defvar *connection-regexp*
  (rx-let ((node (group (+ alnum))))
    (rx node " = (" node ", " node ")")))

(defun parse-connection (line)
  (-map #'intern (cdr (s-match *connection-regexp* line))))

(defun read-08 (string)
  (seq-let [steps network] (s-split "\n\n" string t)
    (list steps (-map #'parse-connection (s-split "\n" network t)))))

(definput *test-08-1* #'read-08
  "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(definput *test-08-2* #'read-08
  "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

(definput *test-08-3* #'read-08
  "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(definput *input-08* #'read-08 "day-08-input.txt")

(defun network-follow (network node step)
  (funcall (if (= step ?L) #'car #'cadr) (cdr (assoc node network))))

(defun/s solve-08-1 ([steps network])
  (let ((len (length steps)))
    (pfor-do ((i (:range 0 nil))
              (:let ((s (v. steps (mod i len)))))
              (next (:step (network-follow network 'AAA s)
                           (network-follow network next s)))
              (:return (eq next 'ZZZ) (1+ i))))))

(expect (solve-08-1 *test-08-1*) 2)
(expect (solve-08-1 *test-08-2*) 6)
(expect (solve-08-1 *input-08*) 12737)

(defun node-suffix-p (node char)
  (= (v. (cat node) -1) char))

(defun/s network-cycle ([steps network] node)
  (let ((seen (st))
        (len (length steps))
        (nodes '()))
    (pfor ((i (:range 0 nil))
           (:let ((s (v. steps (mod i len)))))
           (prev (:step nil next))
           (next (:step (network-follow network node s)
                        (network-follow network next s)))
           (:do (push next nodes))
           (:return (or (node-suffix-p prev ?Z)
                        (s. seen (list next (mod i len))))
                    i)
           (:do (setf (s. seen (list next (mod i len))) t)))
      )))

(defun/s solve-08-2 ([steps network])
  (let ((starts (--filter (node-suffix-p it ?A) (-map #'car network))))
    (apply #'cl-lcm (--map (network-cycle (list steps network) it) starts))))

(expect (solve-08-2 *test-08-3*) 6)
(expect (solve-08-2 *input-08*) 9064949303801)

(comment
 (defun network-show (network)
   (with-graphviz-file "day-08.dot"
     (digraph
      (for (((node left right) network))
        (if (equal left right)
            (edge node left)
          (edge node left :label 'L)
          (edge node right :label 'R))))))

 (network-show (cl-second *test-08-1*))
 (network-show (cl-second *test-08-2*))
 (network-show (cl-second *test-08-3*)))
