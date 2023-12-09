;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 's)

;;; For part 2 we assume a couple of things about the directed graph given in
;;; the input:
;;;
;;; (1) Each starting node runs into a cycle.
;;;
;;; (2) Each starting node runs into exactly one ending node, infinitely many
;;;     times (i.e. the unique ending node is part of the cycle).
;;;
;;; (3) The distance between each starting node and its corresponding ending
;;;     node (we call this the "meet") is equal to the length of the cycle.
;;;
;;; Generally, a path from any starting node A looks like the following (we use
;;; ->+ to denote one or more steps):
;;;
;;; A (start) ->+ C (cycle begin) ->+ Z (end) ->+ E (cycle end) -> C
;;;
;;; Z lies somewhere within the cycle, and it is allowed that Z = C or Z = E.
;;;
;;; If |XY| is the numbers of steps required to get from X to Y, then assumption
;;; (3) amounts to the requirement |AZ| (the meet) = |CC| = |ZZ|. Note that for
;;; this to be true, it is not necessary that Z = E, but only that |AC| (the
;;; "prefix") = |ZC| (the "suffix"). If Z = E, then |AC| = |ZC| = 1. This
;;; assumption guarantees that each path will repeatedly visit the ending node
;;; after a uniform number of steps.
;;;
;;; Relaxing assumption (3) would allow for the first visit to the ending node
;;; to take a different number of steps than all subsequent visits. If |AZ| is
;;; the meet m and |ZZ| is the cycle length c, one could still find a
;;; "synchronization point" for two paths, i.e. the moment at which they both
;;; land on an ending node. This happens when m_1 + k_1 c_1 = m_2 + k_2 c_2, for
;;; some integers k_1 and k_2, which reduces to the linear Diophantine equation
;;; c_1 k_1 - c_2 k_2 = m_2 - m_1. This equation has a solution only when the
;;; right-hand side is a multiple of gcd(k_1, k_2), which could be trivially
;;; guaranteed by making all k_i coprime.
;;;
;;; See <https://en.wikipedia.org/wiki/Diophantine_equation#One_equation>.
;;;
;;; Relaxing assumption (2) would be a *lot* harder, as each path could have an
;;; arbitrary number of occurrences of an ending node. One would then have to
;;; compute all of the different possible combinations of synchronization points
;;; between all paths.

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
  (let ((neighbors (cdr (assoc node network))))
    (if (= step ?L) (cl-first neighbors) (cl-second neighbors))))

(defun/s network-end-distance ([steps network] testfn start)
  (let ((len (length steps)))
    (pfor-do ((i (:range 0 nil))
              (:let ((s (v. steps (mod i len)))))
              (cur (:step (network-follow network start s)
                          (network-follow network cur s))))
      (when (funcall testfn cur)
        (cl-return (1+ i))))))

(defun solve-08-1 (document)
  (network-end-distance document (-cut eq <> 'ZZZ) 'AAA))

(expect (solve-08-1 *test-08-1*) 2)
(expect (solve-08-1 *test-08-2*) 6)
(expect (solve-08-1 *input-08*) 12737)

(defun node-suffix-p (node char)
  (= (v. (cat node) -1) char))

(defun network-starts (network)
  (--filter (node-suffix-p it ?A) (-map #'car network)))

(defun solve-08-2 (document)
  (let ((testfn (-cut node-suffix-p <> ?Z)))
    (->> (network-starts (cl-second document))
         (--map (network-end-distance document testfn it))
         (apply #'cl-lcm))))

(expect (solve-08-2 *test-08-3*) 6)
(expect (solve-08-2 *input-08*) 9064949303801)

(comment
 ;; Some utilities for inspecting the graphs and double-checking our assumptions
 ;; stated at the top.

 (defun/s network-analyze-path ([steps network] start)
   (let ((len (length steps))
         (seen (st))
         (meets (ht))
         (ends nil))
     (pfor-do ((i (:range 0 nil))
               (:let ((s (v. steps (mod i len)))))
               (prev (:step start cur))
               (cur (:step (network-follow network start s)
                           (network-follow network cur s)))
               ;; Guard against dead ends.
               (:while cur)
               (:let ((state (list cur (mod i len))))))
       ;; The first time we see an ending node.
       (when (and (node-suffix-p cur ?Z) (not (h. meets cur)))
         (push cur ends))
       ;; We detected a cycle.
       (when (s. seen state)
         (cl-return (list start
                          (list cur (h. meets cur))
                          (--map (list it (h. meets it)) ends)
                          (list prev (1+ i))
                          meets)))
       (setf (s. seen state) t
             (nilf (h. meets cur)) (1+ i)))))

 (defun network-show (document &optional allowed)
   (pcase-let ((`(,steps ,network) document)
               (allowed (-some--> allowed (if (sequencep it) (setify it) it))))
     (cl-labels ((allowedp (node)
                   (or (not allowed) (s. allowed node))))
       (with-graphviz-file "day-08.dot"
         (digraph
          (attr :label steps :labelloc 'top :labeljust 'center)
          (for-do ((conn network)
                   (:let ((`(,node ,left ,right) conn)))
                   (:when (allowedp node)))
            (if (and (equal left right) (allowedp left))
                (edge node left)
              (when (allowedp left)
                (edge node left :label 'L))
              (when (allowedp right)
                (edge node right :label 'R)))))))))

 (defun network-analyze (document showp &rest nodes)
   (let* ((starts (or nodes (network-starts (cl-second document))))
          (data (--map (network-analyze-path document it) starts)))
     (when showp
       (let ((allowed (-some--> nodes
                        (cons (setify it) (-map #'-last-item data))
                        (apply #'set-union it))))
         (network-show document allowed)))
     (-map #'butlast data)))

 (network-analyze *test-08-1* 'showp)
 (network-analyze *test-08-2* 'showp)
 (network-analyze *test-08-3* 'showp)
 (network-analyze *test-08-3* 'showp '11A)
 (network-analyze *test-08-3* 'showp '22A)

 (network-analyze *input-08* nil)
 (network-analyze *input-08* 'showp 'LPA)
 (network-analyze *input-08* 'showp 'XKA)
 (network-analyze *input-08* 'showp 'QGA)
 (network-analyze *input-08* 'showp 'AAA)
 (network-analyze *input-08* 'showp 'HHA)
 (network-analyze *input-08* 'showp 'LTA))
