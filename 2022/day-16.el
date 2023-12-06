;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 's)

;;; For both parts, we use a dynamic programming approach with memoization. Our
;;; state consists of the remaining time, the current valve position, and the
;;; set of all still-closed non-zero-flow valves. At every step, we branch by
;;; trying to open each of the valves in our set recursively and take the
;;; maximum of their solutions.
;;;
;;; For part 2, we encode the sets as bitsets in order to lower the memory
;;; consumption. Because the elephant's behavior is in parallel to ours, we can
;;; split the problem into two parts and solve them independently, with the same
;;; approach as in part 1. We split the set of valves into those to be explored
;;; by us versus the elephant. The optimal split is found by iterating through
;;; all possible splits, which is easy to do due to the bitset representation
;;; (iterating through [0, 2^n]).

(defvar *valve-regexp*
  (rx-let ((valve (+ (any "A-Z"))))
    (rx "Valve " (group valve) " has flow rate=" (group (+ digit)) ";"
        " tunnel" (? "s") " lead" (? "s") " to valve" (? "s") " "
        (group (+ valve (? ", "))))))

(defun parse-valve (line)
  (seq-let [valve flow next] (cdr (s-match *valve-regexp* line))
    (list valve (int flow) (s-split ", " next))))

(defun read-16 (string)
  (ht-from-alist (-map #'parse-valve (s-split "\n" string t))))

(definput *test-16* #'read-16
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II")

(definput *input-16* #'read-16 "day-16-input.txt")

(defun valve-distances (valves)
  (graph-floyd-warshall
   (graph-create (lambda (node)
                   (seq-let [_ adjacent] (h. valves node)
                     (-map #'list adjacent)))
                 (ht-keys valves))))

(defun useful-valves (valves)
  (for (((node info) (:ht valves))
        (:let ((`(,flow . ,_) info)))
        (:when (not (zerop flow))))
    node))

(defun solve-16-1 (valves)
  (let ((dist (valve-distances valves))
        (useful (useful-valves valves))
        (rec nil))
    (setf rec (memoize
               (lambda (time cur closed)
                 (apply #'max 0
                        (for ((v (:st closed))
                              (:let ((cost (1+ (h. dist cur v)))
                                     (`(,flow . ,_) (h. valves v))))
                              (:when (< cost time)))
                          (+ (* (- time cost) flow)
                             (funcall rec (- time cost) v
                                      (set-without closed v))))))))
    (funcall rec 30 "AA" (setify useful))))

(expect (solve-16-1 *test-16*) 1651)
(expect (solve-16-1 *input-16*) 2181 :gc 1000000000)

(defun solve-16-2 (valves)
  (let ((dist (valve-distances valves))
        (useful (vecify (useful-valves valves)))
        (rec nil))
    (setf rec (memoize
               (lambda (time cur closed)
                 (apply #'max 0
                        (for ([(mask (:step 1 (* 2 mask)))
                               (v useful)]
                              (:when (non-zero-p (logand closed mask)))
                              (:let ((cost (1+ (h. dist cur v)))
                                     (`(,flow . ,_) (h. valves v))))
                              (:when (< cost time)))
                          (+ (* (- time cost) flow)
                             (funcall rec (- time cost) v
                                      (logand closed (lognot mask)))))))))
    (apply #'max 0
           (let ((bitset (1- (ash 1 (length useful)))))
             (for ((mask (:range 0 (1+ bitset)))
                   (:let ((me mask)
                          (elephant (logand bitset (lognot mask))))))
               (+ (funcall rec 26 "AA" me)
                  (funcall rec 26 "AA" elephant)))))))

(expect (solve-16-2 *test-16*) 1707)
(expect (solve-16-2 *input-16*) 2824 :gc 1000000000)

(comment
 (defun valves-show (valves)
   (with-graphviz "day-16.dot"
     (princ "digraph {\n")
     (princ "  AA [style=filled, color=red];\n")
     (for-do (((k v) (:ht valves)))
       (printf "  %s -> %s;\n" k (s-join ", " (cl-second v)))
       (when (non-zero-p (car v))
         (printf "  %s [style=filled, color=yellow];\n" k)))
     (princ "}\n")))

 (valves-show *test-16*)
 (valves-show *input-16*))
