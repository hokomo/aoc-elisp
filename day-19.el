;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (when (require 'aoc-emacs nil t) (aoc-mode 1)); -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 's)

;;; The code generalizes to N different resources and robots. We assume the i-th
;;; robot within a blueprint produces exactly 1 i-th resource per minute, and
;;; that the goal is to maximize the production of the (N - 1)-th resource.
;;;
;;; For both parts, we use a dynamic programming approach with memoization. Our
;;; state consists of the remaining time, the current inventory, and the current
;;; "rates" (current amount of built robots).
;;;
;;; We employ 3 optimizations, roughly from least to most important:
;;;
;;; - At every step, we branch into trying to build each type of robot next. We
;;;   do this by calculating the amount of time we might have to wait first in
;;;   order to accumulate enough resources to build the robot. This allows us to
;;;   skip over multiple time units at a time and do away with a "do nothing"
;;;   action that we would need if we were to branch 1 time unit at a time.
;;;
;;; - We never attempt to build more non-geode robots than we need. Since we can
;;;   only build 1 robot at a time and building each one takes 1 minute, it only
;;;   ever makes sense to build as many i-th robots as is the maximum amount of
;;;   the i-th resource required to build any robot. Building more than that
;;;   threshold would mean that the robots would just keep accumulating
;;;   leftovers that we wouldn't have time to spend.
;;;
;;;   We can even improve that a little bit by also taking into account the
;;;   current amount of the i-th resource and the amount of it we would
;;;   accumulate until the end with the current number of i-th robots. If that
;;;   quantity is higher than what we could ever possibly spend by building any
;;;   robot that requires the highest amount of the i-th resource at every turn
;;;   until the end, then there's no point in building any more of i-th robots.
;;;
;;; - We keep track of the globally best solution so far and use it to
;;;   immediately discard branches that couldn't possibly compete with it, even
;;;   if they were to build 1 geode robot every turn until the end.
;;;
;;;   Note that keeping track of the highest amount of geodes achieved *up to
;;;   each time unit* is not the same and is not correct, as a branch might end
;;;   up speeding up later on and beat a solution that had a higher count early
;;;   on.

(defvar *blueprint-regexp*
  (rx-let ((n (group (+ digit))))
    (rx "Blueprint " n ": Each ore robot costs " n " ore. \
Each clay robot costs " n " ore. \
Each obsidian robot costs " n " ore and " n " clay. \
Each geode robot costs " n " ore and " n " obsidian.")))

(defun parse-blueprint (string)
  (seq-let [id ore clay ob1 ob2 geo1 geo2]
      (-map #'int (cdr (s-match *blueprint-regexp* string)))
    (list id `[,ore 0 0 0] `[,clay 0 0 0] `[,ob1 ,ob2 0 0] `[,geo1 0 ,geo2 0])))

(defun read-19 (string)
  (-map #'parse-blueprint (s-split "\n" string t)))

(definput *test-19* #'read-19
  "Blueprint 1: \
Each ore robot costs 4 ore. \
Each clay robot costs 2 ore. \
Each obsidian robot costs 3 ore and 14 clay. \
Each geode robot costs 2 ore and 7 obsidian. \

Blueprint 2: \
Each ore robot costs 2 ore. \
Each clay robot costs 3 ore. \
Each obsidian robot costs 3 ore and 8 clay. \
Each geode robot costs 3 ore and 12 obsidian.")

(definput *input-19* #'read-19 "input-19.txt")

(defun robot-wait (inv rates cost)
  (let ((waits (vmap (lambda (i r c)
                       (let ((m (max 0 (- c i))))
                         (if (zerop m) 0 (and (non-zero-p r) (ceiling m r)))))
                     inv rates cost)))
    (and (cl-every #'identity waits) (1+ (cl-reduce #'max waits)))))

(defun robot-neighbors (time inv rates costs)
  (for ((:let ((maxs (apply #'vmap #'max* costs))
               (n (length inv))))
        [(i (:range n))
         (cost costs)]
        (:let ((wait (robot-wait inv rates cost))))
        (:when (and wait
                    (< wait time)
                    ;; Build as many geode robots as possible, but only as many
                    ;; i-th robots as possibly needed.
                    (or (= i (1- n))
                        (< (+ (v. inv i) (* (v. rates i) time))
                           (* (v. maxs i) time)))))
        (:let* ((vwait (make-vector n wait))
                (ninv (v- (v+ inv (v* rates vwait)) cost))
                (nrates (copy-sequence rates)))))
    (cl-incf (v. nrates i))
    (list (- time wait) ninv nrates)))

(defun max-geodes (time costs)
  (let ((n (length costs))
        (max-geodes 0)
        (rec nil))
    (setf rec (memoize
               (lambda (time inv rates)
                 ;; Return immediately if this branch has no chance of improving
                 ;; upon the current best, even if it could build a geode bot
                 ;; every turn until the end (in that case, the number of geodes
                 ;; acquired at each step *after the first* follows the
                 ;; progression 1, 2, 3, ...).
                 (if (<= (+ (v. inv (1- n))
                            (* (v. rates (1- n)) time)
                            (* (sum-n (1- time))))
                         max-geodes)
                     0
                   (let ((sol (apply
                               #'max 0
                               (for ((s (robot-neighbors time inv rates costs))
                                     (:let ((`(,ntime ,ninv ,nrates) s))))
                                 (+ (* (v. (v- nrates rates) (1- n)) ntime)
                                    (funcall rec ntime ninv nrates))))))
                     (prog1 sol
                       (setf max-geodes (max max-geodes sol))))))))
    (funcall rec time [0 0 0 0] [1 0 0 0])))

(defun/s blueprint-quality ([id &rest costs])
  (* id (max-geodes 24 costs)))

(defun solve-19-1 (blueprints)
  (-sum (-map #'blueprint-quality blueprints)))

(expect (solve-19-1 *test-19*) 33 :gc 1000000000)
(expect (solve-19-1 *input-19*) 1958 :gc 5000000000)

(defun solve-19-2 (blueprints)
  (-product (--map (max-geodes 32 (cdr it)) (-take 3 blueprints))))

(expect (solve-19-2 *test-19*) 3472 :gc 1000000000)
(expect (solve-19-2 *input-19*) 4257 :gc 5000000000)
