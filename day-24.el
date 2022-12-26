;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (when (require 'aoc-emacs nil t) (aoc-mode 1)); -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'ht)
(require 's)

;;; We use the point [i j] to represent the location in the i-th row and j-th
;;; column, starting from the origin [0 0] in the top-left.

(defun read-24 (string)
  (vecify (s-split "\n" string t)))

(definput *test-24* #'read-24
  "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#")

(definput *input-24* #'read-24 "input-24.txt")

(defun blizzard-dir (c)
  (pcase-exhaustive c (?^ [-1 0]) (?v [1 0]) (?< [0 -1]) (?> [0 1])))

(defun valley-start-end (valley)
  (pcase-let ((`[,m ,_] (vdims valley)))
    (list `[0 ,(cl-position ?. (v. valley 0))]
          `[,(1- m) ,(cl-position ?. (v. valley (1- m)))])))

(defun blizzard-exists-p (valley p time)
  ;; NOTE: P must be a position *within* the walls, i.e. not the start or the
  ;; end, or any of the walls.
  (cl-loop with dims = (vdims valley)
           with vmax = (v2- dims [2 2])
           with vtime = `[,time ,time]
           for c across "^v<>"
           for dir = (blizzard-dir c)
           for ndir = (v2* [-1 -1] dir)
           for q = (v2wrap [1 1] vmax (v2+ p (v2* vtime ndir)))
             thereis (= c (v2.. valley q))))

(defun valley-neighbors (valley start end p time)
  (for ((:let ((dims (vdims valley))))
        (dir (cons [0 0] (h. *neighbors-2* 4)))
        (:let ((q (v2+ p dir))))
        (:when (and (v2< [-1 -1] q dims)
                    (/= (v2.. valley q) ?#)
                    (or (v2= q start)
                        (v2= q end)
                        (not (blizzard-exists-p valley q time))))))
    q))

(cl-defun valley-shortest-path (valley from to &optional (time 0))
  (pcase-let ((`(,start ,end) (valley-start-end valley))
              (q (queue-create))
              (seen (ht)))
    (let ((state (list from time)))
      (queue-append q state)
      (setf (h. seen state) t))
    (for-do ((:while (not (queue-empty q)))
             (:let ((`(,u ,total) (queue-dequeue q))))
             (:return (v2= u to) total)
             (v (valley-neighbors valley start end u (1+ total)))
             (:let ((state (list v (1+ total)))))
             (:when (not (h. seen state))))
      (queue-append q state)
      (setf (h. seen state) t))))

(defun solve-24-1 (valley)
  (pcase-let ((`(,start ,end) (valley-start-end valley)))
    (valley-shortest-path valley start end)))

(expect (solve-24-1 *test-24*) 18)
(expect (solve-24-1 *input-24*) 251 :gc 1000000000)

(defun solve-24-2 (valley)
  (pcase-let ((`(,start ,end) (valley-start-end valley)))
    (cl-loop repeat 3
             for from = start then to
             and to = end then from
             for time = 0 then total
             for total = (valley-shortest-path valley from to time)
             finally (cl-return total))))

(expect (solve-24-2 *test-24*) 54)
(expect (solve-24-2 *input-24*) 758 :gc 1000000000)
