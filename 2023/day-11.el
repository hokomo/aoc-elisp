;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 'queue)
(require 's)

(defun read-11 (string)
  (vecify (s-split "\n" string t)))

(definput *test-11* #'read-11
  "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

(definput *input-11* #'read-11 "day-11-input.txt")

(defun universe-galaxies (universe)
  (let ((galaxies '()))
    (with-tensor (i j) universe
      (when (= (v. universe i j) ?#)
        (push `[,i ,j] galaxies)))
    (nreverse galaxies)))

(defun universe-expansions (universe)
  (seq-let [h w] (vdims universe)
    (list (for ((r (:range h))
                (:let ((emptyp t)))
                (c (:range w))
                (:do (when (= (v. universe r c) ?\#)
                       (setf emptyp nil)))
                (:when (and emptyp (= c (1- w)))))
            r)
          (for ((c (:range w))
                (:let ((emptyp t)))
                (r (:range h))
                (:do (when (= (v. universe r c) ?\#)
                       (setf emptyp nil)))
                (:when (and emptyp (= r (1- h)))))
            c))))

(cl-defun galaxy-distances (universe gap)
  (pcase-let* ((galaxies (vecify (universe-galaxies universe)))
               (len (length galaxies))
               (`(,rs ,cs) (universe-expansions universe)))
    (for ((i (:range 0 len))
          (j (:range (1+ i) len))
          (:let* ((u (v. galaxies i))
                  (v (v. galaxies j))
                  (vmin (v2min u v))
                  (vmax (v2max u v))
                  (d (v2abs (v2- v u))))))
      (with-vref
        (+ d.x d.y
           (* (--count (< vmin.x it vmax.x) rs) (1- gap))
           (* (--count (< vmin.y it vmax.y) cs) (1- gap)))))))

(defun solve-11-1 (universe)
  (-sum (galaxy-distances universe 2)))

(expect (solve-11-1 *test-11*) 374)
(expect (solve-11-1 *input-11*) 10494813)

(defun solve-11-2 (universe)
  (-sum (galaxy-distances universe 1000000)))

(expect (solve-11-2 *test-11*) 82000210)
(expect (solve-11-2 *input-11*) 840988812853)
