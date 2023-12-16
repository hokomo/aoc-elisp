;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 's)

(defun read-16 (string)
  (vecify (s-split "\n" string t)))

(definput *test-16* #'read-16
  ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....")

(definput *input-16* #'read-16 "day-16-input.txt")

(defun beam-bounce (tile dir)
  (cl-ecase tile
    (?. (list dir))
    (?| (if (member dir '([-1 0] [1 0])) (list dir) `([-1 0] [1 0])))
    (?- (if (member dir '([0 -1] [0 1])) (list dir) `([0 -1] [0 1])))
    (?/ (list (if (member dir '([-1 0] [1 0])) (v2cw dir) (v2ccw dir))))
    (?\\ (list (if (member dir '([0 1] [0 -1])) (v2cw dir) (v2ccw dir))))))

(defun beam-step (grid beam)
  (pcase-let ((dims (vdims grid))
              (`(,pos ,dir) beam))
    (for ((ndir (beam-bounce (v2.. grid pos) dir))
          (:let ((npos (v2+ pos ndir))))
          (:when (v2< [-1 -1] npos dims)))
      (list npos ndir))))

(defun beam-simulate (grid init)
  (let ((seen (st))
        (energized (st)))
    (for-do ([(beams (:step (list init) nbeams))
              (:let ((nbeams '())))
              (:while beams)]
             (beam beams)
             (:when (not (s. seen beam)))
             (:let ((`(,pos ,_) beam))))
      (setf (s. seen beam) t
            (s. energized pos) t)
      (dolist (nbeam (beam-step grid beam))
        (push nbeam nbeams)))
    (set-size energized)))

(defun solve-16-1 (grid)
  (beam-simulate grid '([0 0] [0 1])))

(expect (solve-16-1 *test-16*) 46)
(expect (solve-16-1 *input-16*) 7623)

(defun beams-edges (grid)
  (seq-let [h w] (vdims grid)
    (append (for ((r (:range h))) `([,r 0] [0 1]))
            (for ((r (:range h))) `([,r ,(1- w)] [0 -1]))
            (for ((c (:range w))) `([0 ,c] [1 0]))
            (for ((c (:range w))) `([,(1- h) ,c] [-1 0])))))

(defun solve-16-2 (grid)
  (-max (-map (-partial #'beam-simulate grid) (beams-edges grid))))

(expect (solve-16-2 *test-16*) 51)
(expect (solve-16-2 *input-16*) 8244 :gc 4000000000)
