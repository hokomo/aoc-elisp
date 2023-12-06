;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 's)

;;; We use the point [x y] to represent the location in the y-th row and x-th
;;; column, starting from the origin [0 0] in the bottom-left.
;;;
;;; The floor of the chamber spans from [0 0] to [6 0].

(defun read-17 (string)
  (s-trim string))

(definput *test-17* #'read-17
  ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(definput *input-17* #'read-17 "day-17-input.txt")

(defvar *chamber-width* 7)

(cl-defstruct (rock (:constructor rock-create (type points vmin vmax))
                    (:copier rock-copy))
  type points vmin vmax)

(defun parse-rock (type string)
  (pcase-let* ((grid (vecify (s-split "\n" string t)))
               (`[,m ,_] (vdims grid))
               (points (st)))
    (with-tensor (i j) grid
      (when (= (v. grid i j) ?#)
        (set-add points `[,j ,(- m i 1)])))
    (apply #'rock-create type points (bounds points))))

(defvar *rocks*
  (->> "
####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##
"
       (s-split "\n\n")
       (-map-indexed #'parse-rock)))

(defun rock-move (rock v)
  (let ((new (rock-copy rock)))
    (prog1 new
      (with-svref rock
        (setf new.points (set-map (-cut v2+ v <>) rock.points)
              new.vmin (v2+ rock.vmin v)
              new.vmax (v2+ rock.vmax v))))))

(defun rock-init (rock h)
  (rock-move rock `[2 ,(+ h 3)]))

(defun jet-dir (jet)
  (pcase-exhaustive jet (?< [-1 0]) (?> [1 0])))

(defun rock-move-jet (rock dir chamber)
  (let ((nrock (rock-move rock dir)))
    (with-svref rock
      (and (<= 0 nrock.vmin.x nrock.vmax.x (1- *chamber-width*))
           (not (set-intersection nrock.points chamber))
           nrock))))

(defun rock-move-down (rock chamber)
  (let ((nrock (rock-move rock [0 -1])))
    (with-svref rock
      (and (<= 0 nrock.vmin.y)
           (not (set-intersection nrock.points chamber))
           nrock))))

(defun simulate-chamber (func n jets)
  (let ((chamber (st))
        (i 0)
        (h nil))
    (for-do ([(k (:range n))
              (r (:in (-cycle *rocks*)))]
             [(rock (:step (rock-init r (if h (1+ h) 0)) ry))
              (:while rock)
              (:let* ((dir (jet-dir (v. jets i)))
                      (rx (or (rock-move-jet rock dir chamber) rock))
                      (ry (rock-move-down rx chamber))))
              (:finally (with-svref rock
                          (funcall func chamber h k i rx)
                          (setf h (max* h rx.vmax.y))
                          (set-nunion chamber rx.points)))])
      (setf i (mod (1+ i) (length jets))))
    (1+ h)))

(defun solve-17-1 (jets)
  (simulate-chamber #'ignore 2022 jets))

(expect (solve-17-1 *test-17*) 3068)
(expect (solve-17-1 *input-17*) 3168)

(defun chamber-profile (chamber rock)
  (let ((profile (make-vector *chamber-width* 0)))
    (prog1 profile
      (for-do ((p (:st chamber)))
        (with-svref rock
          (setf (v. profile p.x) (max (v. profile p.x)
                                      (- p.y rock.vmin.y))))))))

(defun solve-17-2 (jets)
  (let ((seen (st))
        (info (ht))
        (done nil)
        (missing nil))
    (cl-block nil
      (simulate-chamber
       (lambda (chamber h k i rock)
         (with-svref rock
           (let ((dh (- (max* h rock.vmax.y) (or h 0))))
             (if missing
                 ;; `missing' has been set, so it's time to simulate the few
                 ;; remaining steps.
                 (if (zerop missing)
                     (cl-return (1+ done))
                   (setf done (+ done dh)
                         missing (1- missing)))
               (let ((state (list rock.type (chamber-profile chamber rock)
                                  i dh)))
                 (if (s. seen state)
                     ;; We've detected a cycle. Compute the period and shortcut
                     ;; as much computation as we can, and then set `missing' to
                     ;; the remaining number of steps.
                     (pcase-let* ((`(,init-k ,init-h) (h. info state))
                                  (rest (- 1000000000000 init-k))
                                  (period (- k init-k))
                                  (reps (/ rest period)))
                       (setf missing (mod rest period)
                             done (+ init-h (* reps (- h init-h)))
                             done (if (zerop missing) done (1+ done))))
                   ;; We still haven't detected a cycle. Keep memorizing.
                   (set-add seen state)
                   (setf (h. info state) (list k h))))))))
       1000000000000
       jets))))

(expect (solve-17-2 *test-17*) 1514285714288)
(expect (solve-17-2 *input-17*) 1554117647070)

(comment
 (defun rocks-show (chamber rock)
   (with-display _?
     (with-svref rock
       (let ((h 0))
         (for-do ((p (:st chamber)))
           (setf h (max h p.y)))
         (for-do ((y (:range (+ h 3 3) -2 -1))
                  (x (:range -1 (1+ *chamber-width*)))
                  (:let ((p `[,x ,y]))))
           (princ (cond
                   ((or (= x -1) (= x *chamber-width*))
                    (if (= y -1) "+" "|"))
                   ((= y -1) "-")
                   ((gethash p chamber)
                    "#")
                   ((and rock (gethash p rock.points))
                    "@")
                   (t ".")))
           (when (= x *chamber-width*)
             (princ "\n"))))))))
