(defun read-17 (string)
  (s-trim string))

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
                 (if (zerop missing)
                     (cl-return (1+ done))
                   (setf done (+ done dh)
                         missing (1- missing)))
               (let ((state (list rock.type (chamber-profile chamber rock)
                                  i dh)))
                 (if (s. seen state)
                     (pcase-let* ((`(,init-k ,init-h) (h. info state))
                                  (rest (- 1000000000000 init-k))
                                  (period (- k init-k))
                                  (reps (/ rest period)))
                       (setf missing (mod rest period)
                             done (+ init-h (* reps (- h init-h)))
                             done (if (zerop missing) done (1+ done))))
                   (set-add seen state)
                   (setf (h. info state) (list k h))))))))
       1000000000000
       jets))))
