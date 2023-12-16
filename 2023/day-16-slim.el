(defun read-16 (string)
  (vecify (s-split "\n" string t)))

(defun beam-continue (tile dir)
  (cl-ecase tile
    (?. (list dir))
    (?| (if (member dir '([-1 0] [1 0])) (list dir) `([-1 0] [1 0])))
    (?- (if (member dir '([0 -1] [0 1])) (list dir) `([0 -1] [0 1])))
    (?/ (list (if (member dir '([-1 0] [1 0])) (v2cw dir) (v2ccw dir))))
    (?\\ (list (if (member dir '([0 1] [0 -1])) (v2cw dir) (v2ccw dir))))))

(defun beam-step (grid beam)
  (pcase-let ((dims (vdims grid))
              (`(,pos ,dir) beam))
    (for ((ndir (beam-continue (v2.. grid pos) dir))
          (:let ((npos (v2+ pos ndir))))
          (:when (v2< [-1 -1] npos dims)))
      (list npos ndir))))

(defun beam-simulate (grid init beamfn)
  (pfor-do ((beams (:step init nbeams))
            (:let ((nbeams '())))
            (:while beams))
    (dolist (beam beams)
      (when (funcall beamfn beam)
        (dolist (nbeam (beam-step grid beam))
          (push nbeam nbeams))))))

(defun solve-16-1 (grid)
  (energize-for grid '(([0 0] [0 1]))))

(defun energize-for (grid init)
  (let ((seen (st))
        (energized (st)))
    (beam-simulate grid init
                   (lambda (beam)
                     (seq-let [pos _] beam
                       (prog1 (not (s. seen beam))
                         (setf (s. energized pos) t
                               (s. seen beam) t)))))
    (set-size energized)))

(defun solve-16-2 (grid)
  (seq-let [h w] (vdims grid)
    (-max (--map (energize-for grid (list it))
                 (append (for ((r (:range h)))
                           `([,r 0] [0 1]))
                         (for ((r (:range h)))
                           `([,r ,(1- w)] [0 -1]))
                         (for ((c (:range w)))
                           `([0 ,c] [1 0]))
                         (for ((c (:range w)))
                           `([,(1- h) ,c] [-1 0])))))))
