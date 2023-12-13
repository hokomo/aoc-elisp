(defun parse-grid (lines)
  (vecify (s-split "\n" lines t)))

(defun read-13 (string)
  (-map #'parse-grid (s-split "\n\n" string t)))

(defun count-smudges (vfn major minor i)
  (cl-loop for i1 from (1- i) downto 0
           for i2 from i to (1- major)
           sum (cl-loop for j from 0 below minor
                        count (/= (funcall vfn i1 j) (funcall vfn i2 j)))))

(defun find-reflection (vfn major minor smudges)
  (cl-loop for i from 1 to (1- major)
           when (= (count-smudges vfn major minor i) smudges)
             return i))

(defun find-reflections (grid smudges)
  (pcase-let ((`[,h ,w] (vdims grid))
              (vfn (lambda (i j) (v. grid i j))))
    (vector (or (find-reflection vfn h w smudges) 0)
            (or (find-reflection (-flip vfn) w h smudges) 0))))

(cl-defun grids-summary (grids &optional (smudges 0))
  (let ((rs (apply #'vmap #'+ (--map (find-reflections it smudges) grids))))
    (with-vref (+ (* 100 rs.x) rs.y))))

(defun solve-13-1 (grids)
  (grids-summary grids 0))

(defun solve-13-2 (grids)
  (grids-summary grids 1))
