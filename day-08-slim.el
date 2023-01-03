;;; We use the point [i j] to represent the location in the i-th row and j-th
;;; column, starting from the origin [0 0] in the top-left.
;;;
;;; Rather than perform a scan in 4 directions for every tree, we perform 4
;;; separate "sweeps" over the whole grid and then aggregate the results.

(defun parse-forest-line (line)
  (vmap #'int line))

(defun read-08 (string)
  (vmap #'parse-forest-line (s-split "\n" string t)))

(defun copy-forest (forest)
  (vmap #'copy-sequence forest))

(defun forest-sweep (func forest dir)
  ;; NOTE: We arrange things so that the inner loop iterates over the individual
  ;; trees in the direction of DIR (one of the 4 orthogonal directions), while
  ;; the outer loop iterates over the individual lines (rows or columns).
  (pcase-let* ((`[,z ,_] dir)
               (dims (vdims forest))
               (`[,m ,n] (if (zerop z) (vrev dims) dims))
               (`(,start ,end ,step) (if (plusp (cl-find-if #'non-zero-p dir))
                                         (list 0 n 1)
                                       (list (1- n) -1 -1)))
               (sweep (copy-forest forest)))
    (prog1 sweep
      (for-do ([(i (:range m))
                (:let ((acc nil)))]
               [(j (:range start end step))
                (:let ((p (if (zerop z) `[,i ,j] `[,j ,i]))))])
        (pcase-let ((`(,nacc ,value) (funcall func acc sweep p)))
          (setf acc nacc (v2.. sweep p) value))))))

(defun forest-visible-sweep (forest dir)
  (forest-sweep (lambda (h sweep p)
                  (list
                   ;; Keep track of the highest tree in the line.
                   (max* h (v2.. forest p))
                   ;; Compute whether the tree is visible.
                   (int (or (not h) (> (v2.. forest p) h)))))
                forest dir))

(defun forest-visible (forest)
  (apply #'tree-map #'logior (for ((dir (h. *neighbors-2* 4)))
                               (forest-visible-sweep forest dir))))

(defun solve-08-1 (forest)
  (tree-reduce #'+ (forest-visible forest) :initial-value 0))

(defun forest-scenic-sweep (forest dir)
  (cl-flet ((dist (sweep p)
              ;; We speed up going backward in the line slightly by skipping by
              ;; each previous tree's viewing distance.
              (for-do ((:let ((sum 0)))
                       [(q (:step (v2+ p dir) (v2+ q (v2* `[,s ,s] dir)) ))
                        (:while (> (v2.. forest p) (v2.. forest q)))
                        (:let ((s (v2.. sweep q))))
                        (:while (non-zero-p s))
                        (:finally (cl-return sum))])
                (cl-incf sum s))))
    (forest-sweep (lambda (h sweep p)
                    (list
                     ;; Keep track of the height of the previous tree.
                     (v2.. forest p)
                     ;; Compute the viewing distance.
                     (cond
                      ((not h) 0)
                      ((> (v2.. forest p) h) (1+ (dist sweep p)))
                      (t 1))))
                  forest (v2* [-1 -1] dir))))

(defun forest-scenic (forest)
  (apply #'tree-map #'* (for ((dir (h. *neighbors-2* 4)))
                          (forest-scenic-sweep forest dir))))

(defun solve-08-2 (forest)
  (tree-reduce #'max (forest-scenic forest) :initial-value 0))
