;;; We use the point [i j] to represent the location in the i-th row and j-th
;;; column, starting from the origin [0 0] in the top-left.

(defun parse-cave-path (line)
  (--map (vmap #'int (s-split "," it)) (s-split " -> " line)))

(defun read-14 (string)
  (-map #'parse-cave-path (s-split "\n" string t)))

(defun cave-create (segments)
  (let ((cave (st)))
    (prog1 cave
      (for-do ((s segments)
               [((p1 p2) (:on s))
                (:while p2)
                (:let* ((dir (v2clamp [-1 -1] [1 1] (v2- p2 p1)))
                        (end (v2+ p2 dir))))]
               (v (:step p1 (v2+ v dir) (v2/= v end))))
        (set-add cave (vrev v))))))

(defun step-cave (cave p)
  (pfor-do ((dir '([1 0] [1 -1] [1 1]))
            (:let* ((q (v2+ p dir))
                    (movedp (not (s. cave q)))))
            (:until movedp)
            (:finally (cl-return (and movedp q))))))

(defun simulate-cave (func cave)
  (for-do ((:let ((start [0 500])))
           [(n (:range 0 nil))
            (:return (s. cave start) (funcall func n start))]
           [(p (:step start q))
            (:let ((q (step-cave cave p))))
            (:return (not q) (set-add cave p))
            (:let ((r (funcall func n q))))
            (:return r (set-add cave r))])))

(defun solve-14-1 (segments)
  (pcase-let* ((cave (cave-create segments))
               (`(,_ [,imax ,_]) (bounds cave)))
    (cl-block nil
      (simulate-cave (lambda (n p)
                       (prog1 nil
                         (when (with-vref (> p.i imax))
                           (cl-return n))))
                     cave))))

(defun solve-14-2 (segments)
  (pcase-let* ((cave (cave-create segments))
               (`(,_ [,imax ,_]) (bounds cave)))
    (cl-block nil
      (simulate-cave (lambda (n p)
                       (prog1 (and (with-vref (= p.i (1+ imax))) p)
                         (when (v2= p [0 500])
                           (cl-return n))))
                     cave))))
