(defun read-12 (string)
  (cl-coerce (s-split "\n" string t) 'vector))

(defun hill-find (hill c)
  (with-tensor (i j) hill
    (when (= (v. hill i j) c)
      (cl-return (vector i j)))))

(defun hill-elevation (hill pos)
  (pcase (v2.. hill pos)
    (?S ?a)
    (?E ?z)
    (c c)))

(defun hill-reachable (hill from to)
  (>= (hill-elevation hill from) (1- (hill-elevation hill to))))

(defun hill-neighbors (hill reachf pos)
  (for ((:let ((dims (vdims hill))))
        (dir (cl-load-time-value (h. *neighbors-2* 4)))
        (:let ((n (v2+ pos dir))))
        (:when (and (v2< [-1 -1] n dims) (funcall reachf hill pos n))))
    n))

(defun hill-steps (hill start reachf pred)
  (let ((start (hill-find hill start))
        (q (queue-create))
        (seen (ht)))
    (queue-append q (list start 0))
    (setf (h. seen start) t)
    (for-do ((:while (not (queue-empty q)))
             (:let ((`(,pos ,steps) (queue-dequeue q))))
             (:return (funcall pred pos) steps)
             (n (:in (hill-neighbors hill reachf pos)))
             (:when (not (h. seen n))))
      (queue-append q (list n (1+ steps)))
      (setf (h. seen n) t))))

(defun solve-12-1 (hill)
  (hill-steps hill ?S #'hill-reachable
              (lambda (pos) (= (v2.. hill pos) ?E))))

(defun solve-12-2 (hill)
  (hill-steps hill ?E (lambda (hill from to) (hill-reachable hill to from))
              (lambda (pos) (= (hill-elevation hill pos) ?a))))
