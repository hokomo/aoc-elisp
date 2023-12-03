(defun read-12 (string)
  (vecify (s-split "\n" string t)))

(defun hill-find (hill c)
  (with-tensor (i j) hill
    (when (= (v. hill i j) c)
      (cl-return `[,i ,j]))))

(defun hill-elevation (hill pos)
  (pcase (v2.. hill pos)
    (?S ?a)
    (?E ?z)
    (c c)))

(defun hill-reachable (hill from to)
  (>= (hill-elevation hill from) (1- (hill-elevation hill to))))

(defun hill-neighbors (hill reachf pos)
  (for ((:let ((dims (vdims hill))))
        (dir (h. *neighbors-2* 4))
        (:let ((n (v2+ pos dir))))
        (:when (and (v2< [-1 -1] n dims) (funcall reachf hill pos n))))
    n))

(defun hill-steps (hill start reachf pred)
  (let ((start (hill-find hill start))
        (q (queue-create))
        (seen (st)))
    (queue-append q (list start 0))
    (set-add seen start)
    (for-do ((:while (not (queue-empty q)))
             (:let ((`(,pos ,steps) (queue-dequeue q))))
             (:return (funcall pred pos) steps)
             (n (:in (hill-neighbors hill reachf pos)))
             (:when (not (s. seen n))))
      (queue-append q (list n (1+ steps)))
      (set-add seen n))))

(defun solve-12-1 (hill)
  (hill-steps hill ?S #'hill-reachable
              (lambda (pos) (= (v2.. hill pos) ?E))))

(defun solve-12-2 (hill)
  (hill-steps hill ?E (lambda (hill from to) (hill-reachable hill to from))
              (lambda (pos) (= (hill-elevation hill pos) ?a))))
