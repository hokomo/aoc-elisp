(defvar *connection-regexp*
  (rx-let ((node (group (+ alnum))))
    (rx node " = (" node ", " node ")")))

(defun parse-connection (line)
  (-map #'intern (cdr (s-match *connection-regexp* line))))

(defun read-08 (string)
  (seq-let [steps network] (s-split "\n\n" string t)
    (list steps (-map #'parse-connection (s-split "\n" network t)))))

(defun network-follow (network node step)
  (funcall (if (= step ?L) #'car #'cadr) (cdr (assoc node network))))

(defun/s solve-08-1 ([steps network])
  (let ((len (length steps)))
    (pfor-do ((i (:range 0 nil))
              (:let ((s (v. steps (mod i len)))))
              (next (:step (network-follow network 'AAA s)
                           (network-follow network next s)))
              (:return (eq next 'ZZZ) (1+ i))))))

(defun node-suffix-p (node char)
  (= (v. (cat node) -1) char))

(defun/s network-cycle ([steps network] node)
  (let ((seen (st))
        (len (length steps))
        (nodes '()))
    (pfor ((i (:range 0 nil))
           (:let ((s (v. steps (mod i len)))))
           (prev (:step nil next))
           (next (:step (network-follow network node s)
                        (network-follow network next s)))
           (:do (push next nodes))
           (:return (or (node-suffix-p prev ?Z)
                        (s. seen (list next (mod i len))))
                    i)
           (:do (setf (s. seen (list next (mod i len))) t)))
      )))

(defun/s solve-08-2 ([steps network])
  (let ((starts (--filter (node-suffix-p it ?A) (-map #'car network))))
    (apply #'cl-lcm (--map (network-cycle (list steps network) it) starts))))
