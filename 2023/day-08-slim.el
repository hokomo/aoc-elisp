(defvar *connection-regexp*
  (rx-let ((node (group (+ alnum))))
    (rx node " = (" node ", " node ")")))

(defun parse-connection (line)
  (-map #'intern (cdr (s-match *connection-regexp* line))))

(defun read-08 (string)
  (seq-let [steps network] (s-split "\n\n" string t)
    (list steps (-map #'parse-connection (s-split "\n" network t)))))

(defun network-follow (network node step)
  (let ((neighbors (cdr (assoc node network))))
    (if (= step ?L) (cl-first neighbors) (cl-second neighbors))))

(defun/s network-end-distance ([steps network] testfn start)
  (let ((len (length steps)))
    (pfor-do ((i (:range 0 nil))
              (:let ((s (v. steps (mod i len)))))
              (cur (:step (network-follow network start s)
                          (network-follow network cur s))))
      (when (funcall testfn cur)
        (cl-return (1+ i))))))

(defun solve-08-1 (document)
  (network-end-distance document (-cut eq <> 'ZZZ) 'AAA))

(defun node-suffix-p (node char)
  (= (v. (cat node) -1) char))

(defun network-starts (network)
  (--filter (node-suffix-p it ?A) (-map #'car network)))

(defun solve-08-2 (document)
  (let ((testfn (-cut node-suffix-p <> ?Z)))
    (->> (network-starts (cl-second document))
         (--map (network-end-distance document testfn it))
         (apply #'cl-lcm))))
