(defun parse-card (line)
  (seq-let [_ body] (s-split ": " line t)
    (seq-let [winning owned] (s-split " | " body t)
      (list (vmap #'int (s-split " " winning t))
            (vmap #'int (s-split " " owned t))))))

(defun read-04 (string)
  (-map #'parse-card (s-split "\n" string t)))

(defun/s card-matches ([winning owned])
  (if-let ((winners (set-intersection (setify winning) (setify owned))))
      (set-size winners)
    0))

(defun card-value (card)
  (let ((matches (card-matches card)))
    (if (zerop matches) 0 (expt 2 (1- matches)))))

(defun solve-04-1 (cards)
  (-sum (-map #'card-value cards)))

(defun solve-04-2 (cards)
  (let ((counts (make-vector (length cards) 1)))
    (for-do ([(i (:range 0 nil))
              (c cards)]
             (j (:range (card-matches c))))
      (cl-incf (v. counts (+ i j 1)) (v. counts i)))
    (cl-reduce #'+ counts)))
