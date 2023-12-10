(defun parse-nums (line)
  (-map #'int (s-split " " line t)))

(defun read-09 (string)
  (-map #'parse-nums (s-split "\n" string t)))

(defun adjacent-difference (nums)
  (--map (apply (-flip #'-) it) (-partition-in-steps 2 1 nums)))

(defun extrapolate-value (op history)
  (named-let rec ((history history))
    (if (-all-p #'zerop history)
        0
      (funcall op history (rec (adjacent-difference history))))))

(defun extrapolate-last (history)
  (extrapolate-value (lambda (history diff) (+ (-last-item history) diff))
                     history))

(defun solve-09-1 (histories)
  (-sum (-map #'extrapolate-last histories)))

(defun extrapolate-first (history)
  (extrapolate-value (lambda (history diff) (- (-first-item history) diff))
                     history))

(defun solve-09-2 (histories)
  (-sum (-map #'extrapolate-first histories)))
