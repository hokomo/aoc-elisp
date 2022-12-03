(defun parse-block (block)
  (mapcar #'string-to-number (s-split "\n" block t)))

(defun read-01 (string)
  (mapcar #'parse-block (s-split "\n\n" string t)))

(defun calorie-sum (n calories)
  (sum (top-n n (mapcar #'sum calories))))

(defun solve-01-1 (calories)
  (calorie-sum 1 calories))

(defun solve-01-2 (calories)
  (calorie-sum 3 calories))
