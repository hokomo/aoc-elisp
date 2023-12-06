(defvar *stats-regexp*
  (rx "Time: " (group (+ nonl)) "
Distance: " (group (+ nonl))))

(defun parse-nums (line)
  (-map #'int (s-split " " line t)))

(defun read-06 (string)
  (seq-let [_ times distances] (s-match *stats-regexp* string)
    (list (parse-nums times) (parse-nums distances))))

(defun/s race-win-range ([time distance])
  (let ((d (sqrt (- (* time time) (* 4 distance)))))
    (list (floor (1+ (/ (- time d) 2)))
          (ceiling (1- (/ (+ time d) 2))))))

(defun race-margin (race)
  (seq-let [from to] (race-win-range race)
    (1+ (- to from))))

(defun solve-06-1 (stats)
  (-product (-map #'race-margin (apply #'-zip-lists stats))))

(defun combine-digits (nums)
  (horner (--mapcat (digits it 10) nums) 10))

(defun solve-06-2 (stats)
  (race-margin (-map #'combine-digits stats)))
