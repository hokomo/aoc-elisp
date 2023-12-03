(defun read-01 (string)
  (s-split "\n" string t))

(defvar *digit-regexp*
  (rx-to-string
   `(or ,@(--map `(group ,it)
                 '("one" "two" "three" "four" "five"
                   "six" "seven" "eight" "nine"))
        digit)))

(defun calibration-value (regexp line)
  (let ((digits (--map (let ((idx (-find-index #'identity (cdr it))))
                         (if idx (1+ idx) (int (car it))))
                       (s-match-strings-all regexp line))))
    (horner (list (-first-item digits) (-last-item digits)) 10)))

(defun solve-01-1 (document)
  (-sum (--map (calibration-value (rx digit) it) document)))

(defun solve-01-2 (document)
  (-sum (--map (calibration-value *digit-regexp* it) document)))
