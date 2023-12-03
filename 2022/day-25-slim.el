(defun read-25 (string)
  (s-split "\n" string t))

(defun parse-snafu (string)
  (horner (for ((c string))
            (pcase-exhaustive c
              (?- -1)
              (?= -2)
              (c (- c ?0))))
          5))

(defun divmod-d (a n d)
  (let ((q (floor (- a d) n)))
    (list q (- a (* n q)))))

(defun balanced-digits (n base)
  (cl-loop for (q r) = (divmod-d n base (- (/ base 2)))
           collect r into ds
           do (setf n q)
           until (zerop n)
           finally (cl-return (nreverse ds))))

(defun format-snafu (n)
  (cl-map 'string
          (lambda (d)
            (pcase-exhaustive d
              (-2 ?=)
              (-1 ?-)
              (d (+ d ?0))))
          (balanced-digits n 5)))

(defun solve-25-1 (numbers)
  (format-snafu (-sum (-map #'parse-snafu numbers))))
