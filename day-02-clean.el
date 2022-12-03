;;; `rps' is short for `rock-paper-scissors'.

(defun parse-line (line)
  (cl-destructuring-bind (x y) (-map #'string-to-char (s-split " " line))
    (list (1+ (- x ?A)) (1+ (- y ?X)))))

(defun read-02 (string)
  (-map #'parse-line (s-split "\n" string t)))

(defun rps-compare (x y)
  (if (or (and (= (- y x) 1) (< x y))
          (and (= x 3) (= y 1)))
      -1
    (if (= x y) 0 1)))

(defun/s round-score ([x y])
  (+ y (* 3 (1+ (rps-compare y x)))))

(defun solve-02-1 (rounds)
  (sum (-map #'round-score rounds)))

(defun rps-weaker (x)
  (1+ (mod (- x 2) 3)))

(defun rps-stronger (x)
  (1+ (mod x 3)))

(defun/s round-shape ([x y])
  (funcall (aref [rps-weaker identity rps-stronger] (1- y)) x))

(defun solve-02-2 (rounds)
  (sum (-map (lambda/s ([x y])
               (round-score (list x (round-shape (list x y)))))
             rounds)))
