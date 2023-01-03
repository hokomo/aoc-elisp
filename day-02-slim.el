(defun parse-round (line)
  (cl-destructuring-bind (x y) (-map #'char (s-split " " line))
    (list (- x ?A) (- y ?X))))

(defun read-02 (string)
  (-map #'parse-round (s-split "\n" string t)))

(defun rps-compare (x y)
  (1- (mod (1+ (- x y)) 3)))

(defun/s round-score ([x y])
  (+ y 1 (* 3 (1+ (rps-compare y x)))))

(defun solve-02-1 (rounds)
  (-sum (-map #'round-score rounds)))

(defun rps-weaker (x)
  (mod (1- x) 3))

(defun rps-stronger (x)
  (mod (1+ x) 3))

(defun/s round-shape ([x y])
  (funcall (aref [rps-weaker identity rps-stronger] y) x))

(defun solve-02-2 (rounds)
  (-sum (-map (lambda/s ([x y])
                (round-score (list x (round-shape (list x y)))))
              rounds)))
