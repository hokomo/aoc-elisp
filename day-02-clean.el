;;; `rps' is short for `rock-paper-scissors'.
;;;
;;; We map each of RPS, XYZ and ABC to 012.

(defun parse-round (line)
  (cl-destructuring-bind (x y) (-map #'char (s-split " " line))
    (list (- x ?A) (- y ?X))))

(defun read-02 (string)
  (-map #'parse-round (s-split "\n" string t)))

(defun rps-compare (x y)
  ;; NOTE: (- x y) is the signed distance between x and y. A positive/negative
  ;; distance corresponds to a win/lose (for player 1), except at the edges
  ;; where the distance of 2/-2 is a lose/win instead. Modding by 3 takes care
  ;; of this edge case and maps LDW (lose-draw-win) to 201. Adding 1 before
  ;; modding maps it to 012, and subtracting 1 at the end maps it to -101.
  (1- (mod (1+ (- x y)) 3)))

(defun/s round-score ([x y])
  (+ y 1 (* 3 (1+ (rps-compare y x)))))

(defun solve-02-1 (rounds)
  (sum (-map #'round-score rounds)))

(defun rps-weaker (x)
  (mod (1- x) 3))

(defun rps-stronger (x)
  (mod (1+ x) 3))

(defun/s round-shape ([x y])
  (funcall (aref [rps-weaker identity rps-stronger] y) x))

(defun solve-02-2 (rounds)
  (sum (-map (lambda/s ([x y])
               (round-score (list x (round-shape (list x y)))))
             rounds)))
