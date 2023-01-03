;;; We use the point [i j] to represent the location in the i-th row and j-th
;;; column, starting from the origin [0 0] in the top-left.

(defun parse-rope-move (line)
  (--map (car (read-from-string it)) (s-split " " line)))

(defun read-09 (string)
  (-map #'parse-rope-move (s-split "\n" string t)))

(defvar *rope-dirs*
  (ht ('U [-1 0]) ('D [1 0]) ('L [0 -1]) ('R [0 1])))

(defun make-rope (knots)
  (vecify (for ((i (:range knots))) (vector 0 0))))

(defun adjust-spine (rope)
  (cl-loop for k from 1 below (length rope)
           while (> (chess-distance (v. rope (1- k)) (v. rope k)) 1)
           for dir = (v2- (v. rope (1- k)) (v. rope k))
           for unit = (v2clamp [-1 -1] [1 1] dir)
           do (setf (v. rope k) (v2+ (v. rope k) unit))))

(defun simulate-rope (func rope moves)
  (prog1 rope
    (cl-loop for (sym n) in moves do
      (dotimes (_ n)
        (setf (v. rope 0) (v2+ (v. rope 0) (h. *rope-dirs* sym)))
        (adjust-spine rope)
        (funcall func rope)))))

(defun unique-tails (knots moves)
  (let* ((rope (make-rope knots))
         (seen (st (v. rope -1))))
    (simulate-rope (lambda (rope)
                     (set-add seen (v. rope -1)))
                   rope moves)
    (set-size seen)))

(defun solve-09-1 (moves)
  (unique-tails 2 moves))

(defun solve-09-2 (moves)
  (unique-tails 10 moves))
