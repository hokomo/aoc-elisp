(defvar *game-regexp*
  (rx "Game " (group (+ digit)) ": " (group (+ nonl))))

(defvar *cube-colors*
  '("red" "green" "blue"))

(defvar *cube-regexp*
  (rx (group (+ digit)) " " (group (eval `(| ,@*cube-colors*)))))

(defun parse-round (string)
  (let ((counts (ht)))
    (pfor-do ((cube (s-split ", " string t))
              (:let (((n color) (cdr (s-match *cube-regexp* cube))))))
      (setf (h. counts color) (int n)))
    `[,@(--map (or (h. counts it) 0) *cube-colors*)]))

(defun parse-game (line)
  (seq-let [id body] (cdr (s-match *game-regexp* line))
    (let ((rounds (-map #'parse-round (s-split "; " body t))))
      (cons (int id) rounds))))

(defun read-02 (string)
  (-map #'parse-game (s-split "\n" string t)))

(defun minimal-cubes (game)
  (apply #'vmap #'max (cdr game)))

(defun solve-02-1 (games)
  (-sum (--map (if (v<= (minimal-cubes it) [12 13 14]) (car it) 0) games)))

(defun solve-02-2 (games)
  (-sum (--map (cl-reduce #'* (minimal-cubes it)) games)))
