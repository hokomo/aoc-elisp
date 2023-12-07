(defun parse-card (card)
  (or (pfor-do ((i (:range 0 nil))
                (c "TJQKA")
                (:return (= card c) (+ i 10))))
      (- card ?0)))

(defun parse-player (line)
  (seq-let [hand bid] (s-split " " line t)
    (list (-map #'parse-card hand) (int bid))))

(defun read-07 (string)
  (-map #'parse-player (s-split "\n" string t)))

(defun hand-type (hand &optional joker)
  (let* ((freq (-sort (-on #'> #'cdr) (-frequencies hand)))
         (jokers (or (and joker (cdr (assoc joker freq))) 0))
         (freq (if joker (cl-remove joker freq :key #'car) freq)))
    (cl-ecase (+ (or (cdr (cl-first freq)) 0) jokers)
      (5 -1)
      (4 -2)
      (3 (if (= (cdr (cl-second freq)) 2) -3 -4))
      (2 (if (= (cdr (cl-second freq)) 2) -5 -6))
      (1 -7))))

(defun poker-winnings (typefn players)
  (->> (--map (cons (funcall typefn (cl-first it)) it) players)
       (-sort (lexicographical-compare
               (-on #'< #'cl-first) (-on (list-compare #'<) #'cl-second)))
       (--map-indexed (* (1+ it-index) (cl-third it)))
       -sum))

(defun solve-07-1 (players)
  (poker-winnings #'hand-type players))

(defun/s replace-jack ([hand bid])
  (list (cl-substitute 1 11 hand) bid))

(defun solve-07-2 (players)
  (poker-winnings (-cut hand-type <> 1) (-map #'replace-jack players)))
