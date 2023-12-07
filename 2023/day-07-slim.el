(defun parse-card (card)
  (or (pfor-do ((i (:range 0 nil))
                (c "TJQKA")
                (:return (= card c) (+ i 10))))
      (- card ?0)))

(defun parse-player (line)
  (seq-let [hand bid] (s-split " " line t)
    (list (vmap #'parse-card hand) (int bid))))

(defun read-07 (string)
  (-map #'parse-poker-game (s-split "\n" string t)))

(defun sort-hand (hand)
  (let* ((freq (frequencies hand))
         (comp (lexicographical-compare (-on #'> (-cut h. freq <>)) #'>)))
    (cl-sort (copy-sequence hand) comp)))

(defun hand-type (hand)
  (pcase-exhaustive (sort-hand hand)
    (`[,x ,x ,x ,x ,x] -1)
    (`[,x ,x ,x ,x ,_] -2)
    (`[,x ,x ,x ,y ,y] -3)
    (`[,x ,x ,x ,_ ,_] -4)
    (`[,x ,x ,y ,y ,_] -5)
    (`[,x ,x ,_ ,_ ,_] -6)
    (`[,x ,_ ,_ ,_ ,_] -7)))

(defalias 'hand<
  (lexicographical-compare (-on #'< #'hand-type) (vector-compare #'<)))

(defun poker-winnings (handfn game)
  (->> (-sort (-on handfn #'car) game)
       (--map-indexed (* (1+ it-index) (cl-second it)))
       -sum))

(defun solve-07-1 (game)
  (poker-winnings #'hand< game))

(defun hand-type2 (hand)
  (let* ((freq (frequencies hand))
         (jokers (or (h. freq 1) 0)))
    (cond
     ((= jokers 0)
      (pcase-exhaustive (sort-hand hand)
        (`[,x ,x ,x ,x ,x] -1)
        (`[,x ,x ,x ,x ,a] -2)
        (`[,x ,x ,x ,y ,y] -3)
        (`[,x ,x ,x ,_ ,_] -4)
        (`[,x ,x ,y ,y ,_] -5)
        (`[,x ,x ,_ ,_ ,_] -6)
        (`[,x ,_ ,_ ,_ ,_] -7)))
     ((= jokers 1)
      (pcase-exhaustive (sort-hand hand)
        (`[,x ,x ,x ,x ,_] -1)
        (`[,x ,x ,x ,_ ,_] -2)
        (`[,x ,x ,y ,y ,_] -3)
        (`[,x ,x ,_ ,_ ,_] -4)
        (`[,x ,_ ,_ ,_ ,_] -6)))
     ((= jokers 2)
      (pcase-exhaustive (sort-hand hand)
        (`[,x ,x ,x ,y ,y] -1)
        (`[,x ,x ,y ,y ,z] -2)
        (`[,x ,x ,_ ,_ ,_] -4)
        (`[,x ,_ ,_ ,_ ,_] -6)))
     ((= jokers 3)
      (pcase-exhaustive (sort-hand hand)
        (`[,j ,j ,j ,x ,x] -1)
        (`[,j ,j ,j ,x ,_] -2)))
     ((= jokers 4) -1)
     ((= jokers 5) -1))))

(defalias 'hand2<
  (lexicographical-compare (-on #'< #'hand-type2) (vector-compare #'<)))

(defun replace-jack (hand)
  (cl-substitute 1 11 hand))

(defun solve-07-2 (game)
  (->> (-map (-lambda ((hand bid)) (list (replace-jack hand) bid)) game)
       (poker-winnings #'hand2<)))
