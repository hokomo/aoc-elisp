(defun parse-range (range)
  (-map #'int (s-split "-" range)))

(defun parse-pair (line)
  (-map #'parse-range (s-split "," line)))

(defun read-04 (string)
  (-map #'parse-pair (s-split "\n" string t)))

(defun range< (x y)
  (funcall (cl-load-time-value
            (lexicographical-compare
             (compare-by #'< #'cl-first)
             (compare-by #'< #'cl-second)))
           x y))

(defun/s complete-overlap-p ([[a b] [c d]])
  ;; NOTE: Assume the pairs are sorted wrt each other.
  (or (>= b d) (and (= c a) (>= d b))))

(defun solve-04-1 (pairs)
  (->> pairs
       (--map (-sort #'range< it))
       (-count #'complete-overlap-p)))

(defun/s overlap-p ([[a b] [c d]])
  ;; NOTE: Assume the pairs are sorted wrt each other.
  (<= c b))

(defun solve-04-2 (pairs)
  (->> pairs
       (--map (-sort #'range< it))
       (-count #'overlap-p)))
