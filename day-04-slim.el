(defun parse-range (range)
  (vmap #'int (s-split "-" range)))

(defun parse-pair (line)
  (-map #'parse-range (s-split "," line)))

(defun read-04 (string)
  (-map #'parse-pair (s-split "\n" string t)))

(defun/s complete-overlap-p ([x y])
  ;; NOTE: Assume the ranges are sorted.
  (with-vref (or (>= x.b y.b) (and (= y.a x.a) (>= y.b x.b)))))

(defun solve-04-1 (pairs)
  (->> pairs
       (--map (-sort #'range< it))
       (-count #'complete-overlap-p)))

(defun/s overlapp ([x y])
  ;; NOTE: Assume the ranges are sorted.
  (with-vref (<= y.a x.b)))

(defun solve-04-2 (pairs)
  (->> pairs
       (--map (-sort #'range< it))
       (-count #'overlapp)))
