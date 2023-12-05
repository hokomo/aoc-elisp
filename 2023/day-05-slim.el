(defvar *almanac-regexp*
  (rx "seeds: " (group (+ nonl)) (group (+ anychar))))

(defun parse-nums (line)
  (vmap #'int (s-split " " line t)))

(defun parse-table (string)
  (-map #'parse-nums (cdr (s-split "\n" string t))))

(defun read-05 (string)
  (seq-let [_ seeds other] (s-match *almanac-regexp* string)
    (cons (parse-nums seeds) (-map #'parse-table (s-split "\n\n" other t)))))

(defun search-category (value category)
  (or (-some (lambda/s ([dst src len])
               (and (<= src value (1- (+ src len))) (+ (- value src) dst)))
              category)
      value))

(defun search-categories (value categories)
  (cl-reduce #'search-category categories :initial-value value))

(defun/s solve-05-1 ([seeds &rest categories])
  (-min (--map (search-categories it categories) (listify seeds))))

(defun search-category-range (range category)
  (pcase-let ((res '())
              (`[,s ,slen] range))
    (cl-block nil
      (pcase-dolist (`[,rdst ,rsrc ,rlen] category)
        (when (zerop slen)
          (cl-return))
        (when (< s rsrc)
          (let ((len (min slen (- rsrc s))))
            (push `[,s ,len] res)
            (setf s (+ s len) slen (- slen len))))
        (when (zerop slen)
          (cl-return))
        (when (<= rsrc s (1- (+ rsrc rlen)))
          (let ((len (- (min (+ s slen) (+ rsrc rlen)) s)))
            (push `[,(+ rdst (- s rsrc)) ,len] res)
            (setf s (+ s len) slen (- slen len))))))
    (unless (zerop slen)
      (push `[,s ,slen] res))
    (nreverse res)))

(defun search-categories-ranges (ranges categories)
  (cl-reduce (lambda (rs c) (--mapcat (search-category-range it c) rs))
             categories :initial-value ranges))

(defun sort-category (category)
  (-sort (-on #'< (-cut v. <> 1)) category))

(defun/s solve-05-2 ([seeds &rest categories])
  (let ((categories (-map #'sort-category categories)))
    (-min (-map #'seq-first (search-categories-ranges
                             (-map #'vecify (-partition 2 (listify seeds)))
                             categories)))))
