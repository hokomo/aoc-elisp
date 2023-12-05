(defvar *almanac-regexp*
  (rx "seeds: " (group (+ nonl)) (group (+ anychar))))

(defun parse-nums (line)
  (-map #'int (s-split " " line t)))

(defun parse-table (string)
  (-map #'parse-nums (cdr (s-split "\n" string t))))

(defun read-05 (string)
  (seq-let [_ seeds other] (s-match *almanac-regexp* string)
    (cons (parse-nums seeds) (-map #'parse-table (s-split "\n\n" other t)))))

(defun search-category-range (range category)
  (pcase-let ((res '())
              (`(,s ,slen) range))
    (pfor-do (((rdst rsrc rlen) category)
              (:until (zerop slen))
              (:do (when (< s rsrc)
                     (let ((len (min slen (- rsrc s))))
                       (push `(,s ,len) res)
                       (setf s (+ s len) slen (- slen len)))))
              (:until (zerop slen))
              (:do (when (<= rsrc s (1- (+ rsrc rlen)))
                     (let ((len (- (min (+ s slen) (+ rsrc rlen)) s)))
                       (push `(,(+ rdst (- s rsrc)) ,len) res)
                       (setf s (+ s len) slen (- slen len)))))
              (:finally (unless (zerop slen)
                          (push `(,s ,slen) res)))))
    (nreverse res)))

(defun search-categories-ranges (ranges categories)
  (cl-reduce (lambda (rs c) (--mapcat (search-category-range it c) rs))
             categories :initial-value ranges))

(defun sort-category (category)
  (-sort (-on #'< #'cl-second) category))

(defun minimal-location (ranges categories)
  (let ((categories (-map #'sort-category categories)))
    (-min (-map #'car (search-categories-ranges ranges categories)))))

(defun/s solve-05-1 ([seeds &rest categories])
  (minimal-location (--map `(,it 1) seeds)  categories))

(defun/s solve-05-2 ([seeds &rest categories])
  (minimal-location (-partition 2 seeds) categories))
