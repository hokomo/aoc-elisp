(defun priority (char)
  (cond
   ((<= ?a char ?z)
    (1+ (- char ?a)))
   ((<= ?A char ?Z)
    (1+ (+ (- char ?A) 26)))))

(defun parse-rucksack (line)
  (seq-partition (-map #'priority line) (/ (length line) 2)))

(defun read-03 (string)
  (-map #'parse-rucksack (s-split "\n" string t)))

(defun/s find-shared ([x y])
  (car (seq-intersection x y)))

(defun solve-03-1 (rucksacks)
  (-sum (-map #'find-shared rucksacks)))

(defun find-badge (group)
  (car (cl-reduce #'seq-intersection (-map #'join group))))

(defun solve-03-2 (rucksacks)
  (-sum (-map #'find-badge (seq-partition rucksacks 3))))
