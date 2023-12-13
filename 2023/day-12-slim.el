(defun parse-record (line)
  (seq-let [springs damaged] (s-split " " line t)
    (list springs (-map #'int (s-split "," damaged t)))))

(defun read-12 (string)
  (-map #'parse-record (s-split "\n" string t)))

(defun range-rle (record range)
  (-let (((beg . end) range)
         (prev nil)
         (count 0)
         (rle '()))
    (cl-loop for i from beg below end
             for c = (v. record i)
             do (if (or (not prev) (= c prev))
                    (cl-incf count)
                  (push (cons prev count) rle)
                  (setf count 1))
                (setf prev c))
    (unless (zerop count)
      (push (cons prev count) rle))
    (nreverse rle)))

(defun record-ranges (record)
  (->> (s-matched-positions-all (rx (+ (any "?#"))) record)
       (--map (range-rle record it))))

(defun range-free-p (range)
  (and (= (length range) 1) (= (caar range) ??)))

(defun count-arrangements (record lengths)
  (named-let rec ((ranges (record-ranges record))
                  (lengths lengths))
    (if (not lengths)
        (and (--all-p (range-free-p it) ranges) 1)
      (-let* (((range . ranges) ranges)
              ((length . lengths) lengths)
              (((char . count) . spans) range))
        (cond
         ((and (not spans) (= char ?#))
          (and (= count length) (rec ranges lengths)))
         ((and (not spans) (= char ??))
          (if (> length count)
              (rec ranges (cons length lengths))
            (+ ;; There are count - length possible placements where the group
               (cl-loop for i from 0 below (- count length)
                        sum (rec `(((?? . ,(- count length i 1))) ,@ranges)
                                 lengths))
               (rec ranges lengths))))
         (t
          ))))))

(defun solve-12-1 (records)
  )

(defun solve-12-2 (records)
  )
