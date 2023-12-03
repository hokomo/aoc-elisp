(defun parse-sensor (line)
  (seq-let [sx sy bx by]
      (-map #'int (cdr (s-match (rx-let ((n (group (? "-") (+ digit))))
                                  (rx "Sensor at x=" n ", y=" n
                                      ": closest beacon is at x=" n ", y=" n))
                                line)))
    `([,sx ,sy] [,bx ,by])))

(defun read-15 (string)
  (-map #'parse-sensor (s-split "\n" string t)))

(defun/s sensor-range ([sensor beacon] y)
  (with-vref
    (let* ((d (taxi-distance sensor beacon))
           (diff (- d (abs (- sensor.y y))))
           (from (- sensor.x diff))
           (to (+ sensor.x diff)))
      (and (plusp (1+ (- to from))) `[,from ,to]))))

(defun merge-intervals (intervals)
  (let ((intervals (sort intervals #'range<))
        (merged '()))
    (for-do ((i intervals)
             (:let ((last (car merged)))))
      (with-vref
        (if (or (not merged) (< last.b (1- i.a)))
            (push i merged)
          (setf last.b (max last.b i.b)))))
    (nreverse merged)))

(defun count-occupied (pairs intervals y)
  (let ((count 0))
    (for-do ((p (:st (setify (-flatten pairs))))
             (i intervals))
      (with-vref
        (when (and (= p.y y) (<= i.a p.x i.b))
          (cl-incf count))))
    count))

(defun solve-15-1 (pairs y)
  (let ((intervals (->> (-map (-cut sensor-range <> y) pairs)
                        (remove nil)
                        merge-intervals)))
    (- (-sum (-map (lambda (i) (with-vref (1+ (- i.b i.a)))) intervals))
       (count-occupied pairs intervals y))))

(defun/s exterior-lines ([sensor beacon])
  (let ((d (taxi-distance sensor beacon)))
    (with-vref
      (list `[1 ,(- (- sensor.y d 1) sensor.x)]
            `[-1 ,(+ (- sensor.y d 1) sensor.x)]
            `[1 ,(- (+ sensor.y d 1) sensor.x)]
            `[-1 ,(+ (+ sensor.y d 1) sensor.x)]))))

(defun line-intersection (l1 l2)
  (with-vref
    (let ((x (/ (- l2.b l1.b) (- l1.a l2.a))))
      `[,x ,(+ (* l1.a x) l1.b)])))

(defun/s sensor-covers-p ([sensor beacon] p)
  (<= (taxi-distance sensor p) (taxi-distance sensor beacon)))

(defun tuning-freq (p)
  (with-vref (+ (* p.x 4000000) p.y)))

(defun solve-15-2 (pairs lim)
  (let ((lines (ht))
        (pos '())
        (neg '())
        (vlim `[,lim ,lim]))
    (for-do ((l (-mapcat #'exterior-lines pairs)))
      (setf (h. lines l) (1+ (or (h. lines l) 0))))
    (for-do (((l count) (:ht lines)))
      (when (> count 1)
        (if (with-vref (plusp l.a))
            (push l pos)
          (push l neg))))
    (for-do ((l1 pos)
             (l2 neg)
             (:let ((p (line-intersection l1 l2)))))
      (when (and (v2<= [0 0] p vlim)
                 (not (cl-some (-cut sensor-covers-p <> p) pairs)))
        (cl-return (tuning-freq p))))))
