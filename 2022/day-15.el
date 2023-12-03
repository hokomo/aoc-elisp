;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (when (require 'aoc-emacs nil t) (aoc-mode 1)); -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 's)

;;; We use the point [x y] to represent the location in the y-th row and x-th
;;; column, starting from the origin [0 0] in the top-left.
;;;
;;; Let's call the area covered by each sensor a "diamond" (a circle in taxicab
;;; geometry). For part 2, the unique point has to be squished between 4
;;; diamonds. In fact, it will lie on the intersection of 2 lines, each one
;;; tracing the immediate exterior of 2 out of 4 diamonds. Therefore, all we
;;; have to do is compute the exterior lines of all diamonds, find those that
;;; are shared between at least 2, and inspect their intersections.

(defun parse-sensor (line)
  (seq-let [sx sy bx by]
      (-map #'int (cdr (s-match (rx-let ((n (group (? "-") (+ digit))))
                                  (rx "Sensor at x=" n ", y=" n
                                      ": closest beacon is at x=" n ", y=" n))
                                line)))
    `([,sx ,sy] [,bx ,by])))

(defun read-15 (string)
  (-map #'parse-sensor (s-split "\n" string t)))

(definput *test-15* #'read-15
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(definput *input-15* #'read-15 "input-15.txt")

(defun/s sensor-range ([sensor beacon] y)
  ;; NOTE:
  ;;
  ;; d = taxi-distance(s, b)
  ;;
  ;; taxi-distance(s, [x y]) <= taxi-distance(s, b)
  ;; abs(sx - x) + abs(sy - y) <= d
  ;; abs(sx - x) <= d - abs(sy - y)
  ;; -(d - abs(sy - y)) <= x - sx <= d - abs(sy - y)
  ;; -(d - abs(sy - y)) + sx <= x <= d - abs(sy - y) + sx
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

(expect (solve-15-1 '(([8 7] [2 10])) 10) 12)
(expect (solve-15-1 *test-15* 10) 26)
(expect (solve-15-1 *input-15* 2000000) 5716881)

(defun/s exterior-lines ([sensor beacon])
  ;; NOTE: Each pair [a b] represents the line y = a x + b.
  (let ((d (taxi-distance sensor beacon)))
    (with-vref
      ;; Return the lines (top-left top-right bottom-right bottom-left).
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

(expect (solve-15-2 *test-15* 20) 56000011)
(expect (solve-15-2 *input-15* 4000000) 10852583132904)
