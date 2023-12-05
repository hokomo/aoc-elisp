;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 's)

(defvar *almanac-regexp*
  (rx "seeds: " (group (+ nonl)) (group (+ anychar))))

(defun parse-nums (line)
  (-map #'int (s-split " " line t)))

(defun parse-table (string)
  (-map #'parse-nums (cdr (s-split "\n" string t))))

(defun read-05 (string)
  (seq-let [_ seeds other] (s-match *almanac-regexp* string)
    (cons (parse-nums seeds) (-map #'parse-table (s-split "\n\n" other t)))))

(definput *test-05* #'read-05
  "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(definput *input-05* #'read-05 "day-05-input.txt")

(defun search-category-range (range category)
  ;;; NOTE: Assume the category's ranges are sorted by the source (`src') value.
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

(expect (solve-05-1 *test-05*) 35)
(expect (solve-05-1 *input-05*) 289863851)

(defun/s solve-05-2 ([seeds &rest categories])
  (minimal-location (-partition 2 seeds) categories))

(expect (solve-05-2 *test-05*) 46)
(expect (solve-05-2 *input-05*) 60568880)
