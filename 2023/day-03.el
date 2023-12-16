;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

;; TODO: Clean up!

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 's)

(defun read-03 (string)
  (vecify (s-split "\n" string t)))

(definput *test-03* #'read-03
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(definput *input-03* #'read-03 "day-03-input.txt")

(defun v2overlap-p (u1 u2 v1 v2)
  (with-vref
    (not (or (< u2.x v1.x) (< u2.y v1.y) (> u1.x v2.x) (> u1.y v2.y)))))

(defvar *entity-regexp*
  (rx (or (group (+ digit)) (group (not (any digit "."))))))

(defun parse-schematic (schematic)
  (for-do ((:let ((entities (ht))
                  (prev-row nil)))
           [(r (:range 0 nil))
            (line schematic)
            (:finally (cl-return entities))]
           (:let ((prev nil)
                  (new-prev-row nil)))
           [(c (:step (string-match *entity-regexp* line)
                      (string-match *entity-regexp* line (+ c len))
                      c))
            ;; TODO: Non-local effect: let vs. let*!
            (:let* ((len (- (match-end 0) (match-beginning 0)))
                    (v `[,r ,c])
                    (num (match-string 1 line))
                    (sym (match-string 2 line))))
            (:finally (setf prev-row new-prev-row
                            new-prev-row nil))])
    (setf (h. entities v) (list (if num (int num) sym) len nil))
    (cl-labels ((neighbor-p (u)
                  (let ((a1 (v2- v `[1 1]))
                        (a2 (v2+ v `[1 ,len]))
                        (b1 (v2- u `[1 1]))
                        (b2 (v2+ u `[1 ,(cl-second (h. entities u))])))
                    (or (v2overlap-p a1 a2 u (v2+ u `[0 ,(1- (cl-second (h. entities u)))]))
                        (v2overlap-p b1 b2 v v))))
                (add-neighbor (u)
                  (push u (cl-third (h. entities v)))
                  (push v (cl-third (h. entities u)))))
      (when (and prev (neighbor-p prev))
        (add-neighbor prev))
      (for-do ((u prev-row)
               (:when (neighbor-p u)))
        (add-neighbor u)))
    (setf prev v)
    (push v new-prev-row)))

(defun solve-03-1 (schematic)
  (let ((entities (parse-schematic schematic)))
    (->> entities
         (ht-map (lambda (k v)
                   (and (integerp (car v))
                        (--any (not (integerp (car (h. entities it))))
                               (cl-third v))
                        (car v))))
         (-remove #'null)
         -sum)))

(expect (solve-03-1 *test-03*) 4361)
(expect (solve-03-1 *input-03*) 538046)

(defun solve-03-2 (schematic)
  (let ((entities (parse-schematic schematic)))
    (->> entities
         (ht-map (lambda (k v)
                   (and (stringp (car v))
                        (let ((list (--filter (integerp it)
                                              (--map (car (h. entities it))
                                                     (cl-third v)))))
                          (and (= (length list) 2) (-product list))))))
         (-remove #'null)
         -sum)))

(expect (solve-03-2 *test-03*) 467835)
(expect (solve-03-2 *input-03*) 81709807)
