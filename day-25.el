;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (when (require 'aoc-emacs nil t) (aoc-mode 1)); -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 's)

;;; The numeral system used is called balanced quinary. Rather than perform
;;; addition directly in balanced quinary, we convert to and from decimal, just
;;; for fun.
;;;
;;; For an n-balanced quinary system, conversion from decimal is done by first
;;; converting to a standard base n system, and then "carrying forward" the
;;; digits.
;;;
;;; See https://www.ias.ac.in/article/fulltext/reso/023/12/1395-1410.

(defun read-25 (string)
  (s-split "\n" string t))

(definput *test-25* #'read-25
  "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122")

(definput *input-25* #'read-25 "input-25.txt")

(defun parse-snafu (string)
  (horner (for ((c string))
            (pcase-exhaustive c
              (?- -1)
              (?= -2)
              (c (- c ?0))))
          5))

(defun balanced-carry-forward (digits base)
  (cl-assert (oddp base))
  (let* ((carry 0)
         (balanced (for ((:let ((half (/ base 2))))
                         (digit (reverse digits)))
                     (let ((digit (+ digit carry)))
                       (prog1 (if (< digit half)
                                  digit
                                (wrap (- half) half digit))
                         (setf carry (int (> digit half))))))))
    (nreverse (if (zerop carry) balanced (cons carry balanced)))))

(defun balanced-digits (n base)
  (balanced-carry-forward (digits n base) base))

(defun format-snafu (n)
  (cl-map 'string
          (lambda (d)
            (pcase-exhaustive d
              (-2 ?=)
              (-1 ?-)
              (d (+ d ?0))))
          (balanced-digits n 5)))

(defun solve-25-1 (numbers)
  (format-snafu (-sum (-map #'parse-snafu numbers))))

(expect (solve-25-1 *test-25*) "2=-1=0")
(expect (solve-25-1 *input-25*) "2-21=02=1-121-2-11-0")
