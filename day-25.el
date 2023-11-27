;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (when (require 'aoc-emacs nil t) (aoc-mode 1)); -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 's)

;;; The numeral system used is called balanced quinary. Rather than perform
;;; addition directly in balanced quinary, we convert to and from decimal, just
;;; for fun.
;;;
;;; One way of converting from decimal to an n-balanced numeral system is by
;;; first converting to a standard base n system, and then "carrying forward"
;;; the digits.
;;;
;;; See https://www.ias.ac.in/article/fulltext/reso/023/12/1395-1410.
;;;
;;; Another way is by performing the standard division algorithm but with
;;; adjusted definitions of quotient and modulo. Instead of using the
;;; conventional "common modulus", we use the "minimal modulus", which directly
;;; gives us the digits. The minimal modulus can be found as a "modulo with
;;; offset" with offset -n/2 (truncated).
;;;
;;; See:
;;;
;;; - https://oeis.org/wiki/Balanced_quinary_numeral_system
;;; - https://mathworld.wolfram.com/MinimalResidue.html
;;; - https://reference.wolfram.com/language/ref/Mod.html
;;; - https://en.wikipedia.org/wiki/Modulo#Modulo_with_offset

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

(defun divmod-d (a n d)
  (let ((q (floor (- a d) n)))
    (list q (- a (* n q)))))

(defun balanced-digits (n base)
  (cl-loop for (q r) = (divmod-d n base (- (/ base 2)))
           collect r into ds
           do (setf n q)
           until (zerop n)
           finally (cl-return (nreverse ds))))

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
