;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (when (require 'aoc-emacs nil t) (aoc-mode 1)); -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 's)

;;; For part 2 we reduce each item's worry level modulo the least common
;;; multiple (LCM) of the monkeys' divisors, which is enough to keep the numbers
;;; from growing too large, yet still retains enough information to perform the
;;; divisbility tests.
;;;
;;; Recall the definition of modulo:
;;;
;;; x mod n = x - k n
;;;
;;; Let m_i be the divisor for the i-th monkey, and let M = lcm(m_1, ..., m_n).
;;; By definition of LCM, M is divisible by any of m_i. Now, for any worry level
;;; x, its residue modulo m_i is preserved after reduction modulo M:
;;;
;;; (x mod M) mod m_i =
;;; (x - k_1 M) mod m_i =
;;; (x - k_1 k_2 m_i) mod m_i =
;;; x mod m_i

(defvar *monkey-regexp*
  (rx "Monkey " (group (+ digit)) ":
  Starting items: " (group (* (+ digit) (? ", "))) "
  Operation: new = old " (group (or "+" "*")) " " (group (or "old" (+ digit))) "
  Test: divisible by " (group (+ digit)) "
    If true: throw to monkey " (group (+ digit)) "
    If false: throw to monkey " (group (+ digit))))

(defun parse-monkey (string)
  (seq-let [_ n items op arg div true false] (s-match *monkey-regexp* string)
    (ht (:n (int n))
        (:items (-map #'int (s-split ", " items)))
        (:op (intern op))
        (:arg (car (read-from-string arg)))
        (:div (int div))
        (:true (int true))
        (:false (int false)))))

(defun read-11 (string)
  (vmap #'parse-monkey (s-split "\n\n" string)))

(definput *test-11* #'read-11
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(definput *input-11* #'read-11 "day-11-input.txt")

(defun copy-monkeys (monkeys)
  (vmap #'ht-copy monkeys))

(defun monkey-turn (monkeys n reducef)
  (pcase-let* ((cur (v. monkeys n))
               ((map :items :count :op :arg :div :true :false) cur))
    (setf (h. cur :items) '()
          (h. cur :count) (+ (or count 0) (length items)))
    (cl-loop for old in items
             for new = (->> (funcall op old (if (eq arg 'old) old arg))
                            (funcall reducef))
             for next = (v. monkeys (if (zerop (mod new div)) true false))
             do (setf (h. next :items) (append (h. next :items) (list new))))))

(defun simulate-monkeys (monkeys reducef rounds)
  (prog1 monkeys
    (dotimes (n rounds)
      (dotimes (i (length monkeys))
        (monkey-turn monkeys i reducef)))))

(defun monkey-business (monkeys reducef rounds)
  (->> (simulate-monkeys (copy-monkeys monkeys) reducef rounds)
       (-map (-cut h. <> :count))
       (top-n 2)
       -product))

(defun solve-11-1 (monkeys)
  (monkey-business monkeys (-cut / <> 3) 20))

(expect (solve-11-1 *test-11*) 10605)
(expect (solve-11-1 *input-11*) 55944)

(defun solve-11-2 (monkeys)
  (let ((modulo (cl-lcm (for ((m monkeys)) (h. m :div)))))
    (monkey-business monkeys (-cut mod <> modulo) 10000)))

(expect (solve-11-2 *test-11*) 2713310158)
(expect (solve-11-2 *input-11*) 15117269860)
