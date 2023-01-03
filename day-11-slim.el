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

(defun solve-11-2 (monkeys)
  (let ((modulo (-product (for ((m monkeys)) (h. m :div)))))
    (monkey-business monkeys (-cut mod <> modulo) 10000)))
