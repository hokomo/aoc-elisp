;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 's)

(defun parse-monkey-expr (line)
  (-map #'read-all-from-string (s-split ":" line)))

(defun read-21 (string)
  (let ((monkeys (ht)))
    (prog1 monkeys
      (cl-loop for line in (s-split "\n" string t)
               for ((name) (arg1 op arg2)) = (parse-monkey-expr line)
               do (setf (h. monkeys name) (if op (list op arg1 arg2) arg1))))))

(definput *test-21* #'read-21
  "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32")

(definput *input-21* #'read-21 "day-21-input.txt")

(defun monkey-solve (monkeys root &optional unknown)
  (let ((invs (ht ('+ '-) ('- '+) ('* '/) ('/ '*))))
    (cl-labels ((rec (name)
                  (if (and unknown (equal name unknown))
                      #'identity
                    (pcase-exhaustive (h. monkeys name)
                      ((and x (pred integerp))
                       x)
                      (`(,op ,arg1 ,arg2)
                       (let* ((args (list (rec arg1) (rec arg2)))
                              (leftp (functionp (car args))))
                         (if (cl-every #'integerp args)
                             (apply op args)
                           (cl-destructuring-bind (func val)
                               (if leftp args (reverse args))
                             (let ((inv (if (or (member op '(+ *)) leftp)
                                            (h. invs op)
                                          (-flip op))))
                               (lambda (res)
                                 (funcall func (funcall inv res val))))))))))))
      (rec root))))

(defun solve-21-1 (monkeys)
  (monkey-solve monkeys 'root))

(expect (solve-21-1 *test-21*) 152)
(expect (solve-21-1 *input-21*) 38914458159166)

(defun solve-21-2 (monkeys)
  (let ((args (-map (-cut monkey-solve monkeys <> 'humn)
                    (cdr (h. monkeys 'root)))))
    (cl-destructuring-bind (func res)
        (if (functionp (car args)) args (reverse args))
      (funcall func res))))

(expect (solve-21-2 *test-21*) 301)
(expect (solve-21-2 *input-21*) 3665520865940)

(comment
 (defun monkey-show (monkeys)
   (with-graphviz "day-21.dot"
     (prind "digraph {\n")
     (for-do (((name expr) (:ht monkeys))
              (:let ((neighbors (and (not (integerp expr)) expr)))))
       (if (not neighbors)
           (printf "  %s [label=\"%s (%s)\"];\n" name name (h. monkeys name))
         (pcase-let ((`(,op ,arg1 ,arg2) neighbors))
           (printf "  %s [label=\"%s (%s)\"];\n" name name op)
           (printf "  %s -> %s [label=left];\n" name arg1)
           (printf "  %s -> %s [label=right];\n" name arg2))))
     (prind "}\n")))

 (monkey-show *test-21*))
