;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (when (require 'aoc-emacs nil t) (aoc-mode 1)); -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 's)

(defun parse-instruction (line)
  (--map (car (read-from-string it)) (s-split " " line)))

(defun read-10 (string)
  (-map #'parse-instruction (s-split "\n" string t)))

(definput *test-10-1* #'read-10
  "noop
addx 3
addx -5")

(definput *test-10-2* #'read-10 "day-10-test-2.txt")

(definput *input-10* #'read-10 "day-10-input.txt")

(defun simulate-cpu (func x instructions)
  (let ((cycle 0))
    (dolist (instruction instructions)
      (pcase instruction
        (`(noop)
         (funcall func cycle x)
         (cl-incf cycle))
        (`(addx ,n)
         (funcall func cycle x)
         (cl-incf cycle)
         (funcall func cycle x)
         (cl-incf cycle)
         (cl-incf x n))))))

(defun solve-10-1 (instructions)
  (let ((sum 0))
    (simulate-cpu (lambda (cycle x)
                    ;; NOTE: "During" the N-th cycle really means "*before*" the
                    ;; N-th cycle.
                    (let ((cycle (1+ cycle)))
                      (when (zerop (mod (- cycle 20) 40))
                        (cl-incf sum (* cycle x)))))
                  1 instructions)
    sum))

(expect (solve-10-1 *test-10-2*) 13140)
(expect (solve-10-1 *input-10*) 16060)

(defun solve-10-2 (instructions)
  (with-buffer _
    (simulate-cpu (lambda (cycle x)
                    ;; NOTE: The pixel indices are 0-based.
                    (princ (if (<= (1- x) (mod cycle 40) (1+ x)) "#" "."))
                    (when (zerop (mod (1+ cycle) 40))
                      (princ "\n")))
                  1 instructions)
    (current-buffer)))

(display (solve-10-2 *test-10-2*))
(display (solve-10-2 *input-10*))
