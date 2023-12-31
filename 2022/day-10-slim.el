(defun parse-instruction (line)
  (--map (car (read-from-string it)) (s-split " " line)))

(defun read-10 (string)
  (-map #'parse-instruction (s-split "\n" string t)))

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
                    (let ((cycle (1+ cycle)))
                      (when (zerop (mod (- cycle 20) 40))
                        (cl-incf sum (* cycle x)))))
                  1 instructions)
    sum))

(defun solve-10-2 (instructions)
  (with-buffer nil
    (simulate-cpu (lambda (cycle x)
                    (princ (if (<= (1- x) (mod cycle 40) (1+ x)) "#" "."))
                    (when (zerop (mod (1+ cycle) 40))
                      (princ "\n")))
                  1 instructions)
    (current-buffer)))
