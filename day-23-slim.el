;;; We use the point [i j] to represent the location in the i-th row and j-th
;;; column, starting from the origin [0 0] in the top-left.

(defun read-23 (string)
  (vecify (s-split "\n" string t)))

(defun elves-create (grid)
  (let ((elves (ht)))
    (prog1 elves
      (with-tensor (i j) grid
        (when (= (v. grid i j) ?#)
          (set-add elves `[,i ,j]))))))

(defvar *sides*
  '(([-1 0] . ([-1 -1] [-1 0] [-1 1]))
    ([1 0] . ([1 -1] [1 0] [1 1]))
    ([0 -1] . ([-1 -1] [0 -1] [1 -1]))
    ([0 1] . ([-1 1] [0 1] [1 1]))))

(defun elf-target (elves sides elf)
  (let ((target nil)
        (count 0))
    (for-do ((side sides)
             (:when (--none-p (s. elves (v2+ elf it)) (cdr side))))
      (unless target
        (setf target (v2+ elf (car side))))
      (cl-incf count))
    (and (/= count 4) target)))

(defun step-elves (elves sides)
  (let ((targets (st))
        (counts (st)))
    ;; Compute the new targets.
    (for-do ((e (:st elves))
             (:let ((target (elf-target elves sides e))))
             (:when target))
      (setf (h. targets e) target
            (h. counts target) (1+ (gethash target counts 0))))
    ;; Move the elves.
    (and (not (ht-empty-p targets))
         (let ((next (st)))
           (prog1 next
             (for-do ((e (:st elves))
                      (:let ((target (h. targets e)))))
               (if (or (not target) (/= (h. counts target) 1))
                   (set-add next e)
                 (set-add next target))))))))

(defun simulate-elves (elves &optional rounds)
  (cl-loop for r from 0
           for sides on (-cycle *sides*)
           for last = elves then next
           for next = (step-elves last (cl-subseq sides 0 4))
           while (and next (or (not rounds) (< r rounds)))
           finally (cl-return (list last r))))

(defun count-ground (elves)
  (pcase-let ((`(,vmin ,vmax) (bounds elves)))
    (with-vref
      (- (* (1+ (- vmax.i vmin.i))
            (1+ (- vmax.j vmin.j)))
         (ht-size elves)))))

(defun solve-23-1 (grid)
  (count-ground (car (simulate-elves (elves-create grid) 10))))

(defun solve-23-2 (grid)
  (1+ (cl-second (simulate-elves (elves-create grid)))))
