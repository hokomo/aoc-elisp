(defun parse-crate-line (line)
  (cl-loop for i from 1 below (length line) by 4
           for c = (aref line i)
           collect (and (/= c ?\s) c)))

(defun parse-crates (string)
  (->> (butlast (s-split "\n" string t))
       (-map #'parse-crate-line)
       (apply #'-pad nil)
       transpose
       (cl-map 'vector (-cut remove nil <>))))

(defun parse-move (line)
  (let ((groups (rx-let ((n (group (+ digit))))
                  (s-match (rx "move " n " from " n " to " n) line))))
    (seq-let [n from to] (-map #'int (cdr groups))
      (list n (1- from) (1- to)))))

(defun read-05 (string)
  (seq-let [crates moves] (s-split "\n\n" string)
    (list (parse-crates crates)
          (-map #'parse-move (s-split "\n" moves t)))))

(defun copy-crates (crates)
  (cl-map 'vector #'copy-list crates))

(defun simulate-crates (crates movef moves)
  (let ((crates (copy-crates crates)))
    (prog1 crates
      (mapc (-cut funcall movef crates <>) moves))))

(defun crates-message (crates)
  (cl-map 'string #'cl-first crates))

(defun/s move-individual (crates [n from to])
  ;; HACK: Iterative pop-push. Doubly-linked lists would be nicer.
  (dotimes (_ n)
    (push (pop (aref crates from)) (aref crates to))))

(defun/s solve-05-1 ([crates moves])
  (crates-message (simulate-crates crates #'move-individual moves)))

(defun/s move-together (crates [n from to])
  (let ((end (nthcdr (1- n) (aref crates from))))
    (cl-rotatef (aref crates from) (cdr end) (aref crates to))))

(defun/s solve-05-2 ([crates moves])
  (crates-message (simulate-crates crates #'move-together moves)))
