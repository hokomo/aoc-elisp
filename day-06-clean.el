(defun read-06 (string)
  (s-trim string))

(defun find-n-different (n string)
  (cl-loop for w in (windows n string)
           for i from 0
           when (= (length (cl-delete-duplicates w)) n)
             return i))

(defun start-of-packet (n signal)
  (+ (find-n-different n signal) n))

(defun solve-06-1 (signal)
  (start-of-packet 4 signal))

(defun solve-06-2 (signal)
  (start-of-packet 14 signal))
