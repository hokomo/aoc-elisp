(defun parse-command (lines)
  (seq-let [command &rest output] (s-split "\n" lines t)
    (cons (s-split " " command) output)))

(defun read-07 (string)
  (-map #'parse-command (s-split (rx "$ ") string t)))

(defun reconstruct-tree (commands)
  (let ((stack '()))
    (cl-flet ((add (tree)
                (push tree (cddr (car stack)))))
      (cl-loop for (command . output) in commands do
        (pcase command
          (`("cd" "..")
           (add (pop stack)))
          (`("cd" ,name)
           (push `(dir ,name) stack))
          (`("ls")
           (dolist (line output)
             (pcase (s-split " " line)
               (`("dir" ,_)
                )
               (`(,size ,name)
                (add `(file ,name ,(int size)))))))))
      (cl-loop while (cdr stack)
               do (add (pop stack))
               finally (cl-return (car stack))))))

(defun size-tree (root)
  (pcase root
    (`(file ,_ ,size)
     size)
    (`(dir ,_ . ,items)
     (let-alist (-group-by #'car items)
       (let* ((sizes (-map #'size-tree .dir))
              (total (+ (-sum (-map #'car sizes))
                        (-sum (-map #'cl-third .file)))))
         `(,total ,@sizes))))))

(defun solve-07-1 (commands)
  (->> (-flatten (size-tree (reconstruct-tree commands)))
       (--filter (< it 100000))
       -sum))

(defun solve-07-2 (commands)
  (seq-let [total &rest sizes] (size-tree (reconstruct-tree commands))
    (let ((missing (- 30000000 (- 70000000 total))))
      (->> (-flatten sizes)
           (--filter (>= it missing))
           -min))))
