;;; We assume each directory is visited exactly (at most and at least) once.

(defun parse-command (lines)
  (seq-let [command &rest output] (s-split "\n" lines t)
    (cons (s-split " " command) output)))

(defun read-07 (string)
  (-map #'parse-command (s-split (rx "$ ") string t)))

(defun reconstruct-tree (commands)
  ;; NOTE: tree ::= (dir <name> . <tree>*) | (file <name> <size>)
  (let ((stack '()))
    (cl-flet ((add (tree)
                ;; Add a tree as a child of the top dir on the stack.
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
                ;; Ignore, since we infer the existence of the directory from
                ;; the corresponding `cd' instruction anyway.
                )
               (`(,size ,name)
                (add `(file ,name ,(int size)))))))))
      ;; Pop all the way to the root entry and return it.
      (cl-loop while (cdr stack)
               do (add (pop stack))
               finally (cl-return (car stack))))))

(defun size-tree (root)
  ;; NOTE: tree ::= (<size> . <tree>*)
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
