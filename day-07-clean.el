(defun parse-command (lines)
  (seq-let [command &rest output] (s-split "\n" lines t)
    (cons (s-split " " command) output)))

(defun read-07 (string)
  (-map #'parse-command (s-split (rx "$ ") string t)))

(defun reconstruct-tree (commands)
  ;; NOTE: tree ::= (dir <name> . <tree>*) | (file <name> <size>)
  (let ((stack '()))
    (cl-flet ((up ()
                ;; Pop the topmost entry on the stack and add it to its parent.
                (pcase-let (((and dir `(dir ,name . ,_)) (pop stack))
                            (`(dir ,_ . ,items) (car stack)))
                  (setf (car (cl-member name items :test #'string=
                                                   :key #'cl-second))
                        dir))))
      (cl-loop for (command . output) in commands do
        (pcase command
          (`("cd" "..")
           (up))
          (`("cd" ,name)
           (push `(dir ,name) stack))
          (`("ls")
           (dolist (line output)
             (push (pcase (s-split " " line)
                     (`("dir" ,name)
                      `(dir ,name))
                     (`(,size ,name)
                      `(file ,name ,(int size))))
                   (cddr (car stack)))))))
      ;; Pop all the way to the root entry and return it.
      (cl-loop while (cdr stack)
               do (up)
               finally (return (car stack))))))

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
