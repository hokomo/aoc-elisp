;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (when (require 'aoc-emacs nil t) (aoc-mode 1)); -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 's)

;;; We assume each directory is visited exactly (at most and at least) once.

(defun parse-command (lines)
  (seq-let [command &rest output] (s-split "\n" lines t)
    (cons (s-split " " command) output)))

(defun read-07 (string)
  (-map #'parse-command (s-split (rx "$ ") string t)))

(definput *test-07* #'read-07
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(definput *input-07* #'read-07 "input-07.txt")

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

(expect (solve-07-1 *test-07*) 95437)
(expect (solve-07-1 *input-07*) 1644735)

(defun solve-07-2 (commands)
  (seq-let [total &rest sizes] (size-tree (reconstruct-tree commands))
    (let ((missing (- 30000000 (- 70000000 total))))
      (->> (-flatten sizes)
           (--filter (>= it missing))
           -min))))

(expect (solve-07-2 *test-07*) 24933642)
(expect (solve-07-2 *input-07*) 1300850)
