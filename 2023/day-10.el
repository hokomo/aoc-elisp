;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 's)

(defun read-10 (string)
  (vecify (s-split "\n" string t)))

(definput *test-10-1* #'read-10
  "-L|F7
7S-7|
L|7||
-L-J|
L|-JF")

(definput *test-10-2* #'read-10
  "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ")

(definput *test-10-3* #'read-10
  "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........")

(definput *test-10-4* #'read-10
  "..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
..........")

(definput *test-10-5* #'read-10
  ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...")

(definput *test-10-6* #'read-10
  "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L")

(definput *input-10* #'read-10 "day-10-input.txt")

(defun pipes-start (pipes)
  (with-tensor (i j) pipes
    (when (= (v. pipes i j) ?S)
      (cl-return `[,i ,j]))))

(defvar *pipes*
  (ht (?| '([-1 0] [1 0]))
      (?- '([0 -1] [0 1]))
      (?L '([-1 0] [0 1]))
      (?J '([-1 0] [0 -1]))
      (?7 '([1 0] [0 -1]))
      (?F '([1 0] [0 1]))))

(defun pipe-follow (pipe idir)
  (let* ((dirs (h. *pipes* pipe))
         (dirs (cl-remove (v2- idir) dirs :test #'equal)))
    (and (= (length dirs) 1) (cl-first dirs))))

(defun pipes-walk (pipes pos idir)
  (let ((dims (vdims pipes))
        (path '()))
    (pfor-do ((cur (:step pos ncur))
              (:let ((pipe (v2.. pipes cur))))
              (dir (:step idir ndir))
              (:do (push cur path))
              (:return (= pipe ?S) (list (nreverse path) dir))
              (:let ((ndir (pipe-follow pipe dir))))
              (:while ndir)
              (:let ((ncur (v2+ cur ndir))))
              (:while (v2< [-1 -1] ncur dims))))))

(defun pipe-identify (&rest odirs)
  (for-do (((pipe dirs) (:ht *pipes*))
           (:return (= (length (cl-union odirs dirs :test #'equal))
                       (length odirs))
                    pipe))))

(defun pipes-loop (pipes)
  (let ((start (pipes-start pipes)))
    (pfor-do ((dir1 (h. *neighbors-2* 4))
              (:let (((path dir2) (pipes-walk pipes (v2+ start dir1) dir1))))
              (:return path (list path (pipe-identify dir1 (v2- dir2))))))))

(defun solve-10-1 (pipes)
  (/ (length (cl-first (pipes-loop pipes))) 2))

(expect (solve-10-1 *test-10-1*) 4)
(expect (solve-10-1 *test-10-2*) 8)
(expect (solve-10-1 *input-10*) 6701)

(defun pipes-inside-loop (pipes loop initial)
  (pcase-let* ((`[,h ,w] (vdims pipes))
               (inside (st)))
    (prog1 inside
      (for-do ((r (:range 0 (1- h)))
               (:let ((insidep nil)
                      (opening nil)))
               (c (:range 0 (1- w)))
               (:let* ((pos `[,r ,c])
                       (tile (v2.. pipes pos))
                       (tile (if (= tile ?S) initial tile)))))
        (if (s. loop pos)
            (cl-case tile
              (?| (setf insidep (not insidep)))
              ((?F ?L) (setf opening tile))
              (?7 (setf insidep (if (= opening ?F) insidep (not insidep))))
              (?J (setf insidep (if (= opening ?L) insidep (not insidep)))))
          (setf (s. inside pos) insidep))))))

(defun solve-10-2 (pipes)
  (seq-let [loop initial] (pipes-loop pipes)
    (set-size (pipes-inside-loop pipes (setify loop) initial))))

(expect (solve-10-2 *test-10-3*) 4)
(expect (solve-10-2 *test-10-4*) 4)
(expect (solve-10-2 *test-10-5*) 8)
(expect (solve-10-2 *test-10-6*) 10)
(expect (solve-10-2 *input-10*) 303)

(comment
 (defun pipes-formatter (pipes)
   (pcase-let* ((`(,loop ,initial) (pipes-loop pipes))
                (loop (setify loop))
                (inside (pipes-inside-loop pipes loop initial)))
     (lambda (c i j)
       (let* ((pos `[,i ,j])
              (props (append
                      (cl-case c
                        (?S `(display ,(string initial)))
                        (?. `(display ,(cat "■"))))
                      (cond ((s. loop pos)
                             `(font-lock-face (:foreground "red")))
                            ((s. inside pos)
                             `(font-lock-face (:foreground "green")))
                            ((= c ?.)
                             `(font-lock-face (:foreground "cyan")))
                            (t `(display ,(cat " ")))))))
         (apply #'propertize (string c) props)))))

 (defun print-pipes (pipes)
   (print-grid pipes :pad nil :fmt (pipes-formatter pipes)))

 (with-display _ (print-pipes *test-10-1*))
 (with-display _ (print-pipes *test-10-2*))
 (with-display _ (print-pipes *test-10-3*))
 (with-display _ (print-pipes *test-10-4*))
 (with-display _ (print-pipes *test-10-5*))
 (with-display _ (print-pipes *test-10-6*))
 (with-display _ (print-pipes *input-10*)))
