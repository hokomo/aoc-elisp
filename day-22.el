;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (when (require 'aoc-emacs nil t) (aoc-mode 1)); -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 's)

;;; We use the point [i j] to represent the location in the i-th row and j-th
;;; column, starting from the origin [0 0] in the top-left.
;;;
;;; We don't attempt to implement fully general parsing of an arbitrary cube net
;;; but just assume the pattern will always be as the one given in the input:
;;;
;;; _##
;;; _#_
;;; ##_
;;; #__
;;;
;;; See https://mathworld.wolfram.com/Net.html.

(defun parse-board-grid (string)
  (let* ((lines (s-split "\n" string t))
         (max (-max (-map #'length lines))))
    (vecify (--map (s-pad-right max " " it) lines))))

(defun parse-board-moves (string)
  (let ((regexp (rx (group (or (+ digit) (any "UDLR"))))))
    (for ([(i (:step 0 (match-end 0)))
           (:let ((m (string-match regexp string i))))
           (:while m)])
      (read-one-from-string (match-string 1 string)))))

(defun read-22 (string)
  (seq-let [grid moves] (s-split "\n\n" string t)
    (list (parse-board-grid grid) (parse-board-moves moves))))

(definput *test-22* #'read-22
  "
        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5")

(definput *input-22* #'read-22 "input-22.txt")

(defun map-create (grid)
  (pcase-let ((`[,m ,n] (vdims grid))
              (map (ht))
              (start nil))
    (with-tensor (i j) grid
      (let ((c (v. grid i j))
            (p `[,i ,j]))
        (unless (= c ?\s)
          (setf (h. map p) c)
          (unless start
            (setf start p)))))
    (list map start)))

(defun map-spans (grid)
  (pcase-let ((`[,m ,n] (vdims grid))
              (rowspans (ht))
              (colspans (ht)))
    (with-tensor (i j) grid
      (let ((c (v. grid i j))
            (p `[,i ,j]))
        (unless (= c ?\s)
          ;; Rowspans
          (cl-symbol-macrolet ((rowspan (h. rowspans i)))
            (unless rowspan
              (setf rowspan (vector nil nil)))
            (with-vref
              (cond
               ((or (= j 0) (= (v. grid i (1- j)) ?\s))
                (setf rowspan.a j))
               ((or (= j (1- n)) (= (v. grid i (1+ j)) ?\s))
                (setf rowspan.b j)))))
          ;; Colspans
          (cl-symbol-macrolet ((colspan (h. colspans j)))
            (unless colspan
              (setf colspan (vector nil nil)))
            (with-vref
              (cond
               ((or (= i 0) (= (v. grid (1- i) j) ?\s))
                (setf colspan.a i))
               ((or (= i (1- m)) (= (v. grid (1+ i) j) ?\s))
                (setf colspan.b i))))))))
    (list rowspans colspans)))

(defun map-walk (wrapf map start moves)
  (let ((cur start)
        (dir [0 1]))
    (dolist (m moves)
      (pcase-exhaustive m
        ('L (setf dir (v2ccw dir)))
        ('R (setf dir (v2cw dir)))
        ((pred integerp)
         (dotimes (_ m)
           (pcase-let* ((ncur (v2+ cur dir))
                        (c (h. map ncur))
                        (ncur (if c ncur (funcall wrapf cur ncur dir)))
                        (`(,ncur, ndir) (if (listp ncur) ncur (list ncur dir))))
             (pcase-exhaustive (or c (h. map ncur))
               (?. (setf cur ncur dir ndir))
               (?# (cl-return))))))))
    (list cur dir)))

(defun map-password (cur dir)
  (seq-let [i j] (vmap #'1+ cur)
    (+ (* 1000 i)
       (* 4 j)
       (pcase-exhaustive dir
         (`[0 1] 0)
         (`[0 -1] 2)
         (`[1 0] 1)
         (`[-1 0] 3)))))

(defun grid-wrap-func (grid)
  (seq-let [rowspans colspans] (map-spans grid)
    (lambda (_cur ncur dir)
      (with-vref
        (cl-symbol-macrolet ((rowspan (h. rowspans ncur.i))
                             (colspan (h. colspans ncur.j)))
          (pcase-exhaustive dir
            (`[-1 0] `[,colspan.b ,ncur.j])
            (`[1 0] `[,colspan.a ,ncur.j])
            (`[0 -1] `[,ncur.i ,rowspan.b])
            (`[0 1] `[,ncur.i ,rowspan.a])))))))

(defun/s solve-22-1 ([grid moves])
  (seq-let [map start] (map-create grid)
    (apply #'map-password (map-walk (grid-wrap-func grid) map start moves))))

(expect (solve-22-1 *test-22*) 6032)
(expect (solve-22-1 *input-22*) 80392)

(defun cube-wrap-func (side)
  (let ((vside `[,side ,side]))
    (lambda (cur _ dir)
      (with-vref
        (pcase-let* ((tile (v2/ cur vside))
                     (tcur (v2mod cur vside))
                     (tidx (+ (* tile.x 3) tile.y))
                     (`(,ntidx ,ntcur ,ndir)
                      (pcase-exhaustive `[,tidx ,dir]
                        (`[1 [-1 0]] `(9 [,tcur.j 0] [0 1]))
                        (`[1 [0 -1]] `(6 [,(- side 1 tcur.i) 0] [0 1]))

                        (`[2 [-1 0]] `(9 [,(1- side) ,tcur.j] [-1 0]))
                        (`[2 [0 1]] `(7 [,(- side 1 tcur.i) ,(1- side)] [0 -1]))
                        (`[2 [1 0]] `(4 [,tcur.j ,(1- side)] [0 -1]))

                        (`[4 [0 1]] `(2 [,(1- side) ,tcur.i] [-1 0]))
                        (`[4 [0 -1]] `(6 [0 ,tcur.i] [1 0]))

                        (`[6 [-1 0]] `(4 [,tcur.j 0] [0 1]))
                        (`[6 [0 -1]] `(1 [,(- side 1 tcur.i) 0] [0 1]))

                        (`[7 [0 1]] `(2 [,(- side 1 tcur.i) ,(1- side)] [0 -1]))
                        (`[7 [1 0]] `(9 [,tcur.j ,(1- side)] [0 -1]))

                        (`[9 [0 1]] `(7 [,(1- side) ,tcur.i] [-1 0]))
                        (`[9 [1 0]] `(2 [0 ,tcur.j] [1 0]))
                        (`[9 [0 -1]] `(1 [0 ,tcur.i] [1 0]))))
                     (ntile `[,(/ ntidx 3) ,(mod ntidx 3)])
                     (ncur (v2+ (v2* ntile vside) ntcur)))
          (list ncur ndir))))))

(defun/s solve-22-2 ([grid moves] side)
  (seq-let [map start] (map-create grid)
    (apply #'map-password (map-walk (cube-wrap-func side) map start moves))))

;; NOTE: This solution is not general enough to handle the example.
(comment (expect (solve-22-2 *test-22* 4) 5031))
(expect (solve-22-2 *input-22* 50) 19534)

(comment
 (defun print-board (grid &optional cur dir)
   (let ((dir (and dir (pcase-exhaustive dir
                         (`[-1 0] ?^)
                         (`[1 0] ?v)
                         (`[0 -1] ?<)
                         (`[0 1] ?>)))))
     (print-grid grid :fmt (lambda (c i j)
                             (format "%c"
                                     (if (and cur dir (v2= `[,i ,j] cur))
                                         dir
                                       c))))))

 (defun board-show (grid &optional cur dir)
   (with-display _?
     (print-board grid cur dir)))

 (board-show (first *test-22*)))
