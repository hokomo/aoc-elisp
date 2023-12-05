;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (when (require 'aoc-emacs nil t) (aoc-mode 1)); -*-

(require 'aoc-util)
(require 'cl-lib)
(require 's)

(defun read-20 (string)
  (vmap #'int (s-split "\n" string t)))

(definput *test-20-1* #'read-20
  "1
2
-3
3
-2
0
4")

(definput *test-20-2* #'read-20
  "10
-62
10
-66
0
111
2")

(definput *input-20* #'read-20 "day-20-input.txt")

(cl-defstruct (ring-node
               (:constructor ring-node-create (value &optional prev next))
               (:copier nil))
  value prev next)

(defun ring-create (numbers)
  (let ((ring (make-vector (length numbers) nil))
        (len (length numbers)))
    (prog1 ring
      (for-do ((i (:range len)))
        (setf (v. ring i) (ring-node-create (v. numbers i))))
      (for-do ((i (:range len)))
        (setf (ring-node-next (v. ring i)) (v. ring (mod (1+ i) len))
              (ring-node-prev (v. ring i)) (v. ring (mod (1- i) len)))))))

(defun ring-follow (node n)
  (with-sref ring-node
    (cl-loop with cur = node
             repeat (abs n)
             do (setf cur (if (cl-minusp n) cur.prev cur.next))
             finally (cl-return cur))))

(defun ring-mix-number (ring i)
  (let* ((node (v. ring i))
         (value (ring-node-value node)))
    (unless (zerop value)
      (with-sref ring-node
        ;; NOTE: Remove the number from the ring first.
        (setf node.prev.next node.next
              node.next.prev node.prev)
        (let* ((n (if (plusp value) value (1- value)))
               ;; NOTE: Use `%' so that the sign of the dividend is preserved.
               ;; Also, reduce by length - 1, not length, as that's the number
               ;; of *moves* it takes to wrap around (fencepost principle).
               (n (% n (1- (length ring))))
               (nprev (ring-follow node n))
               (nnext nprev.next))
          (setf nprev.next node
                nnext.prev node
                node.next nnext
                node.prev nprev))))))

(cl-defun ring-mix (ring &optional (n 1))
  (prog1 ring
    (for-do ((_ (:range n))
             (i (:range (length ring))))
      (ring-mix-number ring i))))

(defun grove-coordinates (ring)
  (cl-loop with cur = (cl-find 0 ring :key #'ring-node-value)
           repeat 3
           do (setf cur (ring-follow cur 1000))
           sum (ring-node-value cur)))

(defun solve-20-1 (numbers)
  (grove-coordinates (ring-mix (ring-create numbers))))

(expect (solve-20-1 *test-20-1*) 3)
(expect (solve-20-1 *test-20-2*) -118)
;; TODO: Figure out why this thing is so slow, even with the right data
;; structure.
(expect (solve-20-1 *input-20*) 19070)

(defun solve-20-2 (numbers)
  (-> (ring-create (vmap (-cut * <> 811589153) numbers))
      (ring-mix 10)
      grove-coordinates))

(expect (solve-20-2 *test-20-1*) 1623178306)
(expect (solve-20-2 *test-20-2*) 41391046803)
(expect (solve-20-2 *input-20*) 14773357352059)
