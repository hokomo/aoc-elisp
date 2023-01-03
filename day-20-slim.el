(defun read-20 (string)
  (vmap #'int (s-split "\n" string t)))

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
             do (setf cur (if (minusp n) cur.prev cur.next))
             finally (cl-return cur))))

(defun ring-mix-number (ring i)
  (let* ((node (v. ring i))
         (value (ring-node-value node)))
    (unless (zerop value)
      (with-sref ring-node
        (setf node.prev.next node.next
              node.next.prev node.prev)
        (let* ((n (if (plusp value) value (1- value)))
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

(defun solve-20-2 (numbers)
  (-> (ring-create (vmap (-cut * <> 811589153) numbers))
      (ring-mix 10)
      grove-coordinates))
