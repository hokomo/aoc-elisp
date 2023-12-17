(defun read-14 (string)
  (vecify (s-split "\n" string t)))

(defun tilt-platform-1 (vfn major minor)
  (for-do ((i (:range major))
           (:let ((bottom 0)))
           (j (:range minor))
           (:let* ((ref (funcall vfn i j))
                   (c (gv-deref ref)))))
    (cl-case c
      (?O (setf (gv-deref ref) ?.
                (gv-deref (funcall vfn i bottom)) ?O
                bottom (1+ bottom)))
      (?# (setf bottom (1+ j))))))

(defun tilt-platform (platform dir)
  (prog1 platform
    (pcase-let* ((flipp (with-vref (not (zerop dir.i))))
                 (backwardp (cl-plusp (vsum dir)))
                 (dims (listify (vdims platform)))
                 ((and dims `(,_ ,min)) (if flipp (reverse dims) dims)))
      (apply #'tilt-platform-1
             (lambda (i j)
               (let* ((idxs `[,i ,(if backwardp (- min 1 j) j)])
                      (idxs (if flipp (vrev idxs) idxs)))
                 (gv-ref (v2.. platform idxs))))
             dims))))

(defun copy-platform (platform)
  (vmap #'copy-sequence platform))

(defun platform-north-load (platform)
  (pcase-let ((`[,h ,_] (vdims platform))
              (load 0))
    (with-tensor (r c) platform
      (when (= (v. platform r c) ?O)
        (cl-incf load (- h r))))
    load))

(defun solve-14-1 (platform)
  (platform-north-load (tilt-platform (copy-platform platform) [-1 0])))

(defvar *spin-dirs* '([-1 0] [0 -1] [1 0] [0 1]))

(defun spin-period (platform spins)
  (let ((tilts (* spins (length *spin-dirs*)))
        (seen (ht)))
    (pfor-do ((i (:range 0 nil))
              (dir (-cycle *spin-dirs*))
              (:return (h. seen platform)
                       (list i (- i (h. seen platform))))
              (:return (= i tilts) (list tilts 1)))
      (setf (h. seen (copy-platform platform)) i)
      (tilt-platform platform dir))))

(defun spin-platform (platform spins)
  (prog1 platform
    (-let* ((tilts (* spins (length *spin-dirs*)))
            ((done period) (spin-period platform spins))
            (remaining (mod (- tilts done) period)))
      (pfor-do ((i (:range remaining))
                (dir (nthcdr (mod (+ done i) 4) (-cycle *spin-dirs*))))
        (tilt-platform platform dir)))))

(defun solve-14-2 (platform)
  (platform-north-load (spin-platform (copy-platform platform) 1000000000)))
