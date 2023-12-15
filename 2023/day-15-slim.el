(defun read-15 (string)
  (s-split "," (s-trim string) t))

(defun holiday-hash (current string)
  (cl-reduce (lambda (acc c) (mod (* (+ acc c) 17) 256))
             string :initial-value current))

(defun solve-15-1 (initseq)
  (-sum (-map (-partial #'holiday-hash 0) initseq)))

(defvar *initstep-regexp*
  (rx (group (+ nonl)) (or (seq "=" (group digit)) "-")))

(defun initstep-parts (initstep)
  (seq-let [label focal] (cdr (s-match *initstep-regexp* initstep))
    (cons label (and focal (list (int focal))))))

(defun arrange-lenses (initseq)
  (let ((boxes (make-vector 256 '())))
    (prog1 boxes
      (for-do ((initstep initseq)
               (:let* ((`(,label ,focal) (initstep-parts initstep))
                       (box (holiday-hash 0 label)))))
        (setf (alist-get label (v. boxes box) nil 'remove #'equal) focal)))))

(defun focusing-power (boxes)
  (let ((power 0))
    (for-do ([(position (:range 1 nil))
              (box (:across boxes))]
             [(slot (:range 1 nil))
              ((_ . focal) (reverse box))])
      (cl-incf power (* position slot focal)))
    power))

(defun solve-15-2 (initseq)
  (focusing-power (arrange-lenses initseq)))
