(defvar *blueprint-regexp*
  (rx-let ((n (group (+ digit))))
    (rx "Blueprint " n ": Each ore robot costs " n " ore. \
Each clay robot costs " n " ore. \
Each obsidian robot costs " n " ore and " n " clay. \
Each geode robot costs " n " ore and " n " obsidian.")))

(defun parse-blueprint (string)
  (seq-let [id ore clay ob1 ob2 geo1 geo2]
      (-map #'int (cdr (s-match *blueprint-regexp* string)))
    (list id `[,ore 0 0 0] `[,clay 0 0 0] `[,ob1 ,ob2 0 0] `[,geo1 0 ,geo2 0])))

(defun read-19 (string)
  (-map #'parse-blueprint (s-split "\n" string t)))

(defun robot-wait (inv rates cost)
  (let ((waits (vmap (lambda (i r c)
                       (let ((m (max 0 (- c i))))
                         (if (zerop m) 0 (and (non-zero-p r) (ceiling m r)))))
                     inv rates cost)))
    (and (cl-every #'identity waits) (1+ (cl-reduce #'max waits)))))

(defun robot-neighbors (time inv rates costs)
  (for ((:let ((maxs (apply #'vmap #'max* costs))
               (n (length inv))))
        [(i (:range n))
         (cost costs)]
        (:let ((wait (robot-wait inv rates cost))))
        (:when (and wait
                    (< wait time)
                    (or (= i (1- n))
                        (< (+ (v. inv i) (* (v. rates i) time))
                           (* (v. maxs i) time)))))
        (:let* ((vwait (make-vector n wait))
                (ninv (v- (v+ inv (v* rates vwait)) cost))
                (nrates (copy-sequence rates)))))
    (cl-incf (v. nrates i))
    (list (- time wait) ninv nrates)))

(defun max-geodes (time costs)
  (let ((n (length costs))
        (max-geodes 0)
        (rec nil))
    (setf rec (memoize
               (lambda (time inv rates)
                 (if (<= (+ (v. inv (1- n))
                            (* (v. rates (1- n)) time)
                            (* (sum-n (1- time))))
                         max-geodes)
                     0
                   (let ((sol (apply
                               #'max 0
                               (for ((s (robot-neighbors time inv rates costs))
                                     (:let ((`(,ntime ,ninv ,nrates) s))))
                                 (+ (* (v. (v- nrates rates) (1- n)) ntime)
                                    (funcall rec ntime ninv nrates))))))
                     (prog1 sol
                       (setf max-geodes (max max-geodes sol))))))))
    (funcall rec time [0 0 0 0] [1 0 0 0])))

(defun/s blueprint-quality ([id &rest costs])
  (* id (max-geodes 24 costs)))

(defun solve-19-1 (blueprints)
  (-sum (-map #'blueprint-quality blueprints)))

(defun solve-19-2 (blueprints)
  (-product (--map (max-geodes 32 (cdr it)) (-take 3 blueprints))))
