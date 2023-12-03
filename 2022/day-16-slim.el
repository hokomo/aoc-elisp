(defvar *valve-regexp*
  (rx-let ((valve (+ (any "A-Z"))))
    (rx "Valve " (group valve) " has flow rate=" (group (+ digit)) ";"
        " tunnel" (? "s") " lead" (? "s") " to valve" (? "s") " "
        (group (+ valve (? ", "))))))

(defun parse-valve (line)
  (seq-let [valve flow next] (cdr (s-match *valve-regexp* line))
    (list valve (int flow) (s-split ", " next))))

(defun read-16 (string)
  (ht-from-alist (-map #'parse-valve (s-split "\n" string t))))

(defun valve-distances (valves)
  (graph-floyd-warshall
   (graph-create (lambda (node)
                   (seq-let [_ adjacent] (h. valves node)
                     (-map #'list adjacent)))
                 (ht-keys valves))))

(defun useful-valves (valves)
  (for (((node info) (:ht valves))
        (:let ((`(,flow . ,_) info)))
        (:when (not (zerop flow))))
    node))

(defun solve-16-1 (valves)
  (let ((dist (valve-distances valves))
        (useful (useful-valves valves))
        (rec nil))
    (setf rec (memoize
               (lambda (time cur closed)
                 (apply #'max 0
                        (for ((v (:st closed))
                              (:let ((cost (1+ (h. dist cur v)))
                                     (`(,flow . ,_) (h. valves v))))
                              (:when (< cost time)))
                          (+ (* (- time cost) flow)
                             (funcall rec (- time cost) v
                                      (set-without closed v))))))))
    (funcall rec 30 "AA" (setify useful))))

(defun solve-16-2 (valves)
  (let ((dist (valve-distances valves))
        (useful (vecify (useful-valves valves)))
        (rec nil))
    (setf rec (memoize
               (lambda (time cur closed)
                 (apply #'max 0
                        (for ([(mask (:step 1 (* 2 mask)))
                               (v useful)]
                              (:when (non-zero-p (logand closed mask)))
                              (:let ((cost (1+ (h. dist cur v)))
                                     (`(,flow . ,_) (h. valves v))))
                              (:when (< cost time)))
                          (+ (* (- time cost) flow)
                             (funcall rec (- time cost) v
                                      (logand closed (lognot mask)))))))))
    (apply #'max 0
           (let ((bitset (1- (ash 1 (length useful)))))
             (for ((mask (:range 0 (1+ bitset)))
                   (:let ((me mask)
                          (elephant (logand bitset (lognot mask))))))
               (+ (funcall rec 26 "AA" me)
                  (funcall rec 26 "AA" elephant)))))))
