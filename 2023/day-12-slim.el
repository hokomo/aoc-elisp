(defun parse-record (line)
  (seq-let [springs damaged] (s-split " " line t)
    (list springs (-map #'int (s-split "," damaged t)))))

(defun read-12 (string)
  (-map #'parse-record (s-split "\n" string t)))

(defalias 'count-arrangements
  (memoize
   (lambda (list groups)
     (let ((list (--drop-while (= it ?.) list)))
       (cond
        ((not list)
         (int (not groups)))
        ((not groups)
         (int (--all-p (/= it ?#) list)))
        (t
         (let ((c (car list))
               (len (car groups)))
           (+
            (seq-let [prefix suffix] (-split-at len list)
              (if (and (= (length prefix) len)
                       (--all-p (/= it ?.) prefix)
                       (not (equal (car suffix) ?#)))
                  (count-arrangements (cdr suffix) (cdr groups))
                0))
            (if (= c ??)
                (count-arrangements (cdr list) groups)
              0)))))))))

(defun prepare-record (record repetitions)
  (seq-let [string groups] record
    (let ((list (listify string)))
      (list (apply #'append list (-repeat (1- repetitions) (cons ?? list)))
            (apply #'append (-repeat repetitions groups))))))

(defun solve-12-1 (records)
  (-sum (--map (apply #'count-arrangements (prepare-record it 1)) records)))

(defun solve-12-2 (records)
  (-sum (--map (apply #'count-arrangements (prepare-record it 5)) records)))
