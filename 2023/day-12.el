;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

(require 'aoc-util)
(require 'dash)
(require 's)

;;; For both parts, we use a dynamic programming approach with memoization. Our
;;; state consists of the remaining list of characters and the remaining list of
;;; groups to assign.
;;;
;;; We convert the strings to lists for easier memoization and structure
;;; sharing.

(defun parse-record (line)
  (seq-let [springs damaged] (s-split " " line t)
    (list springs (-map #'int (s-split "," damaged t)))))

(defun read-12 (string)
  (-map #'parse-record (s-split "\n" string t)))

(definput *test-12* #'read-12
  "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

(definput *input-12* #'read-12 "day-12-input.txt")

(defalias 'count-arrangements
  (memoize
   (lambda (list groups)
     (let ((list (--drop-while (= it ?.) list)))
       (cond
        ;; The list is done, so the groups must be as well.
        ((not list)
         (int (not groups)))
        ;; The groups are done, so the rest must not be `#'s.
        ((not groups)
         (int (--all-p (/= it ?#) list)))
        ;; Otherwise, inspect the current non-`.' character.
        (t
         (let ((c (car list))
               (len (car groups)))
           (+
            ;; A group could start at this position, whether because we decided
            ;; so or are forced to do so (i.e. `c' is a `?' or a `#').
            (seq-let [prefix suffix] (-split-at len list)
              (if (and (= (length prefix) len)
                       (--all-p (/= it ?.) prefix)
                       ;; Account for a `.' at the end of a group. We use
                       ;; `equal' to conveniently handle the case when
                       ;; `suffix' is nil.
                       (not (equal (car suffix) ?#)))
                  (count-arrangements (cdr suffix) (cdr groups))
                0))
            ;; Or, we could decide to put a `.' at this position.
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

(expect (solve-12-1 *test-12*) 21)
(expect (solve-12-1 *input-12*) 7732)

(defun solve-12-2 (records)
  (-sum (--map (apply #'count-arrangements (prepare-record it 5)) records)))

(expect (solve-12-2 *test-12*) 525152)
(expect (solve-12-2 *input-12*) 4500070301581)
