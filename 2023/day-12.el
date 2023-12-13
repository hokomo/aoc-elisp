;; -*- lexical-binding: t; aoc-save-slim-p: t; -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 's)

;;; We use a backtracking approach with memoization.
;;;
;;; Each record is represented as a sequence of contiguous ranges containing
;;; only `#' or `?', delimited by one or more `.'. Each range is further
;;; represented using the run-length encoding, as a sequence of counts of each
;;; character in the range.
;;;
;;; E.g. the record
;;;
;;;     ..###..???..??##???##?..
;;;
;;; is represented as the list
;;;
;;;    (((?# . 3))
;;;     ((?? . 3))
;;;     ((?? . 2) (?# . 2) (?? . 3) (?# . 2) (?? . 1)))

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

(defun range-rle (record range)
  (-let (((beg . end) range)
         (prev nil)
         (count 0)
         (rle '()))
    (cl-loop for i from beg below end
             for c = (v. record i)
             do (if (or (not prev) (= c prev))
                    (cl-incf count)
                  (push (cons prev count) rle)
                  (setf count 1))
                (setf prev c))
    (unless (zerop count)
      (push (cons prev count) rle))
    (nreverse rle)))

(defun record-ranges (record)
  (->> (s-matched-positions-all (rx (+ (any "?#"))) record)
       (--map (range-rle record it))))

(defun range-free-p (range)
  (and (= (length range) 1) (= (caar range) ??)))

(defun count-arrangements (record lengths)
  (named-let rec ((ranges (record-ranges record))
                  (lengths lengths))
    (if (not lengths)
        (and (--all-p (range-free-p it) ranges) 1)
      (-let* (((range . ranges) ranges)
              ((length . lengths) lengths)
              (((char . count) . spans) range))
        (cond
         ;; A complete range (all `#'s). The current group *must* fit here.
         ((and (not spans) (= char ?#))
          (and (= count length) (rec ranges lengths)))
         ;; A free range (all `?'). The current group *could* fit here.
         ((and (not spans) (= char ??))
          (if (> length count)
              ;; If the group doesn't fit, we have no choice but to move on
              ;; (effectively assigning all `.'s to this range).
              (rec ranges (cons length lengths))
            ;; Otherwise, the group can be placed at many different starting
            ;; positions. Every `?' to the left of the placement is assigned to
            ;; `.', but all `?' to the right could be used by upcoming groups.
            (+ ;; There are count - length possible placements where the group
               ;; does not touch the end of the range, which means we need to
               ;; make sure to properly delimit the the group with an ending
               ;; `.'. Therefore there are count - length - i - 1 `?'s
               ;; remaining.
               (cl-loop for i from 0 below (- count length)
                        sum (rec `(((?? . ,(- count length i 1))) ,@ranges)
                                 lengths))
               ;; If the group is placed at the end, delimiting it is not
               ;; necessary because ranges are guaranteed to be contiguous and
               ;; not touch.
               (rec ranges lengths))))
         ;; A mixed range (both `#' and `?'). The current group *could* fit in a
         ;; part of this range, in which case we also need an explicit `.' to
         ;; mark its end.
         (t
          ;; TODO
          ))))))

(defun solve-12-1 (records)
  )

(expect (solve-12-1 *test-12*) nil)
(expect (solve-12-1 *input-12*) nil)

(defun solve-12-2 (records)
  )

(expect (solve-12-2 *test-12*) nil)
(expect (solve-12-2 *input-12*) nil)
