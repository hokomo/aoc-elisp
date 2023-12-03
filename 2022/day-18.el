;; -*- lexical-binding: t; eval: (add-to-list 'load-path (expand-file-name "")); eval: (when (require 'aoc-emacs nil t) (aoc-mode 1)); -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 'queue)
(require 's)

;; HACK: For part 2, the input is small enough that we just create a bounding
;; box and fill out the whole exterior using a BFS.

(defun parse-droplet (line)
  (vmap #'int (s-split "," line)))

(defun read-18 (string)
  (-map #'parse-droplet (s-split "\n" string t)))

(definput *test-18* #'read-18
  "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5")

(definput *input-18* #'read-18 "input-18.txt")

(defun droplet-surface (droplets droplet)
  (let ((count 0))
    (for-do ((dir (h. *neighbors-3* 6))
             (:let ((p (v3+ droplet dir))))
             (:when (not (s. droplets p))))
      (cl-incf count))
    count))

(defun solve-18-1 (droplets)
  (let ((droplets (setify droplets)))
    (-sum (-map (-cut droplet-surface droplets <>) (ht-keys droplets)))))

(expect (solve-18-1 *test-18*) 64)
(expect (solve-18-1 *input-18*) 4390)

(defun droplets-exterior (droplets)
  (pcase-let* ((`(,vmin ,vmax) (bounds droplets))
               (vmin (vmap #'1- vmin))
               (vmax (vmap #'1+ vmax))
               (q (make-queue))
               (exterior (st)))
    (prog1 exterior
      (queue-append q vmin)
      (set-add exterior vmin)
      (for-do ((:while (not (queue-empty q)))
               (:let ((p (queue-dequeue q))))
               (dir (h. *neighbors-3* 6))
               (:let ((r (v3+ p dir))))
               (:when (and (v3<= vmin r vmax)
                           (not (s. exterior r))
                           (not (s. droplets r)))))
        (queue-append q r)
        (set-add exterior r)))))

(defun solve-18-2 (droplets)
  (let* ((droplets (setify droplets))
         (exterior (droplets-exterior droplets))
         (count 0))
    (for-do ((p (:st droplets))
             (dir (h. *neighbors-3* 6))
             (:let ((r (v3+ p dir))))
             (:when (and (not (s. droplets r))
                         (s. exterior r))))
      (cl-incf count))
    count))

(expect (solve-18-2 *test-18*) 58)
(expect (solve-18-2 *input-18*) 2534)
